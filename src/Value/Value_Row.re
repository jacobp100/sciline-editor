open Value_Types;
open Value_Builders;

let rec parseRest = (~current=None, elements) =>
  switch (current, elements) {
  | (Some(a), [Resolved(next), ...rest]) =>
    parseRest(~current=Some(AST.mul(a, next)), rest)
  | (None, [Resolved(next), ...rest]) =>
    parseRest(~current=Some(next), rest)
  | (Some(a), [Unresolved(`Percent, _)]) => `Ok(AST.percent(a))
  | (_, [UnresolvedFunction(_, i'), ..._] | [Unresolved(_, i'), ..._]) =>
    `Error(i')
  | (Some(v), []) => `Ok(v)
  | (None, []) => `UnknownError
  };
let next = parseRest;

let rec parsePostfixes = elements =>
  switch (elements) {
  | [
      Resolved(next),
      Unresolved(`UnitConversion({fromUnits, toUnits}), _),
      ...rest,
    ] =>
    parsePostfixes([
      Resolved(AST.convert(next, fromUnits, toUnits)),
      ...rest,
    ])
  | [Resolved(next), Unresolved(`Factorial, _), ...rest] =>
    parsePostfixes([Resolved(AST.factorial(next)), ...rest])
  | [Resolved(next), Unresolved(`Conj, _), ...rest] =>
    parsePostfixes([Resolved(AST.conj(next)), ...rest])
  | _ => next(elements)
  };
let next = parsePostfixes;

let parseNumbers = elements => {
  let next' = (numberState, angleState, rest) => {
    switch (Value_NumberParser.toNode(numberState), angleState) {
    | (None, Some((angleAccum, _))) =>
      next([Resolved(angleAccum), ...rest])
    | (Some(number), None) => next([Resolved(number), ...rest])
    | _ => next(elements)
    };
  };
  let rec iter = (numberState, angleState, rest) => {
    switch (rest) {
    | [Unresolved((`Degree | `ArcMinute | `ArcSecond) as angle, i'), ...rest] =>
      let number =
        switch (Value_NumberParser.toNode(numberState), angle) {
        | (Some(number), `Degree) =>
          Some(AST.mul(number, AST.div(AST.pi, AST.ofInt(180))))
        | (Some(number), `ArcMinute) =>
          Some(AST.mul(number, AST.div(AST.pi, AST.ofInt(10800))))
        | (Some(number), `ArcSecond) =>
          Some(AST.mul(number, AST.div(AST.pi, AST.ofInt(648000))))
        | (None, _) => None
        };
      switch (number, angleState, angle) {
      | (Some(number), None, _) =>
        iter(Value_NumberParser.empty, Some((number, angle)), rest)
      | (Some(number), Some((angleAccum, `Degree)), `ArcMinute | `ArcSecond)
      | (Some(number), Some((angleAccum, `ArcMinute)), `ArcSecond) =>
        iter(
          Value_NumberParser.empty,
          Some((AST.add(angleAccum, number), angle)),
          rest,
        )
      | _ => `Error(i')
      };
    | [Unresolved(element, _), ...after] =>
      switch (Value_NumberParser.reduce(numberState, element)) {
      | Some(s) => iter(s, angleState, after)
      | None => next'(numberState, angleState, rest)
      }
    | _ => next'(numberState, angleState, rest)
    };
  };
  iter(Value_NumberParser.empty, None, elements);
};
let next = parseNumbers;

let rec parseUnary = elements =>
  switch (elements) {
  | [Unresolved((`Add | `Sub) as op, i'), ...rest] =>
    switch (parseUnary(rest)) {
    | `Ok(root) =>
      let root = op == `Sub ? AST.neg(root) : root;
      `Ok(root);
    | `Error(_) as e => e
    | `UnknownError => `Error(i')
    }
  | _ => next(elements)
  };
let next = parseUnary;

let rec parseParenFreeFunctions = elements => {
  let rec iter = (after, i) =>
    switch (after) {
    | [UnresolvedFunction(fn, i'), ...after] =>
      switch (parseParenFreeFunctions(after)) {
      | `Ok(arg) =>
        ListUtil.takeUpto(elements, i)
        ->Belt.List.concat([Resolved(handleFunction(arg, fn))])
        ->next
      | `Error(_) as e => e
      | `UnknownError => `Error(i')
      }
    | [_, ...after] => iter(after, i + 1)
    | [] => next(elements)
    };
  iter(elements, 0);
};
let next = parseParenFreeFunctions;

let binaryOperatorParser = (~operatorHandled, ~next) => {
  let rec iter = (unaryPosition, current, after, i) =>
    switch (after) {
    | [Unresolved(#AST_Types.operatorAtom as op, i'), ...after] =>
      let nextAccum =
        !unaryPosition && operatorHandled(. op)
          ? Some((op, after, i, i')) : current;
      iter(true, nextAccum, after, i + 1);
    | [_, ...after] => iter(false, current, after, i + 1)
    | [] => current
    };
  let rec inner = elements =>
    switch (iter(true, None, elements, 0)) {
    | Some((op, after, i, i')) =>
      switch (ListUtil.takeUpto(elements, i)->inner, next(after)) {
      | (`Ok(before), `Ok(after)) => `Ok(handleOp(op, before, after))
      | (`Error(_) as e, _)
      | (_, `Error(_) as e) => e
      | (`UnknownError, _)
      | (_, `UnknownError) => `Error(i')
      }
    | None => next(elements)
    };
  inner;
};

let parseMulDiv =
  binaryOperatorParser(
    ~operatorHandled=(. op) => op == `Mul || op == `Div || op == `Dot,
    ~next,
  );
let next = parseMulDiv;

let parseAddSub =
  binaryOperatorParser(
    ~operatorHandled=(. op) => op == `Add || op == `Sub,
    ~next,
  );
let next = parseAddSub;

let handleBrackets = elements => {
  let rec iter = (accum, after) =>
    switch (after) {
    | [UnresolvedFunction(fn, _), Unresolved(`OpenBracket, i'), ...after] =>
      iter(Value_BracketAccum.openBracket(accum, i', Some(fn)), after)
    | [Unresolved(`OpenBracket, i'), ...after] =>
      iter(Value_BracketAccum.openBracket(accum, i', None), after)
    | [Unresolved(`CloseBracket(superscript), i'), ...after] =>
      switch (Value_BracketAccum.closeBracket(accum)) {
      | Some((accum, func, elements)) =>
        switch (next(elements)) {
        | `Ok(arg) =>
          let node =
            func
            ->Belt.Option.mapWithDefault(arg, handleFunction(arg))
            ->withSuperscript(superscript)
            ->Resolved;
          iter(Value_BracketAccum.append(accum, node), after);
        | `Error(_) as e => e
        | `UnknownError => `Error(i')
        }
      | None => `Error(i')
      }
    | [element, ...after] =>
      iter(Value_BracketAccum.append(accum, element), after)
    | [] =>
      switch (Value_BracketAccum.toList(accum)) {
      | `Ok(elements) => next(elements)
      | `Error(i) => `Error(i)
      }
    };
  iter(Value_BracketAccum.empty, elements);
};
let next = handleBrackets;