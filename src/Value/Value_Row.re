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
  let rec iter = (state, rest) => {
    let nextStateAfter =
      switch (rest) {
      | [Unresolved(element, _), ...after] =>
        Value_NumberParser.reduce(state, element)
        ->Belt.Option.map(s => (s, after))
      | _ => None
      };
    switch (nextStateAfter) {
    | Some((state, after)) => iter(state, after)
    | None =>
      let nextElements =
        switch (Value_NumberParser.toNode(state)) {
        | Some(number) => [Resolved(number), ...rest]
        | None => elements
        };
      next(nextElements);
    };
  };
  iter(Value_NumberParser.empty, elements);
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
  let rec iter = (i, after) =>
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
    | [_, ...after] => iter(i + 1, after)
    | [] => next(elements)
    };
  iter(0, elements);
};
let next = parseParenFreeFunctions;

let rec parseMulDiv = elements => {
  let rec iter = (i, after) =>
    switch (after) {
    | [Unresolved((`Mul | `Div | `Dot) as op, i'), ...after] =>
      switch (ListUtil.takeUpto(elements, i)->next, parseMulDiv(after)) {
      | (`Ok(before), `Ok(after)) => `Ok(handleOp(op, before, after))
      | (`Error(_) as e, _)
      | (_, `Error(_) as e) => e
      | (`UnknownError, _)
      | (_, `UnknownError) => `Error(i')
      }
    | [_, ...after] => iter(i + 1, after)
    | [] => next(elements)
    };
  iter(0, elements);
};
let next = parseMulDiv;

let rec parseAddSub = elements => {
  let rec iter = (i, after) =>
    switch (after) {
    | [Unresolved((`Add | `Sub) as op, _), ...after] =>
      switch (ListUtil.takeUpto(elements, i)->next, parseAddSub(after)) {
      | (`Ok(before), `Ok(after)) => `Ok(handleOp(op, before, after))
      | _ =>
        /* Assume this was unary operator. Ignore it and try to handle it later */
        iter(i + 1, after)
      }
    | [_, ...after] => iter(i + 1, after)
    | [] => next(elements)
    };
  iter(0, elements);
};
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