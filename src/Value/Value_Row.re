open Value_Types;
open Value_Builders;

let rec parseRest = (~current=None, elements) =>
  switch (elements) {
  | [Resolved(next), ...rest] =>
    let current =
      switch (current) {
      | Some(a) => Some(AST.mul(a, next))
      | None => Some(next)
      };
    parseRest(~current, rest);
  | [UnresolvedFunction(_, i'), ..._]
  | [Unresolved(_, i'), ..._] => `Error(i')
  | [] =>
    switch (current) {
    | Some(v) => `Ok(v)
    | None => `UnknownError
    }
  };
let next = parseRest;

let rec parseFactorials = elements =>
  switch (elements) {
  | [Resolved(next), Unresolved(`Factorial, _), ...rest] =>
    parseFactorials([Resolved(AST.factorial(next)), ...rest])
  | [Resolved(next), Unresolved(`Conj, _), ...rest] =>
    parseFactorials([Resolved(AST.conj(next)), ...rest])
  | _ => next(elements)
  };
let next = parseFactorials;

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
        switch (Value_NumberParser.get(state)) {
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
  | [Unresolved(`Operator((Add | Sub) as op), i'), ...rest] =>
    switch (parseUnary(rest)) {
    | `Ok(root) =>
      let root = op == Sub ? AST.neg(root) : root;
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
    | [Unresolved(`Operator((Mul | Div | Dot) as op), i'), ...after] =>
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
    | [Unresolved(`Operator((Add | Sub) as op), _), ...after] =>
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

let rec handleBrackets = elements => {
  let rec iter = (openBracketState, i, after) =>
    switch (openBracketState, after) {
    | (
        _,
        [UnresolvedFunction(fn, _), Unresolved(`OpenBracket, i'), ...after],
      ) =>
      iter(Some((Some(fn), i, i')), i + 2, after)
    | (_, [Unresolved(`OpenBracket, i'), ...after]) =>
      iter(Some((None, i, i')), i + 1, after)
    | (
        Some((fn, openIndex, _)),
        [Unresolved(`CloseBracket(superscript), i'), ...after],
      ) =>
      let before = elements->ListUtil.takeUpto(i);
      let (before, inner) =
        before
        ->Belt.List.splitAt(openIndex)
        ->Belt.Option.getWithDefault((before, []));
      let inner = Belt.List.tailExn(inner);
      let inner = fn != None ? Belt.List.tailExn(inner) : inner;
      switch (next(inner)) {
      | `Ok(arg) =>
        let arg =
          fn
          ->Belt.Option.mapWithDefault(arg, handleFunction(arg))
          ->withSuperscript(superscript);
        handleBrackets(
          Belt.List.concat(before, [Resolved(arg), ...after]),
        );
      | `Error(_) as e => e
      | `UnknownError => `Error(i')
      };
    | (None, [Unresolved(`CloseBracket(_), _), ..._]) => `Error(i)
    | (_, [_, ...after]) => iter(openBracketState, i + 1, after)
    | (Some((_, _, i')), []) => `Error(i')
    | (None, []) => next(elements)
    };
  iter(None, 0, elements);
};
let next = handleBrackets;
