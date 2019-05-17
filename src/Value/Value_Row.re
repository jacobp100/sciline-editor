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
  | [Unresolved(_, i'), ..._] => (None, Some(i'))
  | [] => (current, None)
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
    let (root, error) = parseUnary(rest);
    switch (root) {
    | Some(root) =>
      let root = op == Sub ? AST.neg(root) : root;
      (Some(root), None);
    | None => (None, OptChain.add(error, i'))
    };
  | _ => next(elements)
  };
let next = parseUnary;

let rec parseParenFreeFunctions = elements => {
  let rec iter = (i, after) =>
    switch (after) {
    | [UnresolvedFunction(fn, i'), ...after] =>
      let (arg, error) = parseParenFreeFunctions(after);
      switch (arg) {
      | Some(arg) =>
        ListUtil.takeUpto(elements, i)
        ->Belt.List.concat([Resolved(handleFunction(arg, fn))])
        ->next
      | None => (None, OptChain.add(error, i'))
      };
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
      let (before, e1) = ListUtil.takeUpto(elements, i)->next;
      let (after, e2) = parseMulDiv(after);
      let error = OptChain.flatAdd(e1, e2);
      switch (before, after) {
      | (Some(before), Some(after)) => (
          Some(handleOp(op, before, after)),
          error,
        )
      | _ => (None, OptChain.add(error, i'))
      };
    | [_, ...after] => iter(i + 1, after)
    | [] => next(elements)
    };
  iter(0, elements);
};
let next = parseMulDiv;

let rec parseAddSub = elements => {
  let rec iter = (i, after) =>
    switch (after) {
    | [Unresolved(`Operator((Add | Sub) as op), i'), ...after] =>
      let (before, _) = ListUtil.takeUpto(elements, i)->next;
      switch (before) {
      | Some(before) =>
        let (after, error) = parseAddSub(after);
        switch (after) {
        | Some(after) => (Some(handleOp(op, before, after)), error)
        | None => (None, OptChain.add(error, i'))
        };
      | None =>
        /* Assume unary; handled later */
        iter(i + 1, after)
      };
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
      let (inner, error) = next(inner);
      switch (inner) {
      | Some(arg) =>
        let arg =
          fn
          ->Belt.Option.mapWithDefault(arg, handleFunction(arg))
          ->withSuperscript(superscript);
        handleBrackets(
          Belt.List.concat(before, [Resolved(arg), ...after]),
        );
      | None => (None, OptChain.add(error, i'))
      };
    | (None, [Unresolved(`CloseBracket(_), _), ..._]) => (None, Some(i))
    | (_, [_, ...after]) => iter(openBracketState, i + 1, after)
    | (Some((_, _, i')), []) => (None, Some(i'))
    | (None, []) => next(elements)
    };
  iter(None, 0, elements);
};
let next = handleBrackets;
