open AST_ReduceMap;
open Value_Types;
open Value_Builders;

let rec parseRest = (~current=Empty, elements) =>
  switch (elements) {
  | [Resolved(next), ...rest] =>
    let current =
      switch (current) {
      | Node(a) => Node(AST.mul(a, next))
      | Empty => Node(next)
      | Error(_) as e => e
      };
    parseRest(~current, rest);
  | [UnresolvedFunction(_, i'), ..._]
  | [Unresolved(_, i'), ..._] => Error(i')
  | [] => current
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
      switch (Value_NumberParser.get(state)) {
      | Some(number) => next([Resolved(number), ...rest])
      | None => next(elements)
      }
    };
  };
  iter(Value_NumberParser.empty, elements);
};
let next = parseNumbers;

let rec parseUnary = elements =>
  switch (elements) {
  | [Unresolved(`Operator((Add | Sub) as op), i'), ...rest] =>
    parseUnary(rest)
    ->overArg(arg => Node(op == Sub ? AST.neg(arg) : arg), i')
  | _ => next(elements)
  };
let next = parseUnary;

let rec parseParenFreeFunctions = elements => {
  let rec iter = (i, after) =>
    switch (after) {
    | [UnresolvedFunction(fn, i'), ...after] =>
      parseParenFreeFunctions(after)
      ->overArg(
          arg =>
            ListUtil.takeUpto(elements, i)
            ->Belt.List.concat([Resolved(handleFunction(arg, fn))])
            ->next,
          i',
        )
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
      let before = ListUtil.takeUpto(elements, i);
      overArgs2(next(before), parseMulDiv(after), handleOp(op), i');
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
      switch (ListUtil.takeUpto(elements, i)->next) {
      | Node(before) =>
        parseAddSub(after)->overArg(handleOp(op, before), i')
      | _ =>
        /* Assume unary; handled later */
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
      | Node(arg) =>
        let arg =
          fn
          ->Belt.Option.mapWithDefault(arg, handleFunction(arg))
          ->partialNodeWithSuperscript(superscript);
        switch (arg) {
        | `Ok(arg) =>
          handleBrackets(Belt.List.concat(before, [arg, ...after]))
        | `Error(i) => Error(i)
        };
      | Empty => Error(i')
      | Error(_) as e => e
      };
    | (None, [Unresolved(`CloseBracket(_), _), ..._]) => Error(i)
    | (_, [_, ...after]) => iter(openBracketState, i + 1, after)
    | (Some((_, _, i')), []) => Error(i')
    | (None, []) => next(elements)
    };
  iter(None, 0, elements);
};
let next = handleBrackets;

let map = (accum, _): finalState =>
  switch (accum) {
  | Row(nodes) => next(MutableListBuilder.toList(nodes))
  | ReduceError(i) => Error(i)
  };
let isNode = element =>
  switch (element) {
  | Node(_) => true
  | _ => false
  };
let toNode = element =>
  switch (element) {
  | Node(arg) => arg
  | _ => failwith("Not a node")
  };
let mapElement = (element: t('a), i) =>
  switch (element) {
  | `Superscript(_) => `Error(i)
  | (
      `Base(_) | `Operator(_) | `OpenBracket | `DecimalSeparator | `Factorial |
      `Conj |
      `Degree |
      `ArcMinute |
      `ArcSecond |
      `ImaginaryUnit(_) |
      `Magnitude(_) |
      `CloseBracket(_) |
      `Digit(_)
    ) as e =>
    `Ok(Unresolved(e, i))
  | `Function(fn) => `Ok(UnresolvedFunction(GenericFunction(fn), i))
  | `NLog({nlogBase: Node(nlogBase)}) =>
    `Ok(UnresolvedFunction(NLog({nlogBase: nlogBase}), i))
  | `Rand(superscript) => AST.rand()->partialNodeWithSuperscript(superscript)
  | `RandInt({randIntA: Node(randIntA), b: Node(b), superscript}) =>
    AST.randInt(randIntA, b)->partialNodeWithSuperscript(superscript)
  | `NPR({statN: Node(statN), r: Node(r)}) =>
    `Ok(Resolved(AST.nPr(statN, r)))
  | `NCR({statN: Node(statN), r: Node(r)}) =>
    `Ok(Resolved(AST.nCr(statN, r)))
  | `Differential({differentialX: Node(differentialX), body: Node(body)}) =>
    `Ok(Resolved(AST.differential(differentialX, body)))
  | `Integral({integralA: Node(integralA), b: Node(b), body: Node(body)}) =>
    `Ok(Resolved(AST.integral(integralA, b, body)))
  | `Sum({
      iterationStart: Node(iterationStart),
      iterationEnd: Node(iterationEnd),
    }) =>
    `Ok(UnresolvedFunction(Sum({iterationStart, iterationEnd}), i))
  | `Product({
      iterationStart: Node(iterationStart),
      iterationEnd: Node(iterationEnd),
    }) =>
    `Ok(UnresolvedFunction(Product({iterationStart, iterationEnd}), i))
  | `Variable({atomNucleus, superscript}) =>
    AST.variable(atomNucleus)->partialNodeWithSuperscript(superscript)
  | `CustomAtom({customAtomValue, superscript}) =>
    AST.ofEncoded(customAtomValue)->partialNodeWithSuperscript(superscript)
  | `Constant({constant, superscript}) =>
    let c =
      switch (constant) {
      | Pi => AST.pi
      | E => AST.e
      };
    c->partialNodeWithSuperscript(superscript);
  | `Frac({fracNum: Node(fracNum), den: Node(den), superscript}) =>
    AST.div(fracNum, den)->partialNodeWithSuperscript(superscript)
  | `Abs({unaryArg: Node(unaryArg), superscript}) =>
    AST.abs(unaryArg)->partialNodeWithSuperscript(superscript)
  | `Floor({unaryArg: Node(unaryArg), superscript}) =>
    AST.floor(unaryArg)->partialNodeWithSuperscript(superscript)
  | `Ceil({unaryArg: Node(unaryArg), superscript}) =>
    AST.ceil(unaryArg)->partialNodeWithSuperscript(superscript)
  | `Round({unaryArg: Node(unaryArg), superscript}) =>
    AST.round(unaryArg)->partialNodeWithSuperscript(superscript)
  | `Sqrt({rootRadicand: Node(rootRadicand), superscript}) =>
    AST.sqrt(rootRadicand)->partialNodeWithSuperscript(superscript)
  | `NRoot({
      nrootDegree: Node(nrootDegree),
      radicand: Node(radicand),
      superscript,
    }) =>
    AST.pow(radicand, AST.div(AST.one, nrootDegree))
    ->partialNodeWithSuperscript(superscript)
  | `Table({tableElements, superscript, numRows, numColumns})
      when tableElements->Belt.Array.every(isNode) =>
    let tableElements = tableElements->Belt.Array.map(toNode);
    /* FIXME */
    let matrix =
      switch (numRows, numColumns) {
      | (2, 1) => AST.vector2(tableElements[0], tableElements[1])
      | (3, 1) =>
        AST.vector3(tableElements[0], tableElements[1], tableElements[2])
      | (2, 2) =>
        AST.matrix2(
          tableElements[0],
          tableElements[1],
          tableElements[2],
          tableElements[3],
        )
      | (3, 3) =>
        AST.matrix3(
          tableElements[0],
          tableElements[1],
          tableElements[2],
          tableElements[3],
          tableElements[4],
          tableElements[5],
          tableElements[6],
          tableElements[7],
          tableElements[8],
        )
      | _ => invalid_arg("create_matrix")
      };
    matrix->partialNodeWithSuperscript(superscript);
  | `NLog(_)
  | `NPR(_)
  | `NCR(_)
  | `Integral(_)
  | `Differential(_)
  | `Sum(_)
  | `Product(_)
  | `Frac(_)
  | `Abs(_)
  | `Floor(_)
  | `Ceil(_)
  | `Round(_)
  | `Sqrt(_)
  | `NRoot(_)
  | `RandInt(_)
  | `Table(_) => `Error(i)
  };
let reduce = (accum, element, (rangeStart, _, _)) =>
  switch (accum) {
  | ReduceError(_) as e => e
  | Row(list) =>
    switch (mapElement(element, rangeStart)) {
    | `Ok(next) => Row(MutableListBuilder.append(list, next))
    | `Error(i) => ReduceError(i)
    }
  };

let parse = (elements: array(AST_Types.t)) => {
  let out =
    reduceMap(
      elements,
      ~reduce,
      ~map,
      ~initial=Row(MutableListBuilder.empty),
    );
  switch (out) {
  | Node(v) => `Ok(v)
  | Empty => `Error(0)
  | Error(i) => `Error(i)
  };
};
