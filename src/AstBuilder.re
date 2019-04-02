open Types;
module SciLine = ScilineCalculator.SciLine;

type funcitionLike =
  | GenericFunction(func)
  | NLog(nlog(SciLine.t))
  | Sum(range(SciLine.t))
  | Product(range(SciLine.t));
type partialNode('a) =
  | Resolved(SciLine.t)
  | Unresolved('a, int)
  | UnresolvedFunction(funcitionLike, int);
type reduceState('a) =
  | Row(MutableListBuilder.t(partialNode('a)))
  | ReduceError(int);
type finalState =
  | Node(SciLine.t)
  | Empty
  | Error(int);

let nodeWithSuperscript = (superscript, a) =>
  switch (superscript) {
  | Node(superscript) => `Ok(SciLine.pow(a, superscript))
  | Empty => `Ok(a)
  | Error(i) => `Error(i)
  };
let partialNodeWithSuperscript = (a, superscript) =>
  switch (nodeWithSuperscript(superscript, a)) {
  | `Ok(a) => `Ok(Resolved(a))
  | `Error(_) as err => err
  };

let handleFunction = (arg, fn) =>
  switch (fn) {
  | GenericFunction(Sin) => SciLine.sin(arg)
  | GenericFunction(_) => failwith("TODO")
  | NLog({nlogBase}) =>
    SciLine.div(SciLine.log(arg), SciLine.log(nlogBase))
  | Sum({rangeStart, rangeEnd}) => SciLine.sum(rangeStart, rangeEnd, arg)
  | Product({rangeStart, rangeEnd}) =>
    SciLine.product(rangeStart, rangeEnd, arg)
  };

let overArg = (a, fn, i) =>
  switch (a) {
  | Node(a) => fn(a)
  | Error(_) as e => e
  | Empty => Error(i)
  };
let overArgs2 = (a, b, fn, i) =>
  switch (a, b) {
  | (Node(a), Node(b)) => fn(a, b)
  | (Error(_) as e, _)
  | (_, Error(_) as e) => e
  | (Empty, _) => Error(i)
  | (_, Empty) => Error(i)
  };
let handleOp = (op, a, b) =>
  switch (op) {
  | Add => Node(SciLine.add(a, b))
  | Sub => Node(SciLine.sub(a, b))
  | Mul => Node(SciLine.mul(a, b))
  | Div => Node(SciLine.div(a, b))
  | Dot => Node(SciLine.mul(a, b))
  };

let numberIsValidForBase = (base, atomNucleus) =>
  switch (base, atomNucleus) {
  | (_, "0" | "1")
  | (None | Some(Oct | Hex), "2" | "3" | "4" | "5" | "6" | "7")
  | (None | Some(Hex), "8" | "9")
  | (Some(Hex), "A" | "B" | "C" | "D" | "E" | "F") => true
  | _ => false
  };

type numState('a, 'b) = {
  numBase: option(base),
  numString: string,
  numSup: 'a,
  numHasDecimal: bool,
  imag: bool,
  imagSup: 'a,
  magSup: 'a,
};

let reduceNumberState = (state, element) =>
  switch (state, element) {
  | ({numString: "", numBase: None, imag: false}, `Base(numBase)) =>
    Some({...state, numBase: Some(numBase)})
  | (
      {numSup: Empty, imag: false, magSup: Empty},
      `Digit({atomNucleus, superscript}),
    )
      when numberIsValidForBase(state.numBase, atomNucleus) =>
    Some({
      ...state,
      numString: state.numString ++ atomNucleus,
      numSup: superscript,
    })
  | (
      {numHasDecimal: false, numSup: Empty, imag: false, magSup: Empty},
      `DecimalSeparator,
    ) =>
    Some({...state, numString: state.numString ++ ".", numHasDecimal: true})
  /* Allow 3^2i, 3^2 i^2, but not 3i^2, because that's ambiguous */
  | (
      {numSup: Node(_), magSup: Empty, imag: false},
      `ImaginaryUnit(Empty as superscript),
    )
  | (
      {numSup: Empty, magSup: Empty, imag: false},
      `ImaginaryUnit((Node(_) | Empty) as superscript),
    ) =>
    Some({...state, imagSup: superscript, imag: true})
  | (_, `Magnitude(magSup)) when state.numString != "" =>
    Some({...state, magSup})
  | _ => None
  };
let numberForState = ({numBase, numString, numSup, imag, magSup}) =>
  if ((numBase == None || numString != "")
      && (imag || numString != "")
      && numString != ".") {
    let stringWithBase =
      numBase->Belt.Option.mapWithDefault("", stringOfBase) ++ numString;
    let out =
      switch (stringWithBase, imag) {
      | ("", true) => SciLine.i
      | (s, true) => SciLine.mul(SciLine.of_string(s), SciLine.i)
      | (s, false) => SciLine.of_string(s)
      };
    let out =
      switch (nodeWithSuperscript(numSup, out), magSup) {
      | (`Ok(a), Node(mag)) =>
        `Ok(SciLine.mul(a, SciLine.pow(SciLine.of_int(10), mag)))
      | (`Ok(_) as a, Empty) => a
      | (`Error(_) as e, _) => e
      | (_, Error(i)) => `Error(i)
      };
    Some(out);
  } else {
    None;
  };

let initialNumberState = {
  numBase: None,
  numString: "",
  numSup: Empty,
  numHasDecimal: false,
  imag: false,
  imagSup: Empty,
  magSup: Empty,
};

let rec parseRest = (~current=Empty, elements) =>
  switch (elements) {
  | [Resolved(next), ...rest] =>
    let current =
      switch (current) {
      | Node(a) => Node(SciLine.mul(a, next))
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
    parseFactorials([Resolved(SciLine.factorial(next)), ...rest])
  | _ => next(elements)
  };
let next = parseFactorials;

let parseNumbers = elements => {
  let rec iter = (state, rest) => {
    let nextStateAfter =
      switch (rest) {
      | [Unresolved(element, _), ...after] =>
        reduceNumberState(state, element)->Belt.Option.map(s => (s, after))
      | _ => None
      };
    switch (nextStateAfter) {
    | Some((state, after)) => iter(state, after)
    | None =>
      switch (numberForState(state)) {
      | Some(`Ok(number)) => next([Resolved(number), ...rest])
      | Some(`Error(i)) => Error(i)
      | None => next(elements)
      }
    };
  };
  iter(initialNumberState, elements);
};
let next = parseNumbers;

let rec parseUnary = elements =>
  switch (elements) {
  | [Unresolved(`Operator((Add | Sub) as op), i'), ...rest] =>
    parseUnary(rest)
    ->overArg(arg => Node(op == Sub ? SciLine.neg(arg) : arg), i')
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

let finalize = (elements, _, _) =>
  switch (elements) {
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
let mapElement = (element, i) =>
  switch (element) {
  | `Placeholder(_) => `Error(i)
  | (
      `Base(_) | `Digit(_) | `Magnitude(_) | `ImaginaryUnit(_) | `Operator(_) |
      `Factorial |
      `Degree |
      `ArcMinute |
      `ArcSecond |
      `OpenBracket |
      `CloseBracket(_)
    ) as e =>
    `Ok(Unresolved(e, i))
  | `Function(fn) => `Ok(UnresolvedFunction(GenericFunction(fn), i))
  | `NLog({nlogBase: Node(nlogBase)}) =>
    `Ok(UnresolvedFunction(NLog({nlogBase: nlogBase}), i))
  | `Sum({rangeStart: Node(rangeStart), rangeEnd: Node(rangeEnd)}) =>
    `Ok(UnresolvedFunction(Sum({rangeStart, rangeEnd}), i))
  | `Product({rangeStart: Node(rangeStart), rangeEnd: Node(rangeEnd)}) =>
    `Ok(UnresolvedFunction(Product({rangeStart, rangeEnd}), i))
  | `Variable({atomNucleus, superscript}) =>
    SciLine.variable(atomNucleus)->partialNodeWithSuperscript(superscript)
  | `Constant({constant, superscript}) =>
    let c =
      switch (constant) {
      | Pi => SciLine.pi
      | E => SciLine.e
      };
    c->partialNodeWithSuperscript(superscript);
  | `Frac({fracNum: Node(fracNum), den: Node(den), superscript}) =>
    SciLine.div(fracNum, den)->partialNodeWithSuperscript(superscript)
  | `Abs({absArg: Node(absArg), superscript}) =>
    SciLine.abs(absArg)->partialNodeWithSuperscript(superscript)
  | `Table({tableElements, superscript, numRows, numColumns})
      when tableElements->Belt.Array.every(isNode) =>
    let tableElements = tableElements->Belt.Array.map(toNode);
    SciLine.matrix_of_elements(numRows, numColumns, tableElements)
    ->partialNodeWithSuperscript(superscript);
  | _ => `Error(i)
  };
let reduceFn = (accum, element, i, _) =>
  switch (accum) {
  | ReduceError(_) as e => e
  | Row(list) =>
    switch (mapElement(element, i)) {
    | `Ok(next) => Row(MutableListBuilder.append(list, next))
    | `Error(i) => ReduceError(i)
    }
  };

let parse = (elements: list(Types.t)) => {
  let out =
    elements->TreeUtil.walk(
      Row(MutableListBuilder.empty),
      finalize,
      reduceFn,
    );
  switch (out) {
  | Node(v) => `Ok(v)
  | Empty => `Error(0)
  | Error(i) => `Error(i)
  };
};
