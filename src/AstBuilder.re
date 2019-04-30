open Types;
module AST = ScilineCalculator.ASTTypes;

type funcitionLike =
  | GenericFunction(func)
  | NLog(nlog(AST.t))
  | Sum(range(AST.t))
  | Product(range(AST.t));
type finalState =
  | Node(AST.t)
  | Empty
  | Error(int);
type partialNode =
  | Resolved(AST.t)
  | Unresolved(node(finalState), int)
  | UnresolvedFunction(funcitionLike, int);
type reduceState =
  | Row(MutableListBuilder.t(partialNode))
  | ReduceError(int);

let nodeWithSuperscript = (superscript, a) =>
  switch (superscript) {
  | Node(superscript) => `Ok(AST.pow(a, superscript))
  | Empty => `Ok(a)
  | Error(i) => `Error(i)
  };
let partialNodeWithSuperscript = (a, superscript) =>
  switch (nodeWithSuperscript(superscript, a)) {
  | `Ok(a) => `Ok(Resolved(a))
  | `Error(_) as err => err
  };

let handleGenericFunction = (arg, fn) =>
  switch (fn) {
  | Sin => AST.sin(arg)
  | Arcsin => AST.asin(arg)
  | Sinh => AST.sinh(arg)
  | Arcsinh => AST.asinh(arg)
  | Cos => AST.cos(arg)
  | Arccos => AST.acos(arg)
  | Cosh => AST.cosh(arg)
  | Arccosh => AST.acosh(arg)
  | Tan => AST.tan(arg)
  | Arctan => AST.atan(arg)
  | Tanh => AST.tanh(arg)
  | Arctanh => AST.atanh(arg)
  | Log => AST.log(arg)
  | Re => AST.re(arg)
  | Im => AST.im(arg)
  | Gamma => AST.gamma(arg)
  };
let handleFunction = (arg, fn) =>
  switch (fn) {
  | GenericFunction(fn) => handleGenericFunction(arg, fn)
  | NLog({nlogBase}) => AST.div(AST.log(arg), AST.log(nlogBase))
  | Sum({rangeStart, rangeEnd}) => AST.sum(rangeStart, rangeEnd, arg)
  | Product({rangeStart, rangeEnd}) => AST.product(rangeStart, rangeEnd, arg)
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
  | Add => Node(AST.add(a, b))
  | Sub => Node(AST.sub(a, b))
  | Mul => Node(AST.mul(a, b))
  | Div => Node(AST.div(a, b))
  | Dot => Node(AST.mul(a, b))
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
  numHasDecimal: bool,
  numSup: option(AST.t),
  imag: option(AST.t),
  magSup: option(AST.t),
  degree: option(AST.t),
  arcMin: option(AST.t),
  arcSec: option(AST.t),
};

let rec reduceNumberState = (state, element) =>
  switch (state, element) {
  | ({numString: "", numBase: None, imag: None}, `Base(numBase)) =>
    Some({...state, numBase: Some(numBase)})
  | (
      {numSup: None, imag: None, magSup: None},
      `Digit({atomNucleus, superscript}),
    )
      when numberIsValidForBase(state.numBase, atomNucleus) =>
    let numSup =
      switch (superscript) {
      | Node(v) => Some(v)
      | _ => None
      };
    Some({...state, numString: state.numString ++ atomNucleus, numSup});
  | (
      {numHasDecimal: false, numSup: None, imag: None, magSup: None},
      `DecimalSeparator,
    ) =>
    Some({...state, numString: state.numString ++ ".", numHasDecimal: true})
  /* Allow 3^2i, 3^2 i^2, but not 3i^2, because that's ambiguous */
  | (
      {numSup: Some(_) | None, magSup: None, imag: None},
      `ImaginaryUnit(Empty),
    ) =>
    Some({...state, imag: Some(AST.i)})
  | (
      {numSup: None, magSup: None, imag: None},
      `ImaginaryUnit(Node(imagSuperscript)),
    ) =>
    Some({...state, imag: Some(AST.pow(AST.i, imagSuperscript))})
  | (_, `Magnitude(Node(magSuperscript))) when state.numString != "" =>
    Some({...state, magSup: Some(magSuperscript)})
  | (
      {numSup: None, imag: None, degree: None, arcMin: None, arcSec: None},
      `Degree,
    ) =>
    parseNumber(state)
    ->Belt.Option.map(degree => {
        let degree = degree->AST.mul(AST.div(AST.pi, AST.ofInt(180)))->Some;
        {...initialNumberState, degree};
      })
  | ({numSup: None, imag: None, arcMin: None, arcSec: None}, `ArcMinute) =>
    parseNumber(state)
    ->Belt.Option.map(arcMin => {
        let {degree} = state;
        let arcMin =
          arcMin->AST.mul(AST.div(AST.pi, AST.ofInt(10800)))->Some;
        {...initialNumberState, degree, arcMin};
      })
  | ({numSup: None, imag: None, arcSec: None}, `ArcSecond) =>
    parseNumber(state)
    ->Belt.Option.map(arcSec => {
        let {degree, arcMin} = state;
        let arcSec =
          arcSec->AST.mul(AST.div(AST.pi, AST.ofInt(648000)))->Some;
        {...initialNumberState, degree, arcMin, arcSec};
      })
  | _ => None
  }
and parseNumber = ({numBase, numString, numSup, imag, magSup}) =>
  if ((numBase == None || numString != "")
      && (imag != None || numString != "")
      && numString != ".") {
    let base =
      switch (numBase) {
      | Some(Bin) => 2
      | Some(Oct) => 8
      | Some(Hex) => 16
      | None => 10
      };
    let out = numString == "" ? AST.one : AST.ofStringBase(base, numString);
    let out = numSup->Belt.Option.mapWithDefault(out, AST.pow(out));
    let out =
      magSup
      ->Belt.Option.map(AST.pow(AST.ofInt(10)))
      ->Belt.Option.mapWithDefault(out, AST.mul(out));
    let out = imag->Belt.Option.mapWithDefault(out, AST.mul(out));
    Some(out);
  } else {
    None;
  }
and numberForState = ({degree, arcMin, arcSec} as s) =>
  if (degree == None && arcMin == None && arcSec == None) {
    parseNumber(s);
  } else if (s.numBase == None
             && s.numString == ""
             && s.imag == None
             && s.magSup == None) {
    let out = AST.zero;
    let out = degree->Belt.Option.mapWithDefault(out, AST.add(out));
    let out = arcMin->Belt.Option.mapWithDefault(out, AST.add(out));
    let out = arcSec->Belt.Option.mapWithDefault(out, AST.add(out));
    Some(out);
  } else {
    None;
  }
and initialNumberState = {
  numBase: None,
  numString: "",
  numSup: None,
  numHasDecimal: false,
  imag: None,
  magSup: None,
  degree: None,
  arcMin: None,
  arcSec: None,
};

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
        reduceNumberState(state, element)->Belt.Option.map(s => (s, after))
      | _ => None
      };
    switch (nextStateAfter) {
    | Some((state, after)) => iter(state, after)
    | None =>
      switch (numberForState(state)) {
      | Some(number) => next([Resolved(number), ...rest])
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

let mapValue = ({TreeUtil.accum}): finalState =>
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
let mapElement = (element, i) =>
  switch (element) {
  | `Placeholder(_) => `Error(i)
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
  | `Sum({rangeStart: Node(rangeStart), rangeEnd: Node(rangeEnd)}) =>
    `Ok(UnresolvedFunction(Sum({rangeStart, rangeEnd}), i))
  | `Product({rangeStart: Node(rangeStart), rangeEnd: Node(rangeEnd)}) =>
    `Ok(UnresolvedFunction(Product({rangeStart, rangeEnd}), i))
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
let reduceFn = ({TreeUtil.accum, rangeStart}, element) =>
  switch (accum) {
  | ReduceError(_) as e => e
  | Row(list) =>
    switch (mapElement(element, rangeStart)) {
    | `Ok(next) => Row(MutableListBuilder.append(list, next))
    | `Error(i) => ReduceError(i)
    }
  };

let parse = (elements: list(Types.t)) => {
  let out =
    TreeUtil.map(
      elements,
      Row(MutableListBuilder.empty),
      mapValue,
      reduceFn,
    );
  switch (out) {
  | Node(v) => `Ok(v)
  | Empty => `Error(0)
  | Error(i) => `Error(i)
  };
};
