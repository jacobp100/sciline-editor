open Types;

type selector =
  | Superscript
  | Subscript
  | Upper
  | Lower
  | Inner
  | FracNum
  | FracDen
  | RootRadicand
  | RootDegree
  | TableCell;

type iteration('accum, 'rollup, 'context) = {
  accum: 'accum,
  rollup: 'rollup,
  context: 'context,
  rangeStart: int,
  rangeEnd: int,
};

let walkI =
    (
      ~initialRollup: 'rollup,
      ~mapContext: ('context, selector) => 'context=(ctx, _) => ctx,
      ~initialContext: 'context,
      elements: list(t),
      initialValue: 'accum,
      mapValue: iteration('accum, 'rollup, 'context) => ('rollup, 'value),
      reduceFn:
        (iteration('accum, 'rollup, 'context), node('value)) =>
        ('rollup, 'accum),
    ) => {
  let rec recurseElement = (context: 'context, i, rollup: 'rollup, element: t) =>
    switch (element) {
    | (
        `Base(_) | `Operator(_) | `Function(_) | `OpenBracket |
        `DecimalSeparator |
        `Conj |
        `Factorial |
        `Degree |
        `ArcMinute |
        `ArcSecond
      ) as e =>
      let i = i + 1;
      (i, rollup, e);
    | `Placeholder(superscript) =>
      /* Only allow selection after if there's a superscript */
      let i = superscript != [] ? i + 1 : i;
      let (i, rollup, superscript) =
        iterArgs(context, i, rollup, superscript, Superscript);
      (i, rollup, `Placeholder(superscript));
    | `ImaginaryUnit(superscript) =>
      let (i, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, rollup, `ImaginaryUnit(superscript));
    | `Magnitude(exponent) =>
      let (i, rollup, exponent) =
        iterArgs1(context, i, rollup, exponent, Superscript);
      (i, rollup, `Magnitude(exponent));
    | `CloseBracket(superscript) =>
      let (i, rollup, superscript) =
        iterArgs1(context, i, rollup, superscript, Superscript);
      (i, rollup, `CloseBracket(superscript));
    | `Rand(superscript) =>
      let (i, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, rollup, `Rand(superscript));
    | `Digit({atomNucleus, superscript}) =>
      let (i, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, rollup, `Digit({atomNucleus, superscript}));
    | `Variable({atomNucleus, superscript}) =>
      let (i, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, rollup, `Variable({atomNucleus, superscript}));
    | `Constant({constant, superscript}) =>
      let (i, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, rollup, `Constant({constant, superscript}));
    | `CustomAtom({customAtomValue, mml, superscript}) =>
      let (i, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, rollup, `CustomAtom({customAtomValue, mml, superscript}));
    | `Frac({fracNum, den, superscript}) =>
      let (i, rollup, fracNum, den, superscript) =
        ((fracNum, FracNum), (den, FracDen), (superscript, Superscript))
        ->iterArgs3(context, i, rollup);
      (i, rollup, `Frac({fracNum, den, superscript}));
    | `Sqrt({rootRadicand, superscript}) =>
      let (i, rollup, rootRadicand, superscript) =
        ((rootRadicand, RootRadicand), (superscript, Superscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Sqrt({rootRadicand, superscript}));
    | `NRoot({nrootDegree, radicand, superscript}) =>
      let (i, rollup, nrootDegree, radicand, superscript) =
        (
          (nrootDegree, RootDegree),
          (radicand, RootRadicand),
          (superscript, Subscript),
        )
        ->iterArgs3(context, i, rollup);
      (i, rollup, `NRoot({nrootDegree, radicand, superscript}));
    | `NLog({nlogBase}) =>
      let (i, rollup, nlogBase) =
        iterArgs1(context, i, rollup, nlogBase, Subscript);
      (i, rollup, `NLog({nlogBase: nlogBase}));
    | `Abs({unaryArg, superscript}) =>
      let (i, rollup, unaryArg, superscript) =
        ((unaryArg, Inner), (superscript, Superscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Abs({unaryArg, superscript}));
    | `Floor({unaryArg, superscript}) =>
      let (i, rollup, unaryArg, superscript) =
        ((unaryArg, Inner), (superscript, Superscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Floor({unaryArg, superscript}));
    | `Ceil({unaryArg, superscript}) =>
      let (i, rollup, unaryArg, superscript) =
        ((unaryArg, Inner), (superscript, Superscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Ceil({unaryArg, superscript}));
    | `Round({unaryArg, superscript}) =>
      let (i, rollup, unaryArg, superscript) =
        ((unaryArg, Inner), (superscript, Superscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Round({unaryArg, superscript}));
    | `RandInt({randIntA, b, superscript}) =>
      let (i, rollup, randIntA, b, superscript) =
        ((randIntA, Subscript), (b, Subscript), (superscript, Subscript))
        ->iterArgs3(context, i, rollup);
      (i, rollup, `RandInt({randIntA, b, superscript}));
    | `NPR({statN, r}) =>
      let (i, rollup, statN, r) =
        ((statN, Superscript), (r, Subscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `NPR({statN, r}));
    | `NCR({statN, r}) =>
      let (i, rollup, statN, r) =
        ((statN, Superscript), (r, Subscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `NCR({statN, r}));
    | `Differential({body, differentialX}) =>
      let (i, rollup, body, differentialX) =
        ((body, Inner), (differentialX, Subscript))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Differential({body, differentialX}));
    | `Integral({integralA, b, body}) =>
      let (i, rollup, integralA, b, body) =
        ((integralA, Upper), (b, Lower), (body, Inner))
        ->iterArgs3(context, i, rollup);
      (i, rollup, `Integral({integralA, b, body}));
    | `Sum({rangeStart, rangeEnd}) =>
      let (i, rollup, rangeStart, rangeEnd) =
        ((rangeStart, Lower), (rangeEnd, Upper))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Sum({rangeStart, rangeEnd}));
    | `Product({rangeStart, rangeEnd}) =>
      let (i, rollup, rangeStart, rangeEnd) =
        ((rangeStart, Lower), (rangeEnd, Upper))
        ->iterArgs2(context, i, rollup);
      (i, rollup, `Product({rangeStart, rangeEnd}));
    | `Table({tableElements, superscript} as t) =>
      let i = i + 1;
      let ((i, rollup), tableElements) =
        tableElements->ArrayUtil.foldMap(
          (i, rollup),
          ((i, rollup), element) => {
            let (i, rollup, a) =
              iterArgs(context, i, rollup, element, TableCell);
            ((i, rollup), a);
          },
        );
      let (i, rollup, superscript) =
        iterArgs(context, i, rollup, superscript, Superscript);
      (i, rollup, `Table({...t, tableElements, superscript}));
    }
  and iter =
      (
        context: 'context,
        rangeStart: int,
        rollup: 'rollup,
        elements: list(t),
      ) => {
    let reduceFn = ((i, rollup, accum), element: t) => {
      let (i', rollup, element) =
        recurseElement(context, i, rollup, element);
      let (rollup, accum) =
        reduceFn(
          {accum, rollup, rangeStart: i, rangeEnd: i', context},
          element,
        );
      (i', rollup, accum);
    };

    let (rangeEnd, rollup, accum) =
      elements->Belt.List.reduce(
        (rangeStart, rollup, initialValue),
        reduceFn,
      );
    let (rollup, accumulated) =
      mapValue({accum, rollup, rangeStart, rangeEnd, context});
    (rangeEnd, rollup, accumulated);
  }
  and iterArgs =
      (
        context: 'context,
        i: int,
        rollup: 'rollup,
        a: list(t),
        selector: selector,
      )
      : (int, 'rollup, 'a) => {
    let indexIncr = a != [] ? 1 : 0;
    let context' = mapContext(context, selector);
    let (finalIndex, rollup, a) = iter(context', i, rollup, a);
    (finalIndex + indexIncr, rollup, a);
  }
  and iterArgs1 = (context, i, rollup, a, aSelector) => {
    let i = i + 1;
    let (i, rollup, a) = iterArgs(context, i, rollup, a, aSelector);
    (i, rollup, a);
  }
  and iterArgs2 = (((a, aSelector), (b, bSelector)), context, i, rollup) => {
    let i = i + 1;
    let (i, rollup, a) = iterArgs(context, i, rollup, a, aSelector);
    let (i, rollup, b) = iterArgs(context, i, rollup, b, bSelector);
    (i, rollup, a, b);
  }
  and iterArgs3 =
      (
        ((a, aSelector), (b, bSelector), (c, cSelector)),
        context,
        i,
        rollup,
      ) => {
    let i = i + 1;
    let (i, rollup, a) = iterArgs(context, i, rollup, a, aSelector);
    let (i, rollup, b) = iterArgs(context, i, rollup, b, bSelector);
    let (i, rollup, c) = iterArgs(context, i, rollup, c, cSelector);
    (i, rollup, a, b, c);
  }
  and iterAtomLike = (context, i, rollup, superscript) => {
    let i = superscript != [] ? i + 1 : i;
    let (i, rollup, superscript) =
      iterArgs1(context, i, rollup, superscript, Superscript);
    (i, rollup, superscript);
  };

  iter(initialContext, 0, initialRollup, elements);
};

let map = (elements, initialValue, mapValue, reduceFn) => {
  let (_, _, out) =
    walkI(
      ~initialRollup=(),
      ~initialContext=(),
      elements,
      initialValue,
      x => ((), mapValue(x)),
      (x, element) => ((), reduceFn(x, element)),
    );
  out;
};

let rollup =
    (
      ~initialRollup: 'rollup,
      ~mapContext=?,
      ~initialContext: 'context,
      elements,
      initialValue,
      mapValue,
      reduceFn,
    ) => {
  let (_, rollup, out) =
    walkI(
      ~initialRollup,
      ~mapContext?,
      ~initialContext,
      elements,
      initialValue,
      mapValue,
      reduceFn,
    );
  (rollup, out);
};

let length = elements => {
  let (length, _, _) =
    walkI(
      ~initialRollup=(),
      ~initialContext=(),
      elements,
      (),
      _ => ((), ()),
      (_, _) => ((), ()),
    );
  length;
};
