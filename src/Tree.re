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
  superscriptIndex: int,
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
      (i, (-1), rollup, e);
    | `Placeholder(superscript) =>
      /* Only allow selection after if there's a superscript */
      let i = superscript != [] ? i + 1 : i;
      let (i, rollup, superscript) =
        iterArgs(context, i, rollup, superscript, Superscript);
      (i, (-1), rollup, `Placeholder(superscript));
    | `ImaginaryUnit(superscript) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `ImaginaryUnit(superscript));
    | `Magnitude(exponent) =>
      let (i, rollup, exponent) =
        iterArgs1(context, i, rollup, exponent, Superscript);
      (i, (-1), rollup, `Magnitude(exponent));
    | `CloseBracket(superscript) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `CloseBracket(superscript));
    | `Rand(superscript) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Rand(superscript));
    | `Digit({atomNucleus, superscript}) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Digit({atomNucleus, superscript}));
    | `Variable({atomNucleus, superscript}) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Variable({atomNucleus, superscript}));
    | `Constant({constant, superscript}) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Constant({constant, superscript}));
    | `CustomAtom({customAtomValue, mml, superscript}) =>
      let (i, superscriptIndex, rollup, superscript) =
        iterAtomLike(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `CustomAtom({customAtomValue, mml, superscript}));
    | `Frac({fracNum, den, superscript}) =>
      let (i, rollup, fracNum, den) =
        ((fracNum, FracNum), (den, FracDen))
        ->iterArgs2(context, i, rollup);
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Frac({fracNum, den, superscript}));
    | `Sqrt({rootRadicand, superscript}) =>
      let (i, rollup, rootRadicand) =
        iterArgs1(context, i, rollup, rootRadicand, RootRadicand);
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Sqrt({rootRadicand, superscript}));
    | `NRoot({nrootDegree, radicand, superscript}) =>
      let (i, rollup, nrootDegree, radicand) =
        (
          (nrootDegree, RootDegree),
          (radicand, RootRadicand),
        )
        ->iterArgs2(context, i, rollup);
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `NRoot({nrootDegree, radicand, superscript}));
    | `NLog({nlogBase}) =>
      let (i, rollup, nlogBase) =
        iterArgs1(context, i, rollup, nlogBase, Subscript);
      (i, (-1), rollup, `NLog({nlogBase: nlogBase}));
    | `Abs({unaryArg, superscript}) =>
      let (i, rollup, unaryArg) =
        iterArgs1(context, i, rollup, unaryArg, Inner)
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Abs({unaryArg, superscript}));
    | `Floor({unaryArg, superscript}) =>
      let (i, rollup, unaryArg) =
        iterArgs1(context, i, rollup, unaryArg, Inner)
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Floor({unaryArg, superscript}));
    | `Ceil({unaryArg, superscript}) =>
      let (i, rollup, unaryArg) =
        iterArgs1(context, i, rollup, unaryArg, Inner)
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Ceil({unaryArg, superscript}));
    | `Round({unaryArg, superscript}) =>
      let (i, rollup, unaryArg) =
        iterArgs1(context, i, rollup, unaryArg, Inner)
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Round({unaryArg, superscript}));
    | `RandInt({randIntA, b, superscript}) =>
      let (i, rollup, randIntA, b) =
        ((randIntA, Subscript), (b, Subscript))
        ->iterArgs2(context, i, rollup);
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `RandInt({randIntA, b, superscript}));
    | `NPR({statN, r}) =>
      let (i, rollup, statN, r) =
        ((statN, Superscript), (r, Subscript))
        ->iterArgs2(context, i, rollup);
      (i, (-1), rollup, `NPR({statN, r}));
    | `NCR({statN, r}) =>
      let (i, rollup, statN, r) =
        ((statN, Superscript), (r, Subscript))
        ->iterArgs2(context, i, rollup);
      (i, (-1), rollup, `NCR({statN, r}));
    | `Differential({body, differentialX}) =>
      let (i, rollup, body, differentialX) =
        ((body, Inner), (differentialX, Subscript))
        ->iterArgs2(context, i, rollup);
      (i, (-1), rollup, `Differential({body, differentialX}));
    | `Integral({integralA, b, body}) =>
      let (i, rollup, integralA, b, body) =
        ((integralA, Upper), (b, Lower), (body, Inner))
        ->iterArgs3(context, i, rollup);
      (i, (-1), rollup, `Integral({integralA, b, body}));
    | `Sum({rangeStart, rangeEnd}) =>
      let (i, rollup, rangeStart, rangeEnd) =
        ((rangeStart, Lower), (rangeEnd, Upper))
        ->iterArgs2(context, i, rollup);
      (i, (-1), rollup, `Sum({rangeStart, rangeEnd}));
    | `Product({rangeStart, rangeEnd}) =>
      let (i, rollup, rangeStart, rangeEnd) =
        ((rangeStart, Lower), (rangeEnd, Upper))
        ->iterArgs2(context, i, rollup);
      (i, (-1), rollup, `Product({rangeStart, rangeEnd}));
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
      let (i, superscriptIndex, rollup, superscript) =
        iterSuperscript(context, i, rollup, superscript);
      (i, superscriptIndex, rollup, `Table({...t, tableElements, superscript}));
    }
  and iter =
      (
        context: 'context,
        rangeStart: int,
        rollup: 'rollup,
        elements: list(t),
      ) => {
    let reduceFn = ((rangeStart, rollup, accum), element: t) => {
      let (rangeEnd, superscriptIndex, rollup, element) =
        recurseElement(context, rangeStart, rollup, element);
      let (rollup, accum) =
        reduceFn(
          {accum, rollup, context, rangeStart, rangeEnd, superscriptIndex},
          element,
        );
      (rangeEnd, rollup, accum);
    };

    let (rangeEnd, rollup, accum) =
      elements->Belt.List.reduce(
        (rangeStart, rollup, initialValue),
        reduceFn,
      );
    let (rollup, accumulated) =
      mapValue({
        accum,
        rollup,
        context,
        rangeStart,
        rangeEnd,
        superscriptIndex: (-1),
      });
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
    let i = i + 1;
    iterSuperscript(context, i, rollup, superscript);
  }
  and iterSuperscript = (context, i, rollup, superscript) => {
    let superscriptIndex = superscript != [] ? i : (-1);
    let i = superscript != [] ? i + 1 : i;
    let (i, rollup, superscript) =
      iterArgs(context, i, rollup, superscript, Superscript);
    (i, superscriptIndex, rollup, superscript);
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
