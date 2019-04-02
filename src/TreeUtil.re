open Types;

let walkI = (elements, initValue, finalize, fn) => {
  let rec reduceFn = ((i, accum), element: t) =>
    switch (element) {
    | (
        `Base(_) | `Operator(_) | `Function(_) | `OpenBracket |
        `DecimalSeparator |
        `Factorial |
        `Degree |
        `ArcMinute |
        `ArcSecond
      ) as e =>
      let i' = i + 1;
      let accum = fn(accum, e, i, i');
      (i', accum);
    | `Placeholder(superscript) =>
      /* Only allow selection after if there's a superscript */
      let i' = superscript != [] ? i + 1 : i;
      let (i', superscript) = iterArgs(i', superscript);
      let accum = fn(accum, `Placeholder(superscript), i, i');
      (i', accum);
    | `ImaginaryUnit(superscript) =>
      let (i', superscript) = iterAtomLike(i, superscript);
      let accum = fn(accum, `ImaginaryUnit(superscript), i, i');
      (i', accum);
    | `Magnitude(exponent) =>
      let (i', exponent) = iterArgs1(i, exponent);
      let accum = fn(accum, `Magnitude(exponent), i, i');
      (i', accum);
    | `CloseBracket(superscript) =>
      let (i', superscript) = iterArgs1(i, superscript);
      let accum = fn(accum, `CloseBracket(superscript), i, i');
      (i', accum);
    | `Digit({atomNucleus, superscript}) =>
      let (i', superscript) = iterAtomLike(i, superscript);
      let accum = fn(accum, `Digit({atomNucleus, superscript}), i, i');
      (i', accum);
    | `Variable({atomNucleus, superscript}) =>
      let (i', superscript) = iterAtomLike(i, superscript);
      let accum = fn(accum, `Variable({atomNucleus, superscript}), i, i');
      (i', accum);
    | `Constant({constant, superscript}) =>
      let (i', superscript) = iterAtomLike(i, superscript);
      let accum = fn(accum, `Constant({constant, superscript}), i, i');
      (i', accum);
    | `Frac({fracNum, den, superscript}) =>
      let (i', fracNum, den, superscript) =
        iterArgs3(i, fracNum, den, superscript);
      let accum = fn(accum, `Frac({fracNum, den, superscript}), i, i');
      (i', accum);
    | `Sqrt({rootRadicand, superscript}) =>
      let (i', rootRadicand, superscript) =
        iterArgs2(i, rootRadicand, superscript);
      let accum = fn(accum, `Sqrt({rootRadicand, superscript}), i, i');
      (i', accum);
    | `NRoot({nrootDegree, radicand, superscript}) =>
      let (i', nrootDegree, radicand, superscript) =
        iterArgs3(i, nrootDegree, radicand, superscript);
      let accum =
        fn(accum, `NRoot({nrootDegree, radicand, superscript}), i, i');
      (i', accum);
    | `NLog({nlogBase}) =>
      let (i', nlogBase) = iterArgs1(i, nlogBase);
      let accum = fn(accum, `NLog({nlogBase: nlogBase}), i, i');
      (i', accum);
    | `Abs({absArg, superscript}) =>
      let (i', absArg, superscript) = iterArgs2(i, absArg, superscript);
      let accum = fn(accum, `Abs({absArg, superscript}), i, i');
      (i', accum);
    | `Sum({rangeStart, rangeEnd}) =>
      let (i', rangeStart, rangeEnd) = iterArgs2(i, rangeStart, rangeEnd);
      let accum = fn(accum, `Sum({rangeStart, rangeEnd}), i, i');
      (i', accum);
    | `Product({rangeStart, rangeEnd}) =>
      let (i', rangeStart, rangeEnd) = iterArgs2(i, rangeStart, rangeEnd);
      let accum = fn(accum, `Product({rangeStart, rangeEnd}), i, i');
      (i', accum);
    | `Table({tableElements, superscript} as t) =>
      let i' = i + 1;
      let (i', tableElements) =
        tableElements->ArrayUtil.foldMap(i', iterArgs);
      let (i', superscript) = iterArgs(i', superscript);
      let element = `Table({...t, tableElements, superscript});
      let accum = fn(accum, element, i, i');
      (i', accum);
    }
  and iter = (i, elements) => {
    let (finalIndex, out) =
      elements->Belt.List.reduce((i, initValue), reduceFn);
    let out = finalize(out, i, finalIndex);
    (finalIndex, out);
  }
  and iterArgs = (i, a) => {
    let indexIncr = a != [] ? 1 : 0;
    let (finalIndex, a) = iter(i, a);
    (finalIndex + indexIncr, a);
  }
  and iterArgs1 = (i, a) => {
    let i' = i + 1;
    let (i', a) = iterArgs(i', a);
    (i', a);
  }
  and iterArgs2 = (i, a, b) => {
    let i' = i + 1;
    let (i', a) = iterArgs(i', a);
    let (i', b) = iterArgs(i', b);
    (i', a, b);
  }
  and iterArgs3 = (i, a, b, c) => {
    let i' = i + 1;
    let (i', a) = iterArgs(i', a);
    let (i', b) = iterArgs(i', b);
    let (i', c) = iterArgs(i', c);
    (i', a, b, c);
  }
  and iterAtomLike = (i, superscript) => {
    let i' = superscript != [] ? i + 1 : i;
    let (i', superscript) = iterArgs1(i', superscript);
    (i', superscript);
  };
  iter(0, elements);
};

let walk = (elements, initValue, finalize, reduceFn) => {
  let (_, out) = walkI(elements, initValue, finalize, reduceFn);
  out;
};

let length = elements => {
  let (length, _) =
    walkI(elements, (), (_, _, _) => (), (_, _, _, _) => ());
  length;
};
