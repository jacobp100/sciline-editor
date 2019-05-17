type atom('a) = {
  atomNucleus: string,
  superscript: 'a,
};
type constantAtom('a) = {
  constant: AST_Types.constant,
  superscript: 'a,
};
type customAtom('a) = {
  customAtomValue: ScilineCalculator.Encoding.encoding,
  mml: string,
  superscript: 'a,
};
type frac('a) = {
  fracNum: 'a,
  den: 'a,
  superscript: 'a,
};
type root('a) = {
  rootRadicand: 'a,
  superscript: 'a,
};
type nroot('a) = {
  nrootDegree: 'a,
  radicand: 'a,
  superscript: 'a,
};
type magnitude('a) = {magnitudeBase: 'a};
type nlog('a) = {nlogBase: 'a};
type unary('a) = {
  unaryArg: 'a,
  superscript: 'a,
};
type randInt('a) = {
  randIntA: 'a,
  b: 'a,
  superscript: 'a,
};
type stat('a) = {
  statN: 'a,
  r: 'a,
};
type differential('a) = {
  differentialX: 'a,
  body: 'a,
};
type integral('a) = {
  integralA: 'a,
  b: 'a,
  body: 'a,
};
type iteration('a) = {
  iterationStart: 'a,
  iterationEnd: 'a,
};
type table('a) = {
  tableElements: array('a),
  superscript: 'a,
  numRows: int,
  numColumns: int,
};

type t('a) = [
  | `Abs(unary('a))
  | `ArcMinute
  | `ArcSecond
  | `Base(AST_Types.base)
  | `Ceil(unary('a))
  | `CloseBracket('a)
  | `Conj
  | `Constant(constantAtom('a))
  | `CustomAtom(customAtom('a))
  | `DecimalSeparator
  | `Degree
  | `Differential(differential('a))
  | `Digit(atom('a))
  | `Factorial
  | `Floor(unary('a))
  | `Frac(frac('a))
  | `Function(AST_Types.func)
  | `ImaginaryUnit('a)
  | `Integral(integral('a))
  | `Magnitude(magnitude('a))
  | `NCR(stat('a))
  | `NLog(nlog('a))
  | `NPR(stat('a))
  | `NRoot(nroot('a))
  | `OpenBracket
  | `Operator(AST_Types.binaryOperator)
  | `Superscript('a)
  | `Product(iteration('a))
  | `Rand('a)
  | `RandInt(randInt('a))
  | `Round(unary('a))
  | `Sqrt(root('a))
  | `Sum(iteration('a))
  | `Table(table('a))
  | `Variable(atom('a))
];

type range = (int, int, int);

let reduceMap =
    (
      input: array(AST_Types.t),
      ~reduce: ('accum, t('a), range) => 'accum,
      ~map: ('accum, range) => 'value,
      ~initial: 'accum,
    )
    : 'value => {
  let rec readNodeExn = i: (t('a), int, int) =>
    switch (Belt.Array.getExn(input, i)) {
    | (
        `Base(_) | `Operator(_) | `OpenBracket | `DecimalSeparator | `Conj |
        `Factorial |
        `Function(_) |
        `Degree |
        `ArcMinute |
        `ArcSecond
      ) as v => (
        v,
        (-1),
        i + 1,
      )
    | `CloseBracketS =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`CloseBracket(superscript), i', s);
    | `ConstantS(constant) =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`Constant({constant, superscript}), i', s);
    | `CustomAtomS({AST_Types.value: customAtomValue, mml}) =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`CustomAtom({customAtomValue, mml, superscript}), i', (-1));
    | `DigitS(atomNucleus) =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`Digit({atomNucleus, superscript}), i', s);
    | `ImaginaryUnitS =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`ImaginaryUnit(superscript), i', s);
    | `RandS =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`Rand(superscript), i', s);
    | `VariableS(atomNucleus) =>
      let s = i + 1;
      let (superscript, i') = readSuperscript(s);
      (`Variable({atomNucleus, superscript}), i', s);
    | `Magnitude1 =>
      let (magnitudeBase, i') = readArg(i + 1);
      (`Magnitude({magnitudeBase: magnitudeBase}), i', (-1));
    | `Superscript1 =>
      let (superscript, i') = readArg(i + 1);
      (`Superscript(superscript), i', (-1));
    | `NLog1 =>
      let (nlogBase, i') = readArg(i + 1);
      (`NLog({nlogBase: nlogBase}), i', (-1));
    | `Abs1S =>
      let (unaryArg, s) = readArg(i + 1);
      let (superscript, i') = readSuperscript(s);
      (`Abs({unaryArg, superscript}), i', s);
    | `Ceil1S =>
      let (unaryArg, s) = readArg(i + 1);
      let (superscript, i') = readSuperscript(s);
      (`Ceil({unaryArg, superscript}), i', s);
    | `Floor1S =>
      let (unaryArg, s) = readArg(i + 1);
      let (superscript, i') = readSuperscript(s);
      (`Floor({unaryArg, superscript}), i', s);
    | `Round1S =>
      let (unaryArg, s) = readArg(i + 1);
      let (superscript, i') = readSuperscript(s);
      (`Round({unaryArg, superscript}), i', s);
    | `Sqrt1S =>
      let (rootRadicand, s) = readArg(i + 1);
      let (superscript, i') = readSuperscript(s);
      (`Sqrt({rootRadicand, superscript}), i', s);
    | `Differential2 =>
      let (body, i') = readArg(i + 1);
      let (differentialX, i') = readArg(i');
      (`Differential({differentialX, body}), i', (-1));
    | `NCR2 =>
      let (statN, i') = readArg(i + 1);
      let (r, i') = readArg(i');
      (`NCR({statN, r}), i', (-1));
    | `NPR2 =>
      let (statN, i') = readArg(i + 1);
      let (r, i') = readArg(i');
      (`NPR({statN, r}), i', (-1));
    | `Product2 =>
      let (iterationStart, i') = readArg(i + 1);
      let (iterationEnd, i') = readArg(i');
      (`Product({iterationStart, iterationEnd}), i', (-1));
    | `Sum2 =>
      let (iterationStart, i') = readArg(i + 1);
      let (iterationEnd, i') = readArg(i');
      (`Sum({iterationStart, iterationEnd}), i', (-1));
    | `Frac2S =>
      let (fracNum, i') = readArg(i + 1);
      let (den, s) = readArg(i');
      let (superscript, i') = readSuperscript(s);
      (`Frac({fracNum, den, superscript}), i', s);
    | `NRoot2S =>
      let (nrootDegree, i') = readArg(i + 1);
      let (radicand, s) = readArg(i');
      let (superscript, i') = readSuperscript(s);
      (`NRoot({nrootDegree, radicand, superscript}), i', (-1));
    | `RandInt2S =>
      let (randIntA, i') = readArg(i + 1);
      let (b, s) = readArg(i');
      let (superscript, i') = readSuperscript(s);
      (`RandInt({randIntA, b, superscript}), i', s);
    | `Integral3 =>
      let (integralA, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (body, i') = readArg(i');
      (`Integral({integralA, b, body}), i', (-1));
    | `TableNS({numRows, numColumns}) =>
      let i' = i + 1;
      let (s, tableElements) =
        ArrayUtil.foldMake(
          numRows * numColumns,
          i',
          (s, _) => {
            let (element, index) = readArg(s);
            (index, element);
          },
        );
      let (superscript, i') = readSuperscript(s);
      (`Table({tableElements, superscript, numRows, numColumns}), i', s);
    | `Arg => failwith("Arg")
    }
  and readArg = (~accum=initial, ~start=?, i) => {
    let start = Belt.Option.getWithDefault(start, i);
    switch (Belt.Array.get(input, i)) {
    | None => failwith("Expected arg but end reached")
    | Some(`Arg) =>
      let i' = i;
      (map(accum, (start, i', (-1))), i');
    | Some(_) =>
      let (node, i', s) = readNodeExn(i);
      readArg(~accum=reduce(accum, node, (i, i', s)), i');
    };
  }
  and readSuperscript = i =>
    switch (Belt.Array.get(input, i)) {
    | Some(`Superscript1) => readArg(i + 1)
    | _ => (map(initial, (i, i, (-1))), i)
    };

  let rec readUntilEnd = (~accum=initial, i) =>
    switch (Belt.Array.get(input, i)) {
    | None => map(accum, (0, i, (-1)))
    | Some(`Arg) => failwith("Unexpected arg at")
    | Some(_) =>
      let (node, i', s) = readNodeExn(i);
      readUntilEnd(~accum=reduce(accum, node, (i, i', s)), i');
    };

  readUntilEnd(0);
};
