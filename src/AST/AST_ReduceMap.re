type superscript('a) = {
  superscriptBody: 'a,
  index: int,
};
type atom('a) = {
  atomNucleus: string,
  superscript: option(superscript('a)),
};
type customAtom('a) = {
  customAtomValue: TechniCalcCalculator.Encoding.encoding,
  mml: string,
  superscript: option(superscript('a)),
};
type frac('a) = {
  fracNum: 'a,
  den: 'a,
  superscript: option(superscript('a)),
};
type root('a) = {
  rootRadicand: 'a,
  superscript: option(superscript('a)),
};
type nroot('a) = {
  nrootDegree: 'a,
  radicand: 'a,
  superscript: option(superscript('a)),
};
type magnitude('a) = {magnitudeValue: 'a};
type nlog('a) = {nlogBase: 'a};
type unary('a) = {
  unaryArg: 'a,
  superscript: option(superscript('a)),
};
type randInt('a) = {
  randIntA: 'a,
  b: 'a,
  superscript: option(superscript('a)),
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
  numRows: int,
  numColumns: int,
  superscript: option(superscript('a)),
};

type t('a) = [
  | `Abs(unary('a))
  | `Add
  | `ArcMinute
  | `ArcSecond
  | `Base(AST_Types.base)
  | `Ceil(unary('a))
  | `CloseBracket(option(superscript('a)))
  | `ConstE(option(superscript('a)))
  | `ConstPi(option(superscript('a)))
  | `Conj
  | `CustomAtom(customAtom('a))
  | `DecimalSeparator
  | `Degree
  | `Differential(differential('a))
  | `Digit(atom('a))
  | `Div
  | `Dot
  | `Factorial
  | `Floor(unary('a))
  | `Frac(frac('a))
  | `Function(AST_Types.func)
  | `ImaginaryUnit(option(superscript('a)))
  | `Integral(integral('a))
  | `Magnitude(magnitude('a))
  | `Mul
  | `NCR(stat('a))
  | `NLog(nlog('a))
  | `NPR(stat('a))
  | `NRoot(nroot('a))
  | `OpenBracket
  | `Percent
  | `Product(iteration('a))
  | `Rand(option(superscript('a)))
  | `RandInt(randInt('a))
  | `Round(unary('a))
  | `Sqrt(root('a))
  | `Sub
  | `Sum(iteration('a))
  | `Superscript('a)
  | `Table(table('a))
  | `UnitConversion(AST_Types.unitConversion)
  | `Variable(atom('a))
];

type range = (int, int);

exception ExpectedArg;
exception UnexpectedArg(int);

let superscriptBody = s => s.superscriptBody;

let reduceMap =
    (
      input: array(AST_Types.t),
      ~reduce: ('accum, t('a), range) => 'accum,
      ~map: ('accum, range) => 'value,
      ~initial: 'accum,
    )
    : 'value => {
  let rec readNodeExn = (i): (t('a), int) =>
    switch (Belt.Array.getExn(input, i)) {
    | #AST_Types.atom as v => (v, i + 1)
    | `CloseBracketS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`CloseBracket(superscript), i');
    | `ConstPiS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`ConstPi(superscript), i');
    | `ConstES =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`ConstE(superscript), i');
    | `CustomAtomS({AST_Types.value: customAtomValue, mml}) =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`CustomAtom({customAtomValue, mml, superscript}), i');
    | `DigitS(atomNucleus) =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`Digit({atomNucleus, superscript}), i');
    | `ImaginaryUnitS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`ImaginaryUnit(superscript), i');
    | `RandS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`Rand(superscript), i');
    | `VariableS(atomNucleus) =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (`Variable({atomNucleus, superscript}), i');
    | `Magnitude1 =>
      let (magnitudeValue, i') = readArg(i + 1);
      (`Magnitude({magnitudeValue: magnitudeValue}), i');
    | `Superscript1 =>
      let (superscript, i') = readArg(i + 1);
      (`Superscript(superscript), i');
    | `NLog1 =>
      let (nlogBase, i') = readArg(i + 1);
      (`NLog({nlogBase: nlogBase}), i');
    | `Abs1S =>
      let (unaryArg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (`Abs({unaryArg, superscript}), i');
    | `Ceil1S =>
      let (unaryArg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (`Ceil({unaryArg, superscript}), i');
    | `Floor1S =>
      let (unaryArg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (`Floor({unaryArg, superscript}), i');
    | `Round1S =>
      let (unaryArg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (`Round({unaryArg, superscript}), i');
    | `Sqrt1S =>
      let (rootRadicand, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (`Sqrt({rootRadicand, superscript}), i');
    | `Differential2 =>
      let (body, i') = readArg(i + 1);
      let (differentialX, i') = readArg(i');
      (`Differential({differentialX, body}), i');
    | `NCR2 =>
      let (statN, i') = readArg(i + 1);
      let (r, i') = readArg(i');
      (`NCR({statN, r}), i');
    | `NPR2 =>
      let (statN, i') = readArg(i + 1);
      let (r, i') = readArg(i');
      (`NPR({statN, r}), i');
    | `Product2 =>
      let (iterationStart, i') = readArg(i + 1);
      let (iterationEnd, i') = readArg(i');
      (`Product({iterationStart, iterationEnd}), i');
    | `Sum2 =>
      let (iterationStart, i') = readArg(i + 1);
      let (iterationEnd, i') = readArg(i');
      (`Sum({iterationStart, iterationEnd}), i');
    | `Frac2S =>
      let (fracNum, i') = readArg(i + 1);
      let (den, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (`Frac({fracNum, den, superscript}), i');
    | `NRoot2S =>
      let (nrootDegree, i') = readArg(i + 1);
      let (radicand, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (`NRoot({nrootDegree, radicand, superscript}), i');
    | `RandInt2S =>
      let (randIntA, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (`RandInt({randIntA, b, superscript}), i');
    | `Integral3 =>
      let (integralA, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (body, i') = readArg(i');
      (`Integral({integralA, b, body}), i');
    | `TableNS({numRows, numColumns}) =>
      let i' = i + 1;
      let (i', tableElements) =
        ArrayUtil.foldMake(
          numRows * numColumns,
          i',
          (s, _) => {
            let (element, index) = readArg(s);
            (index, element);
          },
        );
      let (superscript, i') = readSuperscript(i');
      (`Table({tableElements, numRows, numColumns, superscript}), i');
    | `Arg => failwith("Arg")
    }
  and readArg = (~accum=initial, ~start=?, i) => {
    let start = Belt.Option.getWithDefault(start, i);
    switch (Belt.Array.get(input, i)) {
    | None => raise(ExpectedArg)
    | Some(`Arg) =>
      let i' = i;
      (map(accum, (start, i')), i' + 1);
    | Some(_) =>
      let (node, i') = readNodeExn(i);
      readArg(~accum=reduce(accum, node, (i, i')), i');
    };
  }
  and readSuperscript = i =>
    switch (Belt.Array.get(input, i)) {
    | Some(`Superscript1) =>
      let (superscriptBody, i') = readArg(i + 1);
      (Some({superscriptBody, index: i}), i');
    | _ => (None, i)
    };

  let rec readUntilEnd = (~accum=initial, i) =>
    switch (Belt.Array.get(input, i)) {
    | None => map(accum, (0, i))
    | Some(`Arg) => raise(UnexpectedArg(i))
    | Some(_) =>
      let (node, i') = readNodeExn(i);
      readUntilEnd(~accum=reduce(accum, node, (i, i')), i');
    };

  readUntilEnd(0);
};