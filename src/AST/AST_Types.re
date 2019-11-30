type base =
  | Bin
  | Oct
  | Hex;
type func =
  | Sin
  | Asin
  | Sinh
  | Asinh
  | Cos
  | Acos
  | Cosh
  | Acosh
  | Tan
  | Atan
  | Tanh
  | Atanh
  | Log
  | Re
  | Im
  | Gamma;
type unitConversion = {
  fromUnits: ScilineCalculator.Unit_Types.units,
  toUnits: ScilineCalculator.Unit_Types.units,
};
type customAtom = {
  value: ScilineCalculator.Encoding.encoding,
  mml: string,
};
type table = {
  numRows: int,
  numColumns: int,
};

type operatorAtom = [ | `Add | `Div | `Dot | `Mul | `Sub];

type atom = [
  operatorAtom
  | `ArcMinute
  | `ArcSecond
  | `Base(base)
  | `Conj
  | `DecimalSeparator
  | `Degree
  | `Factorial
  | `Function(func)
  | `OpenBracket
  | `Percent
  | `UnitConversion(unitConversion)
];

type atomS = [
  | `CloseBracketS
  | `ConstES
  | `ConstPiS
  | `CustomAtomS(customAtom)
  | `DigitS(string)
  | `ImaginaryUnitS
  | `RandS
  | `VariableS(string)
];

type atom1 = [ | `Magnitude1 | `NLog1 | `Superscript1];
type atom1S = [ | `Abs1S | `Ceil1S | `Floor1S | `Round1S | `Sqrt1S];
type atom2 = [ | `Differential2 | `NCR2 | `NPR2 | `Product2 | `Sum2];
type atom2S = [ | `Frac2S | `NRoot2S | `RandInt2S];
type atom3 = [ | `Integral3];
type atomNS = [ | `TableNS(table)];

type t = [
  | `Arg
  | atom
  | atomS
  | atom1
  | atom1S
  | atom2
  | atom2S
  | atom3
  | atomNS
];

let argCountExn = (arg: t) =>
  switch (arg) {
  | `Arg => failwith("arg")
  | #atom
  | #atomS => 0
  | #atom1
  | #atom1S => 1
  | #atom2
  | #atom2S => 2
  | #atom3 => 3
  | `TableNS({numRows, numColumns}) => numRows * numColumns
  };

let argEndIndex = (ast: array(t), index) => {
  let rec iter = (~pending, ast: array(t), index) =>
    switch (Belt.Array.get(ast, index)) {
    | Some(`Arg) =>
      if (pending == 0) {
        index + 1;
      } else {
        iter(~pending=pending - 1, ast, index + 1);
      }
    | Some(v) => iter(~pending=pending + argCountExn(v), ast, index + 1)
    | None => index
    };
  iter(~pending=0, ast, index);
};

let rec normalizationState = (ast, remaining, i) =>
  switch (remaining, Belt.Array.get(ast, i)) {
  | (0, Some(`Arg)) => `GenericError
  | (_, Some(`Arg)) => normalizationState(ast, remaining - 1, i + 1)
  | (_, Some(v)) =>
    normalizationState(ast, remaining + argCountExn(v), i + 1)
  | (0, None) => `Ok
  | (_, None) => `TooFewArgsError(remaining)
  };

let normalize = (ast: array(t)) =>
  switch (normalizationState(ast, 0, 0)) {
  | `Ok => ast
  | `GenericError =>
    Js.log("Non-normalized ast (fixing)");
    let remaining = ref(0);
    let ast =
      Belt.Array.keep(ast, element =>
        if (element != `Arg) {
          remaining := remaining^ + argCountExn(element);
          true;
        } else if (remaining^ != 0) {
          remaining := remaining^ - 1;
          true;
        } else {
          false;
        }
      );
    if (remaining^ != 0) {
      Belt.Array.concat(ast, Belt.Array.make(remaining^, `Arg));
    } else {
      ast;
    };
  | `TooFewArgsError(remaining) =>
    Js.log("Too few args in ast (fixing)");
    Belt.Array.concat(ast, Belt.Array.make(remaining, `Arg));
  };