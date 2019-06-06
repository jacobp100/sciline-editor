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
type customAtom = {
  value: ScilineCalculator.Encoding.encoding,
  mml: string,
};
type table = {
  numRows: int,
  numColumns: int,
};

type t = [
  | `Arg
  | `Add
  | `ArcMinute
  | `ArcSecond
  | `Base(base)
  | `Conj
  | `DecimalSeparator
  | `Degree
  | `Div
  | `Dot
  | `Factorial
  | `Function(func)
  | `Mul
  | `OpenBracket
  | `Sub
  | `CloseBracketS
  | `ConstES
  | `ConstPiS
  | `CustomAtomS(customAtom)
  | `DigitS(string)
  | `ImaginaryUnitS
  | `RandS
  | `VariableS(string)
  | `Magnitude1
  | `Superscript1
  | `NLog1
  | `Abs1S
  | `Ceil1S
  | `Floor1S
  | `Round1S
  | `Sqrt1S
  | `Differential2
  | `NCR2
  | `NPR2
  | `Product2
  | `Sum2
  | `Frac2S
  | `NRoot2S
  | `RandInt2S
  | `Integral3
  | `TableNS(table)
];

let argCountExn = (arg: t) =>
  switch (arg) {
  | `Arg => failwith("arg")
  | `Add
  | `ArcMinute
  | `ArcSecond
  | `Base(_)
  | `Conj
  | `DecimalSeparator
  | `Degree
  | `Div
  | `Dot
  | `Factorial
  | `Function(_)
  | `Mul
  | `OpenBracket
  | `Sub
  | `CloseBracketS
  | `ConstES
  | `ConstPiS
  | `CustomAtomS(_)
  | `DigitS(_)
  | `ImaginaryUnitS
  | `RandS
  | `VariableS(_) => 0
  | `Magnitude1
  | `Superscript1
  | `NLog1
  | `Abs1S
  | `Ceil1S
  | `Floor1S
  | `Round1S
  | `Sqrt1S => 1
  | `Differential2
  | `NCR2
  | `NPR2
  | `Product2
  | `Sum2
  | `Frac2S
  | `NRoot2S
  | `RandInt2S => 2
  | `Integral3 => 3
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
