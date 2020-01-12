open Jest;
open AST_Types;

let parseEval = v =>
  switch (Value.parse(v)) {
  | `Ok(v) => Some(ScilineCalculator.AST.eval(v))
  | _ => None
  };

let ofString = ScilineCalculator.Types.ofString;

test("Parses with bodmas", (.) => {
  parseEval([|`DigitS("1"), `Sub, `DigitS("2"), `Add, `DigitS("3")|])
  ->expect
  ->toEqual(Some(ofString("2")));

  parseEval([|`DigitS("4"), `Div, `DigitS("2"), `Mul, `DigitS("3")|])
  ->expect
  ->toEqual(Some(ofString("6")));

  Js.undefined;
});

test("Parses brackets", (.) => {
  parseEval([|
    `DigitS("2"),
    `Mul,
    `OpenBracket,
    `DigitS("3"),
    `Add,
    `DigitS("4"),
    `CloseBracketS,
    `Mul,
    `DigitS("2"),
  |])
  ->expect
  ->toEqual(Some(ofString("28")));

  Js.undefined;
});

test("Parses functions", (.) => {
  parseEval([|`Function(Cos), `DigitS("0")|])
  ->expect
  ->toEqual(Some(ofString("1")));

  parseEval([|`Function(Cos), `OpenBracket, `DigitS("0"), `CloseBracketS|])
  ->expect
  ->toEqual(Some(ofString("1")));

  Js.undefined;
});

test("Parses iteration operators", (.) => {
  parseEval([|
    `Sum2,
    `DigitS("0"),
    `Arg,
    `DigitS("3"),
    `Arg,
    `VariableS("x"),
    `Add,
    `DigitS("1"),
  |])
  ->expect
  ->toEqual(Some(ofString("7")));

  parseEval([|
    `Sum2,
    `DigitS("0"),
    `Arg,
    `DigitS("3"),
    `Arg,
    `OpenBracket,
    `VariableS("x"),
    `Add,
    `DigitS("1"),
    `CloseBracketS,
  |])
  ->expect
  ->toEqual(Some(ofString("10")));

  Js.undefined;
});