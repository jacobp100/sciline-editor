open Jest;

let parseEval = v =>
  switch (Value.parse(v)) {
  | `Ok(v) => Some(ScilineCalculator.AST.eval(v))
  | _ => None
  };

let ofInt = ScilineCalculator.Types.ofInt;
let ofString = ScilineCalculator.Types.ofString;

test("Parses numbers", (.) => {
  parseEval([|`DigitS("1"), `DigitS("2"), `DigitS("3")|])
  ->expect
  ->toEqual(Some(ofString("123")));

  parseEval([|
    `DigitS("1"),
    `DecimalSeparator,
    `DigitS("2"),
    `DigitS("3"),
  |])
  ->expect
  ->toEqual(Some(ofString("1.23")));

  parseEval([|`DigitS("0"), `DecimalSeparator, `DigitS("5")|])
  ->expect
  ->toEqual(Some(ofString("0.5")));

  parseEval([|`DecimalSeparator, `DigitS("5")|])
  ->expect
  ->toEqual(Some(ofString("0.5")));

  parseEval([|`DigitS("5"), `DecimalSeparator|])
  ->expect
  ->toEqual(Some(ofString("5")));

  Js.undefined;
});

test("Does not parse invalid numbers", (.) => {
  parseEval([|
    `DigitS("1"),
    `DecimalSeparator,
    `DigitS("2"),
    `DecimalSeparator,
    `DigitS("3"),
  |])
  ->expect
  ->toEqual(None);

  Js.undefined;
});

test("Parses superscripts on numbers", (.) => {
  parseEval([|`DigitS("2"), `Superscript1, `DigitS("2"), `Arg|])
  ->expect
  ->toEqual(Some(ofString("4")));

  parseEval([|
    `DigitS("1"),
    `DigitS("0"),
    `Superscript1,
    `DigitS("2"),
    `Arg,
  |])
  ->expect
  ->toEqual(Some(ofString("100")));

  parseEval([|
    `DigitS("1"),
    `Superscript1,
    `DigitS("2"),
    `Arg,
    `DigitS("0"),
    `Superscript1,
    `DigitS("2"),
    `Arg,
  |])
  ->expect
  ->toEqual(None);

  Js.undefined;
});

test("Parses magnitudes", (.) => {
  parseEval([|`DigitS("1"), `Magnitude1, `DigitS("3"), `Arg|])
  ->expect
  ->toEqual(Some(ofString("1000")));

  parseEval([|
    `DigitS("2"),
    `Superscript1,
    `DigitS("2"),
    `Arg,
    `Magnitude1,
    `DigitS("3"),
    `Arg,
  |])
  ->expect
  ->toEqual(Some(ofString("4000")));

  Js.undefined;
});

test("Parses imaginary units", (.) => {
  let mulI = ScilineCalculator.Value.(mul(i));

  parseEval([|`DigitS("2"), `ImaginaryUnitS|])
  ->expect
  ->toEqual(Some(ofString("2")->mulI));

  parseEval([|`DigitS("1"), `DigitS("0"), `ImaginaryUnitS|])
  ->expect
  ->toEqual(Some(ofString("10")->mulI));

  parseEval([|
    `DigitS("1"),
    `DigitS("0"),
    `Superscript1,
    `DigitS("2"),
    `Arg,
    `ImaginaryUnitS,
  |])
  ->expect
  ->toEqual(Some(ofString("100")->mulI));

  parseEval([|
    `DigitS("1"),
    `DigitS("0"),
    `ImaginaryUnitS,
    `Superscript1,
    `DigitS("2"),
    `Arg,
  |])
  ->expect
  ->toEqual(Some(ofString("-10")));

  parseEval([|
    `DigitS("1"),
    `DigitS("0"),
    `Superscript1,
    `DigitS("2"),
    `Arg,
    `ImaginaryUnitS,
    `Superscript1,
    `DigitS("2"),
    `Arg,
  |])
  ->expect
  ->toEqual(Some(ofString("-100")));

  Js.undefined;
});

test("Angles", (.) => {
  let piOver = ScilineCalculator.Value.(div(pi));

  parseEval([|`DigitS("1"), `Degree|])
  ->expect
  ->toEqual(Some(piOver(ofInt(180))));

  parseEval([|`DigitS("1"), `ArcMinute|])
  ->expect
  ->toEqual(Some(piOver(ofInt(180 * 60))));

  parseEval([|`DigitS("1"), `ArcSecond|])
  ->expect
  ->toEqual(Some(piOver(ofInt(180 * 60 * 60))));

  Js.undefined;
});