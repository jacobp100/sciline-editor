open Jest;

let openTag = {j|<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">|j};
let closeTag = {j|</math>|j};
let openRow = {j|$openTag<mrow>|j};
let closeRow = {j|</mrow>$closeTag|j};
let invalidAttrs = {j|class="invalid" stretchy="false"|j};

test("Formats numbers", (.) => {
  Mml.create([|`DigitS("1")|])
  ->expect
  ->toEqual({j|$openTag<mn id="0:1">1</mn>$closeTag|j});

  Mml.create([|`DigitS("1"), `DigitS("2")|])
  ->expect
  ->toEqual({j|$openRow<mn id="0:1">1</mn><mn id="1:2">2</mn>$closeRow|j});

  Mml.create([|`DigitS("1"), `DigitS("2"), `DigitS("3")|])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mn id="1:2">2</mn><mn id="2:3">3</mn>$closeRow|j},
    );

  Mml.create([|`DigitS("1"), `DigitS("2"), `DigitS("3"), `DigitS("4")|])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mn>,</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn>$closeRow|j},
    );

  Mml.create([|
    `DigitS("1"),
    `DigitS("2"),
    `DigitS("3"),
    `DigitS("4"),
    `DigitS("5"),
    `DigitS("6"),
    `DigitS("7"),
  |])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mn>,</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn><mn>,</mn><mn id="4:5">5</mn><mn id="5:6">6</mn><mn id="6:7">7</mn>$closeRow|j},
    );

  Mml.create([|
    `DigitS("0"),
    `DecimalSeparator,
    `DigitS("1"),
    `DigitS("2"),
    `DigitS("3"),
    `DigitS("4"),
    `DigitS("5"),
    `DigitS("6"),
    `DigitS("7"),
  |])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">0</mn><mn id="1:2">.</mn><mn id="2:3">1</mn><mn id="3:4">2</mn><mn id="4:5">3</mn><mn id="5:6">4</mn><mn id="6:7">5</mn><mn id="7:8">6</mn><mn id="8:9">7</mn>$closeRow|j},
    );

  Mml.create([|
    `Base(Hex),
    `DigitS("2"),
    `DigitS("3"),
    `DigitS("4"),
    `DigitS("5"),
    `DigitS("6"),
    `DigitS("7"),
  |])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">0x</mn><mn id="1:2">2</mn><mn id="2:3">3</mn><mn id="3:4">4</mn><mn id="4:5">5</mn><mn id="5:6">6</mn><mn id="6:7">7</mn>$closeRow|j},
    );

  Js.undefined;
});

test("Invalid brackets", (.) => {
  Mml.create([|`DigitS("1"), `OpenBracket, `DigitS("2"), `DigitS("3")|])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mo id="1:2" $invalidAttrs>(</mo><mn id="2:3">2</mn><mn id="3:4">3</mn>$closeRow|j},
    );
  Mml.create([|`DigitS("1"), `DigitS("2"), `CloseBracketS, `DigitS("3")|])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mn id="1:2">2</mn><mo id="2:3" $invalidAttrs>)</mo><mn id="3:4">3</mn>$closeRow|j},
    );

  Js.undefined;
});

test("Bracket states", (.) => {
  Mml.create([|
    `DigitS("1"),
    `OpenBracket,
    `DigitS("2"),
    `CloseBracketS,
    `DigitS("3"),
  |])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mrow><mo id="1:2">(</mo><mn id="2:3">2</mn><mo id="3:4">)</mo></mrow><mn id="4:5">3</mn>$closeRow|j},
    );
  Mml.create([|
    `DigitS("1"),
    `OpenBracket,
    `DigitS("2"),
    `OpenBracket,
    `DigitS("3"),
    `CloseBracketS,
    `DigitS("4"),
    `CloseBracketS,
    `DigitS("5"),
  |])
  ->expect
  ->toEqual(
      {j|$openRow<mn id="0:1">1</mn><mrow><mo id="1:2">(</mo><mn id="2:3">2</mn><mrow><mo id="3:4">(</mo><mn id="4:5">3</mn><mo id="5:6">)</mo></mrow><mn id="6:7">4</mn><mo id="7:8">)</mo></mrow><mn id="8:9">5</mn>$closeRow|j},
    );

  Js.undefined;
});