open Types;

let createElement = (~attributes=[], element, body) => {
  let attributes =
    attributes->Belt.List.map(((p, v)) => p ++ "=\"" ++ v ++ "\"")
    |> String.concat(" ");
  let head =
    switch (attributes) {
    | "" => "<" ++ element ++ ">"
    | attributes => "<" ++ element ++ " " ++ attributes ++ ">"
    };
  head ++ body ++ "</" ++ element ++ ">";
};

let wrapSuperscript = (~attributes=?, superscript, element) =>
  switch (superscript) {
  | "" => element
  | s => createElement(~attributes?, "msup", element ++ s)
  };

let elementWithIndex = (~superscript="", element, i, i', body) => {
  let attributes = [("id", string_of_int(i) ++ ":" ++ string_of_int(i'))];
  switch (superscript) {
  | "" => createElement(~attributes, element, body)
  | s => wrapSuperscript(~attributes, s, createElement(element, body))
  };
};

let atomLikeWithIndex = (~superscript="", element, i, i', body) => {
  let attributes = [("id", string_of_int(i) ++ ":" ++ string_of_int(i'))];
  switch (superscript) {
  | "" => createElement(~attributes, element, body)
  | s =>
    let atomAttributes = [("id", ":" ++ string_of_int(i + 1))];
    createElement(~attributes=atomAttributes, element, body)
    |> wrapSuperscript(~attributes, s);
  };
};

let stringOfOperator = x =>
  switch (x) {
  | Add => "+"
  | Sub => "-"
  | Mul => "&times;"
  | Div => "&div;"
  | Dot => "&#xb7;"
  };

let stringOfFunction = x =>
  switch (x) {
  | Sin => "sin"
  | Cos => "cos"
  | Tan => "tan"
  | Log => "log"
  };

let stringOfConstant = constant =>
  switch (constant) {
  | Pi => "&pi;"
  | E => "e"
  };

let xSetRow = value =>
  createElement(
    "mrow",
    createElement("mi", "x") ++ createElement("mo", "=") ++ value,
  );

let concatAccum = (element, accum) =>
  switch (accum) {
  | [current, ...rest] => [current ++ element, ...rest]
  | _ => failwith("Empty")
  };
let finalize = (body, _, _) =>
  switch (body->Belt.List.reverse |> String.concat("")) {
  | "" as body => body
  | body => createElement("mrow", body)
  };
let reduceFn = (accum, element, i, i') =>
  switch (element) {
  | `OpenBracket => [elementWithIndex("mo", i, i', "("), ...accum]
  | `CloseBracket(superscript) =>
    switch (accum) {
    | [closed, next, ...rest] =>
      let inner =
        closed
        ++ elementWithIndex("mo", i, i', ")")
        |> createElement("mrow")
        |> wrapSuperscript(superscript);
      [next ++ inner, ...rest];
    | _ =>
      elementWithIndex(~superscript, "mo", i, i', ")")->concatAccum(accum)
    }
  | `Placeholder(superscript) =>
    elementWithIndex(~superscript, "mi", i, i', "&#x25a1;")
    ->concatAccum(accum)
  | `Base(base) =>
    (stringOfBase(base) |> elementWithIndex("mn", i, i'))
    ->concatAccum(accum)
  | `Digit({atomNucleus, superscript}) =>
    atomLikeWithIndex(~superscript, "mn", i, i', atomNucleus)
    ->concatAccum(accum)
  | `Degree => elementWithIndex("mn", i, i', "&deg;")->concatAccum(accum)
  | `ArcMinute =>
    let superscript = createElement("mn", "&prime;");
    elementWithIndex(~superscript, "mn", i, i', "")->concatAccum(accum);
  | `ArcSecond =>
    let superscript = createElement("mn", "&#8243;");
    elementWithIndex(~superscript, "mn", i, i', "")->concatAccum(accum);
  | `DecimalSeparator =>
    concatAccum(elementWithIndex("mn", i, i', "."), accum)
  | `ImaginaryUnit(superscript) =>
    atomLikeWithIndex(~superscript, "mi", i, i', "i")->concatAccum(accum)
  | `Magnitude(exponent) =>
    concatAccum(
      createElement("mo", stringOfOperator(Mul))
      ++ createElement("mn", "10")
      |> elementWithIndex(~superscript=exponent, "mrow", i, i'),
      accum,
    )
  | `Variable({atomNucleus, superscript}) =>
    atomLikeWithIndex(~superscript, "mi", i, i', atomNucleus)
    ->concatAccum(accum)
  | `Constant({constant, superscript}) =>
    atomLikeWithIndex(~superscript, "mi", i, i', stringOfConstant(constant))
    ->concatAccum(accum)
  | `Function(f) =>
    elementWithIndex("mi", i, i', stringOfFunction(f))->concatAccum(accum)
  | `Factorial => elementWithIndex("mo", i, i', "!")->concatAccum(accum)
  | `Operator(v) =>
    elementWithIndex("mo", i, i', stringOfOperator(v))->concatAccum(accum)
  | `Frac({fracNum, den, superscript}) =>
    elementWithIndex(~superscript, "mfrac", i, i', fracNum ++ den)
    ->concatAccum(accum)
  | `Sqrt({rootRadicand, superscript}) =>
    elementWithIndex(~superscript, "msqrt", i, i', rootRadicand)
    ->concatAccum(accum)
  | `NRoot({nrootDegree, radicand, superscript}) =>
    concatAccum(
      radicand
      ++ nrootDegree
      |> elementWithIndex(~superscript, "mroot", i, i'),
      accum,
    )
  | `NLog({nlogBase}) =>
    concatAccum(
      createElement("mi", "log")
      ++ nlogBase
      |> elementWithIndex("msub", i, i'),
      accum,
    )
  | `Abs({absArg, superscript}) =>
    concatAccum(
      createElement(
        "mrow",
        elementWithIndex("mo", i, i', "|")
        ++ absArg
        ++ elementWithIndex("mo", i, i', "|"),
      )
      |> wrapSuperscript(superscript),
      accum,
    )
  | `Sum({rangeStart, rangeEnd}) =>
    concatAccum(
      elementWithIndex("mo", i, i', "&#x2211;")
      ++ xSetRow(rangeStart)
      ++ rangeEnd
      |> createElement("munderover"),
      accum,
    )
  | `Product({rangeStart, rangeEnd}) =>
    concatAccum(
      elementWithIndex("mo", i, i', "&#x220F;")
      ++ xSetRow(rangeStart)
      ++ rangeEnd
      |> createElement("munderover"),
      accum,
    )
  | `Table({tableElements, superscript, numRows, numColumns}) =>
    let inner =
      Belt.List.makeBy(numRows, row =>
        Belt.List.makeBy(numColumns, column =>
          createElement(
            "mtd",
            tableElements->Belt.Array.getUnsafe(row * numColumns + column),
          )
        )
        |> String.concat("")
        |> createElement("mtr")
      )
      |> String.concat("")
      |> createElement("mtable");
    concatAccum(
      createElement("mo", "[")
      ++ inner
      ++ createElement("mo", "]")
      |> createElement("mrow")
      |> wrapSuperscript(superscript),
      accum,
    );
  };

let create = elements =>
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", "block"),
    ],
    elements->TreeUtil.walk([""], finalize, reduceFn),
  );
