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

let elementWithIndex = (~attributes=[], ~superscript="", element, i, i', body) => {
  let attributes = [
    ("id", string_of_int(i) ++ ":" ++ string_of_int(i')),
    ...attributes,
  ];
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
  | Arcsin => "arcsin"
  | Sinh => "sinh"
  | Arcsinh => "arcsinh"
  | Cos => "cos"
  | Arccos => "arccos"
  | Cosh => "cosh"
  | Arccosh => "arccosh"
  | Tan => "tan"
  | Arctan => "arctan"
  | Tanh => "tanh"
  | Arctanh => "arctanh"
  | Log => "log"
  | Re => "re"
  | Im => "im"
  | Gamma => "&Gamma;"
  };
let stringOfBase = base =>
  switch (base) {
  | Bin => "0b"
  | Oct => "0o"
  | Hex => "0x"
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

type bracketGroup = {
  i: int,
  i': int,
  body: string,
};
type accum = (string, list(bracketGroup));

let defaultAccum = ("", []);

let append = (element, (level0Body, bracketGroup)) =>
  switch (bracketGroup) {
  | [{body} as bracketGroup, ...rest] => (
      level0Body,
      [{...bracketGroup, body: body ++ element}, ...rest],
    )
  | [] => (level0Body ++ element, [])
  };
let openBracket = (i, i', (level0Body, bracketGroup)) => (
  level0Body,
  [{i, i', body: ""}, ...bracketGroup],
);

let invalidClass = "invalid";
let closeBracket = (i, i', superscript, (level0Body, bracketGroup)) =>
  switch (bracketGroup) {
  | [closed, ...rest] =>
    let body =
      elementWithIndex("mo", closed.i, closed.i', "(")
      ++ closed.body
      ++ elementWithIndex("mo", i, i', ")")
      |> createElement("mrow")
      |> wrapSuperscript(superscript);
    switch (rest) {
    | [next, ...rest] => (
        level0Body,
        [{...next, body: next.body ++ body}, ...rest],
      )
    | [] => (level0Body ++ body, [])
    };
  | [] =>
    let attributes = [("class", invalidClass), ("stretchy", "false")];
    let element = elementWithIndex(~attributes, "mo", i, i', ")");
    (level0Body ++ element, []);
  };
let accumToString = ((level0Body, bracketGroup)) => {
  let attributes = [("class", invalidClass), ("stretchy", "false")];
  let closed =
    bracketGroup
    ->Belt.List.map(({i, i', body}) =>
        elementWithIndex(~attributes, "mo", i, i', "(") ++ body
      )
    ->Belt.List.reverse
    |> String.concat("");
  level0Body ++ closed;
};

let concatAccum = (element, accum) =>
  switch (accum) {
  | [current, ...rest] => [current ++ element, ...rest]
  | _ => failwith("Empty")
  };
let mapValue = ({TreeUtil.accum}): string =>
  switch (accumToString(accum)) {
  | "" as body => body
  | body => createElement("mrow", body)
  };
let reduceFn = ({TreeUtil.accum, rangeStart: i, rangeEnd: i'}, element) =>
  switch (element) {
  | `OpenBracket => openBracket(i, i', accum)
  | `CloseBracket(superscript) => closeBracket(i, i', superscript, accum)
  | `Placeholder(superscript) =>
    let attributes = [("class", "placeholder")];
    elementWithIndex(~attributes, ~superscript, "mi", i, i', "&#x25a1;")
    ->append(accum);
  | `Base(base) =>
    (stringOfBase(base) |> elementWithIndex("mn", i, i'))->append(accum)
  | `Digit({atomNucleus, superscript}) =>
    atomLikeWithIndex(~superscript, "mn", i, i', atomNucleus)->append(accum)
  | `Degree => elementWithIndex("mn", i, i', "&deg;")->append(accum)
  | `ArcMinute =>
    let superscript = createElement("mn", "&prime;");
    elementWithIndex(~superscript, "mn", i, i', "")->append(accum);
  | `ArcSecond =>
    let superscript = createElement("mn", "&#8243;");
    elementWithIndex(~superscript, "mn", i, i', "")->append(accum);
  | `Conj =>
    let superscript = createElement("mn", "*");
    elementWithIndex(~superscript, "mn", i, i', "")->append(accum);
  | `DecimalSeparator => append(elementWithIndex("mn", i, i', "."), accum)
  | `ImaginaryUnit(superscript) =>
    atomLikeWithIndex(~superscript, "mi", i, i', "i")->append(accum)
  | `Magnitude(exponent) =>
    append(
      createElement("mo", stringOfOperator(Mul))
      ++ createElement("mn", "10")
      |> elementWithIndex(~superscript=exponent, "mrow", i, i'),
      accum,
    )
  | `Variable({atomNucleus, superscript}) =>
    atomLikeWithIndex(~superscript, "mi", i, i', atomNucleus)->append(accum)
  | `Constant({constant, superscript}) =>
    atomLikeWithIndex(~superscript, "mi", i, i', stringOfConstant(constant))
    ->append(accum)
  | `CustomAtom({mml, superscript}) =>
    atomLikeWithIndex(~superscript, "mrow", i, i', mml)->append(accum)
  | `Function(Gamma) =>
    let attributes = [("mathvariant", "normal")];
    elementWithIndex(~attributes, "mi", i, i', stringOfFunction(Gamma))
    ->append(accum);
  | `Function(f) =>
    elementWithIndex("mi", i, i', stringOfFunction(f))->append(accum)
  | `Factorial => elementWithIndex("mo", i, i', "!")->append(accum)
  | `Operator(v) =>
    elementWithIndex("mo", i, i', stringOfOperator(v))->append(accum)
  | `Frac({fracNum, den, superscript}) =>
    elementWithIndex(~superscript, "mfrac", i, i', fracNum ++ den)
    ->append(accum)
  | `Sqrt({rootRadicand, superscript}) =>
    elementWithIndex(~superscript, "msqrt", i, i', rootRadicand)
    ->append(accum)
  | `NRoot({nrootDegree, radicand, superscript}) =>
    append(
      radicand
      ++ nrootDegree
      |> elementWithIndex(~superscript, "mroot", i, i'),
      accum,
    )
  | `NLog({nlogBase}) =>
    append(
      createElement("mi", "log")
      ++ nlogBase
      |> elementWithIndex("msub", i, i'),
      accum,
    )
  | (`Abs(arg) | `Floor(arg) | `Ceil(arg) | `Round(arg)) as unary =>
    let {unaryArg, superscript} = arg;
    let (leftBracket, rightBracket) =
      switch (unary) {
      | `Abs(_) => ("|", "|")
      | `Floor(_) => ("&lfloor;", "&rfloor;")
      | `Ceil(_) => ("&lceil;", "&rceil;")
      | `Round(_) => ("&lfloorl;", "&rceil;")
      };
    let body =
      createElement("mo", leftBracket)
      ++ unaryArg
      ++ createElement("mo", rightBracket);
    append(
      elementWithIndex("mrow", i, i', body) |> wrapSuperscript(superscript),
      accum,
    );
  | `Rand(superscript) =>
    atomLikeWithIndex(~superscript, "mi", i, i', "Rand")->append(accum)
  | `RandInt({randIntA, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "Rand#")
        ++ createElement("mrow", randIntA ++ createElement("mo", ",") ++ b),
      );
    atomLikeWithIndex(~superscript, "mrow", i, i', body)->append(accum);
  | `NPR({statN, r}) =>
    let nucleus =
      createElement(~attributes=[("mathvariant", "bold")], "mi", "P");
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    append(elementWithIndex("mrow", i, i', body), accum);
  | `NCR({statN, r}) =>
    let nucleus =
      createElement(~attributes=[("mathvariant", "bold")], "mi", "C");
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    append(elementWithIndex("mrow", i, i', body), accum);
  | `Differential({body, differentialX}) =>
    let pre =
      createElement(
        "mfrac",
        createElement(~attributes=[("mathvariant", "normal")], "mi", "d")
        ++ createElement("mi", "dx"),
      );
    let post =
      createElement(
        ~attributes=[("align", "left")],
        "munder",
        createElement("mo", "|") ++ xSetRow(differentialX),
      );
    append(elementWithIndex("mrow", i, i', pre ++ body ++ post), accum);
  | `Integral({integralA, b, body}) =>
    let pre =
      createElement("mo", "&#x222B;")
      ++ integralA
      ++ b
      |> createElement("msubsup");
    let post = createElement("mi", "dx");
    append(elementWithIndex("mrow", i, i', pre ++ body ++ post), accum);
  | `Sum({rangeStart, rangeEnd}) =>
    append(
      elementWithIndex("mo", i, i', "&#x2211;")
      ++ xSetRow(rangeStart)
      ++ rangeEnd
      |> createElement("munderover"),
      accum,
    )
  | `Product({rangeStart, rangeEnd}) =>
    append(
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
    let body =
      createElement("mo", "[")
      ++ inner
      ++ createElement("mo", "]")
      |> createElement("mrow")
      |> wrapSuperscript(superscript);
    append(elementWithIndex("mrow", i, i', body), accum);
  };

let create = elements =>
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", "block"),
    ],
    TreeUtil.map(elements, defaultAccum, mapValue, reduceFn),
  );
