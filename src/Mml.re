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

type range = (int, int, int);

let range = ({Tree.rangeStart: i, rangeEnd: i', superscriptIndex: s}) => (
  i,
  i',
  s,
);

let elementWithIndex =
    (~attributes=[], ~superscript="", element, (i, i', s), body) => {
  switch (superscript) {
  | "" =>
    let attributes = [
      ("id", string_of_int(i) ++ ":" ++ string_of_int(i')),
      ...attributes,
    ];
    createElement(~attributes, element, body);
  | superscript =>
    let base =
      createElement(
        ~attributes=[("id", ":" ++ string_of_int(s)), ...attributes],
        element,
        body,
      );
    createElement(
      ~attributes=[("id", string_of_int(i) ++ ":" ++ string_of_int(i'))],
      "msup",
      base ++ superscript,
    );
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

type accum = (string, list(bracketGroup))
and bracketGroup = {
  range,
  body: string,
};
let defaultAccum = ("", []);

let append = (element, (level0Body, bracketGroup)) =>
  switch (bracketGroup) {
  | [{body} as bracketGroup, ...rest] => (
      level0Body,
      [{...bracketGroup, body: body ++ element}, ...rest],
    )
  | [] => (level0Body ++ element, [])
  };
let openBracket = (arg, (level0Body, bracketGroup)) => (
  level0Body,
  [{range: arg->range, body: ""}, ...bracketGroup],
);

let invalidClass = "invalid";
let closeBracket = (superscript, arg, (level0Body, bracketGroup)) =>
  switch (bracketGroup) {
  | [closed, ...rest] =>
    let body =
      elementWithIndex("mo", closed.range, "(")
      ++ closed.body
      ++ elementWithIndex(~superscript, "mo", arg->range, ")")
      |> createElement("mrow");
    switch (rest) {
    | [next, ...rest] => (
        level0Body,
        [{...next, body: next.body ++ body}, ...rest],
      )
    | [] => (level0Body ++ body, [])
    };
  | [] =>
    let attributes = [("class", invalidClass), ("stretchy", "false")];
    let element =
      elementWithIndex(~attributes, ~superscript, "mo", arg->range, ")");
    (level0Body ++ element, []);
  };
let accumToString = ((level0Body, bracketGroup)) => {
  let attributes = [("class", invalidClass), ("stretchy", "false")];
  let closed =
    bracketGroup
    ->Belt.List.map(({range, body}) =>
        elementWithIndex(~attributes, "mo", range, "(") ++ body
      )
    ->Belt.List.reverse
    |> String.concat("");
  level0Body ++ closed;
};

let mapValue = ({Tree.accum}): string =>
  switch (accumToString(accum)) {
  | "" as body => body
  | body => createElement("mrow", body)
  };
let reduceFn = ({Tree.accum} as arg, element) =>
  switch (element) {
  | `OpenBracket => openBracket(arg, accum)
  | `CloseBracket(superscript) => closeBracket(superscript, arg, accum)
  | `Placeholder(superscript) =>
    let attributes = [("class", "placeholder")];
    let body = elementWithIndex(~attributes, "mi", arg->range, "&#x25a1;");
    /* It's done this way so the superscript doesn't have the placeholder class */
    wrapSuperscript(superscript, body)->append(accum);
  | `Base(base) =>
    elementWithIndex("mn", arg->range, stringOfBase(base))->append(accum)
  | `Digit({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mn", arg->range, atomNucleus)
    ->append(accum)
  | `Degree => elementWithIndex("mn", arg->range, "&deg;")->append(accum)
  | `ArcMinute =>
    let superscript = createElement("mn", "&prime;");
    elementWithIndex(~superscript, "mn", arg->range, "")->append(accum);
  | `ArcSecond =>
    let superscript = createElement("mn", "&#8243;");
    elementWithIndex(~superscript, "mn", arg->range, "")->append(accum);
  | `Conj =>
    let superscript = createElement("mn", "*");
    elementWithIndex(~superscript, "mn", arg->range, "")->append(accum);
  | `DecimalSeparator =>
    elementWithIndex("mn", arg->range, ".")->append(accum)
  | `ImaginaryUnit(superscript) =>
    elementWithIndex(~superscript, "mi", arg->range, "i")->append(accum)
  | `Magnitude(exponent) =>
    let body =
      createElement("mo", stringOfOperator(Mul))
      ++ createElement("mn", "10");
    elementWithIndex(~superscript=exponent, "mrow", arg->range, body)
    ->append(accum);
  | `Variable({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mi", arg->range, atomNucleus)
    ->append(accum)
  | `Constant({constant, superscript}) =>
    let body = stringOfConstant(constant);
    elementWithIndex(~superscript, "mi", arg->range, body)->append(accum);
  | `CustomAtom({mml, superscript}) =>
    elementWithIndex(~superscript, "mrow", arg->range, mml)->append(accum)
  | `Function(Gamma) =>
    let attributes = [("mathvariant", "normal")];
    elementWithIndex(~attributes, "mi", arg->range, stringOfFunction(Gamma))
    ->append(accum);
  | `Function(f) =>
    elementWithIndex("mi", arg->range, stringOfFunction(f))->append(accum)
  | `Factorial => elementWithIndex("mo", arg->range, "!")->append(accum)
  | `Operator(v) =>
    elementWithIndex("mo", arg->range, stringOfOperator(v))->append(accum)
  | `Frac({fracNum, den, superscript}) =>
    elementWithIndex(~superscript, "mfrac", arg->range, fracNum ++ den)
    ->append(accum)
  | `Sqrt({rootRadicand, superscript}) =>
    elementWithIndex(~superscript, "msqrt", arg->range, rootRadicand)
    ->append(accum)
  | `NRoot({nrootDegree, radicand, superscript}) =>
    let body = radicand ++ nrootDegree;
    elementWithIndex(~superscript, "mroot", arg->range, body)->append(accum);
  | `NLog({nlogBase}) =>
    let body = createElement("mi", "log") ++ nlogBase;
    elementWithIndex("msub", arg->range, body)->append(accum);
  | (`Abs(fnArg) | `Floor(fnArg) | `Ceil(fnArg) | `Round(fnArg)) as unary =>
    let {unaryArg, superscript} = fnArg;
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
    elementWithIndex(~superscript, "mrow", arg->range, body)->append(accum);
  | `Rand(superscript) =>
    elementWithIndex(~superscript, "mi", arg->range, "Rand")->append(accum)
  | `RandInt({randIntA, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "Rand#")
        ++ createElement("mrow", randIntA ++ createElement("mo", ",") ++ b),
      );
    elementWithIndex(~superscript, "mrow", arg->range, body)->append(accum);
  | (`NPR({statN, r}) | `NCR({statN, r})) as fnArg =>
    let symbol =
      switch (fnArg) {
      | `NPR(_) => "P"
      | `NCR(_) => "C"
      };
    let nucleus =
      createElement(~attributes=[("mathvariant", "bold")], "mi", symbol);
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    elementWithIndex("mrow", arg->range, body)->append(accum);
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
    elementWithIndex("mrow", arg->range, pre ++ body ++ post)->append(accum);
  | `Integral({integralA, b, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;") ++ integralA ++ b,
      );
    let post = createElement("mi", "dx");
    elementWithIndex("mrow", arg->range, pre ++ body ++ post)->append(accum);
  | (`Sum({rangeStart, rangeEnd}) | `Product({rangeStart, rangeEnd})) as fnArg =>
    let atom =
      switch (fnArg) {
      | `Sum(_) => "&#x2211;"
      | `Product(_) => "&#x220F;"
      };
    let body =
      elementWithIndex("mo", arg->range, atom)
      ++ xSetRow(rangeStart)
      ++ rangeEnd;
    createElement("munderover", body)->append(accum);
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
    elementWithIndex("mrow", arg->range, body)->append(accum);
  };

let create = elements =>
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", "block"),
    ],
    Tree.map(elements, defaultAccum, mapValue, reduceFn),
  );
