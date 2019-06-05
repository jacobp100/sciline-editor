open AST_ReduceMap;
open MML_Builders;

let map = (accum, range) => MML_Accum.toString(accum, range);

let reduce = (accum, element: t(string), range) =>
  switch (element) {
  | `OpenBracket => MML_Accum.openBracket(accum, range)
  | `CloseBracket(superscript) =>
    MML_Accum.closeBracket(accum, range, superscript)
  | `Superscript(superscript) =>
    placeholder(~superscript=Some(superscript), range)
    ->MML_Accum.append(accum)
  | `Base(base) =>
    elementWithIndex("mn", range, MML_Util.stringOfBase(base))
    ->MML_Accum.append(accum)
  | `Digit({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mn", range, atomNucleus)
    ->MML_Accum.append(accum)
  | `Degree =>
    elementWithIndex("mn", range, "&deg;")->MML_Accum.append(accum)
  | `ArcMinute =>
    let body = "<mn />" ++ createElement("mn", "&prime;");
    elementWithIndex("msup", range, body)->MML_Accum.append(accum);
  | `ArcSecond =>
    let body = "<mn />" ++ createElement("mn", "&#8243;");
    elementWithIndex("msup", range, body)->MML_Accum.append(accum);
  | `Conj =>
    let body = "<mn />" ++ createElement("mn", "*");
    elementWithIndex("msup", range, body)->MML_Accum.append(accum);
  | `DecimalSeparator =>
    elementWithIndex("mn", range, ".")->MML_Accum.append(accum)
  | `ImaginaryUnit(superscript) =>
    elementWithIndex(~superscript, "mi", range, "i")->MML_Accum.append(accum)
  | `Magnitude({magnitudeBase}) =>
    let body =
      createElement("mo", MML_Util.stringOfOperator(`Mul))
      ++ createElement("mn", "10");
    elementWithIndex(~superscript=Some(magnitudeBase), "mrow", range, body)
    ->MML_Accum.append(accum);
  | `Variable({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mi", range, atomNucleus)
    ->MML_Accum.append(accum)
  | `ConstPi(superscript) =>
    elementWithIndex(~superscript, "mi", range, "&pi;")
    ->MML_Accum.append(accum)
  | `ConstE(superscript) =>
    elementWithIndex(~superscript, "mi", range, "e")->MML_Accum.append(accum)
  | `CustomAtom({mml, superscript}) =>
    elementWithIndex(~superscript, "mrow", range, mml)
    ->MML_Accum.append(accum)
  | `Function(f) =>
    let attributes = f == AST_Types.Gamma ? [("mathvariant", "normal")] : [];
    elementWithIndex(~attributes, "mi", range, MML_Util.stringOfFunction(f))
    ->MML_Accum.append(accum);
  | `Factorial => elementWithIndex("mo", range, "!")->MML_Accum.append(accum)
  | (`Add | `Sub | `Mul | `Div | `Dot) as v =>
    elementWithIndex("mo", range, MML_Util.stringOfOperator(v))
    ->MML_Accum.append(accum)
  | `Frac({fracNum, den, superscript}) =>
    elementWithIndex(~superscript, "mfrac", range, fracNum ++ den)
    ->MML_Accum.append(accum)
  | `Sqrt({rootRadicand, superscript}) =>
    elementWithIndex(~superscript, "msqrt", range, rootRadicand)
    ->MML_Accum.append(accum)
  | `NRoot({nrootDegree, radicand, superscript}) =>
    elementWithIndex(~superscript, "mroot", range, radicand ++ nrootDegree)
    ->MML_Accum.append(accum)
  | `NLog({nlogBase}) =>
    let body = createElement("mi", "log") ++ nlogBase;
    elementWithIndex("msub", range, body)->MML_Accum.append(accum);
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
    elementWithIndex(~superscript, "mrow", range, body)
    ->MML_Accum.append(accum);
  | `Rand(superscript) =>
    elementWithIndex(~superscript, "mi", range, "Rand")
    ->MML_Accum.append(accum)
  | `RandInt({randIntA, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "Rand#")
        ++ createElement("mrow", randIntA ++ createElement("mo", ",") ++ b),
      );
    elementWithIndex(~superscript, "mrow", range, body)
    ->MML_Accum.append(accum);
  | (`NPR({statN, r}) | `NCR({statN, r})) as fnArg =>
    let symbol =
      switch (fnArg) {
      | `NPR(_) => "P"
      | `NCR(_) => "C"
      };
    let nucleus =
      createElement(~attributes=[("mathvariant", "bold")], "mi", symbol);
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    elementWithIndex("mrow", range, body)->MML_Accum.append(accum);
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
    elementWithIndex("mrow", range, pre ++ body ++ post)
    ->MML_Accum.append(accum);
  | `Integral({integralA, b, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;") ++ integralA ++ b,
      );
    let post = createElement("mi", "dx");
    elementWithIndex("mrow", range, pre ++ body ++ post)
    ->MML_Accum.append(accum);
  | (
      `Sum({iterationStart, iterationEnd}) |
      `Product({iterationStart, iterationEnd})
    ) as fnArg =>
    let atom =
      switch (fnArg) {
      | `Sum(_) => "&#x2211;"
      | `Product(_) => "&#x220F;"
      };
    let body =
      elementWithIndex("mo", range, atom)
      ++ xSetRow(iterationStart)
      ++ iterationEnd;
    createElement("munderover", body)->MML_Accum.append(accum);
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
    let body = createElement("mo", "[") ++ inner ++ createElement("mo", "]");
    elementWithIndex(~superscript, "mrow", range, body)
    ->MML_Accum.append(accum);
  };

let create = elements => {
  let body =
    if (Belt.Array.length(elements) != 0) {
      AST_ReduceMap.reduceMap(
        elements,
        ~reduce,
        ~map,
        ~initial=MML_Accum.empty,
      );
    } else {
      "";
    };
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", "block"),
    ],
    body,
  );
};
