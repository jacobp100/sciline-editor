open AST_ReduceMap;
open Mml_Builders;

let map = (accum, range) => Mml_Accum.toString(accum, range);

let reduce = (accum, element: t(string), range) =>
  switch (element) {
  | `OpenBracket => Mml_Accum.appendOpenBracket(accum, range)
  | `CloseBracket(superscript) =>
    Mml_Accum.appendCloseBracket(accum, range, superscript)
  | `Digit({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mn", range, atomNucleus)
    ->Mml_Accum.appendDigit(accum)
  | `DecimalSeparator =>
    elementWithIndex("mn", range, ".")
    ->Mml_Accum.appendDecimalSeparator(accum)
  | `Base(base) =>
    elementWithIndex("mn", range, Mml_Util.stringOfBase(base))
    ->Mml_Accum.appendBasePrefix(accum)
  | `Superscript(superscript) =>
    placeholder(~superscript=Some(superscript), range)
    ->Mml_Accum.append(accum)
  | `Percent => elementWithIndex("mn", range, "%")->Mml_Accum.append(accum)
  | `Degree =>
    elementWithIndex("mo", range, "&#x00B0;")->Mml_Accum.append(accum)
  | `ArcMinute =>
    let body = "<mo />" ++ createElement("mo", "&#x2032;");
    ignore(elementWithIndex("msup", range, body)->Mml_Accum.append(accum));
    // FIXME - superscript
    elementWithIndex("mo", range, "&#x2032;")->Mml_Accum.append(accum);
  | `ArcSecond =>
    let body = "<mo />" ++ createElement("mo", "&#x2033;");
    ignore(elementWithIndex("msup", range, body)->Mml_Accum.append(accum));
    // FIXME - superscript
    elementWithIndex("mo", range, "&#x2033;")->Mml_Accum.append(accum);
  | `ImaginaryUnit(superscript) =>
    elementWithIndex(~superscript, "mi", range, "i")->Mml_Accum.append(accum)
  | `Conj => elementWithIndex("mo", range, "&#x2a;")->Mml_Accum.append(accum)
  | `Magnitude({magnitudeBase}) =>
    let body =
      createElement("mo", Mml_Util.stringOfOperator(`Mul))
      ++ createElement("mn", "10");
    elementWithIndex(~superscript=Some(magnitudeBase), "mrow", range, body)
    ->Mml_Accum.append(accum);
  | `Variable({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mi", range, atomNucleus)
    ->Mml_Accum.append(accum)
  | `ConstPi(superscript) =>
    elementWithIndex(~superscript, "mi", range, "&#x03C0;")
    ->Mml_Accum.append(accum)
  | `ConstE(superscript) =>
    elementWithIndex(~superscript, "mi", range, "e")->Mml_Accum.append(accum)
  | `CustomAtom({mml, superscript}) =>
    elementWithIndex(~superscript, "mrow", range, mml)
    ->Mml_Accum.append(accum)
  | `Function(f) =>
    let attributes = f == AST_Types.Gamma ? [("mathvariant", "normal")] : [];
    elementWithIndex(~attributes, "mi", range, Mml_Util.stringOfFunction(f))
    ->Mml_Accum.append(accum);
  | `Factorial => elementWithIndex("mo", range, "!")->Mml_Accum.append(accum)
  | (`Add | `Sub | `Mul | `Div | `Dot) as v =>
    elementWithIndex("mo", range, Mml_Util.stringOfOperator(v))
    ->Mml_Accum.append(accum)
  | `Frac({fracNum, den, superscript}) =>
    elementWithIndex(~superscript, "mfrac", range, fracNum ++ den)
    ->Mml_Accum.append(accum)
  | `Sqrt({rootRadicand, superscript}) =>
    elementWithIndex(~superscript, "msqrt", range, rootRadicand)
    ->Mml_Accum.append(accum)
  | `NRoot({nrootDegree, radicand, superscript}) =>
    elementWithIndex(~superscript, "mroot", range, radicand ++ nrootDegree)
    ->Mml_Accum.append(accum)
  | `NLog({nlogBase}) =>
    let body = createElement("mi", "log") ++ nlogBase;
    elementWithIndex("msub", range, body)->Mml_Accum.append(accum);
  | (`Abs(fnArg) | `Floor(fnArg) | `Ceil(fnArg) | `Round(fnArg)) as unary =>
    let {unaryArg, superscript} = fnArg;
    let (leftBracket, rightBracket) =
      switch (unary) {
      | `Abs(_) => ("|", "|")
      | `Floor(_) => ("&#x230A;", "&#x230B;")
      | `Ceil(_) => ("&#x2308;", "&#x2309;")
      | `Round(_) => ("&#x230A;", "&#x2309;")
      };
    let body =
      createElement("mo", leftBracket)
      ++ unaryArg
      ++ createElement("mo", rightBracket);
    elementWithIndex(~superscript, "mrow", range, body)
    ->Mml_Accum.append(accum);
  | `Rand(superscript) =>
    elementWithIndex(~superscript, "mi", range, "Rand")
    ->Mml_Accum.append(accum)
  | `RandInt({randIntA, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "Rand#")
        ++ createElement("mrow", randIntA ++ createElement("mo", ",") ++ b),
      );
    elementWithIndex(~superscript, "mrow", range, body)
    ->Mml_Accum.append(accum);
  | (`NPR({statN, r}) | `NCR({statN, r})) as fnArg =>
    let symbol =
      switch (fnArg) {
      | `NPR(_) => "P"
      | `NCR(_) => "C"
      };
    let nucleus =
      createElement(~attributes=[("mathvariant", "bold")], "mi", symbol);
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    elementWithIndex("mrow", range, body)->Mml_Accum.append(accum);
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
    ->Mml_Accum.append(accum);
  | `Integral({integralA, b, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;") ++ integralA ++ b,
      );
    let post = createElement("mi", "dx");
    elementWithIndex("mrow", range, pre ++ body ++ post)
    ->Mml_Accum.append(accum);
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
    createElement("munderover", body)->Mml_Accum.append(accum);
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
    ->Mml_Accum.append(accum);
  | `UnitConversion({fromUnits, toUnits}) =>
    let body =
      Mml_Units.unitsMml(fromUnits)
      ++ "<mo>&RightArrow;</mo>"
      ++ Mml_Units.unitsMml(toUnits);
    elementWithIndex("mrow", range, body)->Mml_Accum.append(accum);
  };

let create = elements => {
  let body =
    if (Belt.Array.length(elements) != 0) {
      AST_ReduceMap.reduceMap(
        elements,
        ~reduce,
        ~map,
        ~initial=Mml_Accum.empty,
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