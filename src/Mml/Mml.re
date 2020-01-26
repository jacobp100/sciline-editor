open AST_ReduceMap;
open Mml_Builders;

let map =
    (type a, module Accum: Mml_Accum.Accum with type t = a, accum: a, range) =>
  Accum.toString(accum, range);

let reduce =
    (
      type a,
      module Accum: Mml_Accum.Accum with type t = a,
      accum: a,
      element: t(string),
      range,
    ) =>
  switch (element) {
  | `OpenBracket => Accum.appendOpenBracket(accum, range)
  | `CloseBracket(superscript) =>
    Accum.appendCloseBracket(accum, range, superscript)
  | `Digit({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mn", range, atomNucleus)
    ->Accum.appendDigit(accum, _)
  | `DecimalSeparator =>
    elementWithIndex("mn", range, ".")
    ->Accum.appendDecimalSeparator(accum, _)
  | `Base(base) =>
    elementWithIndex("mn", range, Mml_Util.stringOfBase(base))
    ->Accum.appendBasePrefix(accum, _)
  | `Superscript(superscript) =>
    placeholder(~superscript=Some(superscript), range)
    ->Accum.append(accum, _)
  | `Percent => elementWithIndex("mn", range, "%")->Accum.append(accum, _)
  | `Degree =>
    elementWithIndex("mo", range, "&#x00B0;")->Accum.append(accum, _)
  | `ArcMinute =>
    let body = "<mo />" ++ createElement("mo", "&#x2032;");
    ignore(elementWithIndex("msup", range, body)->Accum.append(accum, _));
    // FIXME - superscript
    elementWithIndex("mo", range, "&#x2032;")->Accum.append(accum, _);
  | `ArcSecond =>
    let body = "<mo />" ++ createElement("mo", "&#x2033;");
    ignore(elementWithIndex("msup", range, body)->Accum.append(accum, _));
    // FIXME - superscript
    elementWithIndex("mo", range, "&#x2033;")->Accum.append(accum, _);
  | `ImaginaryUnit(superscript) =>
    elementWithIndex(~superscript, "mi", range, "i")->Accum.append(accum, _)
  | `Conj => elementWithIndex("mo", range, "&#x2a;")->Accum.append(accum, _)
  | `Magnitude({magnitudeBase}) =>
    let body =
      createElement("mo", Mml_Util.stringOfOperator(`Mul))
      ++ createElement("mn", "10");
    elementWithIndex(~superscript=Some(magnitudeBase), "mrow", range, body)
    ->Accum.append(accum, _);
  | `Variable({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mi", range, atomNucleus)
    ->Accum.append(accum, _)
  | `ConstPi(superscript) =>
    elementWithIndex(~superscript, "mi", range, "&#x03C0;")
    ->Accum.append(accum, _)
  | `ConstE(superscript) =>
    elementWithIndex(~superscript, "mi", range, "e")->Accum.append(accum, _)
  | `CustomAtom({mml, superscript}) =>
    elementWithIndex(~superscript, "mrow", range, mml)
    ->Accum.append(accum, _)
  | `Function(f) =>
    let attributes = f == AST_Types.Gamma ? [("mathvariant", "normal")] : [];
    elementWithIndex(~attributes, "mi", range, Mml_Util.stringOfFunction(f))
    ->Accum.append(accum, _);
  | `Factorial => elementWithIndex("mo", range, "!")->Accum.append(accum, _)
  | (`Add | `Sub | `Mul | `Div | `Dot) as v =>
    elementWithIndex("mo", range, Mml_Util.stringOfOperator(v))
    ->Accum.append(accum, _)
  | `Frac({fracNum, den, superscript}) =>
    elementWithIndex(~superscript, "mfrac", range, fracNum ++ den)
    ->Accum.append(accum, _)
  | `Sqrt({rootRadicand, superscript}) =>
    elementWithIndex(~superscript, "msqrt", range, rootRadicand)
    ->Accum.append(accum, _)
  | `NRoot({nrootDegree, radicand, superscript}) =>
    elementWithIndex(~superscript, "mroot", range, radicand ++ nrootDegree)
    ->Accum.append(accum, _)
  | `NLog({nlogBase}) =>
    let body = createElement("mi", "log") ++ nlogBase;
    elementWithIndex("msub", range, body)->Accum.append(accum, _);
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
    ->Accum.append(accum, _);
  | `Rand(superscript) =>
    elementWithIndex(~superscript, "mi", range, "Rand")
    ->Accum.append(accum, _)
  | `RandInt({randIntA, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "Rand#")
        ++ createElement("mrow", randIntA ++ createElement("mo", ",") ++ b),
      );
    elementWithIndex(~superscript, "mrow", range, body)
    ->Accum.append(accum, _);
  | (`NPR({statN, r}) | `NCR({statN, r})) as fnArg =>
    let symbol =
      switch (fnArg) {
      | `NPR(_) => "P"
      | `NCR(_) => "C"
      };
    let nucleus =
      createElement(~attributes=[("mathvariant", "bold")], "mi", symbol);
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    elementWithIndex("mrow", range, body)->Accum.append(accum, _);
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
    ->Accum.append(accum, _);
  | `Integral({integralA, b, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;") ++ integralA ++ b,
      );
    let post = createElement("mi", "dx");
    elementWithIndex("mrow", range, pre ++ body ++ post)
    ->Accum.append(accum, _);
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
    createElement("munderover", body)->Accum.append(accum, _);
  | `Table({tableElements, superscript, numRows, numColumns}) =>
    let inner =
      Belt.List.makeBy(numRows, row =>
        Belt.List.makeBy(numColumns, column =>
          createElement(
            "mtd",
            tableElements->Belt.Array.getUnsafe(row * numColumns + column),
          )
        )
        ->String.concat("", _)
        ->createElement("mtr", _)
      )
      ->String.concat("", _)
      ->createElement("mtable", _);
    let body = createElement("mo", "[") ++ inner ++ createElement("mo", "]");
    elementWithIndex(~superscript, "mrow", range, body)
    ->Accum.append(accum, _);
  | `UnitConversion({fromUnits, toUnits}) =>
    let body =
      Mml_Units.unitsMml(fromUnits)
      ++ "<mo>&RightArrow;</mo>"
      ++ Mml_Units.unitsMml(toUnits);
    elementWithIndex("mrow", range, body)->Accum.append(accum, _);
  };

let create = elements => {
  module M = Mml_Accum.WithDigitGrouping;
  let body =
    if (Belt.Array.length(elements) != 0) {
      AST_ReduceMap.reduceMap(
        elements,
        ~reduce=reduce((module M)),
        ~map=map((module M)),
        ~initial=M.empty,
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