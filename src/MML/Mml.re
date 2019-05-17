open AST_ReduceMap;
open MML_Builders;

let map = (accum, range) => MML_Row.make(MML_Accum.toString(accum), range);

let reduce = (accum, element: t(MML_RowTypes.t), range) =>
  switch (element) {
  | `OpenBracket => MML_Accum.openBracket(range, accum)
  | `CloseBracket(superscript) =>
    MML_Accum.closeBracket(superscript, range, accum)
  | `Superscript(superscript) =>
    /* It's done this way so the superscript doesn't have the placeholder class */
    wrapSuperscript(superscript, MML_Util.placeholder(range))
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
    let superscript = createElement("mn", "&prime;")->MML_Row.makeUnsafe;
    elementWithIndex(~superscript, "mn", range, "")->MML_Accum.append(accum);
  | `ArcSecond =>
    let superscript = createElement("mn", "&#8243;")->MML_Row.makeUnsafe;
    elementWithIndex(~superscript, "mn", range, "")->MML_Accum.append(accum);
  | `Conj =>
    let superscript = createElement("mn", "*")->MML_Row.makeUnsafe;
    elementWithIndex(~superscript, "mn", range, "")->MML_Accum.append(accum);
  | `DecimalSeparator =>
    elementWithIndex("mn", range, ".")->MML_Accum.append(accum)
  | `ImaginaryUnit(superscript) =>
    elementWithIndex(~superscript, "mi", range, "i")->MML_Accum.append(accum)
  | `Magnitude({magnitudeBase}) =>
    let body =
      createElement("mo", MML_Util.stringOfOperator(Mul))
      ++ createElement("mn", "10");
    elementWithIndex(~superscript=magnitudeBase, "mrow", range, body)
    ->MML_Accum.append(accum);
  | `Variable({atomNucleus, superscript}) =>
    elementWithIndex(~superscript, "mi", range, atomNucleus)
    ->MML_Accum.append(accum)
  | `Constant({constant, superscript}) =>
    let body = MML_Util.stringOfConstant(constant);
    elementWithIndex(~superscript, "mi", range, body)
    ->MML_Accum.append(accum);
  | `CustomAtom({mml, superscript}) =>
    elementWithIndex(~superscript, "mrow", range, mml)
    ->MML_Accum.append(accum)
  | `Function(AST_Types.Gamma) =>
    let attributes = [("mathvariant", "normal")];
    elementWithIndex(
      ~attributes,
      "mi",
      range,
      MML_Util.stringOfFunction(Gamma),
    )
    ->MML_Accum.append(accum);
  | `Function(f) =>
    elementWithIndex("mi", range, MML_Util.stringOfFunction(f))
    ->MML_Accum.append(accum)
  | `Factorial => elementWithIndex("mo", range, "!")->MML_Accum.append(accum)
  | `Operator(v) =>
    elementWithIndex("mo", range, MML_Util.stringOfOperator(v))
    ->MML_Accum.append(accum)
  | `Frac({fracNum, den, superscript}) =>
    let body = fracNum->MML_Row.toPlaceholder ++ den->MML_Row.toPlaceholder;
    elementWithIndex(~superscript, "mfrac", range, body)
    ->MML_Accum.append(accum);
  | `Sqrt({rootRadicand, superscript}) =>
    let body = rootRadicand->MML_Row.toPlaceholder;
    elementWithIndex(~superscript, "msqrt", range, body)
    ->MML_Accum.append(accum);
  | `NRoot({nrootDegree, radicand, superscript}) =>
    let body =
      radicand->MML_Row.toPlaceholder ++ nrootDegree->MML_Row.toPlaceholder;
    elementWithIndex(~superscript, "mroot", range, body)
    ->MML_Accum.append(accum);
  | `NLog({nlogBase}) =>
    let body = createElement("mi", "log") ++ nlogBase->MML_Row.toPlaceholder;
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
      ++ unaryArg->MML_Row.toPlaceholder
      ++ createElement("mo", rightBracket);
    elementWithIndex(~superscript, "mrow", range, body)
    ->MML_Accum.append(accum);
  | `Rand(superscript) =>
    elementWithIndex(~superscript, "mi", range, "Rand")
    ->MML_Accum.append(accum)
  | `RandInt({randIntA, b, superscript}) =>
    let randIntA = randIntA->MML_Row.toPlaceholder;
    let b = b->MML_Row.toPlaceholder;
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
    let statN = statN->MML_Row.toPlaceholder;
    let r = r->MML_Row.toPlaceholder;
    let body = createElement("msubsup", nucleus ++ r ++ statN);
    elementWithIndex("mrow", range, body)->MML_Accum.append(accum);
  | `Differential({body, differentialX}) =>
    let pre =
      createElement(
        "mfrac",
        createElement(~attributes=[("mathvariant", "normal")], "mi", "d")
        ++ createElement("mi", "dx"),
      );
    let body = body->MML_Row.toPlaceholder;
    let post =
      createElement(
        ~attributes=[("align", "left")],
        "munder",
        createElement("mo", "|")
        ++ MML_Util.xSetRow(differentialX->MML_Row.toPlaceholder),
      );
    elementWithIndex("mrow", range, pre ++ body ++ post)
    ->MML_Accum.append(accum);
  | `Integral({integralA, b, body}) =>
    let pre =
      createElement(
        "msubsup",
        createElement("mo", "&#x222B;")
        ++ integralA->MML_Row.toPlaceholder
        ++ b->MML_Row.toPlaceholder,
      );
    let body = body->MML_Row.toPlaceholder;
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
      ++ MML_Util.xSetRow(iterationStart->MML_Row.toPlaceholder)
      ++ iterationEnd->MML_Row.toPlaceholder;
    createElement("munderover", body)->MML_Accum.append(accum);
  | `Table({tableElements, superscript, numRows, numColumns}) =>
    let inner =
      Belt.List.makeBy(numRows, row =>
        Belt.List.makeBy(numColumns, column =>
          createElement(
            "mtd",
            tableElements
            ->Belt.Array.getUnsafe(row * numColumns + column)
            ->MML_Row.toPlaceholder,
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
    elementWithIndex("mrow", range, body)->MML_Accum.append(accum);
  };

let create = elements =>
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", "block"),
    ],
    AST_ReduceMap.reduceMap(elements, ~reduce, ~map, ~initial=MML_Accum.empty)
    ->MML_Row.toString,
  );
