open AST_ReduceMap;
open Mml_Builders;

let map = (accum, range) => Mml_Accum.toString(accum, range);

let%private bracketGroup =
            (leftBracket, rightBracket, arg, superscript, range) => {
  let body =
    createElement("mo", leftBracket)
    ++ arg
    ++ createElement("mo", rightBracket);
  elementWithRange(~superscript?, "mrow", range, body);
};

let%private sumProduct = (symbol, start, end_, range) => {
  let body = elementWithRange("mo", range, symbol) ++ xSetRow(start) ++ end_;
  createElement("munderover", body);
};

let%private nprNcr = (symbol, n, r, range) => {
  let nucleus =
    createElement(~attributes=[("mathvariant", "bold")], "mi", symbol);
  let body =
    createElement(
      "mmultiscripts",
      nucleus ++ r ++ "<none /><mprescripts />" ++ n ++ "<none />",
    );
  elementWithRange("mrow", range, body);
};

let%private table = (~numRows, ~numColumns, elements, superscript, range) => {
  let inner =
    Belt.List.makeBy(numRows, row =>
      Belt.List.makeBy(numColumns, column =>
        createElement(
          "mtd",
          elements->Belt.Array.getUnsafe(row * numColumns + column),
        )
      )
      ->String.concat("", _)
      ->createElement("mtr", _)
    )
    ->String.concat("", _)
    ->createElement("mtable", _);
  let body = createElement("mo", "[") ++ inner ++ createElement("mo", "]");
  elementWithRange(~superscript?, "mrow", range, body);
};

let reduce = (accum, element: t(string), range) =>
  switch (element) {
  | OpenBracket => Mml_Accum.appendOpenBracket(accum, range)
  | CloseBracket(superscript) =>
    Mml_Accum.appendCloseBracket(accum, range, superscript)
  | Digit({nucleus, superscript}) =>
    elementWithRange(~superscript?, "mn", range, nucleus)
    ->Mml_Accum.appendDigit(accum, _)
  | DecimalSeparator =>
    elementWithRange("mn", range, ".")
    ->Mml_Accum.appendDecimalSeparator(accum, _)
  | Base(base) =>
    elementWithRange("mn", range, Mml_Util.stringOfBase(base))
    ->Mml_Accum.appendBasePrefix(accum, _)
  | Superscript(superscript) =>
    let placeholder =
      createElement(
        ~attributes=Placeholder.attributes,
        Placeholder.element,
        Placeholder.body,
      );
    elementWithRange("msup", range, placeholder ++ superscript)
    ->Mml_Accum.append(accum, _);
  | Percent => elementWithRange("mn", range, "%")->Mml_Accum.append(accum, _)
  | Angle(angle) =>
    let symbol =
      switch (angle) {
      | Degree => "&#x00B0;"
      | ArcMinute => "&#x2032;"
      | ArcSecond => "&#x2033;"
      };
    elementWithRange("mo", range, symbol)->Mml_Accum.append(accum, _);
  | ImaginaryUnit(superscript) =>
    elementWithRange(~superscript?, "mi", range, "i")
    ->Mml_Accum.append(accum, _)
  | Conj =>
    elementWithRange("mo", range, "&#x2a;")->Mml_Accum.append(accum, _)
  | Magnitude({value}) =>
    let body =
      createElement("mo", Mml_Util.stringOfOperator(Mul))
      ++ createElement("mn", "10");
    let body = createElement("mrow", body);
    elementWithRange("msup", range, body ++ value)
    ->Mml_Accum.append(accum, _);
  | Variable({nucleus, superscript}) =>
    elementWithRange(~superscript?, "mi", range, nucleus)
    ->Mml_Accum.append(accum, _)
  | ConstPi(superscript) =>
    elementWithRange(~superscript?, "mi", range, "&#x03C0;")
    ->Mml_Accum.append(accum, _)
  | ConstE(superscript) =>
    elementWithRange(~superscript?, "mi", range, "e")
    ->Mml_Accum.append(accum, _)
  | CustomAtom({mml, superscript}) =>
    elementWithRange(~superscript?, "mrow", range, mml)
    ->Mml_Accum.append(accum, _)
  | Function({func, squareResultSuperscript: superscript}) =>
    let attributes =
      func == AST_ReduceMap.Gamma ? [("mathvariant", "normal")] : [];
    Mml_Util.stringOfFunction(func)
    ->elementWithRange(~superscript?, ~attributes, "mi", range, _)
    ->Mml_Accum.append(accum, _);
  | Factorial =>
    elementWithRange("mo", range, "!")->Mml_Accum.append(accum, _)
  | Operator(op) =>
    elementWithRange("mo", range, Mml_Util.stringOfOperator(op))
    ->Mml_Accum.append(accum, _)
  | Frac({num, den, superscript}) =>
    elementWithRange(~superscript?, "mfrac", range, num ++ den)
    ->Mml_Accum.append(accum, _)
  | Sqrt({radicand, superscript}) =>
    elementWithRange(~superscript?, "msqrt", range, radicand)
    ->Mml_Accum.append(accum, _)
  | NRoot({degree, radicand, superscript}) =>
    elementWithRange(~superscript?, "mroot", range, radicand ++ degree)
    ->Mml_Accum.append(accum, _)
  | NLog({base}) =>
    let body = createElement("mi", "log") ++ base;
    elementWithRange("msub", range, body)->Mml_Accum.append(accum, _);
  | Abs({arg, superscript}) =>
    bracketGroup("|", "|", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Floor({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x230B;", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Ceil({arg, superscript}) =>
    bracketGroup("&#x2308;", "&#x2309;", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Round({arg, superscript}) =>
    bracketGroup("&#x230A;", "&#x2309;", arg, superscript, range)
    ->Mml_Accum.append(accum, _)
  | Rand(superscript) =>
    elementWithRange(~superscript?, "mi", range, "Rand")
    ->Mml_Accum.append(accum, _)
  | RandInt({a, b, superscript}) =>
    let body =
      createElement(
        "msub",
        createElement("mi", "Rand#")
        ++ createElement("mrow", a ++ createElement("mo", ",") ++ b),
      );
    elementWithRange(~superscript?, "mrow", range, body)
    ->Mml_Accum.append(accum, _);
  | NPR({n, r}) => nprNcr("P", n, r, range)->Mml_Accum.append(accum, _)
  | NCR({n, r}) => nprNcr("C", n, r, range)->Mml_Accum.append(accum, _)
  | Differential({body, x}) =>
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
        createElement("mo", "|") ++ xSetRow(x),
      );
    elementWithRange("mrow", range, pre ++ body ++ post)
    ->Mml_Accum.append(accum, _);
  | Integral({a, b, body}) =>
    let pre =
      createElement("msubsup", createElement("mo", "&#x222B;") ++ a ++ b);
    let post = createElement("mi", "dx");
    elementWithRange("mrow", range, pre ++ body ++ post)
    ->Mml_Accum.append(accum, _);
  | Sum({start, end_}) =>
    sumProduct("&#x2211;", start, end_, range)->Mml_Accum.append(accum, _)
  | Product({start, end_}) =>
    sumProduct("&#x220F;", start, end_, range)->Mml_Accum.append(accum, _)
  | Vector({elements, superscript}) =>
    let numRows = Belt.Array.length(elements);
    table(~numRows, ~numColumns=1, elements, superscript, range)
    ->Mml_Accum.append(accum, _);
  | Table({elements, superscript, numRows, numColumns}) =>
    table(~numRows, ~numColumns, elements, superscript, range)
    ->Mml_Accum.append(accum, _)
  | UnitConversion({fromUnits, toUnits}) =>
    let body =
      Mml_Units.unitPowersToMml(fromUnits)
      ++ "<mo>&RightArrow;</mo>"
      ++ Mml_Units.unitPowersToMml(toUnits);
    elementWithRange("mrow", range, body)->Mml_Accum.append(accum, _);
  };

let create = (~digitGrouping=true, ~inline=false, elements) => {
  let body =
    if (Belt.Array.length(elements) != 0) {
      AST_ReduceMap.reduceMap(
        elements,
        ~reduce,
        ~map,
        ~initial=Mml_Accum.make(~digitGrouping),
      );
    } else {
      "";
    };
  createElement(
    "math",
    ~attributes=[
      ("xmlns", "http://www.w3.org/1998/Math/MathML"),
      ("display", inline ? "inline" : "block"),
    ],
    body,
  );
};
