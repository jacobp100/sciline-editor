open Types;

[@bs.deriving abstract]
type key = {
  value: t,
  flags: int,
};

module Flags = {
  let none = 0b000;
  let premium = 0b001;
};

let keys: Js.Dict.t(key) = Js.Dict.empty();

keys->Js.Dict.set(
  "0",
  key(~value=`Digit({atomNucleus: "0", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "1",
  key(~value=`Digit({atomNucleus: "1", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "2",
  key(~value=`Digit({atomNucleus: "2", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "3",
  key(~value=`Digit({atomNucleus: "3", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "4",
  key(~value=`Digit({atomNucleus: "4", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "5",
  key(~value=`Digit({atomNucleus: "5", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "6",
  key(~value=`Digit({atomNucleus: "6", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "7",
  key(~value=`Digit({atomNucleus: "7", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "8",
  key(~value=`Digit({atomNucleus: "8", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "9",
  key(~value=`Digit({atomNucleus: "9", superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "A",
  key(
    ~value=`Digit({atomNucleus: "A", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "B",
  key(
    ~value=`Digit({atomNucleus: "B", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "C",
  key(
    ~value=`Digit({atomNucleus: "C", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "D",
  key(
    ~value=`Digit({atomNucleus: "D", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "E",
  key(
    ~value=`Digit({atomNucleus: "E", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "F",
  key(
    ~value=`Digit({atomNucleus: "F", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(".", key(~value=`DecimalSeparator, ~flags=Flags.none));
keys->Js.Dict.set("+", key(~value=`Operator(Add), ~flags=Flags.none));
keys->Js.Dict.set("-", key(~value=`Operator(Sub), ~flags=Flags.none));
keys->Js.Dict.set("*", key(~value=`Operator(Mul), ~flags=Flags.none));
keys->Js.Dict.set("/", key(~value=`Operator(Div), ~flags=Flags.none));
keys->Js.Dict.set(
  "_",
  key(
    ~value=`Frac({fracNum: [], den: [], superscript: []}),
    ~flags=Flags.none,
  ),
);
keys->Js.Dict.set(
  "^",
  key(~value=`Placeholder([`Placeholder([])]), ~flags=Flags.none),
);
keys->Js.Dict.set("!", key(~value=`Factorial, ~flags=Flags.premium));
keys->Js.Dict.set("(", key(~value=`OpenBracket, ~flags=Flags.none));
keys->Js.Dict.set(")", key(~value=`CloseBracket([]), ~flags=Flags.none));
keys->Js.Dict.set("base2", key(~value=`Base(Bin), ~flags=Flags.premium));
keys->Js.Dict.set("base8", key(~value=`Base(Oct), ~flags=Flags.premium));
keys->Js.Dict.set("base16", key(~value=`Base(Hex), ~flags=Flags.premium));
keys->Js.Dict.set(
  "sqrt",
  key(~value=`Sqrt({rootRadicand: [], superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "nroot",
  key(
    ~value=`NRoot({nrootDegree: [], radicand: [], superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "abs",
  key(~value=`Abs({absArg: [], superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set("log", key(~value=`Function(Log), ~flags=Flags.premium));
keys->Js.Dict.set(
  "log2",
  key(
    ~value=`NLog({nlogBase: [`Digit({atomNucleus: "2", superscript: []})]}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "log10",
  key(
    ~value=
      `NLog({
        nlogBase: [
          `Digit({atomNucleus: "1", superscript: []}),
          `Digit({atomNucleus: "0", superscript: []}),
        ],
      }),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "logn",
  key(~value=`NLog({nlogBase: []}), ~flags=Flags.premium),
);
keys->Js.Dict.set("sin", key(~value=`Function(Sin), ~flags=Flags.premium));
keys->Js.Dict.set(
  "arcsin",
  key(~value=`Function(Arcsin), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "sinh",
  key(~value=`Function(Sinh), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "arcsinh",
  key(~value=`Function(Arcsinh), ~flags=Flags.premium),
);
keys->Js.Dict.set("cos", key(~value=`Function(Cos), ~flags=Flags.premium));
keys->Js.Dict.set(
  "arccos",
  key(~value=`Function(Arccos), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "cosh",
  key(~value=`Function(Cosh), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "arccosh",
  key(~value=`Function(Arccosh), ~flags=Flags.premium),
);
keys->Js.Dict.set("tan", key(~value=`Function(Tan), ~flags=Flags.premium));
keys->Js.Dict.set(
  "arctan",
  key(~value=`Function(Arctan), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "tanh",
  key(~value=`Function(Tanh), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "arctanh",
  key(~value=`Function(Arctanh), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "i",
  key(~value=`ImaginaryUnit([]), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "x",
  key(
    ~value=`Variable({atomNucleus: "x", superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "pi",
  key(~value=`Constant({constant: Pi, superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "e",
  key(~value=`Constant({constant: E, superscript: []}), ~flags=Flags.none),
);
keys->Js.Dict.set(
  "ans",
  key(
    ~value=`Variable({atomNucleus: "Ans", superscript: []}),
    ~flags=Flags.none,
  ),
);
keys->Js.Dict.set("re", key(~value=`Function(Re), ~flags=Flags.premium));
keys->Js.Dict.set("im", key(~value=`Function(Im), ~flags=Flags.premium));
keys->Js.Dict.set("conj", key(~value=`Conj, ~flags=Flags.premium));
keys->Js.Dict.set(
  "gamma",
  key(~value=`Function(Gamma), ~flags=Flags.premium),
);
keys->Js.Dict.set("rand", key(~value=`Rand([]), ~flags=Flags.premium));
keys->Js.Dict.set(
  "randint",
  key(
    ~value=`RandInt({randIntA: [], b: [], superscript: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "npr",
  key(~value=`NPR({statN: [], r: []}), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "ncr",
  key(~value=`NCR({statN: [], r: []}), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "differential",
  key(
    ~value=`Differential({differentialX: [], body: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "integral",
  key(
    ~value=`Integral({integralA: [], b: [], body: []}),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "sum",
  key(~value=`Sum({rangeStart: [], rangeEnd: []}), ~flags=Flags.premium),
);
keys->Js.Dict.set(
  "product",
  key(~value=`Product({rangeStart: [], rangeEnd: []}), ~flags=Flags.premium),
);
keys->Js.Dict.set("dot", key(~value=`Operator(Dot), ~flags=Flags.none));
keys->Js.Dict.set(
  "magnitude",
  key(~value=`Magnitude([]), ~flags=Flags.none),
);
keys->Js.Dict.set("degree", key(~value=`Degree, ~flags=Flags.premium));
keys->Js.Dict.set("arcminute", key(~value=`ArcMinute, ~flags=Flags.premium));
keys->Js.Dict.set("arcsecond", key(~value=`ArcSecond, ~flags=Flags.premium));
keys->Js.Dict.set(
  "vec2",
  key(
    ~value=
      `Table({
        tableElements: [|[`Placeholder([])], [`Placeholder([])]|],
        superscript: [],
        numRows: 2,
        numColumns: 1,
      }),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "vec3",
  key(
    ~value=
      `Table({
        tableElements: [|
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
        |],
        superscript: [],
        numRows: 3,
        numColumns: 1,
      }),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "mat2",
  key(
    ~value=
      `Table({
        tableElements: [|
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
        |],
        superscript: [],
        numRows: 2,
        numColumns: 2,
      }),
    ~flags=Flags.premium,
  ),
);
keys->Js.Dict.set(
  "mat3",
  key(
    ~value=
      `Table({
        tableElements: [|
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
          [`Placeholder([])],
        |],
        superscript: [],
        numRows: 3,
        numColumns: 3,
      }),
    ~flags=Flags.premium,
  ),
);
