open Types;

[@bs.deriving abstract]
type key = {
  value: t,
  flags: int,
};

let flags_none = 0b000;
let flags_premium = 0b001;

let keys: Js.Dict.t(key) = Js.Dict.empty();

Js.Dict.set(
  keys,
  "0",
  key(~value=`Digit({atomNucleus: "0", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "1",
  key(~value=`Digit({atomNucleus: "1", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "2",
  key(~value=`Digit({atomNucleus: "2", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "3",
  key(~value=`Digit({atomNucleus: "3", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "4",
  key(~value=`Digit({atomNucleus: "4", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "5",
  key(~value=`Digit({atomNucleus: "5", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "6",
  key(~value=`Digit({atomNucleus: "6", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "7",
  key(~value=`Digit({atomNucleus: "7", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "8",
  key(~value=`Digit({atomNucleus: "8", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "9",
  key(~value=`Digit({atomNucleus: "9", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "A",
  key(~value=`Digit({atomNucleus: "A", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "B",
  key(~value=`Digit({atomNucleus: "B", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "C",
  key(~value=`Digit({atomNucleus: "C", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "D",
  key(~value=`Digit({atomNucleus: "D", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "E",
  key(~value=`Digit({atomNucleus: "E", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "F",
  key(~value=`Digit({atomNucleus: "F", superscript: []}), ~flags=flags_none),
);
Js.Dict.set(keys, ".", key(~value=`DecimalSeparator, ~flags=flags_none));
Js.Dict.set(keys, "+", key(~value=`Operator(Add), ~flags=flags_none));
Js.Dict.set(keys, "-", key(~value=`Operator(Sub), ~flags=flags_none));
Js.Dict.set(keys, "*", key(~value=`Operator(Mul), ~flags=flags_none));
Js.Dict.set(keys, "/", key(~value=`Operator(Div), ~flags=flags_none));
Js.Dict.set(
  keys,
  "_",
  key(
    ~value=`Frac({fracNum: [], den: [], superscript: []}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "^",
  key(~value=`Placeholder([`Placeholder([])]), ~flags=flags_none),
);
Js.Dict.set(keys, "!", key(~value=`Factorial, ~flags=flags_none));
Js.Dict.set(keys, "(", key(~value=`OpenBracket, ~flags=flags_none));
Js.Dict.set(keys, ")", key(~value=`CloseBracket([]), ~flags=flags_none));
Js.Dict.set(keys, "base2", key(~value=`Base(Bin), ~flags=flags_none));
Js.Dict.set(keys, "base8", key(~value=`Base(Oct), ~flags=flags_none));
Js.Dict.set(keys, "base16", key(~value=`Base(Hex), ~flags=flags_none));
Js.Dict.set(keys, "exp", key(~value=`Magnitude([]), ~flags=flags_none));
Js.Dict.set(
  keys,
  "sqrt",
  key(~value=`Sqrt({rootRadicand: [], superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "nroot",
  key(
    ~value=`NRoot({nrootDegree: [], radicand: [], superscript: []}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "abs",
  key(~value=`Abs({absArg: [], superscript: []}), ~flags=flags_none),
);
Js.Dict.set(keys, "log", key(~value=`Function(Log), ~flags=flags_none));
Js.Dict.set(
  keys,
  "log2",
  key(
    ~value=`NLog({nlogBase: [`Digit({atomNucleus: "2", superscript: []})]}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "log10",
  key(
    ~value=
      `NLog({
        nlogBase: [
          `Digit({atomNucleus: "1", superscript: []}),
          `Digit({atomNucleus: "0", superscript: []}),
        ],
      }),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "logn",
  key(~value=`NLog({nlogBase: []}), ~flags=flags_none),
);
Js.Dict.set(keys, "sin", key(~value=`Function(Sin), ~flags=flags_none));
Js.Dict.set(
  keys,
  "arcsin",
  key(~value=`Function(Arcsin), ~flags=flags_none),
);
Js.Dict.set(keys, "sinh", key(~value=`Function(Sinh), ~flags=flags_none));
Js.Dict.set(
  keys,
  "arcsinh",
  key(~value=`Function(Arcsinh), ~flags=flags_none),
);
Js.Dict.set(keys, "cos", key(~value=`Function(Cos), ~flags=flags_none));
Js.Dict.set(
  keys,
  "arccos",
  key(~value=`Function(Arccos), ~flags=flags_none),
);
Js.Dict.set(keys, "cosh", key(~value=`Function(Cosh), ~flags=flags_none));
Js.Dict.set(
  keys,
  "arccosh",
  key(~value=`Function(Arccosh), ~flags=flags_none),
);
Js.Dict.set(keys, "tan", key(~value=`Function(Tan), ~flags=flags_none));
Js.Dict.set(
  keys,
  "arctan",
  key(~value=`Function(Arctan), ~flags=flags_none),
);
Js.Dict.set(keys, "tanh", key(~value=`Function(Tanh), ~flags=flags_none));
Js.Dict.set(
  keys,
  "arctanh",
  key(~value=`Function(Arctanh), ~flags=flags_none),
);
Js.Dict.set(keys, "i", key(~value=`ImaginaryUnit([]), ~flags=flags_none));
Js.Dict.set(
  keys,
  "x",
  key(
    ~value=`Variable({atomNucleus: "x", superscript: []}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "pi",
  key(~value=`Constant({constant: Pi, superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "e",
  key(~value=`Constant({constant: E, superscript: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "ans",
  key(
    ~value=`Variable({atomNucleus: "Ans", superscript: []}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(keys, "re", key(~value=`Function(Re), ~flags=flags_none));
Js.Dict.set(keys, "im", key(~value=`Function(Im), ~flags=flags_none));
Js.Dict.set(keys, "conj", key(~value=`Conj, ~flags=flags_none));
Js.Dict.set(keys, "gamma", key(~value=`Function(Gamma), ~flags=flags_none));
Js.Dict.set(keys, "rand", key(~value=`Rand([]), ~flags=flags_none));
Js.Dict.set(
  keys,
  "randint",
  key(
    ~value=`RandInt({randIntA: [], b: [], superscript: []}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "npr",
  key(~value=`NPR({statN: [], r: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "ncr",
  key(~value=`NCR({statN: [], r: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "differential",
  key(
    ~value=`Differential({differentialX: [], body: []}),
    ~flags=flags_none,
  ),
);
Js.Dict.set(
  keys,
  "integral",
  key(~value=`Integral({integralA: [], b: [], body: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "sum",
  key(~value=`Sum({rangeStart: [], rangeEnd: []}), ~flags=flags_none),
);
Js.Dict.set(
  keys,
  "product",
  key(~value=`Product({rangeStart: [], rangeEnd: []}), ~flags=flags_none),
);
Js.Dict.set(keys, "dot", key(~value=`Operator(Dot), ~flags=flags_none));
Js.Dict.set(
  keys,
  "magnitude",
  key(~value=`Magnitude([]), ~flags=flags_none),
);
Js.Dict.set(keys, "degree", key(~value=`Degree, ~flags=flags_none));
Js.Dict.set(keys, "arcminute", key(~value=`ArcMinute, ~flags=flags_none));
Js.Dict.set(keys, "arcsecond", key(~value=`ArcSecond, ~flags=flags_none));
