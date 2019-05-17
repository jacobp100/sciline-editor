open AST_ReduceMap;
open Value_Types;

let numberIsValidForBase = (base, atomNucleus) =>
  switch (base, atomNucleus) {
  | (_, "0" | "1")
  | (None | Some(AST_Types.Oct | Hex), "2" | "3" | "4" | "5" | "6" | "7")
  | (None | Some(Hex), "8" | "9")
  | (Some(Hex), "A" | "B" | "C" | "D" | "E" | "F") => true
  | _ => false
  };

type numState('a, 'b) = {
  numBase: option(AST_Types.base),
  numString: string,
  numHasDecimal: bool,
  numSup: option(AST.t),
  imag: option(AST.t),
  magSup: option(AST.t),
  degree: option(AST.t),
  arcMin: option(AST.t),
  arcSec: option(AST.t),
};

let rec reduce = (state, element) =>
  switch (state, element) {
  | ({numString: "", numBase: None, imag: None}, `Base(numBase)) =>
    Some({...state, numBase: Some(numBase)})
  | (
      {numSup: None, imag: None, magSup: None},
      `Digit({atomNucleus, superscript}),
    )
      when numberIsValidForBase(state.numBase, atomNucleus) =>
    let numSup =
      switch (superscript) {
      | Node(v) => Some(v)
      | _ => None
      };
    Some({...state, numString: state.numString ++ atomNucleus, numSup});
  | (
      {numHasDecimal: false, numSup: None, imag: None, magSup: None},
      `DecimalSeparator,
    ) =>
    Some({...state, numString: state.numString ++ ".", numHasDecimal: true})
  /* Allow 3^2i, 3^2 i^2, but not 3i^2, because that's ambiguous */
  | (
      {numSup: Some(_) | None, magSup: None, imag: None},
      `ImaginaryUnit(Empty),
    ) =>
    Some({...state, imag: Some(AST.i)})
  | (
      {numSup: None, magSup: None, imag: None},
      `ImaginaryUnit(Node(imagSuperscript)),
    ) =>
    Some({...state, imag: Some(AST.pow(AST.i, imagSuperscript))})
  | (_, `Magnitude({magnitudeBase: Node(magSuperscript)}))
      when state.numString != "" =>
    Some({...state, magSup: Some(magSuperscript)})
  | (
      {numSup: None, imag: None, degree: None, arcMin: None, arcSec: None},
      `Degree,
    ) =>
    parseNumber(state)
    ->Belt.Option.map(degree => {
        let degree = degree->AST.mul(AST.div(AST.pi, AST.ofInt(180)))->Some;
        {...empty, degree};
      })
  | ({numSup: None, imag: None, arcMin: None, arcSec: None}, `ArcMinute) =>
    parseNumber(state)
    ->Belt.Option.map(arcMin => {
        let {degree} = state;
        let arcMin =
          arcMin->AST.mul(AST.div(AST.pi, AST.ofInt(10800)))->Some;
        {...empty, degree, arcMin};
      })
  | ({numSup: None, imag: None, arcSec: None}, `ArcSecond) =>
    parseNumber(state)
    ->Belt.Option.map(arcSec => {
        let {degree, arcMin} = state;
        let arcSec =
          arcSec->AST.mul(AST.div(AST.pi, AST.ofInt(648000)))->Some;
        {...empty, degree, arcMin, arcSec};
      })
  | _ => None
  }
and parseNumber = ({numBase, numString, numSup, imag, magSup}) =>
  if ((numBase == None || numString != "")
      && (imag != None || numString != "")
      && numString != ".") {
    let base =
      switch (numBase) {
      | Some(Bin) => 2
      | Some(Oct) => 8
      | Some(Hex) => 16
      | None => 10
      };
    let out = numString == "" ? AST.one : AST.ofStringBase(base, numString);
    let out = numSup->Belt.Option.mapWithDefault(out, AST.pow(out));
    let out =
      magSup
      ->Belt.Option.map(AST.pow(AST.ofInt(10)))
      ->Belt.Option.mapWithDefault(out, AST.mul(out));
    let out = imag->Belt.Option.mapWithDefault(out, AST.mul(out));
    Some(out);
  } else {
    None;
  }
and get = ({degree, arcMin, arcSec} as s) =>
  if (degree == None && arcMin == None && arcSec == None) {
    parseNumber(s);
  } else if (s.numBase == None
             && s.numString == ""
             && s.imag == None
             && s.magSup == None) {
    let out = AST.zero;
    let out = degree->Belt.Option.mapWithDefault(out, AST.add(out));
    let out = arcMin->Belt.Option.mapWithDefault(out, AST.add(out));
    let out = arcSec->Belt.Option.mapWithDefault(out, AST.add(out));
    Some(out);
  } else {
    None;
  }
and empty = {
  numBase: None,
  numString: "",
  numSup: None,
  numHasDecimal: false,
  imag: None,
  magSup: None,
  degree: None,
  arcMin: None,
  arcSec: None,
};
