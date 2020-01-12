open AST_ReduceMap;
open Value_Types;

module AST = ScilineCalculator.AST_Types;

let numberIsValidForBase = (base, atomNucleus) =>
  switch (base, atomNucleus) {
  | (_, "0" | "1")
  | (None | Some(AST_Types.Oct | Hex), "2" | "3" | "4" | "5" | "6" | "7")
  | (None | Some(Hex), "8" | "9")
  | (Some(Hex), "A" | "B" | "C" | "D" | "E" | "F") => true
  | _ => false
  };

type angle = {
  degree: option(node),
  arcMin: option(node),
  arcSec: option(node),
};

type numState = {
  numBase: option(AST_Types.base),
  numString: string,
  numHasDecimal: bool,
  numSup: option(node),
  magSup: option(node),
  angle: option(angle),
};

let rec reduce = (state, element) =>
  switch (state, element) {
  | ({numBase: None, numString: ""}, `Base(numBase)) =>
    Some({...state, numBase: Some(numBase)})
  | ({numSup: None, magSup: None}, `Digit({atomNucleus, superscript}))
      when numberIsValidForBase(state.numBase, atomNucleus) =>
    Some({
      ...state,
      numString: state.numString ++ atomNucleus,
      numSup: superscript,
    })
  | ({numHasDecimal: false, numSup: None, magSup: None}, `DecimalSeparator) =>
    Some({...state, numString: state.numString ++ ".", numHasDecimal: true})
  | ({magSup: None, angle: None}, `Magnitude({magnitudeBase}))
      when state.numString != "" =>
    Some({...state, magSup: Some(magnitudeBase)})
  | (
      {
        numBase: None,
        numSup: None,
        angle: None | Some({degree: None, arcMin: None, arcSec: None}),
      },
      `Degree,
    ) =>
    parseNumber(state)
    ->Belt.Option.map(degree => {
        let degree = degree->AST.mul(AST.div(AST.pi, AST.ofInt(180)))->Some;
        let angle = Some({degree, arcMin: None, arcSec: None});
        {...empty, angle};
      })
  | (
      {
        numBase: None,
        numSup: None,
        angle: None | Some({arcMin: None, arcSec: None}),
      },
      `ArcMinute,
    ) =>
    parseNumber(state)
    ->Belt.Option.map(arcMin => {
        let degree = Belt.Option.flatMap(state.angle, s => s.degree);
        let arcMin =
          arcMin->AST.mul(AST.div(AST.pi, AST.ofInt(10800)))->Some;
        let angle = Some({degree, arcMin, arcSec: None});
        {...empty, angle};
      })
  | (
      {numBase: None, numSup: None, angle: None | Some({arcSec: None})},
      `ArcSecond,
    ) =>
    parseNumber(state)
    ->Belt.Option.map(arcSec => {
        let degree = Belt.Option.flatMap(state.angle, s => s.degree);
        let arcMin = Belt.Option.flatMap(state.angle, s => s.arcMin);
        let arcSec =
          arcSec->AST.mul(AST.div(AST.pi, AST.ofInt(648000)))->Some;
        let angle = Some({degree, arcMin, arcSec});
        {...empty, angle};
      })
  | _ => None
  }
and parseNumber = state =>
  switch (state) {
  | {numBase: Some(_), numString: ""}
  | {numString: "."} => None
  | {numBase, numString, numSup, magSup} =>
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
    Some(out);
  }
and toNode = state =>
  switch (state) {
  /* Angles */
  | {numString: "", angle: Some({degree, arcMin, arcSec})} =>
    let out = AST.zero;
    let out = degree->Belt.Option.mapWithDefault(out, AST.add(out));
    let out = arcMin->Belt.Option.mapWithDefault(out, AST.add(out));
    let out = arcSec->Belt.Option.mapWithDefault(out, AST.add(out));
    Some(out);
  /* Normal parsing */
  | _ => parseNumber(state)
  }
and empty = {
  numBase: None,
  numString: "",
  numHasDecimal: false,
  numSup: None,
  magSup: None,
  angle: None,
};