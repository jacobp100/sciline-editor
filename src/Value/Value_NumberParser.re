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

type numState = {
  numBase: option(AST_Types.base),
  numString: string,
  numHasDecimal: bool,
  numSup: option(node),
  magSup: option(node),
};

let reduce = (state, element) =>
  switch (state, element) {
  | (
      {numBase: None, numString: "", numSup: None, magSup: None},
      `Base(numBase),
    ) =>
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
  | ({magSup: None}, `Magnitude({magnitudeBase})) when state.numString != "" =>
    Some({...state, magSup: Some(magnitudeBase)})
  | _ => None
  };

let toNode = state =>
  switch (state) {
  | {numString: "" | "."} => None
  | {numBase, numString, numSup, magSup} =>
    let base =
      switch (numBase) {
      | Some(Bin) => 2
      | Some(Oct) => 8
      | Some(Hex) => 16
      | None => 10
      };
    let out = AST.ofStringBase(base, numString);
    let out = numSup->Belt.Option.mapWithDefault(out, AST.pow(out));
    let out =
      magSup
      ->Belt.Option.map(AST.pow(AST.ofInt(10)))
      ->Belt.Option.mapWithDefault(out, AST.mul(out));
    Some(out);
  };

let empty = {
  numBase: None,
  numString: "",
  numHasDecimal: false,
  numSup: None,
  magSup: None,
};