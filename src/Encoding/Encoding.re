type unitConversion = {
  fromUnits: TechniCalcCalculator.Unit_Types.units,
  toUnits: TechniCalcCalculator.Unit_Types.units,
};
type customAtom = {
  mml: string,
  value: TechniCalcCalculator.Value_Encoding.encoding,
};

type t = {
  elements: string,
  unitConversions: array(unitConversion),
  customAtoms: array(customAtom),
  labels: array(string),
  variables: array(string),
};

let specialCharBias = 256;
let specialCharBase = 32;

let%private appendTo = (mutableArray, value, specialCharIndex) => {
  let index = MutableArrayBuilder.length(mutableArray^);
  mutableArray := MutableArrayBuilder.append(mutableArray^, value);
  specialCharBias + specialCharIndex * specialCharBase + index;
};

let encode = (input: array(AST.t)): t => {
  let unitConversions = ref(MutableArrayBuilder.empty);
  let customAtoms = ref(MutableArrayBuilder.empty);
  let labels = ref(MutableArrayBuilder.empty);
  let variables = ref(MutableArrayBuilder.empty);

  let elements =
    input
    ->Belt.Array.mapU((. element) => {
        let code =
          switch (element) {
          | UnitConversion({fromUnits, toUnits}) =>
            appendTo(unitConversions, {fromUnits, toUnits}, 0)
          | CustomAtomS({mml, value}) =>
            appendTo(customAtoms, {mml, value}, 1)
          | LabelS({mml}) => appendTo(labels, mml, 3)
          | VariableS(string) => appendTo(variables, string, 2)
          | element => Encoding_Element.toInt(element)
          };

        Encoding_VarInt.encodeElement(code);
      })
    ->StringUtil.join;

  {
    elements,
    unitConversions: MutableArrayBuilder.toArray(unitConversions^),
    customAtoms: MutableArrayBuilder.toArray(customAtoms^),
    labels: MutableArrayBuilder.toArray(labels^),
    variables: MutableArrayBuilder.toArray(variables^),
  };
};

let decode =
    ({elements, unitConversions, customAtoms, labels, variables}: t)
    : option(array(AST.t)) => {
  Encoding_VarInt.decodeU(elements, (. value) =>
    if (value < specialCharBias) {
      Encoding_Element.ofInt(value);
    } else {
      let specialCharType = (value - specialCharBias) / specialCharBase;
      let argumentIndex = (value - specialCharBias) mod specialCharBase;
      switch (specialCharType) {
      | 0 =>
        switch (Belt.Array.get(unitConversions, argumentIndex)) {
        | Some({fromUnits, toUnits}) =>
          Some(UnitConversion({fromUnits, toUnits}))
        | None => None
        }
      | 1 =>
        switch (Belt.Array.get(customAtoms, argumentIndex)) {
        | Some({mml, value}) => Some(CustomAtomS({mml, value}))
        | None => None
        }
      | 3 =>
        switch (Belt.Array.get(labels, argumentIndex)) {
        | Some(mml) => Some(LabelS({mml: mml}))
        | None => None
        }
      | 2 =>
        switch (Belt.Array.get(variables, argumentIndex)) {
        | Some(string) => Some(VariableS(string))
        | None => None
        }
      | _ => None
      };
    }
  );
};
