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
  variables: array(string),
};

let specialCharBias = 256;
let specialCharBase = 32;

let encode = (input: array(AST.t)): t => {
  let unitConversions = ref(MutableArrayBuilder.empty);
  let customAtoms = ref(MutableArrayBuilder.empty);
  let variables = ref(MutableArrayBuilder.empty);

  let elements =
    input
    ->Belt.Array.mapU((. element) => {
        let code =
          switch (element) {
          | UnitConversion({fromUnits, toUnits}) =>
            let index = MutableArrayBuilder.length(unitConversions^);
            unitConversions :=
              (unitConversions^)
              ->MutableArrayBuilder.append({fromUnits, toUnits});
            specialCharBias + 0 * specialCharBase + index;
          | CustomAtomS({mml, value}) =>
            let index = MutableArrayBuilder.length(customAtoms^);
            customAtoms :=
              (customAtoms^)->MutableArrayBuilder.append({mml, value});
            specialCharBias + 1 * specialCharBase + index;
          | VariableS(string) =>
            let index = MutableArrayBuilder.length(variables^);
            variables := (variables^)->MutableArrayBuilder.append(string);
            specialCharBias + 2 * specialCharBase + index;
          | element => Encoding_Element.toInt(element)
          };

        Encoding_VarInt.encodeElement(code);
      })
    ->StringUtil.join;

  {
    elements,
    unitConversions: MutableArrayBuilder.toArray(unitConversions^),
    customAtoms: MutableArrayBuilder.toArray(customAtoms^),
    variables: MutableArrayBuilder.toArray(variables^),
  };
};

let decode =
    ({elements, unitConversions, customAtoms, variables}: t)
    : option(array(AST.t)) => {
  Encoding_VarInt.decodeU(elements, (. value) =>
    if (value < specialCharBias) {
      Encoding_Element.ofInt(value);
    } else {
      let specialCharType = (value - specialCharBias) / specialCharBase;
      let argumentIndex = (value - specialCharBias) mod specialCharBase;
      switch (specialCharType) {
      | 0 =>
        let {fromUnits, toUnits} =
          Belt.Array.getExn(unitConversions, argumentIndex);
        Some(UnitConversion({fromUnits, toUnits}));
      | 1 =>
        let {mml, value} = Belt.Array.getExn(customAtoms, argumentIndex);
        Some(CustomAtomS({mml, value}));
      | 2 =>
        let string = Belt.Array.getExn(variables, argumentIndex);
        Some(VariableS(string));
      | _ => None
      };
    }
  );
};
