type unitConversion = {
  fromUnits: TechniCalcCalculator.Unit_Types.units,
  toUnits: TechniCalcCalculator.Unit_Types.units,
};
type customAtom = {
  value: TechniCalcCalculator.Encoding.encoding,
  mml: string,
};

type t = {
  elements: string,
  unitConversions: array(unitConversion),
  customAtoms: array(customAtom),
  variables: array(string),
};

let specialCharBias = 256;
let specialCharBase = 32;

let encode = (input: array(AST_Types.t)): t => {
  let elements = Belt.Array.make(Belt.Array.length(input), "");
  let unitConversions = ref([]);
  let customAtoms = ref([]);
  let variables = ref([]);

  for (i in 0 to Belt.Array.length(input) - 1) {
    let code =
      switch (Belt.Array.getExn(input, i)) {
      | UnitConversion({fromUnits, toUnits}) =>
        let index = Belt.List.length(unitConversions^);
        unitConversions := [{fromUnits, toUnits}, ...unitConversions^];
        specialCharBias + 0 * specialCharBase + index;
      | CustomAtomS({value, mml}) =>
        let index = Belt.List.length(customAtoms^);
        customAtoms := [{value, mml}, ...customAtoms^];
        specialCharBias + 1 * specialCharBase + index;
      | VariableS(string) =>
        let index = Belt.List.length(variables^);
        variables := [string, ...variables^];
        specialCharBias + 2 * specialCharBase + index;
      | element => Encoding_Element.toInt(element)
      };

    Belt.Array.setExn(elements, i, Encoding_VarInt.encode(code));
  };

  {
    elements: Js.Array.joinWith("", elements),
    unitConversions: Belt.List.toArray(unitConversions^),
    customAtoms: Belt.List.toArray(customAtoms^),
    variables: Belt.List.toArray(variables^),
  };
};

let decode =
    ({elements, unitConversions, customAtoms, variables}: t)
    : array(AST_Types.t) => {
  let output =
    Belt.Array.make(Encoding_VarInt.bytesLength(elements), AST_Types.Arg);

  let elementIndex = ref(0);
  let stringIndex = ref(0);
  while (stringIndex^ < String.length(elements)) {
    let {Encoding_VarInt.value, charactersRead} =
      Encoding_VarInt.decode(elements, stringIndex^);
    let element =
      if (value < specialCharBias) {
        Encoding_Element.ofInt(value);
      } else {
        let specialCharType = (value - specialCharBias) / specialCharBase;
        let argumentIndex = (value - specialCharBias) mod specialCharBase;
        switch (specialCharType) {
        | 0 =>
          let {fromUnits, toUnits} =
            Belt.Array.getExn(unitConversions, argumentIndex);
          UnitConversion({fromUnits, toUnits});
        | 1 =>
          let {value, mml} = Belt.Array.getExn(customAtoms, argumentIndex);
          CustomAtomS({value, mml});
        | 2 =>
          let string = Belt.Array.getExn(variables, argumentIndex);
          VariableS(string);
        | _ => failwith("Invalid encoding")
        };
      };

    Belt.Array.setExn(output, elementIndex^, element);
    stringIndex := stringIndex^ + charactersRead;
    elementIndex := elementIndex^ + 1;
  };

  output;
};
