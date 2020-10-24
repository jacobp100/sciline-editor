type t = {
  [@bs.as "q"]
  elements: string,
  /*
   All marked as optional for url encoding.
   Will never be return None from this module, however.
   */
  [@bs.as "u"]
  unitConversions: option(array(string)),
  [@bs.as "c"]
  customAtoms: option(array(string)),
  [@bs.as "l"]
  labels: option(array(string)),
  [@bs.as "v"]
  variables: option(array(string)),
};

let specialCharBias = 256;
let specialCharBase = 32;

let%private appendTo = (mutableArray, value, specialCharIndex) => {
  let index = MutableArrayBuilder.length(mutableArray^);
  mutableArray := MutableArrayBuilder.append(mutableArray^, value);
  specialCharBias + specialCharIndex * specialCharBase + index;
};

let%private encodeUnitConversion = (~fromUnits, ~toUnits) =>
  Encoding_Units.ofUnits(fromUnits) ++ ":" ++ Encoding_Units.ofUnits(toUnits);
let%private decodeUnitConversion = string =>
  switch (StringUtil.split(string, ~separator=":")) {
  | [|fromUnits, toUnits|] =>
    switch (
      Encoding_Units.ofString(fromUnits),
      Encoding_Units.ofString(toUnits),
    ) {
    | (Some(fromUnits), Some(toUnits)) =>
      Some(AST_Types.UnitConversion({fromUnits, toUnits}))
    | _ => None
    }
  | _ => None
  };

let%private encodeCustomAtom = (~mml, ~value) => mml ++ ":" ++ value;
let%private decodeCustomAtom = string =>
  switch (StringUtil.split(string, ~separator=":")) {
  | [|mml, value|] => Some(AST_Types.CustomAtomS({mml, value}))
  | _ => None
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
            encodeUnitConversion(~fromUnits, ~toUnits)
            ->appendTo(unitConversions, _, 0)
          | CustomAtomS({mml, value}) =>
            encodeCustomAtom(~mml, ~value)->appendTo(customAtoms, _, 1)
          | LabelS({mml}) => appendTo(labels, mml, 3)
          | VariableS(string) => appendTo(variables, string, 2)
          | element => Encoding_Element.toInt(element)
          };

        Encoding_VarInt.encodeElement(code);
      })
    ->StringUtil.join;

  {
    elements,
    unitConversions: MutableArrayBuilder.toArray(unitConversions^)->Some,
    customAtoms: MutableArrayBuilder.toArray(customAtoms^)->Some,
    labels: MutableArrayBuilder.toArray(labels^)->Some,
    variables: MutableArrayBuilder.toArray(variables^)->Some,
  };
};

let%private optionalArrayGet = (array: option(array(string)), index) =>
  switch (array) {
  | Some(array) => Belt.Array.get(array, index)
  | None => None
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
        switch (optionalArrayGet(unitConversions, argumentIndex)) {
        | Some(encoded) => decodeUnitConversion(encoded)
        | None => None
        }
      | 1 =>
        switch (optionalArrayGet(customAtoms, argumentIndex)) {
        | Some(encoded) => decodeCustomAtom(encoded)
        | None => None
        }
      | 3 =>
        switch (optionalArrayGet(labels, argumentIndex)) {
        | Some(mml) => Some(LabelS({mml: mml}))
        | None => None
        }
      | 2 =>
        switch (optionalArrayGet(variables, argumentIndex)) {
        | Some(string) => Some(VariableS(string))
        | None => None
        }
      | _ => None
      };
    }
  );
};
