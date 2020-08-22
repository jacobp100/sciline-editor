/* Characters set from URL-safe base64 variant */
let%private characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";

let%private charToIndex = character =>
  switch (character) {
  | 'a'..'z' => Char.code(character) - Char.code('a')
  | 'A'..'Z' => Char.code(character) - Char.code('A') + 26
  | '0'..'9' => Char.code(character) - Char.code('0') + 52
  | '-' => 62
  | '_' => 63
  | _ => failwith("Invalid character")
  };

let encodeElement = index =>
  if (index < 32) {
    StringUtil.stringCharAtUnsafe(characters, index);
  } else {
    let char0 = StringUtil.stringCharAtUnsafe(characters, index / 32 + 31);
    let char1 = StringUtil.stringCharAtUnsafe(characters, index mod 32);
    char0 ++ char1;
  };

let decodedLength = string => {
  let rec iter = (~elementIndex, ~stringIndex) =>
    if (stringIndex < String.length(string)) {
      let index0 = StringUtil.charAtUnsafe(string, stringIndex)->charToIndex;
      let charactersRead = index0 < 32 ? 1 : 2;
      iter(
        ~elementIndex=elementIndex + 1,
        ~stringIndex=stringIndex + charactersRead,
      );
    } else {
      elementIndex;
    };
  iter(~elementIndex=0, ~stringIndex=0);
};

let decodeU = (string, callback) => {
  let output =
    Belt.Array.makeUninitializedUnsafe(decodedLength(string))->Some->ref;

  let elementIndex = ref(0);
  let stringIndex = ref(0);
  while (stringIndex^ < String.length(string) && output^ != None) {
    let index0 = StringUtil.charAtUnsafe(string, stringIndex^)->charToIndex;
    let element =
      if (index0 < 32) {
        stringIndex := stringIndex^ + 1;
        callback(. index0);
      } else if (stringIndex^ + 1 < String.length(string)) {
        let index1 =
          StringUtil.charAtUnsafe(string, stringIndex^ + 1)->charToIndex;
        let value = index1 + (index0 - 31) * 32;
        stringIndex := stringIndex^ + 2;
        callback(. value);
      } else {
        None;
      };

    switch (output^, element) {
    | (Some(output), Some(element)) =>
      Belt.Array.setExn(output, elementIndex^, element)
    | _ => output := None
    };

    elementIndex := elementIndex^ + 1;
  };

  output^;
};
