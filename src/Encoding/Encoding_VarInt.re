/* Characters set from URL-safe base64 variant */
let%private characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";

// TODO: Make private once we remove migrations in app
let charToIndex = character =>
  switch (character) {
  | 'a'..'z' => Char.code(character) - Char.code('a')
  | 'A'..'Z' => Char.code(character) - Char.code('A') + 26
  | '0'..'9' => Char.code(character) - Char.code('0') + 52
  | '-' => 62
  | '_' => 63
  | _ => assert(false)
  };

let encodeElement = index => {
  let rec iter = (~current="", ~flag=false, index) => {
    let power = index / 32;
    let char =
      StringUtil.stringCharAtUnsafe(
        characters,
        index mod 32 + (flag ? 32 : 0),
      );
    let current = char ++ current;
    if (power == 0) {
      current;
    } else {
      iter(~current, ~flag=true, power);
    };
  };
  iter(index);
};

let decodedLength = string => {
  let rec iter = (~elementIndex, ~stringIndex) =>
    if (stringIndex < String.length(string)) {
      let index = StringUtil.charAtUnsafe(string, stringIndex)->charToIndex;
      let isContinuation = index >= 32;
      iter(
        ~elementIndex=elementIndex + (isContinuation ? 0 : 1),
        ~stringIndex=stringIndex + 1,
      );
    } else {
      elementIndex;
    };
  iter(~elementIndex=0, ~stringIndex=0);
};

let%private decodeElement = (string, stringIndex, callback) => {
  let rec iter = (~accum=0, index) =>
    if (index < String.length(string)) {
      let value = StringUtil.charAtUnsafe(string, index)->charToIndex;

      if (value >= 32) {
        iter(~accum=(accum + value mod 32) * 32, index + 1);
      } else {
        stringIndex := index + 1;
        callback(. accum + value);
      };
    } else {
      None;
    };
  iter(stringIndex^);
};

let decodeU = (string, callback) => {
  let length = decodedLength(string);
  let output = Belt.Array.makeUninitializedUnsafe(length);

  let stringIndex = ref(0);

  let rec iter = index =>
    if (index < length) {
      switch (decodeElement(string, stringIndex, callback)) {
      | Some(element) =>
        Belt.Array.setExn(output, index, element);
        iter(index + 1);
      | None => None
      };
    } else {
      Some(output);
    };

  iter(0);
};
