/* Characters set from URL-safe base64 variant */
let%private characters =
  Js.String.split(
    "",
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_",
  );

let%private charToIndex = character =>
  switch (character) {
  | 'a'..'z' => Char.code(character) - Char.code('a')
  | 'A'..'Z' => Char.code(character) - Char.code('A') + 26
  | '0'..'9' => Char.code(character) - Char.code('0') + 52
  | '-' => 62
  | '_' => 63
  | _ => failwith("Invalid character")
  };

let encode = (index: int) =>
  if (index >= 32) {
    let char0 = Belt.Array.getUnsafe(characters, index / 32 + 31);
    let char1 = Belt.Array.getUnsafe(characters, index mod 32);
    char0 ++ char1;
  } else {
    Belt.Array.getUnsafe(characters, index);
  };

type readResult = {
  value: int,
  charactersRead: int,
};

let bytesLength = string => {
  let rec iter = i =>
    if (i < String.length(string)) {
      let index0 = StringUtil.charAtUnsafe(string, i)->charToIndex;
      let charactersRead = index0 >= 32 ? 2 : 1;
      iter(i + charactersRead);
    } else {
      i;
    };
  iter(0);
};

let decode = (string, i) => {
  let index0 = StringUtil.charAtUnsafe(string, i)->charToIndex;
  if (index0 >= 32) {
    let index1 = StringUtil.charAtUnsafe(string, i + 1)->charToIndex;
    {value: index1 + (index0 - 31) * 32, charactersRead: 2};
  } else {
    {value: index0, charactersRead: 1};
  };
};
