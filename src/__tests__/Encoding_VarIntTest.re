open Jest;

test("encodes and decodes", (.) => {
  for (value in 0 to 1024) {
    let encoded = Encoding_VarInt.encode(value);
    let {Encoding_VarInt.value: decoded, charactersRead} =
      Encoding_VarInt.decode(encoded, 0);
    expect(decoded)->toEqual(value);
    expect(charactersRead)->toEqual(Js.String.length(encoded));
  };

  Js.undefined;
});
