open Jest;

test("encodes and decodes", (.) => {
  for (value in 0 to 1024) {
    let encoded = Encoding_VarInt.encodeElement(value);

    expect(Encoding_VarInt.decodedLength(encoded))->toEqual(1);

    let decoded =
      Encoding_VarInt.decodeU(encoded, (. x) => Some(x))->Belt.Option.getExn;
    expect(decoded)->toEqual([|value|]);
  };

  Js.undefined;
});
