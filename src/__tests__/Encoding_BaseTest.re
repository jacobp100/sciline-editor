open Jest;

test("integers", (.) => {
  for (value in 0 to 4096) {
    let encoded = Encoding_Base.encodeInt(value);

    let decoded = Encoding_Base.read(encoded, Encoding_Base.readInt);
    expect(decoded)->toEqual(Some(value));
  }
});

test("strings", (.) => {
  let string = "Hello world !@#$%^&*()";
  let encoded = Encoding_Base.encodeString(string);
  let decoded = Encoding_Base.read(encoded, Encoding_Base.readString);
  expect(decoded)->toEqual(Some(string));
});

test("arrays", (.) => {
  let array = [|1, 2, 3|];
  let encoded =
    Encoding_Base.encodeArray(array, (. value) => {
      Encoding_Base.encodeInt(value)
    });
  let decoded =
    Encoding_Base.read(encoded, reader => {
      Encoding_Base.readArray(reader, (. reader) => {
        Encoding_Base.readInt(reader)
      })
    });
  expect(decoded)->toEqual(Some(array));
});
