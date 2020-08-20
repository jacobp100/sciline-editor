open Jest;

test("encodes and decodes", (.) => {
  open AST_Types;
  let value = [|N1_S, Sub, N2_S, Add, N3_S|];
  let encoded = Encoding.encode(value);
  let decoded = Encoding.decode(encoded);

  expect(value)->toEqual(decoded);

  Js.undefined;
});
