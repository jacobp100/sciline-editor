open Jest;
open AST_Types;
open AST_Delete;

test("should delete empty magnitude", (.) => {
  let (elements, index) = deleteIndex([|Magnitude1, Arg|], 1);

  expect(elements)->toEqual([||]);
  expect(index)->toEqual(0);

  Js.undefined;
});

test("should not delete filled magnitude", (.) => {
  let (elements, index) = deleteIndex([|Magnitude1, N0_S, Arg|], 1);

  expect(elements)->toEqual([|Magnitude1, N0_S, Arg|]);
  expect(index)->toEqual(0);

  Js.undefined;
});
