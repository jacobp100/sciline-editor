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

test("should insert parts of fraction when deleting", (.) => {
  let (elements, index) = deleteIndex([|Frac2S, N1_S, Arg, N2_S, Arg|], 1);

  expect(elements)->toEqual([|N1_S, N2_S|]);
  expect(index)->toEqual(0);

  Js.undefined;
});

test("should not delete arg elements", (.) => {
  let (elements, index) = deleteIndex([|Magnitude1, N0_S, Arg|], 3);

  expect(elements)->toEqual([|Magnitude1, N0_S, Arg|]);
  expect(index)->toEqual(2);

  Js.undefined;
});
