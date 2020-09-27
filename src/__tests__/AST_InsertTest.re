open Jest;
open AST_Types;
open AST_Insert;

test("should insert fraction consuming non-operator characters", (.) => {
  let (elements, index) =
    insertIndex([|N1_S, Add, N2_S, N3_S, Add, N4_S|], Frac2S, 3)
    ->Belt.Option.getExn;

  expect(elements)
  ->toEqual([|N1_S, Add, Frac2S, N2_S, Arg, N3_S, Arg, Add, N4_S|]);
  expect(index)->toEqual(5);

  Js.undefined;
});
