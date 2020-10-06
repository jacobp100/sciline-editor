open Jest;
open AST_Types;
open AST_Insert;

test("should insert fraction consuming characters", (.) => {
  let (elements, index) =
    insertIndex([|N1_S, N2_S|], Frac2S, 1)->Belt.Option.getExn;

  expect(elements)->toEqual([|Frac2S, N1_S, Arg, N2_S, Arg|]);
  expect(index)->toEqual(3);

  Js.undefined;
});

test("should insert fraction consuming non-operator characters", (.) => {
  let (elements, index) =
    insertIndex([|N1_S, Add, N2_S, N3_S, Add, N4_S|], Frac2S, 3)
    ->Belt.Option.getExn;

  expect(elements)
  ->toEqual([|N1_S, Add, Frac2S, N2_S, Arg, N3_S, Arg, Add, N4_S|]);
  expect(index)->toEqual(5);

  Js.undefined;
});

test("should insert fraction inside bracket group", (.) => {
  let (elements, index) =
    insertIndex(
      [|N1_S, OpenBracket, N2_S, N3_S, CloseBracketS, N4_S|],
      Frac2S,
      3,
    )
    ->Belt.Option.getExn;

  expect(elements)
  ->toEqual([|
      N1_S,
      OpenBracket,
      Frac2S,
      N2_S,
      Arg,
      N3_S,
      Arg,
      CloseBracketS,
      N4_S,
    |]);
  expect(index)->toEqual(5);

  Js.undefined;
});

test("should move bracket groups when inserting fraction after", (.) => {
  let (elements, index) =
    insertIndex([|OpenBracket, N1_S, CloseBracketS|], Frac2S, 3)
    ->Belt.Option.getExn;

  expect(elements)
  ->toEqual([|Frac2S, OpenBracket, N1_S, CloseBracketS, Arg, Arg|]);
  expect(index)->toEqual(5);

  Js.undefined;
});

test("should move bracket groups when inserting fraction before", (.) => {
  let (elements, index) =
    insertIndex([|OpenBracket, N1_S, CloseBracketS|], Frac2S, 0)
    ->Belt.Option.getExn;

  expect(elements)
  ->toEqual([|Frac2S, Arg, OpenBracket, N1_S, CloseBracketS, Arg|]);
  expect(index)->toEqual(1);

  Js.undefined;
});

test("should move function when inserting fraction before", (.) => {
  let (elements, index) =
    insertIndex([|Superscript1, Arg|], Frac2S, 2)->Belt.Option.getExn;

  expect(elements)->toEqual([|Frac2S, Superscript1, Arg, Arg, Arg|]);
  expect(index)->toEqual(4);

  Js.undefined;
});

test("should move function when inserting fraction after", (.) => {
  let (elements, index) =
    insertIndex([|Superscript1, Arg|], Frac2S, 0)->Belt.Option.getExn;

  expect(elements)->toEqual([|Frac2S, Arg, Superscript1, Arg, Arg|]);
  expect(index)->toEqual(1);

  Js.undefined;
});

test("should insert fraction in 1st argument of 2ary function", (.) => {
  let (elements, index) =
    insertIndex([|RandInt2S, Arg, Arg|], Frac2S, 1)->Belt.Option.getExn;

  expect(elements)->toEqual([|RandInt2S, Frac2S, Arg, Arg, Arg, Arg|]);
  expect(index)->toEqual(2);

  Js.undefined;
});

test("should insert fraction in 2nd argument of 2ary function", (.) => {
  let (elements, index) =
    insertIndex([|RandInt2S, Arg, Arg|], Frac2S, 2)->Belt.Option.getExn;

  expect(elements)->toEqual([|RandInt2S, Arg, Frac2S, Arg, Arg, Arg|]);
  expect(index)->toEqual(3);

  Js.undefined;
});

test("should insert fraction in another fraction's numerator", (.) => {
  let (elements, index) =
    insertIndex([|Frac2S, Arg, Arg|], Frac2S, 1)->Belt.Option.getExn;

  expect(elements)->toEqual([|Frac2S, Frac2S, Arg, Arg, Arg, Arg|]);
  expect(index)->toEqual(2);

  Js.undefined;
});

test("should insert fraction in another fraction's denominator", (.) => {
  let (elements, index) =
    insertIndex([|Frac2S, Arg, Arg|], Frac2S, 2)->Belt.Option.getExn;

  expect(elements)->toEqual([|Frac2S, Arg, Frac2S, Arg, Arg, Arg|]);
  expect(index)->toEqual(3);

  Js.undefined;
});

test("should not move matrices or vectors when inserting fraction after", (.) => {
  let (elements, _) =
    insertIndex([|Vector2S, Arg, Arg|], Frac2S, 3)->Belt.Option.getExn;

  expect(elements)->toEqual([|Vector2S, Arg, Arg, Frac2S, Arg, Arg|]);

  Js.undefined;
});

test("should not move matrices or vectors when inserting fraction before", (.) => {
  let (elements, _) =
    insertIndex([|Vector2S, Arg, Arg|], Frac2S, 0)->Belt.Option.getExn;

  expect(elements)->toEqual([|Frac2S, Arg, Arg, Vector2S, Arg, Arg|]);

  Js.undefined;
});

test("should not insert iteratables inside iterator ranges", (.) => {
  let result = insertIndex([|Sum2, Arg, Arg|], Sum2, 1);

  expect(result)->toEqual(None);

  Js.undefined;
});

test("should insert iteratables outside of iterator ranges", (.) => {
  let (elements, _) =
    insertIndex([|Sum2, Arg, Arg|], Sum2, 3)->Belt.Option.getExn;

  expect(elements)->toEqual([|Sum2, Arg, Arg, Sum2, Arg, Arg|]);

  Js.undefined;
});

test("should not insert tables inside tables", (.) => {
  let result = insertIndex([|Vector2S, Arg, Arg|], Vector2S, 1);

  expect(result)->toEqual(None);

  Js.undefined;
});

test("should insert tables outside of tables", (.) => {
  let (elements, _) =
    insertIndex([|Vector2S, Arg, Arg|], Vector2S, 3)->Belt.Option.getExn;

  expect(elements)->toEqual([|Vector2S, Arg, Arg, Vector2S, Arg, Arg|]);

  Js.undefined;
});
