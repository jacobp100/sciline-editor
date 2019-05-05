let addRange = ((a: int, b: int), ranges) => {
  let (out, state) =
    Belt.List.reduce(
      ranges,
      (MutableListBuilder.empty, `NotInserted),
      ((out, state), (a0, b0) as r) =>
      switch (state) {
      | `Inserted => (out->MutableListBuilder.append(r), `Inserted)
      | `Open(a', _) when a >= a0 && a <= b0 => (
          out->MutableListBuilder.append((a', b0)),
          `Inserted,
        )
      | `Open(a', _) when b < a0 => (
          out
          ->MutableListBuilder.append((a', b))
          ->MutableListBuilder.append(r),
          `Inserted,
        )
      | `Open(a', _) => (out, `Open((a', b0)))
      | `NotInserted when a0 <= a && b0 >= b => (
          out->MutableListBuilder.append(r),
          `Inserted,
        )
      | `NotInserted when a0 <= a => (out, `Open((a0, b0)))
      | `NotInserted => (out->MutableListBuilder.append(r), `NotInserted)
      }
    );

  let out =
    switch (state) {
    | `NotInserted => out->MutableListBuilder.append((a, b))
    | `Open(a', b') => out->MutableListBuilder.append((a', max(b', b)))
    | `Inserted => out
    };

  out->MutableListBuilder.toList;
};

[@bs.deriving abstract]
type stuffReturn = {
  length: int,
  bracketGroups: array((int, int)),
  noTableGroups: array((int, int)),
};

type rollup = {
  bracketGroups: list((int, int)),
  noTableGroups: list((int, int)),
};

let reduceBracketGroupsAccum =
    (bracketGroups, {Tree.accum, rangeStart, rangeEnd}, element) =>
  switch (accum, element) {
  | (_, `OpenBracket) => (bracketGroups, [rangeStart, ...accum])
  | ([openBracket, ...rest], `CloseBracket(_)) => (
      [(openBracket, rangeEnd), ...bracketGroups],
      rest,
    )
  | _ => (bracketGroups, accum)
  };

let reduceTableGroups =
    (noTableGroups, {Tree.context, rangeStart, rangeEnd}) =>
  rangeStart != rangeEnd && context == Mutation.NonTableElements ?
    addRange((rangeStart, rangeEnd), noTableGroups) : noTableGroups;

let getMetadata = elements => {
  let (length, {bracketGroups, noTableGroups}, _) =
    Tree.walkI(
      ~initialContext=Mutation.AnyElement,
      ~mapContext=Mutation.reduceSelectorFlags,
      ~initialRollup={bracketGroups: [], noTableGroups: []},
      elements,
      [],
      ({Tree.rollup} as arg) => {
        let noTableGroups = reduceTableGroups(rollup.noTableGroups, arg);
        ({...rollup, noTableGroups}, None);
      },
      ({Tree.rollup} as arg, element) => {
        let (bracketGroups, accum) =
          reduceBracketGroupsAccum(rollup.bracketGroups, arg, element);
        ({...rollup, bracketGroups}, accum);
      },
    );

  stuffReturn(
    ~length,
    ~bracketGroups=Belt.List.toArray(bracketGroups),
    ~noTableGroups=Belt.List.toArray(noTableGroups),
  );
};
