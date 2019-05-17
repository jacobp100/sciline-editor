let noTableRanges = (ast: array(AST_Types.t)) => {
  let allowedTablesStack = ref([]);
  let range = ref(Ranges.empty);

  for (i in 0 to Belt.Array.length(ast)) {
    switch (allowedTablesStack^) {
    | [false, ..._] => range := Ranges.addSequentialIndex(range^, i)
    | _ => ()
    };

    let element = Belt.Array.getExn(ast, i);
    switch (element) {
    | `Arg =>
      switch (Belt.List.tail(allowedTablesStack^)) {
      | Some(tail) => allowedTablesStack := tail
      | None => ()
      }
    | `Frac2S =>
      allowedTablesStack :=
        [/* num */ true, /* den */ false, ...allowedTablesStack^]
    | `Abs1S
    | `Floor1S
    | `Ceil1S
    | `Round1S => allowedTablesStack := [true, ...allowedTablesStack^]
    | _ =>
      for (_ in 1 to AST_Types.argCountExn(element)) {
        allowedTablesStack := [false, ...allowedTablesStack^];
      }
    };
  };

  range^;
};

let elementIsValid = (ast: array(AST_Types.t), element: AST_Types.t, index) =>
  switch (element) {
  | `TableNS(_) => noTableRanges(ast)->Ranges.contains(index)
  | _ => true
  };
