let rec matchNEmptyArgs = (ast, ~index, ~count) =>
  if (count == 0) {
    true;
  } else if (ast->Belt.Array.get(index) != Some(`Arg)) {
    false;
  } else {
    matchNEmptyArgs(ast, ~index=index + 1, ~count=count - 1);
  };

let nArgsSlice = (~skipInitial=0, ast, index) => {
  let element = Belt.Array.getExn(ast, index);
  let count = AST_Types.argCountExn(element);
  let current = ref([||]);

  let next = ref(index + 1);
  for (i in 0 to count - 1) {
    let offset = next^;
    next := AST_Types.argEndIndex(ast, offset);
    let len = next^ - offset - 1;
    if (i >= skipInitial) {
      let elements = Belt.Array.slice(ast, ~offset, ~len);
      current := Belt.Array.concat(current^, elements);
    };
  };

  current^;
};

type deletionMode =
  | Keep
  | Delete
  | Spread(array(AST_Types.t));

let deletionMode = (ast, index) =>
  switch (Belt.Array.get(ast, index)) {
  | Some(`Arg) => Keep
  | Some(`Frac2S) => Spread(nArgsSlice(ast, index))
  | Some(`Sqrt1S) => Spread(nArgsSlice(ast, index))
  | Some(`NRoot2S) =>
    let degreeIsEmpty = matchNEmptyArgs(ast, ~index=index + 1, ~count=1);
    degreeIsEmpty ? Spread(nArgsSlice(ast, index, ~skipInitial=1)) : Keep;
  | Some(v) =>
    let argCount = AST_Types.argCountExn(v);
    let argsEmpty = matchNEmptyArgs(ast, ~index=index + 1, ~count=argCount);
    argsEmpty ? Delete : Keep;
  | None => Keep
  };

let deleteAtIndexExn = (ast, startIndex) => {
  let element = Belt.Array.getExn(ast, startIndex);
  let argCount = AST_Types.argCountExn(element);
  let endIndex = ref(startIndex + 1);
  for (_ in 1 to argCount) {
    endIndex := AST_Types.argEndIndex(ast, endIndex^);
  };
  let endIndex = endIndex^;
  if (endIndex >= Belt.Array.length(ast)) {
    Belt.Array.slice(ast, ~offset=0, ~len=startIndex);
  } else {
    Belt.Array.concat(
      Belt.Array.slice(ast, ~offset=0, ~len=startIndex),
      Belt.Array.sliceToEnd(ast, endIndex),
    );
  };
};

let deleteEmptySuperscript = (ast, index) =>
  switch (Belt.Array.get(ast, index), Belt.Array.get(ast, index + 1)) {
  | (Some(`Superscript1), Some(`Arg)) =>
    Belt.Array.concat(
      Belt.Array.slice(ast, ~offset=0, ~len=index),
      Belt.Array.sliceToEnd(ast, index + 2),
    )
  | _ => ast
  };

let deleteIndex = (ast: array(AST_Types.t), index: int) => {
  let index = index - 1;
  let ast = AST_Types.normalize(ast);
  let ast =
    switch (deletionMode(ast, index)) {
    | Keep => ast
    | Delete => deleteAtIndexExn(ast, index)->deleteEmptySuperscript(index)
    | Spread(elements) =>
      deleteAtIndexExn(ast, index)
      ->deleteEmptySuperscript(index)
      ->ArrayUtil.insertArray(elements, index)
    };
  (ast, index);
};
