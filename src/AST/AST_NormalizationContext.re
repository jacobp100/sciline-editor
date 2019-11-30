type iterators = [ | `Sum2 | `Product2 | `Differential2 | `Integral3];

let validityStackReducer = prependValidityStack => {
  let reducerFn = ((range, validityStack), element, i) => {
    let range =
      switch (validityStack) {
      | [false, ..._] => Ranges.addSequentialIndex(range, i)
      | _ => range
      };
    let validityStack =
      switch (element) {
      | `Arg =>
        Belt.List.tail(validityStack)
        ->Belt.Option.getWithDefault(validityStack)
      | e => prependValidityStack(validityStack, e)
      };
    (range, validityStack);
  };
  ast => Belt.Array.reduceWithIndex(ast, (Ranges.empty, []), reducerFn)->fst;
};

let noTableRanges =
  validityStackReducer((validityStack, element) =>
    switch (element) {
    | `Frac2S => [/* num */ true, /* den */ false, ...validityStack]
    | `Abs1S
    | `Floor1S
    | `Ceil1S
    | `Round1S => [true, ...validityStack]
    | _ =>
      let argCount = AST_Types.argCountExn(element);
      validityStack->ListUtil.prependMany(argCount, false);
    }
  );

let noIterationRanges =
  validityStackReducer((validityStack, element) => {
    let argCount = AST_Types.argCountExn(element);
    switch (element) {
    | #iterators => validityStack->ListUtil.prependMany(argCount, false)
    | _ => validityStack->ListUtil.prependMany(argCount, true)
    };
  });

let elementIsValid = (ast: array(AST_Types.t), element: AST_Types.t, index) =>
  switch (element) {
  | #iterators => !noIterationRanges(ast)->Ranges.contains(index)
  | `TableNS(_) => !noTableRanges(ast)->Ranges.contains(index)
  | _ => true
  };