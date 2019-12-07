type countDirection =
  | Forwards
  | Backwards;

let countInsertables = (x: array(AST_Types.t), ~from, ~direction) => {
  let step = direction == Forwards ? 1 : (-1);
  let rec iter = (~current, ~bracketLevel, ~argLevel, ~from) =>
    switch (Belt.Array.get(x, from)) {
    | None => current
    | Some(#AST_Types.operatorAtom) when bracketLevel == 0 && argLevel == 0 => current
    | Some(v) =>
      let bracketLevel =
        switch (v) {
        | `OpenBracket => bracketLevel + 1
        | `CloseBracketS => bracketLevel - 1
        | _ => bracketLevel
        };
      let argLevel =
        switch (v) {
        | `Arg => argLevel - 1
        | _ => argLevel + AST_Types.argCountExn(v)
        };
      let shouldBreak =
        switch (direction) {
        | Forwards => argLevel < 0 || bracketLevel < 0
        | Backwards => argLevel > 0 || bracketLevel > 0
        };
      if (shouldBreak) {
        current;
      } else {
        iter(
          ~current=current + 1,
          ~bracketLevel,
          ~argLevel,
          ~from=from + step,
        );
      };
    };
  iter(~current=0, ~bracketLevel=0, ~argLevel=0, ~from);
};

let insertElement = (ast, element, index) => {
  switch (element) {
  | `Superscript1
  | `Sqrt1S =>
    let e = countInsertables(ast, ~from=index, ~direction=Forwards);
    let (ast, arg) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let combined = Belt.Array.concatMany([|[|element|], arg, [|`Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, combined, index);
    (ast, index + 1);
  | `NRoot2S =>
    let e = countInsertables(ast, ~from=index, ~direction=Forwards);
    let (ast, radicand) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let combined =
      Belt.Array.concatMany([|[|element, `Arg|], radicand, [|`Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, combined, index);
    (ast, index + 1);
  | `Frac2S =>
    let s = countInsertables(ast, ~from=index - 1, ~direction=Backwards);
    let e = countInsertables(ast, ~from=index, ~direction=Forwards);
    let (ast, den) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let (ast, num) = ArrayUtil.splice(ast, ~offset=index - s, ~len=s);
    let frac =
      Belt.Array.concatMany([|[|element|], num, [|`Arg|], den, [|`Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, frac, index - s);
    let nextIndex = s > 0 ? index + 2 : index + 1;
    (ast, nextIndex);
  | _ =>
    let elements =
      switch (AST_Types.argCountExn(element)) {
      | 0 => [|element|]
      | argCount =>
        let elements = Belt.Array.make(argCount + 1, `Arg);
        Belt.Array.setExn(elements, 0, element);
        elements;
      };
    let ast = ArrayUtil.insertArray(ast, elements, index);
    (ast, index + 1);
  };
};

let insertIndex = (ast: array(AST_Types.t), element: AST_Types.t, index: int) => {
  let ast = AST_Types.normalize(ast);
  if (AST_NormalizationContext.elementIsValid(ast, element, index)) {
    Some(insertElement(ast, element, index));
  } else {
    None;
  };
};

let insertArrayIndex = (ast, elements, index) => {
  let valid =
    Belt.Array.every(elements, element =>
      AST_NormalizationContext.elementIsValid(ast, element, index)
    );
  if (valid) {
    let ast = ArrayUtil.insertArray(ast, elements, index);
    let nextIndex = index + Belt.Array.length(elements);
    Some((ast, nextIndex));
  } else {
    None;
  };
};
