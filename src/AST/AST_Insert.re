open AST_Types;

type countDirection =
  | Forwards
  | Backwards;

let%private skipFunction = (x, ~from, ~direction) => {
  let step = direction == Forwards ? 1 : (-1);
  let rec iter = (~index, ~argLevel) =>
    switch (Belt.Array.get(x, index)) {
    | None => None
    | Some(v) =>
      let index = index + step;
      let argLevel =
        switch (v) {
        | Arg => argLevel - 1
        | _ => argLevel + AST_Types.argCountExn(v)
        };
      switch (compare(argLevel, 0), direction) {
      | (0, _) =>
        let fn = direction == Forwards ? Belt.Array.getExn(x, from) : v;
        Some((index, fn));
      | ((-1), Backwards)
      | (1, Forwards) => iter(~index, ~argLevel)
      | _ => None
      };
    };
  iter(~index=from, ~argLevel=0);
};

let%private skipInsertables = (x: array(t), ~from, ~direction) => {
  let rec iter = (~index, ~bracketLevel) =>
    switch (Belt.Array.get(x, index)) {
    | None
    | Some(Add | Sub | Mul | Div | Dot) when bracketLevel == 0 => Some(index)
    | None => None
    | Some(v) =>
      let bracketLevel =
        switch (v) {
        | OpenBracket => bracketLevel + 1
        | CloseBracketS => bracketLevel - 1
        | _ => bracketLevel
        };
      let shouldBreak =
        switch (direction) {
        | Forwards => bracketLevel < 0
        | Backwards => bracketLevel > 0
        };
      let nextIndex =
        shouldBreak ? None : skipFunction(x, ~from=index, ~direction);
      switch (nextIndex) {
      | Some((_, Matrix4S | Matrix9S | Vector2S | Vector3S | Sum2 | Product2))
      | None => Some(index)
      | Some((index, _)) => iter(~index, ~bracketLevel)
      };
    };
  iter(~index=from, ~bracketLevel=0);
};

let%private countInsertables = (x: array(t), ~from, ~direction) =>
  switch (skipInsertables(x, ~from, ~direction)) {
  | Some(index) => abs(from - index)
  | None => 0
  };

let%private insertElement = (ast, element, index) => {
  switch (element) {
  | AST_Types.Superscript1
  | Sqrt1S =>
    let e = countInsertables(ast, ~from=index, ~direction=Forwards);
    let (ast, arg) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let combined = Belt.Array.concatMany([|[|element|], arg, [|Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, combined, index);
    (ast, index + 1);
  | NRoot2S =>
    let e = countInsertables(ast, ~from=index, ~direction=Forwards);
    let (ast, radicand) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let combined =
      Belt.Array.concatMany([|[|element, Arg|], radicand, [|Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, combined, index);
    (ast, index + 1);
  | Frac2S =>
    let s = countInsertables(ast, ~from=index - 1, ~direction=Backwards);
    let e = countInsertables(ast, ~from=index, ~direction=Forwards);
    let (ast, den) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let (ast, num) = ArrayUtil.splice(ast, ~offset=index - s, ~len=s);
    let frac =
      Belt.Array.concatMany([|[|element|], num, [|Arg|], den, [|Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, frac, index - s);
    let nextIndex = s > 0 ? index + 2 : index + 1;
    (ast, nextIndex);
  | _ =>
    let elements =
      switch (AST_Types.argCountExn(element)) {
      | 0 => [|element|]
      | argCount =>
        let elements = Belt.Array.make(argCount + 1, AST_Types.Arg);
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
