let rec count = (~current=0, x, ~from, ~step=1, fn) =>
  switch (Belt.Array.get(x, from)) {
  | Some(v) when fn(v) =>
    count(~current=current + 1, x, ~from=from + step, ~step, fn)
  | _ => current
  };

let isDigit = x =>
  switch (x) {
  | `DigitS(_)
  | `DecimalSeparator => true
  | _ => false
  };

let insertElement = (ast, element, index) => {
  switch (element) {
  | `Frac2S =>
    let s = count(ast, ~from=index - 1, ~step=-1, isDigit);
    let e = count(ast, ~from=index, ~step=1, isDigit);
    let (ast, den) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let (ast, num) = ArrayUtil.splice(ast, ~offset=index - s, ~len=s);
    let frac =
      Belt.Array.concatMany([|[|element|], num, [|`Arg|], den, [|`Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, frac, index - s);
    let nextIndex = s > 0 ? index + 2 : index + 1;
    (ast, nextIndex);
  | `Sqrt1S =>
    let e = count(ast, ~from=index, ~step=1, isDigit);
    let (ast, radicand) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let sqrt = Belt.Array.concatMany([|[|element|], radicand, [|`Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, sqrt, index);
    (ast, index + 1);
  | `NRoot2S =>
    let e = count(ast, ~from=index, ~step=1, isDigit);
    let (ast, radicand) = ArrayUtil.splice(ast, ~offset=index, ~len=e);
    let nroot = Belt.Array.concatMany([|[|element|], radicand, [|`Arg|]|]);
    let ast = ArrayUtil.insertArray(ast, nroot, index);
    (ast, index + 1);
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
