let insertIndex = (ast: array(AST_Types.t), element: AST_Types.t, index: int) => {
  let ast = AST_Types.normalize(ast);
  if (AST_NormalizationContext.elementIsValid(ast, element, index)) {
    let elements =
      switch (AST_Types.argCountExn(element)) {
      | 0 => [|element|]
      | argCount =>
        let elements = Belt.Array.make(argCount + 1, `Arg);
        Belt.Array.setExn(elements, 0, element);
        elements;
      };
    ArrayUtil.insertArray(ast, elements, index);
  } else {
    ast;
  };
};
