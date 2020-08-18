module AST = TechniCalcCalculator.AST_Types;

let withSuperscript = (value, superscript) =>
  switch (superscript) {
  | Some(AST_ReduceMap.{superscriptBody}) => AST.pow(value, superscriptBody)
  | None => value
  };

let handleGenericFunction = (arg, fn) =>
  switch (fn) {
  | AST_ReduceMap.Sin => AST.sin(arg)
  | Asin => AST.asin(arg)
  | Sinh => AST.sinh(arg)
  | Asinh => AST.asinh(arg)
  | Cos => AST.cos(arg)
  | Acos => AST.acos(arg)
  | Cosh => AST.cosh(arg)
  | Acosh => AST.acosh(arg)
  | Tan => AST.tan(arg)
  | Atan => AST.atan(arg)
  | Tanh => AST.tanh(arg)
  | Atanh => AST.atanh(arg)
  | Log => AST.log(arg)
  | Re => AST.re(arg)
  | Im => AST.im(arg)
  | Gamma => AST.gamma(arg)
  };

let handleFunction = (arg, fn) =>
  switch (fn) {
  | Value_Types.GenericFunction({func, squareResultSuperscript}) =>
    let value = handleGenericFunction(arg, func);
    switch (squareResultSuperscript) {
    | Some(squareResultSuperscript) =>
      AST.pow(value, squareResultSuperscript)
    | None => value
    };
  | NLog({base}) => AST.div(AST.log(arg), AST.log(base))
  | Sum({start, end_}) => AST.sum(start, end_, arg)
  | Product({start, end_}) => AST.product(start, end_, arg)
  };

let handleOp = (op, a, b) =>
  switch (op) {
  | AST_ReduceMap.Add => AST.add(a, b)
  | Sub => AST.sub(a, b)
  | Mul => AST.mul(a, b)
  | Div => AST.div(a, b)
  | Dot => AST.dot(a, b)
  };
