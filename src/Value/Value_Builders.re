open AST_ReduceMap;
open Value_Types;

let nodeWithSuperscript = (superscript, a) =>
  switch (superscript) {
  | Some(Node(superscript)) => `Ok(AST.pow(a, superscript))
  | Some(Error(i)) => `Error(i)
  | None => `Ok(a)
  };
let partialNodeWithSuperscript = (a, superscript) =>
  switch (nodeWithSuperscript(superscript, a)) {
  | `Ok(a) => `Ok(Resolved(a))
  | `Error(_) as err => err
  };

let handleGenericFunction = (arg, fn) =>
  switch (fn) {
  | AST_Types.Sin => AST.sin(arg)
  | Arcsin => AST.asin(arg)
  | Sinh => AST.sinh(arg)
  | Arcsinh => AST.asinh(arg)
  | Cos => AST.cos(arg)
  | Arccos => AST.acos(arg)
  | Cosh => AST.cosh(arg)
  | Arccosh => AST.acosh(arg)
  | Tan => AST.tan(arg)
  | Arctan => AST.atan(arg)
  | Tanh => AST.tanh(arg)
  | Arctanh => AST.atanh(arg)
  | Log => AST.log(arg)
  | Re => AST.re(arg)
  | Im => AST.im(arg)
  | Gamma => AST.gamma(arg)
  };
let handleFunction = (arg, fn) =>
  switch (fn) {
  | GenericFunction(fn) => handleGenericFunction(arg, fn)
  | NLog({nlogBase}) => AST.div(AST.log(arg), AST.log(nlogBase))
  | Sum({iterationStart, iterationEnd}) =>
    AST.sum(iterationStart, iterationEnd, arg)
  | Product({iterationStart, iterationEnd}) =>
    AST.product(iterationStart, iterationEnd, arg)
  };

let overArg = (a, fn, i) =>
  switch (a) {
  | Node(a) => fn(a)
  | Error(_) as e => e
  | Empty => Error(i)
  };
let overArgs2 = (a, b, fn, i) =>
  switch (a, b) {
  | (Node(a), Node(b)) => fn(a, b)
  | (Error(_) as e, _)
  | (_, Error(_) as e) => e
  | (Empty, _) => Error(i)
  | (_, Empty) => Error(i)
  };
let handleOp = (op, a, b) =>
  switch (op) {
  | AST_Types.Add => Node(AST.add(a, b))
  | Sub => Node(AST.sub(a, b))
  | Mul => Node(AST.mul(a, b))
  | Div => Node(AST.div(a, b))
  | Dot => Node(AST.mul(a, b))
  };
