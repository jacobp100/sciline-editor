open AST_Types;
open Value_Types;

let withSuperscript = (value, superscript) =>
  switch (superscript) {
  | Some(superscript) => AST.pow(value, superscript)
  | None => value
  };

let getConstant = c =>
  switch (c) {
  | Pi => AST.pi
  | E => AST.e
  };

let handleGenericFunction = (arg, fn) =>
  switch (fn) {
  | Sin => AST.sin(arg)
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
  | GenericFunction(fn) => handleGenericFunction(arg, fn)
  | NLog({nlogBase}) => AST.div(AST.log(arg), AST.log(nlogBase))
  | Sum({iterationStart, iterationEnd}) =>
    AST.sum(iterationStart, iterationEnd, arg)
  | Product({iterationStart, iterationEnd}) =>
    AST.product(iterationStart, iterationEnd, arg)
  };

let handleOp = (op, a, b) =>
  switch (op) {
  | Add => AST.add(a, b)
  | Sub => AST.sub(a, b)
  | Mul => AST.mul(a, b)
  | Div => AST.div(a, b)
  | Dot => AST.mul(a, b)
  };
