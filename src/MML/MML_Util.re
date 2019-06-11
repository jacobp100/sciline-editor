open AST_Types;

let stringOfOperator = x =>
  switch (x) {
  | `Add => "+"
  | `Sub => "-"
  | `Mul => "&times;"
  | `Div => "&div;"
  | `Dot => "&#xb7;"
  };

let stringOfFunction = x =>
  switch (x) {
  | Sin => "sin"
  | Asin => "arcsin"
  | Sinh => "sinh"
  | Asinh => "arcsinh"
  | Cos => "cos"
  | Acos => "arccos"
  | Cosh => "cosh"
  | Acosh => "arccosh"
  | Tan => "tan"
  | Atan => "arctan"
  | Tanh => "tanh"
  | Atanh => "arctanh"
  | Log => "log"
  | Re => "re"
  | Im => "im"
  | Gamma => "&Gamma;"
  };

let stringOfBase = base =>
  switch (base) {
  | Bin => "0b"
  | Oct => "0o"
  | Hex => "0x"
  };
