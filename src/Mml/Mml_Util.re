let stringOfOperator = x =>
  switch (x) {
  | AST_ReduceMap.Add => "+"
  | Sub => "-"
  | Mul => "&#x00D7;"
  | Div => "&#x00F7;"
  | Dot => "&#xb7;"
  };

let stringOfFunction = x =>
  switch (x) {
  | AST_ReduceMap.Sin => "sin"
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
  | Gamma => "&#x0393;"
  };

let stringOfBase = base =>
  switch (base) {
  | AST_ReduceMap.Bin => "0b"
  | Oct => "0o"
  | Hex => "0x"
  };
