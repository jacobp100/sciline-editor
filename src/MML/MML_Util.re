open MML_Builders;

let stringOfOperator = x =>
  switch (x) {
  | AST_Types.Add => "+"
  | Sub => "-"
  | Mul => "&times;"
  | Div => "&div;"
  | Dot => "&#xb7;"
  };

let stringOfFunction = x =>
  switch (x) {
  | AST_Types.Sin => "sin"
  | Arcsin => "arcsin"
  | Sinh => "sinh"
  | Arcsinh => "arcsinh"
  | Cos => "cos"
  | Arccos => "arccos"
  | Cosh => "cosh"
  | Arccosh => "arccosh"
  | Tan => "tan"
  | Arctan => "arctan"
  | Tanh => "tanh"
  | Arctanh => "arctanh"
  | Log => "log"
  | Re => "re"
  | Im => "im"
  | Gamma => "&Gamma;"
  };

let stringOfBase = base =>
  switch (base) {
  | AST_Types.Bin => "0b"
  | Oct => "0o"
  | Hex => "0x"
  };

let stringOfConstant = constant =>
  switch (constant) {
  | AST_Types.Pi => "&pi;"
  | E => "e"
  };

let xSetRow = value =>
  createElement(
    "mrow",
    createElement("mi", "x") ++ createElement("mo", "=") ++ value,
  );

let placeholder = range =>
  elementWithIndex(
    ~attributes=[("class", "placeholder")],
    "mi",
    range,
    "&#x25a1;",
  );
