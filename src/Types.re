type base =
  | Bin
  | Oct
  | Hex;
type binaryOperator =
  | Add
  | Sub
  | Mul
  | Div
  | Dot;
type func =
  | Sin
  | Arcsin
  | Sinh
  | Arcsinh
  | Cos
  | Arccos
  | Cosh
  | Arccosh
  | Tan
  | Arctan
  | Tanh
  | Arctanh
  | Log
  | Re
  | Im
  | Gamma;
type constant =
  | Pi
  | E;
type atom('a) = {
  atomNucleus: string,
  superscript: 'a,
};
type constantAtom('a) = {
  constant,
  superscript: 'a,
};
type customAtom('a) = {
  customAtomValue: ScilineCalculator.Encoding.encoding,
  mml: string,
  superscript: 'a,
};
type frac('a) = {
  fracNum: 'a,
  den: 'a,
  superscript: 'a,
};
type root('a) = {
  rootRadicand: 'a,
  superscript: 'a,
};
type nroot('a) = {
  nrootDegree: 'a,
  radicand: 'a,
  superscript: 'a,
};
type nlog('a) = {nlogBase: 'a};
type unary('a) = {
  unaryArg: 'a,
  superscript: 'a,
};
type randInt('a) = {
  randIntA: 'a,
  b: 'a,
  superscript: 'a,
};
type stat('a) = {
  statN: 'a,
  r: 'a,
};
type differential('a) = {
  differentialX: 'a,
  body: 'a,
};
type integral('a) = {
  integralA: 'a,
  b: 'a,
  body: 'a,
};
type range('a) = {
  rangeStart: 'a,
  rangeEnd: 'a,
};
type table('a) = {
  tableElements: array('a),
  superscript: 'a,
  numRows: int,
  numColumns: int,
};

type t = [
  | `Base(base)
  | `Operator(binaryOperator)
  | `Function(func)
  | `OpenBracket
  | `DecimalSeparator
  | `Conj
  | `Factorial
  | `Degree
  | `ArcMinute
  | `ArcSecond
  | `Placeholder(list(t))
  | `ImaginaryUnit(list(t))
  | `Magnitude(list(t))
  | `CloseBracket(list(t))
  | `Digit(atom(list(t)))
  | `Variable(atom(list(t)))
  | `Constant(constantAtom(list(t)))
  | `CustomAtom(customAtom(list(t)))
  | `Frac(frac(list(t)))
  | `Sqrt(root(list(t)))
  | `NRoot(nroot(list(t)))
  | `NLog(nlog(list(t)))
  | `Abs(unary(list(t)))
  | `Floor(unary(list(t)))
  | `Ceil(unary(list(t)))
  | `Round(unary(list(t)))
  | `Rand(list(t))
  | `RandInt(randInt(list(t)))
  | `NPR(stat(list(t)))
  | `NCR(stat(list(t)))
  | `Differential(differential(list(t)))
  | `Integral(integral(list(t)))
  | `Sum(range(list(t)))
  | `Product(range(list(t)))
  | `Table(table(list(t)))
];
