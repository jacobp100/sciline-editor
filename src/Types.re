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

type atomNode('a) = [
  | `ImaginaryUnit('a)
  | `Rand('a)
  | `Digit(atom('a))
  | `Variable(atom('a))
  | `Constant(constantAtom('a))
  | `CustomAtom(customAtom('a))
];

type node('a) = [
  atomNode('a)
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
  | `Placeholder('a)
  | `Magnitude('a)
  | `CloseBracket('a)
  | `Frac(frac('a))
  | `Sqrt(root('a))
  | `NRoot(nroot('a))
  | `NLog(nlog('a))
  | `Abs(unary('a))
  | `Floor(unary('a))
  | `Ceil(unary('a))
  | `Round(unary('a))
  | `RandInt(randInt('a))
  | `NPR(stat('a))
  | `NCR(stat('a))
  | `Differential(differential('a))
  | `Integral(integral('a))
  | `Sum(range('a))
  | `Product(range('a))
  | `Table(table('a))
];

type t = node(list(t));

type atomT = atomNode(list(t));
