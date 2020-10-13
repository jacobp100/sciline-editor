open AST_ReduceMap;
open Value_Builders;

let map = (element: t('a), i, i') =>
  switch (element) {
  | AST_ReduceMap.Superscript(_) => assert(false)
  | Function({func, squareResultSuperscript}) =>
    let squareResultSuperscript =
      Belt.Option.map(squareResultSuperscript, superscriptBody);
    Value_Types.UnresolvedFunction(
      GenericFunction({func, squareResultSuperscript}),
      i,
      i',
    );
  | (Angle(_) | Base(_) | Conj | DecimalSeparator | Factorial | OpenBracket) as e
  | (Operator(_) | Percent | UnitConversion(_)) as e
  | (CloseBracket(_) | Digit(_) | Magnitude(_)) as e => Unresolved(e, i, i')
  | ImaginaryUnit(superscript) =>
    let superscript =
      Belt.Option.map(superscript, superscriptBody)
      ->Belt.Option.getWithDefault(AST.one);
    Resolved(AST.pow(AST.i, superscript));
  | NLog({base}) => UnresolvedFunction(NLog({base: base}), i, i')
  | Sum({start, end_}) => UnresolvedFunction(Sum({start, end_}), i, i')
  | Product({start, end_}) =>
    UnresolvedFunction(Product({start, end_}), i, i')
  | Rand(superscript) => Resolved(AST.rand->withSuperscript(superscript))
  | RandInt({a, b, superscript}) =>
    Resolved(AST.randInt(a, b)->withSuperscript(superscript))
  | NPR({n, r}) => Resolved(AST.nPr(n, r))
  | NCR({n, r}) => Resolved(AST.nCr(n, r))
  | Differential({x, body}) => Resolved(AST.differential(x, body))
  | Integral({a, b, body}) => Resolved(AST.integral(a, b, body))
  | Variable({nucleus, superscript}) =>
    Resolved(AST.variable(nucleus)->withSuperscript(superscript))
  | CustomAtom({value, superscript}) =>
    Resolved(AST.ofEncoded(value)->withSuperscript(superscript))
  | ConstPi(superscript) => AST.pi->withSuperscript(superscript)->Resolved
  | ConstE(superscript) => AST.e->withSuperscript(superscript)->Resolved
  | Frac({num, den, superscript}) =>
    Resolved(AST.div(num, den)->withSuperscript(superscript))
  | Min({a, b, superscript}) =>
    Resolved(AST.min(a, b)->withSuperscript(superscript))
  | Max({a, b, superscript}) =>
    Resolved(AST.max(a, b)->withSuperscript(superscript))
  | Gcd({a, b, superscript}) =>
    Resolved(AST.gcd(a, b)->withSuperscript(superscript))
  | Lcm({a, b, superscript}) =>
    Resolved(AST.lcm(a, b)->withSuperscript(superscript))
  | Abs({arg, superscript}) =>
    Resolved(AST.abs(arg)->withSuperscript(superscript))
  | Floor({arg, superscript}) =>
    Resolved(AST.floor(arg)->withSuperscript(superscript))
  | Ceil({arg, superscript}) =>
    Resolved(AST.ceil(arg)->withSuperscript(superscript))
  | Round({arg, superscript}) =>
    Resolved(AST.round(arg)->withSuperscript(superscript))
  | Sqrt({radicand, superscript}) =>
    Resolved(AST.sqrt(radicand)->withSuperscript(superscript))
  | NRoot({degree, radicand, superscript}) =>
    AST.pow(radicand, AST.div(AST.one, degree))
    ->withSuperscript(superscript)
    ->Resolved
  | Vector({elements, superscript}) =>
    AST.vector(elements)->withSuperscript(superscript)->Resolved
  | Table({elements, numRows, numColumns, superscript}) =>
    AST.matrix(numRows, numColumns, elements)
    ->withSuperscript(superscript)
    ->Resolved
  };
