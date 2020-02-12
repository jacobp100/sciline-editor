open AST_ReduceMap;
open Value_Types;
open Value_Builders;

let map = (element: t('a), i) =>
  switch (element) {
  | `Superscript(_) => failwith("superscript")
  | `Function(fn) => UnresolvedFunction(GenericFunction(fn), i)
  | (#AST_Types.atom | `CloseBracket(_) | `Digit(_) | `Magnitude(_)) as e =>
    Unresolved(e, i)
  | `ImaginaryUnit(superscript) =>
    let superscript =
      Belt.Option.map(superscript, superscriptBody)
      ->Belt.Option.getWithDefault(AST.one);
    Resolved(AST.pow(AST.i, superscript));
  | `NLog({nlogBase}) => UnresolvedFunction(NLog({nlogBase: nlogBase}), i)
  | `Sum({iterationStart, iterationEnd}) =>
    UnresolvedFunction(Sum({iterationStart, iterationEnd}), i)
  | `Product({iterationStart, iterationEnd}) =>
    UnresolvedFunction(Product({iterationStart, iterationEnd}), i)
  | `Rand(superscript) => Resolved(AST.rand()->withSuperscript(superscript))
  | `RandInt({randIntA, b, superscript}) =>
    Resolved(AST.randInt(randIntA, b)->withSuperscript(superscript))
  | `NPR({statN, r}) => Resolved(AST.nPr(statN, r))
  | `NCR({statN, r}) => Resolved(AST.nCr(statN, r))
  | `Differential({differentialX, body}) =>
    Resolved(AST.differential(differentialX, body))
  | `Integral({integralA, b, body}) =>
    Resolved(AST.integral(integralA, b, body))
  | `Variable({atomNucleus, superscript}) =>
    Resolved(AST.variable(atomNucleus)->withSuperscript(superscript))
  | `CustomAtom({customAtomValue, superscript}) =>
    Resolved(AST.ofEncoded(customAtomValue)->withSuperscript(superscript))
  | `ConstPi(superscript) => AST.pi->withSuperscript(superscript)->Resolved
  | `ConstE(superscript) => AST.e->withSuperscript(superscript)->Resolved
  | `Frac({fracNum, den, superscript}) =>
    Resolved(AST.div(fracNum, den)->withSuperscript(superscript))
  | `Abs({unaryArg, superscript}) =>
    Resolved(AST.abs(unaryArg)->withSuperscript(superscript))
  | `Floor({unaryArg, superscript}) =>
    Resolved(AST.floor(unaryArg)->withSuperscript(superscript))
  | `Ceil({unaryArg, superscript}) =>
    Resolved(AST.ceil(unaryArg)->withSuperscript(superscript))
  | `Round({unaryArg, superscript}) =>
    Resolved(AST.round(unaryArg)->withSuperscript(superscript))
  | `Sqrt({rootRadicand, superscript}) =>
    Resolved(AST.sqrt(rootRadicand)->withSuperscript(superscript))
  | `NRoot({nrootDegree, radicand, superscript}) =>
    AST.pow(radicand, AST.div(AST.one, nrootDegree))
    ->withSuperscript(superscript)
    ->Resolved
  | `Table({tableElements, numRows, numColumns, superscript}) =>
    let element =
      numColumns == 1
        ? AST.vector(tableElements)
        : AST.matrix(numRows, numColumns, tableElements);
    element->withSuperscript(superscript)->Resolved;
  };