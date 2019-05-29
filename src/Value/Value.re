open AST_ReduceMap;
open Value_Types;
open Value_Builders;

let mapElement = (element: t('a), i) =>
  switch (element) {
  | `Superscript(_) => failwith("superscript")
  | (
      `Base(_) | `Operator(_) | `OpenBracket | `DecimalSeparator | `Factorial |
      `Conj |
      `Degree |
      `ArcMinute |
      `ArcSecond |
      `ImaginaryUnit(_) |
      `Magnitude(_) |
      `CloseBracket(_) |
      `Digit(_)
    ) as e =>
    Unresolved(e, i)
  | `Function(fn) => UnresolvedFunction(GenericFunction(fn), i)
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
  | `Constant({constant, superscript}) =>
    Value_Builders.getConstant(constant)
    ->withSuperscript(superscript)
    ->Resolved
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
      numColumns == 1 ?
        AST.vector(tableElements) :
        AST.matrix(numRows, numColumns, tableElements);
    element->withSuperscript(superscript)->Resolved;
  };

let parse = (elements: array(AST_Types.t)) => {
  let error = ref(None);

  let reduce = (accum, element, (i, _, _)) =>
    if (error^ == None) {
      switch (element) {
      | `Superscript(_) =>
        error := Some(i);
        accum;
      | _ =>
        let value = mapElement(element, i);
        MutableListBuilder.append(accum, value);
      };
    } else {
      accum;
    };

  let map = (accum, (i, _, _)): AST.t =>
    if (error^ == None) {
      let elements = MutableListBuilder.toList(accum);
      switch (Value_Row.next(elements)) {
      | `Ok(root) => root
      | `Error(i) =>
        error := Some(i);
        AST.nan;
      | `UnknownError =>
        error := Some(i);
        AST.nan;
      };
    } else {
      AST.nan;
    };

  let root =
    reduceMap(elements, ~reduce, ~map, ~initial=MutableListBuilder.empty);

  switch (error^) {
  | Some(i) => `Error(i)
  | None => `Ok(root)
  };
};
