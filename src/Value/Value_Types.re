type node = TechniCalcCalculator.AST_Types.t;

type funcitionLike =
  | GenericFunction({
      func: AST_ReduceMap.func,
      squareResultSuperscript: option(node),
    })
  | NLog({base: node})
  | Sum({
      start: node,
      end_: node,
    })
  | Product({
      start: node,
      end_: node,
    });

type partialNode =
  | Resolved(node)
  | Unresolved(AST_ReduceMap.t(node), int)
  | UnresolvedFunction(funcitionLike, int);
