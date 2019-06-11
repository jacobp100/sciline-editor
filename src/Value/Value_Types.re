open AST_ReduceMap;

type node = ScilineCalculator.AST_Types.t;

type funcitionLike =
  | GenericFunction(AST_Types.func)
  | NLog(nlog(node))
  | Sum(iteration(node))
  | Product(iteration(node));

type partialNode =
  | Resolved(node)
  | Unresolved(t(node), int)
  | UnresolvedFunction(funcitionLike, int);
