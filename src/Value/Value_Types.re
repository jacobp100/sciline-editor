open AST_ReduceMap;
module AST = ScilineCalculator.ASTTypes;

type funcitionLike =
  | GenericFunction(AST_Types.func)
  | NLog(nlog(AST.t))
  | Sum(iteration(AST.t))
  | Product(iteration(AST.t));

type partialNode =
  | Resolved(AST.t)
  | Unresolved(t(AST.t), int)
  | UnresolvedFunction(funcitionLike, int);
