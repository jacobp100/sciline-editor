open AST_ReduceMap;
module AST = ScilineCalculator.ASTTypes;

type funcitionLike =
  | GenericFunction(AST_Types.func)
  | NLog(nlog(AST.t))
  | Sum(iteration(AST.t))
  | Product(iteration(AST.t));
type finalState =
  | Node(AST.t)
  | Empty
  | Error(int);
type partialNode =
  | Resolved(AST.t)
  | Unresolved(t(finalState), int)
  | UnresolvedFunction(funcitionLike, int);
type reduceState =
  | Row(MutableListBuilder.t(partialNode))
  | ReduceError(int);
