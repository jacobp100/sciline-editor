let takeUpto = (elements, n) =>
  elements->Belt.List.take(n)->Belt.Option.getWithDefault(elements);
