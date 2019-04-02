let takeUpto = (elements, n) =>
  elements->Belt.List.take(n)->Belt.Option.getWithDefault(elements);

let countWhile = (elements, fn) => {
  let rec iter = (i, rest) =>
    switch (rest) {
    | [element, ...rest] => fn(element) ? iter(i + 1, rest) : i
    | [] => i
    };
  iter(0, elements);
};

let splitWhile = (elements, fn) =>
  elements
  ->Belt.List.splitAt(countWhile(elements, fn))
  ->Belt.Option.getWithDefault((elements, []));
