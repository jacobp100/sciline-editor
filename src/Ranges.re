type t = list((int, int));

let empty: t = [];

let addSequentialIndex = (x, i) =>
  switch (x) {
  | [(a, b), ...rest] when b == i - 1 => [(a, i), ...rest]
  | rest => [(i, i), ...rest]
  };

let contains = (x, i) => Belt.List.some(x, ((a, b)) => a <= i && b >= i);

let toArray = x => Belt.List.toArray(x);
