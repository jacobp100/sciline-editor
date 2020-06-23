external mutableCell: ('a, list('a)) => list('a) = "#makemutablelist";
[@bs.set] external unsafeMutateTail: (list('a), list('a)) => unit = "tl";

type t('a) =
  | Empty
  | Initialized(list('a), ref(list('a)));

let empty = Empty;

let append = (list, element) => {
  let next = mutableCell(element, []);
  switch (list) {
  | Empty => Initialized(next, ref(next))
  | Initialized(_, tail) =>
    unsafeMutateTail(tail^, next);
    tail := next;
    list;
  };
};

let toList = list =>
  switch (list) {
  | Empty => []
  | Initialized(value, _) => value
  };