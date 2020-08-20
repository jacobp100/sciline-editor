external mutableCell: ('a, list('a)) => list('a) = "#makemutablelist";
[@bs.set] external unsafeMutateTail: (list('a), list('a)) => unit = "tl";

type t('a) =
  | Empty
  | Initialized({
      start: list('a),
      end_: ref(list('a)),
    });

let empty = Empty;

let append = (list, element) => {
  let next = mutableCell(element, []);
  switch (list) {
  | Empty => Initialized({start: next, end_: ref(next)})
  | Initialized({end_}) =>
    unsafeMutateTail(end_^, next);
    end_ := next;
    list;
  };
};

let toList = list =>
  switch (list) {
  | Empty => []
  | Initialized({start}) => start
  };
