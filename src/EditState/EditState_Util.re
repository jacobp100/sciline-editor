let%private isEndOfIsolatedIndex = (elements: array(AST.t), index: int) =>
  switch (Belt.Array.get(elements, index - 1)) {
  | Some(LabelS(_)) =>
    switch (Belt.Array.get(elements, index)) {
    | Some(Arg)
    | None => true
    | _ => false
    }
  | _ => false
  };

let preferredInsertionIndex = (~index, ~elements, ~allowLabelEditing, ~step) => {
  /* Note returning an index of length _is_ valid */
  let length = Belt.Array.length(elements);
  let clampedIndex = max(index, 0)->min(length);

  if (allowLabelEditing) {
    clampedIndex;
  } else {
    let preferredIndex = ref(clampedIndex);
    while (preferredIndex^ > 0
           && preferredIndex^ < length
           && isEndOfIsolatedIndex(elements, preferredIndex^)) {
      preferredIndex := preferredIndex^ + step;
    };
    preferredIndex^;
  };
};
