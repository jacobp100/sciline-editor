open AST_ReduceMap;

module AST = TechniCalcCalculator.AST_Types;

let parse = (elements: array(AST_Types.t)) => {
  let error = ref(None);

  let reduce = (accum, element, (i, _)) =>
    if (error^ == None) {
      switch (element) {
      | Superscript(_) =>
        error := Some(i);
        MutableListBuilder.empty;
      | _ =>
        let value = Value_Element.map(element, i);
        MutableListBuilder.append(accum, value);
      };
    } else {
      MutableListBuilder.empty;
    };

  let map = (accum, (i, _)): AST.t =>
    if (error^ == None) {
      let elements = MutableListBuilder.toList(accum);
      switch (Value_Row.next(elements)) {
      | Ok(root) => root
      | Error(i) =>
        error := Some(i);
        AST.nan;
      | UnknownError =>
        error := Some(i);
        AST.nan;
      };
    } else {
      AST.nan;
    };

  let root =
    reduceMap(elements, ~reduce, ~map, ~initial=MutableListBuilder.empty);

  switch (error^) {
  | Some(i) => Error(i)
  | None => Ok(root)
  };
};
