open MML_Builders;

type range = (int, int, int);
type bracketGroup = {
  range,
  body: string,
};
type t = {
  elements: int,
  level0Body: string,
  bracketGroups: list(bracketGroup),
};

let empty = {elements: 0, level0Body: "", bracketGroups: []};

let append = (element, {elements, level0Body, bracketGroups}): t =>
  switch (bracketGroups) {
  | [{body} as bracketGroup, ...rest] => {
      elements,
      level0Body,
      bracketGroups: [{...bracketGroup, body: body ++ element}, ...rest],
    }
  | [] => {
      elements: elements + 1,
      level0Body: level0Body ++ element,
      bracketGroups: [],
    }
  };

let openBracket = ({elements, level0Body, bracketGroups}, range): t => {
  elements,
  level0Body,
  bracketGroups: [{range, body: ""}, ...bracketGroups],
};

let _invalidAttributes = [("class", "invalid"), ("stretchy", "false")];
let closeBracket = (accum, range, superscript): t =>
  switch (accum.bracketGroups) {
  | [closed, ...nextBracketGroups] =>
    let body =
      elementWithIndex("mo", closed.range, "(")
      ++ closed.body
      ++ elementWithIndex(~superscript, "mo", range, ")")
      |> createElement("mrow");
    append(body, {...accum, bracketGroups: nextBracketGroups});
  | [] =>
    let attributes = _invalidAttributes;
    let body = elementWithIndex(~attributes, ~superscript, "mo", range, ")");
    append(body, accum);
  };

let toString = ({elements, level0Body, bracketGroups}, range) => {
  let attributes = _invalidAttributes;
  let closed =
    bracketGroups
    ->Belt.List.map(({range, body}) =>
        elementWithIndex(~attributes, "mo", range, "(") ++ body
      )
    ->Belt.List.reverse
    |> String.concat("");
  let body = level0Body ++ closed;

  if (body == "") {
    placeholder(range);
  } else if (elements > 1) {
    createElement("mrow", body);
  } else {
    body;
  };
};
