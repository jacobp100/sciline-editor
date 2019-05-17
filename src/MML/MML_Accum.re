open MML_Builders;

type range = (int, int, int);
type bracketGroup = {
  range,
  body: string,
};
type t = {
  level0Body: string,
  bracketGroups: list(bracketGroup),
};

let empty = {level0Body: "", bracketGroups: []};

let append = (element, {level0Body, bracketGroups}): t =>
  switch (bracketGroups) {
  | [{body} as bracketGroup, ...rest] => {
      level0Body,
      bracketGroups: [{...bracketGroup, body: body ++ element}, ...rest],
    }
  | [] => {level0Body: level0Body ++ element, bracketGroups: []}
  };

let openBracket = (range, {level0Body, bracketGroups}): t => {
  level0Body,
  bracketGroups: [{range, body: ""}, ...bracketGroups],
};

let _invalidClass = "invalid";
let closeBracket = (superscript, range, {level0Body, bracketGroups}): t =>
  switch (bracketGroups) {
  | [closed, ...rest] =>
    let body =
      elementWithIndex("mo", closed.range, "(")
      ++ closed.body
      ++ elementWithIndex(~superscript, "mo", range, ")")
      |> createElement("mrow");
    switch (rest) {
    | [next, ...rest] => {
        level0Body,
        bracketGroups: [{...next, body: next.body ++ body}, ...rest],
      }
    | [] => {level0Body: level0Body ++ body, bracketGroups: []}
    };
  | [] =>
    let attributes = [("class", _invalidClass), ("stretchy", "false")];
    let element =
      elementWithIndex(~attributes, ~superscript, "mo", range, ")");
    {level0Body: level0Body ++ element, bracketGroups: []};
  };

let toString = ({level0Body, bracketGroups}, range) => {
  let attributes = [("class", _invalidClass), ("stretchy", "false")];
  let closed =
    bracketGroups
    ->Belt.List.map(({range, body}) =>
        elementWithIndex(~attributes, "mo", range, "(") ++ body
      )
    ->Belt.List.reverse
    |> String.concat("");
  let body = level0Body ++ closed;
  body != "" ? createElement("mrow", body) : MML_Util.placeholder(range);
};
