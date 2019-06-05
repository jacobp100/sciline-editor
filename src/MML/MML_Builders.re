let createElement = (~attributes=[], element, body) => {
  let attributes =
    attributes->Belt.List.map(((p, v)) => p ++ "=\"" ++ v ++ "\"")
    |> String.concat(" ");
  let head =
    switch (attributes) {
    | "" => "<" ++ element ++ ">"
    | attributes => "<" ++ element ++ " " ++ attributes ++ ">"
    };
  head ++ body ++ "</" ++ element ++ ">";
};

let elementWithIndex =
    (~attributes=[], ~superscript=None, element, (i, i', s), body) =>
  switch (superscript) {
  | None =>
    let attributes = [
      ("id", string_of_int(i) ++ ":" ++ string_of_int(i')),
      ...attributes,
    ];
    createElement(~attributes, element, body);
  | Some(superscript) =>
    let base =
      createElement(
        ~attributes=[("id", ":" ++ string_of_int(s)), ...attributes],
        element,
        body,
      );
    createElement(
      ~attributes=[("id", string_of_int(i) ++ ":" ++ string_of_int(i'))],
      "msup",
      base ++ superscript,
    );
  };

let placeholder = (~superscript=?, range) =>
  elementWithIndex(
    ~superscript?,
    ~attributes=[("class", "placeholder")],
    "mi",
    range,
    "&#x25a1;",
  );

let xSetRow = value =>
  createElement(
    "mrow",
    createElement("mi", "x") ++ createElement("mo", "=") ++ value,
  );
