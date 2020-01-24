open Mml_Builders;

module DigitGroups = {
  type state =
    | Normal(string)
    | SkipGrouping(string)
    | GroupingDigits({
        body: string,
        numbersRev: list(string),
      });
  type t = {
    state,
    length: int,
  };

  let empty = {state: Normal(""), length: 0};

  let rec _flattenDigits = (~body, ~numbersRev) =>
    switch (numbersRev) {
    | [c, b, a, ...tail] when tail != [] =>
      _flattenDigits(~body, ~numbersRev=tail) ++ "<mn>,</mn>" ++ a ++ b ++ c
    | [number, ...tail] => _flattenDigits(~body, ~numbersRev=tail) ++ number
    | [] => body
    };

  let toString = v =>
    switch (v.state) {
    | Normal(body)
    | SkipGrouping(body) => body
    | GroupingDigits({body, numbersRev}) =>
      _flattenDigits(~body, ~numbersRev)
    };

  let length = v => v.length;

  let append = (v, element) => {
    state: Normal(toString(v) ++ element),
    length: v.length + 1,
  };

  let appendDigit = (v, element) => {
    state:
      switch (v.state) {
      | Normal(body) => GroupingDigits({body, numbersRev: [element]})
      | SkipGrouping(body) => SkipGrouping(body ++ element)
      | GroupingDigits({body, numbersRev}) =>
        GroupingDigits({body, numbersRev: [element, ...numbersRev]})
      },
    length: v.length + 1,
  };

  let appendDecimalSeparator = (v, element) => {
    state: SkipGrouping(toString(v) ++ element),
    length: v.length + 1,
  };

  let appendBasePrefix = appendDecimalSeparator;

  let concat = (a, b) => {
    state: Normal(toString(a) ++ toString(b)),
    length: a.length + b.length,
  };

  let map = (a, fn) => {state: Normal(toString(a)->fn), length: a.length};
};

module BracketGroups = {
  type range = (int, int, int);
  type bracketGroup = {
    openBracketRange: range,
    body: DigitGroups.t,
  };
  type t = {
    level0Body: DigitGroups.t,
    bracketGroups: list(bracketGroup),
  };

  let empty = {level0Body: DigitGroups.empty, bracketGroups: []};

  let transformTopLevelWithArg = ({level0Body, bracketGroups}, arg, fn) =>
    switch (bracketGroups) {
    | [{body} as bracketGroup, ...rest] => {
        level0Body,
        bracketGroups: [{...bracketGroup, body: fn(body, arg)}, ...rest],
      }
    | [] => {level0Body: fn(level0Body, arg), bracketGroups: []}
    };

  let appendOpenBracket = ({bracketGroups} as v, openBracketRange) => {
    ...v,
    bracketGroups: [
      {openBracketRange, body: DigitGroups.empty},
      ...bracketGroups,
    ],
  };

  let _flattenBracketGroups = (~attributes=?, {openBracketRange, body}) => {
    let openBracket =
      elementWithIndex(~attributes?, "mo", openBracketRange, "(");
    DigitGroups.empty
    ->DigitGroups.append(openBracket)
    ->DigitGroups.concat(body);
  };

  let _invalidAttributes = [("class", "invalid"), ("stretchy", "false")];
  let appendCloseBracket = (accum, range, superscript): t =>
    switch (accum.bracketGroups) {
    | [closed, ...nextBracketGroupss] =>
      let body =
        _flattenBracketGroups(closed)
        ->DigitGroups.append(
            elementWithIndex(~superscript, "mo", range, ")"),
          )
        ->DigitGroups.map(createElement("mrow"));
      transformTopLevelWithArg(
        {...accum, bracketGroups: nextBracketGroupss},
        body,
        DigitGroups.concat,
      );
    | [] =>
      let attributes = _invalidAttributes;
      let body =
        elementWithIndex(~attributes, ~superscript, "mo", range, ")");
      transformTopLevelWithArg(accum, body, DigitGroups.append);
    };

  let flatten = ({level0Body, bracketGroups}) => {
    let rec iter = bracketGroups =>
      switch (bracketGroups) {
      | [bracketGroup, ...tail] =>
        let body =
          _flattenBracketGroups(~attributes=_invalidAttributes, bracketGroup);
        DigitGroups.concat(iter(tail), body);
      | [] => level0Body
      };
    iter(bracketGroups);
  };

  let toString = (v, range) => {
    let body = flatten(v);
    switch (DigitGroups.length(body)) {
    | 0 => placeholder(range)
    | 1 => DigitGroups.toString(body)
    | _ => createElement("mrow", DigitGroups.toString(body))
    };
  };
};

let empty = BracketGroups.empty;
let append = (body, element) =>
  BracketGroups.transformTopLevelWithArg(body, element, DigitGroups.append);
let appendDigit = (body, element) =>
  BracketGroups.transformTopLevelWithArg(
    body,
    element,
    DigitGroups.appendDigit,
  );
let appendDecimalSeparator = (body, element) =>
  BracketGroups.transformTopLevelWithArg(
    body,
    element,
    DigitGroups.appendDecimalSeparator,
  );
let appendBasePrefix = (body, element) =>
  BracketGroups.transformTopLevelWithArg(
    body,
    element,
    DigitGroups.appendBasePrefix,
  );
let appendOpenBracket = BracketGroups.appendOpenBracket;
let appendCloseBracket = BracketGroups.appendCloseBracket;
let toString = BracketGroups.toString;