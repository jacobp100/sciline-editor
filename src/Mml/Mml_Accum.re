open Mml_Builders;

module DigitGroups = {
  type state =
    | GroupingDisabled
    | Normal
    | SkipGrouping /* After decimal points etc. */
    | GroupingDigits({numbersRev: list(string)});
  type t = {
    state,
    body: string,
    length: int,
  };

  let make = (~digitGrouping) => {
    state: digitGrouping ? Normal : GroupingDisabled,
    body: "",
    length: 0,
  };

  let digitGroupingEnabled = x => x.state === GroupingDisabled;

  let rec _flattenDigits = (~body, ~numbersRev) =>
    switch (numbersRev) {
    | [c, b, a, ...tail] when tail != [] =>
      _flattenDigits(~body, ~numbersRev=tail) ++ "<mn>,</mn>" ++ a ++ b ++ c
    | [number, ...tail] => _flattenDigits(~body, ~numbersRev=tail) ++ number
    | [] => body
    };

  let toString = ({state, body}) =>
    switch (state) {
    | GroupingDisabled
    | Normal
    | SkipGrouping => body
    | GroupingDigits({numbersRev}) => _flattenDigits(~body, ~numbersRev)
    };

  let length = v => v.length;

  let append = (v, element) => {
    state: Normal,
    body: toString(v) ++ element,
    length: v.length + 1,
  };

  let appendDigit = ({state, body, length}, element) => {
    let length = length + 1;
    switch (state) {
    | GroupingDisabled
    | SkipGrouping => {state, body: body ++ element, length}
    | Normal => {
        state: GroupingDigits({numbersRev: [element]}),
        body,
        length,
      }
    | GroupingDigits({numbersRev}) => {
        state: GroupingDigits({numbersRev: [element, ...numbersRev]}),
        body,
        length,
      }
    };
  };

  let appendDecimalSeparator = (v, element) => {
    state: v.state == GroupingDisabled ? GroupingDisabled : SkipGrouping,
    body: toString(v) ++ element,
    length: v.length + 1,
  };

  let appendBasePrefix = appendDecimalSeparator;

  let concat = (a, b) => {
    state: a.state == GroupingDisabled ? GroupingDisabled : Normal,
    body: toString(a) ++ toString(b),
    length: a.length + b.length,
  };

  let map = (a, fn) => {
    state: a.state == GroupingDisabled ? GroupingDisabled : Normal,
    body: toString(a)->fn,
    length: a.length,
  };
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

  let make = (~digitGrouping) => {
    level0Body: DigitGroups.make(~digitGrouping),
    bracketGroups: [],
  };

  let transformTopLevelWithArg = ({level0Body, bracketGroups}, arg, fn) =>
    switch (bracketGroups) {
    | [{body} as bracketGroup, ...rest] => {
        level0Body,
        bracketGroups: [{...bracketGroup, body: fn(body, arg)}, ...rest],
      }
    | [] => {level0Body: fn(level0Body, arg), bracketGroups: []}
    };

  let appendOpenBracket = (v, openBracketRange) => {
    ...v,
    bracketGroups: [
      {
        openBracketRange,
        body:
          DigitGroups.digitGroupingEnabled(v.level0Body)
          ->DigitGroups.make(~digitGrouping=_),
      },
      ...v.bracketGroups,
    ],
  };

  let _flattenBracketGroup = (~attributes=?, v, {openBracketRange, body}) => {
    let openBracket =
      elementWithIndex(~attributes?, "mo", openBracketRange, "(");
    DigitGroups.digitGroupingEnabled(v.level0Body)
    ->DigitGroups.make(~digitGrouping=_)
    ->DigitGroups.append(openBracket)
    ->DigitGroups.concat(body);
  };

  let _invalidAttributes = [("class", "invalid"), ("stretchy", "false")];
  let appendCloseBracket = (accum, range, superscript): t =>
    switch (accum.bracketGroups) {
    | [closed, ...nextBracketGroups] =>
      let (i, i', s) = range;
      // We want the superscript to be over the whole bracket group,
      // not just over the close bracket
      // Every other element works differently to this
      let appliedRange = superscript != None ? (i, s, (-1)) : range;
      let closeBracket = elementWithIndex("mo", appliedRange, ")");

      _flattenBracketGroup(accum, closed)
      ->DigitGroups.append(closeBracket)
      ->DigitGroups.map(body =>
          switch (superscript) {
          | Some(superscript) =>
            createElement(
              ~attributes=[("id", ":" ++ string_of_int(i'))],
              "msup",
              createElement("mrow", body) ++ superscript,
            )
          | None => createElement("mrow", body)
          }
        )
      ->transformTopLevelWithArg(
          {...accum, bracketGroups: nextBracketGroups},
          _,
          DigitGroups.concat,
        );
    | [] =>
      let attributes = _invalidAttributes;
      elementWithIndex(~attributes, ~superscript?, "mo", range, ")")
      ->transformTopLevelWithArg(accum, _, DigitGroups.append);
    };

  let flatten = ({level0Body, bracketGroups} as v) => {
    let rec iter = bracketGroups =>
      switch (bracketGroups) {
      | [bracketGroup, ...tail] =>
        let body =
          _flattenBracketGroup(
            ~attributes=_invalidAttributes,
            v,
            bracketGroup,
          );
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

let make = BracketGroups.make;
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