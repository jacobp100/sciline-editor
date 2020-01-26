open Mml_Builders;

type range = (int, int, int);
type digitGroupState =
  | Normal(string)
  | SkipGrouping(string)
  | GroupingDigits({
      body: string,
      numbersRev: list(string),
    });
type digitGroup = {
  state: digitGroupState,
  length: int,
};

module type RowBuilder = {
  type t;
  let empty: t;
  let toString: t => string;
  let length: t => int;
  let append: (t, string) => t;
  let concat: (t, t) => t;
  let map: (t, string => string) => t;
};

module type Accum = {
  type t;
  let empty: t;
  let append: (t, string) => t;
  let appendDigit: (t, string) => t;
  let appendDecimalSeparator: (t, string) => t;
  let appendBasePrefix: (t, string) => t;
  let appendOpenBracket: (t, range) => t;
  let appendCloseBracket: (t, range, option(string)) => t;
  let toString: (t, range) => string;
};

module Concatenation: RowBuilder = {
  type t = string;
  let empty = "";
  let toString = x => x;
  let length = String.length;
  let append = (body, element) => body ++ element;
  let concat = (a, b) => a ++ b;
  let map = (body, fn) => fn(body);
};

module DigitGroups: RowBuilder with type t = digitGroup = {
  type t = digitGroup;

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

  let concat = (a, b) => {
    state: Normal(toString(a) ++ toString(b)),
    length: a.length + b.length,
  };

  let map = (a, fn) => {state: Normal(toString(a)->fn), length: a.length};
};

module DigitGroupExt = {
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
    state: SkipGrouping(DigitGroups.toString(v) ++ element),
    length: v.length + 1,
  };

  let appendBasePrefix = appendDecimalSeparator;
};

module BracketGroups = (Builder: RowBuilder) => {
  type bracketGroup = {
    openBracketRange: range,
    body: Builder.t,
  };
  type t = {
    level0Body: Builder.t,
    bracketGroups: list(bracketGroup),
  };

  let empty = {level0Body: Builder.empty, bracketGroups: []};

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
      {openBracketRange, body: Builder.empty},
      ...bracketGroups,
    ],
  };

  let _flattenBracketGroups = (~attributes=?, {openBracketRange, body}) => {
    let openBracket =
      elementWithIndex(~attributes?, "mo", openBracketRange, "(");
    Builder.empty->Builder.append(openBracket)->Builder.concat(body);
  };

  let _invalidAttributes = [("class", "invalid"), ("stretchy", "false")];
  let appendCloseBracket = (accum, range, superscript): t =>
    switch (accum.bracketGroups) {
    | [closed, ...nextBracketGroupss] =>
      let body =
        _flattenBracketGroups(closed)
        ->Builder.append(elementWithIndex(~superscript, "mo", range, ")"))
        ->Builder.map(createElement("mrow"));
      transformTopLevelWithArg(
        {...accum, bracketGroups: nextBracketGroupss},
        body,
        Builder.concat,
      );
    | [] =>
      let attributes = _invalidAttributes;
      let body =
        elementWithIndex(~attributes, ~superscript, "mo", range, ")");
      transformTopLevelWithArg(accum, body, Builder.append);
    };

  let flatten = ({level0Body, bracketGroups}) => {
    let rec iter = bracketGroups =>
      switch (bracketGroups) {
      | [bracketGroup, ...tail] =>
        let body =
          _flattenBracketGroups(~attributes=_invalidAttributes, bracketGroup);
        Builder.concat(iter(tail), body);
      | [] => level0Body
      };
    iter(bracketGroups);
  };

  let toString = (v, range) => {
    let body = flatten(v);
    switch (Builder.length(body)) {
    | 0 => placeholder(range)
    | 1 => Builder.toString(body)
    | _ => createElement("mrow", Builder.toString(body))
    };
  };
};

module WithDigitGrouping: Accum = {
  module Accum = BracketGroups(DigitGroups);

  type t = Accum.t;
  let empty = Accum.empty;
  let append = (body, element) =>
    Accum.transformTopLevelWithArg(body, element, DigitGroups.append);

  let appendDigit = (body, element) =>
    Accum.transformTopLevelWithArg(body, element, DigitGroupExt.appendDigit);
  let appendDecimalSeparator = (body, element) =>
    Accum.transformTopLevelWithArg(
      body,
      element,
      DigitGroupExt.appendDecimalSeparator,
    );
  let appendBasePrefix = (body, element) =>
    Accum.transformTopLevelWithArg(
      body,
      element,
      DigitGroupExt.appendBasePrefix,
    );
  let appendOpenBracket = Accum.appendOpenBracket;
  let appendCloseBracket = Accum.appendCloseBracket;
  let toString = Accum.toString;
};

module WithoutDigitGrouping: Accum = {
  module Accum = BracketGroups(Concatenation);

  type t = Accum.t;
  let empty = Accum.empty;
  let append = (body, element) =>
    Accum.transformTopLevelWithArg(body, element, Concatenation.append);
  let appendDigit = append;
  let appendDecimalSeparator = append;
  let appendBasePrefix = append;
  let appendOpenBracket = Accum.appendOpenBracket;
  let appendCloseBracket = Accum.appendCloseBracket;
  let toString = Accum.toString;
};