type expectation('t);
type testEach('t);
[@bs.val]
external test: (string, (. unit) => Js.undefined('t)) => unit = "test";
[@bs.val] external expect: 't => expectation('t) = "expect";
[@bs.send] external toEqual: (expectation('t), 't) => unit = "toEqual";