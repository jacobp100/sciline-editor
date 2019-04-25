open Types;

let insertIndex = Mutation.insertIndex;
let deleteIndex = Mutation.deleteIndex;
let length = TreeUtil.length;
let toMml = Mml.create;
let parse = elements =>
  switch (AstBuilder.parse(elements)) {
  | `Ok(node) => (None, Some(node))
  | `Error(i) => (Some(i), None)
  };
let keys = Keys.keys;

let customAtom = (~value, ~mml) =>
  `CustomAtom({
    customAtomValue: SciLine.encode(value),
    mml,
    superscript: [],
  });
let customAtomEncoded = (~value as customAtomValue, ~mml) =>
  `CustomAtom({customAtomValue, mml, superscript: []});
