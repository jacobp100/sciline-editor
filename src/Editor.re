let insertIndex = Mutation.insertIndex;
let deleteIndex = Mutation.deleteIndex;
let toMml = Mml.create;
let parse = elements =>
  switch (AstBuilder.parse(elements)) {
  | `Ok(node) => (None, Some(node))
  | `Error(i) => (Some(i), None)
  };
let keys = Keys.keys;
let commands = Keys.commands;
