[@bs.send] external stringCharAtUnsafe: (string, int) => string = "charAt";
[@bs.send] external charAtUnsafe: (string, int) => char = "charCodeAt";
[@bs.send] external join: (array(string), [@bs.as ""] _) => string = "join";
[@bs.send]
external split: (string, ~separator: string) => array(string) = "split";
