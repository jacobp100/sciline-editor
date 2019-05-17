open AST_Types;

let test = [|`DigitS("8")|];

test->Value.parse->Js.log;
