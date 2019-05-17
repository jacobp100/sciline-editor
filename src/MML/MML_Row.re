open MML_RowTypes;

let make = (body, range) => {body, range};

/* If we know the range is not used */
let makeUnsafe = body => {body, range: ((-1), (-1), (-1))};

let toString = row => row.body;

let toPlaceholder = ({body, range}) =>
  body == "" ? MML_Util.placeholder(range) : body;
