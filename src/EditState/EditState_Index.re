open EditState_Types;
open EditState_Util;

let setIndex = ({elements, allowLabelEditing}, index) => {
  let index =
    preferredInsertionIndex(~step=-1, ~index, ~elements, ~allowLabelEditing);
  {index, elements, allowLabelEditing};
};

let previous = ({index, elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(
      ~step=-1,
      ~index=index - 1,
      ~elements,
      ~allowLabelEditing,
    );
  {index, elements, allowLabelEditing};
};

let next = ({index, elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(
      ~step=1,
      ~index=index + 1,
      ~elements,
      ~allowLabelEditing,
    );
  {index, elements, allowLabelEditing};
};

let moveStart = ({elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(
      ~step=-1,
      ~index=0,
      ~elements,
      ~allowLabelEditing,
    );
  {index, elements, allowLabelEditing};
};

let moveEnd = ({elements, allowLabelEditing}) => {
  let index =
    preferredInsertionIndex(
      ~step=-1,
      ~index=Belt.Array.length(elements),
      ~elements,
      ~allowLabelEditing,
    );
  {index, elements, allowLabelEditing};
};
