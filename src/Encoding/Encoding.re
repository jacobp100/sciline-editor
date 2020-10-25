let encode = input => Encoding_Elements.encodeElements(input);

let decode = string =>
  Encoding_Base.read(string, Encoding_Elements.readElements);
