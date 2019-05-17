let foldMap = (arr, initialValue, fn) => {
  let length = Belt.Array.length(arr);
  if (length == 0) {
    (initialValue, [||]);
  } else {
    let nextArr = Belt.Array.makeUninitializedUnsafe(length);
    let accum = ref(initialValue);
    for (x in 0 to length - 1) {
      let (nextAccum, element) = fn(accum^, arr->Belt.Array.getUnsafe(x));
      Belt.Array.setUnsafe(nextArr, x, element);
      accum := nextAccum;
    };
    (accum^, nextArr);
  };
};

let foldMake = (n, initialValue, fn) => {
  let nextArr = Belt.Array.makeUninitializedUnsafe(n);
  let accum = ref(initialValue);
  for (i in 0 to n) {
    let (nextAccum, element) = fn(accum^, i);
    Belt.Array.setExn(nextArr, i, element);
    accum := nextAccum;
  };
  (accum^, nextArr);
};

let insertArray = (x, other, index) =>
  if (index == 0) {
    Belt.Array.concat(other, x);
  } else if (index >= Belt.Array.length(x)) {
    Belt.Array.concat(x, other);
  } else {
    Belt.Array.concatMany([|
      Belt.Array.slice(x, ~offset=0, ~len=index),
      other,
      Belt.Array.sliceToEnd(x, index),
    |]);
  };

let splice = (x, ~offset, ~len) =>
  if (len <= 0) {
    (x, [||]);
  } else {
    let out =
      Belt.Array.concat(
        Belt.Array.slice(x, ~offset=0, ~len=offset),
        Belt.Array.sliceToEnd(x, offset + len),
      );
    let subarray = Belt.Array.slice(x, ~offset, ~len);
    (out, subarray);
  };
