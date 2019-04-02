let foldMap = (arr, initialValue, fn) => {
  let length = arr->Belt.Array.length;
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
