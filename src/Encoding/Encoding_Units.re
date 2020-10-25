open TechniCalcCalculator.Encoding;

/*
 Encoding *msuuuuuuuu
 m - absolute power - 1
 s - sign of power
 u - unit (capacity up to 256)

 Since a power of 0 is invalid, treat it as 1

 This order assumes most cases will be a power of 1 or -1,
 and aims to reduce the size of encoding
 */

let encode = (units: array(TechniCalcCalculator.Unit_Types.unitPower)) => {
  encodeInt(Belt.Array.length(units))
  ++ units
     ->Belt.Array.mapU((. (unit, power)) => {
         let magnitude = max(abs(power) - 1, 0) lsl 9;
         let sign = (magnitude >= 0 ? 0 : 1) lsl 8;
         let unit = Encoding_Unit.toInt(unit);
         let value = magnitude lor sign lor unit;
         encodeInt(value);
       })
     ->StringUtil.join;
};

let%private decodeUnit =
  (. reader) =>
    switch (readInt(reader)) {
    | Some(value) =>
      switch (Encoding_Unit.ofInt(value land 0b11111111)) {
      | Some(unit) =>
        let magnitude = value lsr 9 + 1;
        let sign = value land 0b100000000 != 0;
        let power = sign ? - magnitude : magnitude;
        Some((unit, power));
      | None => None
      }
    | None => None
    };

let decode = reader => readArray(reader, decodeUnit);
