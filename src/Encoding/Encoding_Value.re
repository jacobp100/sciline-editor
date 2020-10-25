open Encoding_Base;

let%private encodeConstant = (constant: TechniCalcCalculator.Real_Constant.t) =>
  switch (constant) {
  | Unit => encodeInt(0)
  | Pi => encodeInt(1)
  | Sqrt(sqrt) => encodeInt(2) ++ encodeInt(sqrt)
  | Exp(exp) => encodeInt(3) ++ encodeInt(exp)
  };

let%private readConstant = reader => {
  switch (readInt(reader)) {
  | Some(0) => Some(TechniCalcCalculator.Real_Constant.Unit)
  | Some(1) => Some(Pi)
  | Some(2) =>
    switch (readInt(reader)) {
    | Some(sqrt) => Some(Sqrt(sqrt))
    | None => None
    }
  | Some(3) =>
    switch (readInt(reader)) {
    | Some(exp) => Some(Exp(exp))
    | None => None
    }
  | _ => None
  };
};

let%private encodeReal = (real: TechniCalcCalculator.Real.t) =>
  switch (real) {
  | Rational(n, d, c) =>
    encodeInt(0) ++ encodeInt(n) ++ encodeInt(d) ++ encodeConstant(c)
  | Decimal(f) =>
    encodeInt(1) ++ TechniCalcCalculator.Decimal.toString(f)->encodeString
  };

let%private readReal = reader =>
  switch (readInt(reader)) {
  | Some(0) =>
    switch (readInt(reader), readInt(reader), readConstant(reader)) {
    | (Some(n), Some(d), Some(c)) =>
      Some(TechniCalcCalculator.Real_Base.ofRational(n, d, c))
    | _ => None
    }
  | Some(1) =>
    switch (readString(reader)) {
    | Some(string) =>
      let decimal =
        TechniCalcCalculator.Decimal.ofString(string)
        ->TechniCalcCalculator.Real_Base.ofDecimal;
      Some(decimal);
    | _ => None
    }
  | _ => None
  };

let%private encodeScalar =
  (. scalar: TechniCalcCalculator.Scalar.t) =>
    switch (scalar) {
    | `Z => encodeInt(0)
    | `R(re) => encodeInt(1) ++ encodeReal(re)
    | `I(im) => encodeInt(2) ++ encodeReal(im)
    | `C(re, im) => encodeInt(3) ++ encodeReal(re) ++ encodeReal(im)
    };

let%private readScalar =
  (. reader) =>
    switch (readInt(reader)) {
    | Some(0) => Some(TechniCalcCalculator.Scalar_Base.zero)
    | Some(1) =>
      switch (readReal(reader)) {
      | Some(real) => Some(`R(real))
      | None => None
      }
    | Some(2) =>
      switch (readReal(reader)) {
      | Some(imag) => Some(`I(imag))
      | None => None
      }
    | Some(3) =>
      switch (readReal(reader), readReal(reader)) {
      | (Some(real), Some(imag)) => Some(`C((real, imag)))
      | _ => None
      }
    | _ => None
    };

let encodeValue = (value: TechniCalcCalculator.Value.t) =>
  switch (value) {
  | #TechniCalcCalculator.Scalar.t as scalar =>
    encodeInt(0) ++ encodeScalar(. scalar)
  | `V(elements) => encodeInt(1) ++ encodeArray(elements, encodeScalar)
  | `M({numRows, numColumns, elements}) =>
    encodeInt(2)
    ++ encodeInt(numRows)
    ++ encodeInt(numColumns)
    ++ encodeArray(elements, encodeScalar)
  | `P(scalar) => encodeInt(3) ++ encodeScalar(. scalar)
  | `N => encodeInt(4)
  };

let readValue = (reader): option(TechniCalcCalculator.Value_Types.t) =>
  switch (readInt(reader)) {
  | Some(0) =>
    switch (readScalar(. reader)) {
    | Some(scalar) => Some(TechniCalcCalculator.Value_Base.ofScalar(scalar))
    | None => None
    }
  | Some(1) =>
    switch (readArray(reader, readScalar)) {
    | Some(elements) =>
      Some(TechniCalcCalculator.Value_Base.ofVector(elements))
    | None => None
    }
  | Some(2) =>
    switch (
      readInt(reader),
      readInt(reader),
      readArray(reader, readScalar),
    ) {
    | (Some(numRows), Some(numColumns), Some(elements)) =>
      let matrix =
        TechniCalcCalculator.Matrix_Base.make(numRows, numColumns, elements)
        ->TechniCalcCalculator.Value_Base.ofMatrix;
      Some(matrix);
    | _ => None
    }
  | Some(3) =>
    switch (readScalar(. reader)) {
    | Some(scalar) =>
      Some(TechniCalcCalculator.Value_Base.ofPercent(scalar))
    | None => None
    }
  | Some(4) => Some(TechniCalcCalculator.Value_Base.nan)
  | _ => None
  };
