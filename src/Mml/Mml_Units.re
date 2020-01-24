let unitMmlSymbol = (unit: ScilineCalculator.Unit_Types.unitType) =>
  switch (unit) {
  /* Time */
  | Second => "s"
  | Minute => "min"
  | Hour => "h"
  | Day => "d"
  | Week => "week"
  | Month => "month"
  | Year => "year"
  | Decade => "decade"
  | Century => "century"
  | Femtosecond => "fs"
  | Picosecond => "ps"
  | Nanosecond => "ns"
  | Microsecond => "&#x3BC;s"
  | Millisecond => "ms"
  /* Length */
  | Meter => "m"
  | Inch => "in"
  | Foot => "ft"
  | Yard => "yd"
  | Mile => "mi"
  | LightYear => "ly"
  | Parsec => "pc"
  | Angstrom => "&#x212B;"
  | Femtometer => "fm"
  | Picometer => "pm"
  | Nanometer => "nm"
  | Micrometer => "&#x3BC;m"
  | Millimeter => "mm"
  | Centimeter => "cm"
  | Kilometer => "km"
  /* Mass */
  | Gram => "g"
  | Tonne => "T"
  | Ounce => "oz"
  | Pound => "lb"
  | Stone => "st"
  | Femtogram => "fg"
  | Picogram => "pg"
  | Nanogram => "ng"
  | Microgram => "&#x3BC;g"
  | Milligram => "mg"
  | Kilogram => "kg"
  /* Area */
  | Acre => "acre"
  | Hectare => "ha"
  /* Volume */
  | Liter => "l"
  | Gallon => "Gal"
  | USGallon => "US Gal"
  | Quart => "qt"
  | Cup => "cup"
  | USCup => "US cup"
  | Teaspoon => "tsp"
  | Tablespoon => "tbsp"
  | FluidOunce => "fl oz"
  | Milliliter => "ml"
  | Centiliter => "cl"
  /* Energy */
  | Joule => "J"
  | Calorie => "cal"
  | ElectronVolt => "eV"
  | BTU => "Btu"
  | Therm => "thm"
  | Femtojoule => "fJ"
  | Picojoule => "pJ"
  | Nanojoule => "nJ"
  | Microjoule => "&#x3BC;J"
  | Millijoule => "mJ"
  | Centijoule => "J"
  | Kilojoule => "kJ"
  | Megajoule => "MJ"
  | Gigajoule => "GJ"
  | Terajoule => "TJ"
  | Petajoule => "PJ"
  /* Power */
  | Watt => "W"
  | Horsepower => "hp"
  | MetricHorsepower => "PS"
  | Nanowatt => "nW"
  | Microwatt => "&#x3BC;W"
  | Milliwatt => "mW"
  | Kilowatt => "kW"
  | Megawatt => "MW"
  | Gigawatt => "GW"
  /* Memory */
  | Bit => "b"
  | Byte => "B"
  | Kilobit => "kb"
  | Megabit => "Mb"
  | Gigabit => "Gb"
  | Terabit => "Tb"
  | Petabit => "Pb"
  | Kibibit => "Kib"
  | Mebibit => "Mib"
  | Gibibit => "Gib"
  | Tebibit => "Tib"
  | Pebibit => "Pib"
  | Kilobyte => "kB"
  | Megabyte => "MB"
  | Gigabyte => "GB"
  | Terabyte => "TB"
  | Petabyte => "PB"
  | Kibibyte => "KiB"
  | Mebibyte => "MiB"
  | Gibibyte => "GiB"
  | Tebibyte => "TiB"
  | Pebibyte => "PiB"
  /* Temperature */
  | Kelvin => "K"
  | Celsius => "&#x00B0;C"
  | Fahrenheit => "&#x00B0;F"
  };

let unitMml = (unit: ScilineCalculator.Unit_Types.unitType) =>
  switch (unit) {
  /* Work around :( */
  | Angstrom => "<mi mathvariant=\"normal\">A</mi>"
  | _ => "<mi mathvariant=\"normal\">" ++ unitMmlSymbol(unit) ++ "</mi>"
  };

let unitPowerMml = ((unit, power): ScilineCalculator.Unit_Types.unitPower) =>
  switch (power) {
  | 1 => unitMml(unit)
  | _ =>
    let powerMml = "<mn>" ++ string_of_int(power) ++ "</mn>";
    "<msup>" ++ unitMml(unit) ++ powerMml ++ "</msup>";
  };

let unitsMml = (units: ScilineCalculator.Unit_Types.units) => {
  let unitsMmlList = Belt.Array.map(units, unitPowerMml)->Belt.List.fromArray;
  String.concat("<mspace width=\"0.1em\" />", unitsMmlList);
};