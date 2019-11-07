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
  | Centimeter => "m"
  | Kilometer => "km"
  | Megameter => "Mm"
  | Gigameter => "Gm"
  | Terameter => "Tm"
  | Petameter => "Pm"
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
  | Megagram => "Mg"
  | Gigagram => "Gg"
  | Teragram => "Tg"
  | Petagram => "Pg"
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
  | Femtojoule => "fj"
  | Picojoule => "pj"
  | Nanojoule => "nj"
  | Microjoule => "&#x3BC;j"
  | Millijoule => "mj"
  | Centijoule => "j"
  | Kilojoule => "kj"
  | Megajoule => "Mj"
  | Gigajoule => "Gj"
  | Terajoule => "Tj"
  | Petajoule => "Pj"
  /* Power */
  | Watt => "W"
  | Femtowatt => "fW"
  | Picowatt => "pW"
  | Nanowatt => "nW"
  | Microwatt => "&#x3BC;W"
  | Milliwatt => "mW"
  | Kilowatt => "kW"
  | Megawatt => "MW"
  | Gigawatt => "GW"
  | Terawatt => "TW"
  | Petawatt => "PW"
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
  | Celsius => "&deg;C"
  | Fahrenheit => "&deg;F"
  };

let unitMml = unit =>
  "<mi mathvariant=\"normal\">" ++ unitMmlSymbol(unit) ++ "</mi>";

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