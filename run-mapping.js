const path = require("path");
const fs = require("fs");

const {
  mapping,
  reverseMapping,
} = require("./src/Encoding/Encoding_BuildMapping.bs");

const js = `export const mapping = ${JSON.stringify(mapping)};
export const reverseMapping = ${JSON.stringify(reverseMapping)};
`;

fs.writeFileSync(path.join(__dirname, "src/Encoding/Encoding_Mapping.js"), js);
