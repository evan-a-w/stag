// Builds syntaxes/stag.tmLanguage.json from the YAML source, substituting
// {{variable}} references with the source's top level `variables`.
const fs = require("fs");
const path = require("path");
const yaml = require("js-yaml");

const dir = path.join(__dirname, "..", "syntaxes");
const { variables = {}, ...grammar } = yaml.load(
  fs.readFileSync(path.join(dir, "stag.tmLanguage.yaml"), "utf8"),
);

const substitute = (value) => {
  if (typeof value === "string") {
    return value.replace(/\{\{(\w+)\}\}/g, (_, name) => {
      if (!(name in variables)) throw new Error(`unknown variable ${name}`);
      return variables[name];
    });
  }
  if (Array.isArray(value)) return value.map(substitute);
  if (value && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([k, v]) => [k, substitute(v)]),
    );
  }
  return value;
};

fs.writeFileSync(
  path.join(dir, "stag.tmLanguage.json"),
  JSON.stringify(substitute(grammar), null, 2) + "\n",
);
