// Tokenizes stag source with the grammar, as VS Code does.
const fs = require("fs");
const path = require("path");
const vsctm = require("vscode-textmate");
const oniguruma = require("vscode-oniguruma");

const root = path.join(__dirname, "..");

async function loadGrammar() {
  const wasm = fs.readFileSync(
    require.resolve("vscode-oniguruma/release/onig.wasm"),
  );
  await oniguruma.loadWASM(wasm.buffer);
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
      createOnigString: (s) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async () =>
      vsctm.parseRawGrammar(
        fs.readFileSync(
          path.join(root, "syntaxes", "stag.tmLanguage.json"),
          "utf8",
        ),
        "stag.tmLanguage.json",
      ),
  });
  return registry.loadGrammar("source.stag");
}

// The tokens of each line, and the rule stack at the end of the text.
function tokenize(grammar, text) {
  let stack = vsctm.INITIAL;
  const lines = text.split(/\r?\n/).map((line) => {
    const result = grammar.tokenizeLine(line, stack);
    stack = result.ruleStack;
    return result.tokens.map((t) => ({
      text: line.slice(t.startIndex, t.endIndex),
      start: t.startIndex,
      scopes: t.scopes,
    }));
  });
  return { lines, stack };
}

module.exports = { loadGrammar, tokenize, INITIAL: vsctm.INITIAL };

if (require.main === module) {
  loadGrammar().then((grammar) => {
    const text = fs.readFileSync(process.argv[2] || 0, "utf8");
    tokenize(grammar, text).lines.forEach((tokens, i) => {
      for (const t of tokens) {
        if (t.text.trim() === "") continue;
        console.log(
          `${i + 1}:${t.start} ${JSON.stringify(t.text)} ${t.scopes.slice(1).join(" ")}`,
        );
      }
    });
  });
}
