// Tokenizes every .stag file of the repository (outside editors/) and fails
// if a token is invalid, or a region (comment, string, quote, ...) is left
// open at the end of a file: signs that the grammar drifted from the lexer.
const { execFileSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { loadGrammar, tokenize } = require("./tokenize");

const repo = execFileSync("git", ["rev-parse", "--show-toplevel"], {
  cwd: __dirname,
  encoding: "utf8",
}).trim();

const files = execFileSync("git", ["ls-files", "--", "*.stag"], {
  cwd: repo,
  encoding: "utf8",
})
  .split("\n")
  .filter((file) => file && !file.startsWith("editors/"));

loadGrammar().then((grammar) => {
  let failures = 0;
  for (const file of files) {
    const text = fs.readFileSync(path.join(repo, file), "utf8");
    const { lines, stack } = tokenize(grammar, text);
    lines.forEach((tokens, i) => {
      for (const t of tokens) {
        const invalid = t.scopes.find((s) => s.startsWith("invalid"));
        if (invalid) {
          failures++;
          console.log(`${file}:${i + 1}:${t.start}: ${invalid} ${JSON.stringify(t.text)}`);
        }
      }
    });
    if (stack.depth !== 1) {
      failures++;
      const scopes = lines.at(-1).at(-1)?.scopes.slice(1).join(" ");
      console.log(`${file}: ends inside ${scopes}`);
    }
  }
  console.log(`${files.length} files, ${failures} failure(s)`);
  if (failures > 0 || files.length === 0) process.exit(1);
});
