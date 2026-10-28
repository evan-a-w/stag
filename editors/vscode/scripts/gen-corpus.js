// Regenerates test/corpus.txt: the quoted strings {|...|} of the repository's
// OCaml tests, which are mostly stag sources, except [%expect] outputs. Each
// snippet follows a line "%%%% FILE:LINE".
const { execFileSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const separator = "%%%% ";
const repo = execFileSync("git", ["rev-parse", "--show-toplevel"], {
  cwd: __dirname,
  encoding: "utf8",
}).trim();
const files = execFileSync("git", ["ls-files", "--", "*/test/*.ml"], {
  cwd: repo,
  encoding: "utf8",
})
  .split("\n")
  .filter((file) => file && !file.startsWith("editors/"));

const seen = new Set();
let out = "";
for (const file of files) {
  const source = fs.readFileSync(path.join(repo, file), "utf8");
  for (const m of source.matchAll(/\{([a-z_]*)\|([\s\S]*?)\|\1\}/g)) {
    const text = m[2];
    if (/\[%expect\s*$/.test(source.slice(0, m.index))) continue;
    if (text.trim() === "" || seen.has(text)) continue;
    if (text.includes(separator)) throw new Error(`${file}: ${separator}`);
    seen.add(text);
    const line = source.slice(0, m.index).split("\n").length;
    out += `${separator}${file}:${line}\n${text}\n`;
  }
}
fs.writeFileSync(path.join(__dirname, "..", "test", "corpus.txt"), out);
console.log(`${seen.size} snippets`);
