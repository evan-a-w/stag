// Differential test of the grammar against the lexer (`stag dump-tokens`):
// over the repository's .stag files, test/corpus.txt and random snippets,
// the comments, strings, quoted strings, chars, ints, labels and type
// variables must span the same bytes, the lexer's other tokens must be outside
// them, and the grammar must mark something invalid exactly when the lexer
// fails (up to the lexer's error, after which the lexer reports nothing).
const { execFile, execFileSync } = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");
const { loadGrammar, tokenize } = require("./tokenize");

const repo = execFileSync("git", ["rev-parse", "--show-toplevel"], {
  cwd: __dirname,
  encoding: "utf8",
}).trim();
const exe =
  process.env.STAG_EXE ||
  path.join(repo, "_build", "default", "driver", "bin", "main.exe");
if (!fs.existsSync(exe)) {
  console.error(
    `${exe} does not exist: build it with \`dune build driver/bin/main.exe\` ` +
      "at the repository root, or set STAG_EXE",
  );
  process.exit(1);
}

const compared = new Set([
  "comment",
  "string",
  "quoted_string",
  "char",
  "int",
  "label",
  "tyvar",
  "weak_tyvar",
]);
// Most specific first.
const classOfScope = [
  ["comment.block", "comment"],
  ["string.quoted.double", "string"],
  ["string.quoted.other", "quoted_string"],
  ["string.quoted.single", "char"],
  ["constant.numeric.integer", "int"],
  ["variable.other.enummember.label", "label"],
  ["entity.name.type.parameter.weak", "weak_tyvar"],
  ["entity.name.type.parameter", "tyvar"],
  ["invalid", "invalid"],
];
const scopeClass = (scope) =>
  classOfScope.find(([prefix]) => scope.startsWith(prefix))?.[1];
// Classes whose regions contain several tokens, from a begin token on.
const regions = new Set(["comment", "string", "quoted_string", "char"]);

function inputs() {
  const result = [];
  const files = execFileSync("git", ["ls-files", "--", "*.stag"], {
    cwd: repo,
    encoding: "utf8",
  })
    .split("\n")
    .filter((file) => file && !file.startsWith("editors/"));
  for (const file of files) {
    result.push({
      name: file,
      text: fs.readFileSync(path.join(repo, file), "utf8"),
    });
  }
  const corpus = fs.readFileSync(path.join(__dirname, "corpus.txt"), "utf8");
  for (const chunk of corpus.split(/^%%%% /m).slice(1)) {
    const newline = chunk.indexOf("\n");
    result.push({
      name: `corpus.txt: ${chunk.slice(0, newline)}`,
      text: chunk.slice(newline + 1, -1),
    });
  }
  const edgeCases = [
    "x-1 --1 -_1 1_ 1__2 12a -0 0x1",
    "4611686018427387903 -4611686018427387904",
    "4611686018427387904",
    "''' '_' 'a'b 'ab' '_a '_ 'a '\\'' '\\x41' '\\256' '\\n '\\q'",
    "'é'",
    "'\\é'",
    "x '\n'",
    "`a `_ `A `0 ` a",
    "{Id| {a_b|x|a_b} {|a|} |}",
    "(* \"*)\" '\"' '\\\"' '*)' {|*)|} {a|*)|} |a} (* *) *) x",
    "(* 'é\"' \"*) x",
    "(* 'é'\"' *) x",
    "(* '\\\n' *) x",
    "(*) *) (**) (* *)*)",
    "[%expect] [%expectx] [%expect_test] let%expect_test let%expect_testx",
    "let%bench x.let%expect_test M.let%bench",
    ".~(* *)( .~ ( .~( (* ( *) ) .<.<x>.>.",
    "<a -> b>{..} <int>{x : int} a->b",
    "x > {|s|} <int>{a|b|a}",
    "\"a\\\n  b\" \"a\\\r\n  b\" \"a\\\nb\"",
    "\"\\256\\255\\x4g\\q\\ \"",
    "\"a\\",
    "\"a\\\n",
    "x \r y \t\r\n z",
  ];
  edgeCases.forEach((text, i) =>
    result.push({ name: `edge case ${i}: ${JSON.stringify(text)}`, text }),
  );
  const randomCount = Number(process.env.RANDOM_SNIPPETS ?? 3000);
  // mulberry32, for snippets that are the same on every run.
  let seed = Number(process.env.RANDOM_SEED ?? 42);
  const random = () => {
    seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const alphabet = [
    ..."'\"\\(*){}|.<>~-_aA0`\n é%[]",
    "(*", "*)", "{|", "|}", "{a|", "|a}", ".<", ">.", ".~", "\r\n", "\\\n",
    "let", "%expect", "%expect_test", "'a'", "'\\", "\\x4", "12",
  ];
  for (let i = 0; i < randomCount; i++) {
    const length = 1 + Math.floor(random() * 16);
    let text = "";
    for (let j = 0; j < length; j++) {
      text += alphabet[Math.floor(random() * alphabet.length)];
    }
    result.push({ name: `random ${JSON.stringify(text)}`, text });
  }
  return result;
}

// Byte offset of each UTF-16 index of [text], and one past the end.
function byteOffsets(text) {
  const offsets = new Array(text.length + 1);
  let byte = 0;
  for (let i = 0; i < text.length; i++) {
    offsets[i] = byte;
    const code = text.codePointAt(i);
    if (code > 0xffff) {
      offsets[++i] = byte;
      byte += 4;
    } else byte += code < 0x80 ? 1 : code < 0x800 ? 2 : 3;
  }
  offsets[text.length] = byte;
  return offsets;
}

// The grammar's spans of each class, the class of each byte (undefined in
// code), and whether it ended inside a region.
function grammarView(grammar, text) {
  const offsets = byteOffsets(text);
  const { lines } = tokenize(grammar, text);
  const lineStarts = [0];
  for (const m of text.matchAll(/\r?\n/g)) {
    lineStarts.push(m.index + m[0].length);
  }
  const spans = [];
  const invalid = [];
  const byteClass = new Array(offsets[text.length]);
  let open = null;
  lines.forEach((tokens, line) => {
    for (const t of tokens) {
      const start = offsets[lineStarts[line] + t.start];
      const stop = offsets[lineStarts[line] + t.start + t.text.length];
      const i = t.scopes.findIndex((s) => scopeClass(s));
      const cls = i < 0 ? undefined : scopeClass(t.scopes[i]);
      byteClass.fill(cls, start, stop);
      if (t.scopes.some((s) => s.startsWith("invalid"))) {
        invalid.push({ cls: "invalid", start, stop });
      }
      const inner = t.scopes[i + 1] || "";
      const begins =
        !regions.has(cls) ||
        open?.cls !== cls ||
        inner.startsWith("punctuation.definition.string.begin") ||
        (cls === "comment" &&
          t.scopes.length === i + 2 &&
          inner.startsWith("punctuation.definition.comment") &&
          t.text === "(*");
      if (cls === undefined) open = null;
      else if (begins) {
        open = { cls, start, stop, closed: !regions.has(cls) };
        spans.push(open);
      } else open.stop = stop;
      if (
        inner.startsWith("punctuation.definition.string.end") ||
        (cls === "comment" &&
          t.scopes.length === i + 2 &&
          inner.startsWith("punctuation.definition.comment") &&
          t.text === "*)")
      ) {
        open.closed = true;
      }
    }
  });
  return { spans, invalid, byteClass };
}

function lexerView(file) {
  return new Promise((resolve, reject) => {
    execFile(exe, ["dump-tokens", file], (error, stdout) => {
      if (error) return reject(error);
      const tokens = [];
      let error_ = null;
      for (const line of stdout.split("\n")) {
        const m = line.match(/^\((\w+) (\d+) (\d+)(?: (.*))?\)$/);
        if (!m) continue;
        const token = { cls: m[1], start: +m[2], stop: +m[3] };
        if (token.cls === "error") error_ = { ...token, message: m[4] };
        else tokens.push(token);
      }
      resolve({ tokens, error: error_ });
    });
  });
}

const show = ({ cls, start, stop }) => `(${cls} ${start} ${stop})`;

// Lexer errors the grammar cannot or should not show.
function silentError({ message }) {
  // The grammar does not know the range of int.
  return /exceeds the range of int/.test(message);
}

// Differences between the views of a source, as strings.
function compare(lexer, grammar, text) {
  const problems = [];
  // The token with the error, which the lexer does not show, starts after
  // the tokens it shows.
  const limit = lexer.error
    ? Math.max(0, ...lexer.tokens.map((t) => t.stop))
    : Infinity;
  const lexerSpans = lexer.tokens.filter((t) => compared.has(t.cls));
  const grammarSpans = grammar.spans.filter(
    (s) => compared.has(s.cls) && s.start < limit,
  );
  const key = (s) => show(s);
  const lexerKeys = new Set(lexerSpans.map(key));
  const grammarKeys = new Set(grammarSpans.map(key));
  for (const s of lexerSpans) {
    if (!grammarKeys.has(key(s))) problems.push(`lexer only: ${show(s)}`);
  }
  for (const s of grammarSpans) {
    if (!lexerKeys.has(key(s))) problems.push(`grammar only: ${show(s)}`);
  }
  for (const t of lexer.tokens) {
    if (compared.has(t.cls)) continue;
    const cls = grammar.byteClass[t.start];
    if (cls !== undefined) {
      problems.push(`lexer ${show(t)} is in a grammar ${cls}`);
    }
  }
  const early = grammar.invalid.find((s) => s.start < limit);
  if (early) problems.push(`grammar invalid before the lexer's: ${show(early)}`);
  if (lexer.error) {
    // An unterminated region, which then runs to the end, contains the error
    // if it is not invalid.
    const { start, stop } = lexer.error;
    const shown =
      grammar.invalid.some((s) => s.start < stop && s.stop > limit) ||
      grammar.spans.some(
        (s) => regions.has(s.cls) && !s.closed && s.start <= start,
      );
    if (!shown && !silentError(lexer.error)) {
      problems.push(`grammar shows no error for the lexer's ${show(lexer.error)}`);
    }
  }
  return problems;
}

async function main() {
  const grammar = await loadGrammar();
  const all = inputs();
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "stag-lexer-diff-"));
  let failures = 0;
  let next = 0;
  const worker = async () => {
    while (next < all.length) {
      const index = next++;
      const { name, text } = all[index];
      const file = path.join(dir, `${index}.stag`);
      fs.writeFileSync(file, text);
      const lexer = await lexerView(file);
      const problems = compare(lexer, grammarView(grammar, text), text);
      if (problems.length > 0) {
        failures++;
        if (failures <= 30) {
          console.log(`${name}:\n  ${problems.join("\n  ")}`);
          if (lexer.error) console.log(`  lexer error: ${show(lexer.error)} ${lexer.error.message}`);
        }
      }
    }
  };
  try {
    await Promise.all(Array.from({ length: os.cpus().length }, worker));
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
  console.log(`${all.length} sources, ${failures} differ from the lexer`);
  if (failures > 0) process.exit(1);
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
