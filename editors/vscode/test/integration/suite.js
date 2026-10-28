// Runs inside VS Code. The stag executable has no `lsp` subcommand yet, so
// the server fails: it must be reported once, without restarts, and
// formatting must fall back to `stag fmt`.
const assert = require("assert");
const { execFileSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const vscode = require("vscode");

async function until(what, condition, timeoutMs = 20_000) {
  const start = Date.now();
  while (!condition()) {
    if (Date.now() - start > timeoutMs) throw new Error(`timed out: ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}

const quiet = () => new Promise((resolve) => setTimeout(resolve, 3_000));

const formatEdits = async (document) =>
  (await vscode.commands.executeCommand(
    "vscode.executeFormatDocumentProvider",
    document.uri,
    { tabSize: 2, insertSpaces: true },
  )) ?? [];

async function formatted(document) {
  const edit = new vscode.WorkspaceEdit();
  edit.set(document.uri, await formatEdits(document));
  await vscode.workspace.applyEdit(edit);
  return document.getText();
}

function isAlive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

// Typing [keys] one by one in an empty stag buffer, as auto-closing pairs
// apply.
async function typed(keys) {
  const document = await vscode.workspace.openTextDocument({
    language: "stag",
    content: "",
  });
  await vscode.window.showTextDocument(document);
  for (const text of keys) {
    await vscode.commands.executeCommand("type", { text });
  }
  const text = document.getText();
  await vscode.commands.executeCommand(
    "workbench.action.revertAndCloseActiveEditor",
  );
  return text;
}

exports.run = async function () {
  const typing = {};
  for (const keys of ["(*", "{|", "(", "{", ".<", "x `"]) {
    typing[keys] = await typed([...keys]);
  }
  console.log("typing:", typing);
  assert.deepStrictEqual(typing, {
    "(*": "(* *)",
    "{|": "{||}",
    "(": "()",
    "{": "{}",
    ".<": ".<>.",
    "x `": "x `",
  });

  const folder = vscode.workspace.workspaceFolders[0].uri.fsPath;
  const file = path.join(folder, "a.stag");
  const document = await vscode.workspace.openTextDocument(file);
  assert.strictEqual(document.languageId, "stag");
  assert.ok(vscode.workspace.isTrusted, "the test workspace is trusted");
  const extension = vscode.extensions.getExtension("stag.stag");
  const api = await extension.activate();
  const { reports } = api;
  const config = vscode.workspace.getConfiguration("stag");
  const setServerPath = (value) =>
    config.update("serverPath", value, vscode.ConfigurationTarget.Workspace);

  await until("the failure is reported", () => reports.length > 0);
  await quiet();
  console.log("reports:", reports);
  assert.strictEqual(reports.length, 1, "reported once");
  assert.match(reports[0], /could not start.*stag fmt/);

  const expected = execFileSync(process.env.STAG_EXE, ["fmt", file], {
    encoding: "utf8",
  });
  assert.notStrictEqual(document.getText(), expected);
  assert.strictEqual(await formatted(document), expected);
  console.log("formatted:", JSON.stringify(document.getText()));

  const broken = await vscode.workspace.openTextDocument({
    language: "stag",
    content: "let x = (\n",
  });
  assert.deepStrictEqual(await formatEdits(broken), [], "syntax error");
  assert.strictEqual(reports.length, 1, "syntax errors are not reported");

  await vscode.commands.executeCommand("stag.restartServer");
  await until("the restart's failure is reported", () => reports.length > 1);
  await quiet();
  assert.strictEqual(reports.length, 2, "reported once more on restart");

  // Concurrent restarts, and a relative serverPath.
  fs.mkdirSync(path.join(folder, "bin"));
  fs.symlinkSync(process.env.STAG_EXE, path.join(folder, "bin", "stag"));
  const restarts = [1, 2, 3].map(() =>
    vscode.commands.executeCommand("stag.restartServer"),
  );
  restarts.push(setServerPath("bin/stag"));
  restarts.push(vscode.commands.executeCommand("stag.restartServer"));
  await Promise.all(restarts);
  await until("the restarts' failure is reported", () => reports.length > 2);
  await quiet();
  const settled = reports.length;
  console.log("reports after concurrent restarts:", reports.slice(2));
  assert.match(reports.at(-1), /`bin\/stag lsp`\) could not start/);
  assert.strictEqual(api.liveServerProcesses(), 0);
  assert.strictEqual(await formatted(document), expected);
  await quiet();
  assert.strictEqual(reports.length, settled, "reports are stable");

  // A server that never answers `initialize`.
  const pids = path.join(folder, "pids");
  const hung = path.join(folder, "hung-stag");
  fs.writeFileSync(
    hung,
    `#!/bin/sh
if [ "$1" = fmt ]; then exec "$STAG_EXE" "$@"; fi
echo $$ >> "${pids}"
exec sleep 600
`,
  );
  fs.chmodSync(hung, 0o755);
  const started = () =>
    fs.existsSync(pids)
      ? fs.readFileSync(pids, "utf8").trim().split("\n").map(Number)
      : [];
  await setServerPath(hung);
  await until("the hung server starts", () => started().length > 0);
  await Promise.all(
    [1, 2, 3, 4].map(() =>
      vscode.commands.executeCommand("stag.restartServer"),
    ),
  );
  // Restarts superseded before their server is spawned spawn nothing.
  await until("the hung server is restarted", () => started().length > 1);
  await quiet();
  const alive = started().filter(isAlive);
  console.log("hung servers:", started(), "alive:", alive);
  assert.strictEqual(alive.length, 1, "one hung server is alive");
  assert.strictEqual(alive[0], started().at(-1));
  assert.strictEqual(api.liveServerProcesses(), 1);
  assert.strictEqual(reports.length, settled, "a hung server is not reported");

  await setServerPath("stag-does-not-exist");
  await until(
    "the missing executable is reported",
    () => reports.length > settled,
  );
  await quiet();
  console.log("reports:", reports);
  assert.strictEqual(reports.length, settled + 1);
  assert.match(reports.at(-1), /stag-does-not-exist` was not found/);
  assert.deepStrictEqual(started().filter(isAlive), [], "hung server stopped");
  assert.strictEqual(api.liveServerProcesses(), 0);
  assert.strictEqual(
    (await formatEdits(document)).length,
    0,
    "no formatter without stag",
  );
};
