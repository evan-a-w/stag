// Runs inside VS Code. The stag executable has no `lsp` subcommand yet, so
// the server fails: it must be reported once, without restarts, and
// formatting must fall back to `stag fmt`.
const assert = require("assert");
const { execFileSync } = require("child_process");
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

async function formatted(document) {
  const edits = await vscode.commands.executeCommand(
    "vscode.executeFormatDocumentProvider",
    document.uri,
    { tabSize: 2, insertSpaces: true },
  );
  const edit = new vscode.WorkspaceEdit();
  edit.set(document.uri, edits ?? []);
  await vscode.workspace.applyEdit(edit);
  return document.getText();
}

exports.run = async function () {
  const folder = vscode.workspace.workspaceFolders[0].uri.fsPath;
  const file = path.join(folder, "a.stag");
  const document = await vscode.workspace.openTextDocument(file);
  assert.strictEqual(document.languageId, "stag");
  const extension = vscode.extensions.getExtension("stag.stag");
  const { reports } = await extension.activate();

  await until("the failure is reported", () => reports.length > 0);
  await quiet();
  console.log("reports:", reports);
  assert.strictEqual(reports.length, 1, "reported once");
  assert.match(reports[0], /language server/);

  const expected = execFileSync(process.env.STAG_EXE, ["fmt", file], {
    encoding: "utf8",
  });
  assert.notStrictEqual(document.getText(), expected);
  assert.strictEqual(await formatted(document), expected);
  console.log("formatted:", JSON.stringify(document.getText()));

  await vscode.commands.executeCommand("stag.restartServer");
  await until("the restart's failure is reported", () => reports.length > 1);
  await quiet();
  assert.strictEqual(reports.length, 2, "reported once more on restart");

  const config = vscode.workspace.getConfiguration("stag");
  await config.update(
    "serverPath",
    "stag-does-not-exist",
    vscode.ConfigurationTarget.Workspace,
  );
  await until("the missing executable is reported", () => reports.length > 2);
  await quiet();
  console.log("reports:", reports);
  assert.strictEqual(reports.length, 3);
  assert.match(reports[2], /stag-does-not-exist` was not found/);
  const edits = await vscode.commands.executeCommand(
    "vscode.executeFormatDocumentProvider",
    document.uri,
    { tabSize: 2, insertSpaces: true },
  );
  assert.strictEqual((edits ?? []).length, 0, "no formatter without stag");
};
