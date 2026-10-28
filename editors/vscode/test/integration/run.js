// Runs suite.js in a VS Code (downloaded into .vscode-test/ on first use),
// with the extension, against the stag executable $STAG_EXE (default: the one
// built by dune in this repository).
const fs = require("fs");
const os = require("os");
const path = require("path");
const { runTests } = require("@vscode/test-electron");

async function main() {
  const extension = path.join(__dirname, "..", "..");
  const exe =
    process.env.STAG_EXE ??
    path.join(extension, "..", "..", "_build", "default", "driver", "bin", "main.exe");
  if (!fs.existsSync(exe)) {
    console.error(`no stag executable at ${exe}: run dune build, or set STAG_EXE`);
    process.exit(1);
  }
  const workspace = fs.mkdtempSync(path.join(os.tmpdir(), "stag-vscode-test-"));
  fs.mkdirSync(path.join(workspace, ".vscode"));
  fs.writeFileSync(
    path.join(workspace, ".vscode", "settings.json"),
    JSON.stringify({ "stag.serverPath": exe }),
  );
  fs.writeFileSync(path.join(workspace, "a.stag"), "let   x =1\n");
  try {
    await runTests({
      extensionDevelopmentPath: extension,
      extensionTestsPath: path.join(__dirname, "suite.js"),
      extensionTestsEnv: { STAG_EXE: exe },
      launchArgs: [workspace, "--disable-extensions"],
    });
  } catch (error) {
    console.error(error);
    process.exit(1);
  } finally {
    fs.rmSync(workspace, { recursive: true, force: true });
  }
}

main();
