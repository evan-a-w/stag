import { execFile } from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";
import {
  CloseAction,
  ErrorAction,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

const hideFailureKey = "stag.hideServerFailure";

// The library shows its own notifications when the server fails to start;
// failures are reported once, by [Server], instead.
class QuietLanguageClient extends LanguageClient {
  override error(message: string, data?: unknown): void {
    super.error(message, data, false);
  }
}

// [serverPath] as an executable: a bare name is looked up on PATH, other
// paths are relative to the first workspace folder.
function resolveExecutable(serverPath: string): string | undefined {
  const isFile = (file: string) => {
    try {
      fs.accessSync(file, fs.constants.X_OK);
      return fs.statSync(file).isFile();
    } catch {
      return false;
    }
  };
  const extensions =
    process.platform === "win32"
      ? ["", ...(process.env.PATHEXT ?? ".EXE;.CMD;.BAT").split(";")]
      : [""];
  const candidates = (file: string) => extensions.map((ext) => file + ext);
  const expanded = serverPath.replace(/^~(?=$|[\\/])/, os.homedir());
  if (expanded.includes("/") || expanded.includes("\\")) {
    const folder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    const absolute = path.resolve(folder ?? process.cwd(), expanded);
    return candidates(absolute).find(isFile);
  }
  for (const dir of (process.env.PATH ?? "").split(path.delimiter)) {
    if (dir === "") continue;
    const found = candidates(path.join(dir, expanded)).find(isFile);
    if (found) return found;
  }
  return undefined;
}

// Formats with [stag fmt], until the language server provides formatting.
class FmtFormatter implements vscode.DocumentFormattingEditProvider {
  constructor(
    private readonly executable: string,
    private readonly output: vscode.LogOutputChannel,
  ) {}

  async provideDocumentFormattingEdits(
    document: vscode.TextDocument,
    _options: vscode.FormattingOptions,
    token: vscode.CancellationToken,
  ): Promise<vscode.TextEdit[]> {
    const name = path.basename(document.fileName) || "untitled.stag";
    const dir = await fs.promises.mkdtemp(path.join(os.tmpdir(), "stag-fmt-"));
    const file = path.join(dir, name);
    try {
      await fs.promises.writeFile(file, document.getText());
      const formatted = await new Promise<string | undefined>((resolve) => {
        const child = execFile(
          this.executable,
          ["fmt", file],
          { timeout: 10_000, maxBuffer: 64 * 1024 * 1024 },
          (error, stdout, stderr) => {
            if (token.isCancellationRequested) return resolve(undefined);
            if (error) {
              const message = (stderr || error.message)
                .split(file)
                .join(document.fileName)
                .trim();
              this.output.error(`stag fmt failed:\n${message}`);
              void vscode.window.showWarningMessage(
                `stag fmt failed: ${message.split("\n")[0]}`,
              );
              return resolve(undefined);
            }
            resolve(stdout);
          },
        );
        token.onCancellationRequested(() => child.kill());
      });
      if (formatted === undefined || formatted === document.getText()) {
        return [];
      }
      const all = new vscode.Range(
        document.positionAt(0),
        document.positionAt(document.getText().length),
      );
      return [vscode.TextEdit.replace(all, formatted)];
    } finally {
      await fs.promises.rm(dir, { recursive: true, force: true });
    }
  }
}

class Server {
  private client: LanguageClient | undefined;
  private formatter: vscode.Disposable | undefined;
  readonly reports: string[] = [];
  // Each start is a generation, so that a failure is reported once, and not
  // for a client that was since stopped.
  private generation = 0;

  constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly output: vscode.LogOutputChannel,
  ) {}

  async start(explicit: boolean): Promise<void> {
    const generation = ++this.generation;
    const serverPath = vscode.workspace
      .getConfiguration("stag")
      .get<string>("serverPath", "stag");
    const executable = resolveExecutable(serverPath);
    if (executable === undefined) {
      this.output.error(`stag executable not found: ${serverPath}`);
      void this.report(
        explicit,
        `stag: \`${serverPath}\` was not found, so the language server and formatting are off. Set \`stag.serverPath\` to the stag executable.`,
      );
      return;
    }
    let reported = false;
    const failed = (message: string) => {
      if (reported || generation !== this.generation) return;
      reported = true;
      this.useFallbackFormatter(executable);
      void this.report(explicit, message);
    };
    const serverOptions: ServerOptions = {
      command: executable,
      args: ["lsp"],
      transport: TransportKind.stdio,
    };
    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ language: "stag" }],
      outputChannel: this.output,
      errorHandler: {
        error: () => ({ action: ErrorAction.Shutdown, handled: true }),
        closed: () => {
          failed(
            `stag: the language server (\`${serverPath} lsp\`) exited. Highlighting still works; restart it with "stag: Restart Language Server".`,
          );
          return { action: CloseAction.DoNotRestart, handled: true };
        },
      },
      initializationFailedHandler: () => false,
    };
    const client = new QuietLanguageClient(
      "stag",
      "stag",
      serverOptions,
      clientOptions,
    );
    this.client = client;
    try {
      await client.start();
    } catch (error) {
      this.output.error(`stag language server failed to start: ${error}`);
      if (this.client === client) this.client = undefined;
      failed(
        `stag: the language server (\`${serverPath} lsp\`) could not start. Highlighting still works, and formatting uses \`stag fmt\`.`,
      );
      return;
    }
    if (generation !== this.generation) return;
    if (!client.initializeResult?.capabilities.documentFormattingProvider) {
      this.useFallbackFormatter(executable);
    }
  }

  async stop(): Promise<void> {
    this.generation++;
    this.formatter?.dispose();
    this.formatter = undefined;
    const client = this.client;
    this.client = undefined;
    if (client?.isRunning()) {
      await client.stop().catch(() => undefined);
    }
  }

  async restart(): Promise<void> {
    await this.stop();
    await this.start(true);
  }

  private useFallbackFormatter(executable: string): void {
    this.formatter?.dispose();
    this.formatter = vscode.languages.registerDocumentFormattingEditProvider(
      { language: "stag" },
      new FmtFormatter(executable, this.output),
    );
  }

  // Unless the user asked to hide them, but always on an explicit restart.
  private async report(explicit: boolean, message: string): Promise<void> {
    if (!explicit && this.context.globalState.get<boolean>(hideFailureKey)) {
      return;
    }
    this.reports.push(message);
    const showOutput = "Show Output";
    const hide = "Don't Show Again";
    const choice = await vscode.window.showWarningMessage(
      message,
      showOutput,
      hide,
    );
    if (choice === showOutput) this.output.show();
    if (choice === hide) {
      await this.context.globalState.update(hideFailureKey, true);
    }
  }
}

let server: Server | undefined;

// The failures reported to the user, for tests.
export interface Api {
  readonly reports: readonly string[];
}

export function activate(context: vscode.ExtensionContext): Api {
  const output = vscode.window.createOutputChannel("stag", { log: true });
  const started = new Server(context, output);
  server = started;
  context.subscriptions.push(
    output,
    vscode.commands.registerCommand("stag.restartServer", () =>
      started.restart(),
    ),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("stag.serverPath")) {
        void started.restart();
      }
    }),
  );
  void started.start(false);
  return { reports: started.reports };
}

export function deactivate(): Promise<void> | undefined {
  return server?.stop();
}
