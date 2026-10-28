import { ChildProcess, execFile, spawn } from "child_process";
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
  State,
} from "vscode-languageclient/node";

const hideFailureKey = "stag.hideServerFailure";

// The library shows its own notifications when the server fails to start;
// failures are reported once, by [Server], instead.
class QuietLanguageClient extends LanguageClient {
  override error(message: string, data?: unknown): void {
    super.error(message, data, false);
  }
}

function baseFolder(): string | undefined {
  const documents = [
    vscode.window.activeTextEditor?.document,
    ...vscode.workspace.textDocuments,
  ];
  for (const document of documents) {
    if (document?.languageId !== "stag") continue;
    const folder = vscode.workspace.getWorkspaceFolder(document.uri);
    if (folder) return folder.uri.fsPath;
  }
  return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

// [serverPath] as an executable: a bare name is looked up on PATH, other
// paths are relative to the workspace folder of the active (or any open) stag
// document, or else to the first workspace folder.
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
    const absolute = path.resolve(baseFolder() ?? process.cwd(), expanded);
    return candidates(absolute).find(isFile);
  }
  for (const dir of (process.env.PATH ?? "").split(path.delimiter)) {
    if (dir === "") continue;
    const found = candidates(path.join(dir, expanded)).find(isFile);
    if (found) return found;
  }
  return undefined;
}

// Batch files can only be run through the shell, so the command line is
// quoted.
function commandLine(
  executable: string,
  args: string[],
): { command: string; args: string[]; shell: boolean } {
  if (!/\.(cmd|bat)$/i.test(executable)) {
    return { command: executable, args, shell: false };
  }
  const quote = (arg: string) => {
    if (arg.includes('"')) {
      throw new Error(`cannot run a batch file with a " in: ${arg}`);
    }
    return `"${arg}"`;
  };
  return {
    command: [executable, ...args].map(quote).join(" "),
    args: [],
    shell: true,
  };
}

function exited(child: ChildProcess): Promise<void> {
  if (child.exitCode !== null || child.signalCode !== null) {
    return Promise.resolve();
  }
  return new Promise((resolve) => child.once("exit", () => resolve()));
}

async function terminate(child: ChildProcess): Promise<void> {
  child.kill();
  const timeout = new Promise<boolean>((resolve) =>
    setTimeout(() => resolve(false), 2_000),
  );
  if (!(await Promise.race([exited(child).then(() => true), timeout]))) {
    child.kill("SIGKILL");
    await exited(child);
  }
}

// Formats with [stag fmt], until the language server provides formatting.
class FmtFormatter implements vscode.DocumentFormattingEditProvider {
  private static warned = false;

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
    const text = document.getText();
    try {
      await fs.promises.writeFile(file, text);
      const formatted = await new Promise<string | undefined>((resolve) => {
        // Exit code 1 is a syntax error in the document, which is expected
        // while editing.
        const failed = (error: Error & { code?: unknown }, stderr: string) => {
          const message = (stderr || error.message)
            .split(file)
            .join(document.fileName)
            .trim();
          this.output.error(`stag fmt failed:\n${message}`);
          const firstLine = message.split("\n")[0];
          if (error.code === 1) {
            vscode.window.setStatusBarMessage(
              `$(warning) stag fmt: ${firstLine}`,
              5_000,
            );
          } else if (!FmtFormatter.warned) {
            FmtFormatter.warned = true;
            void vscode.window.showWarningMessage(
              `stag fmt failed: ${firstLine}`,
            );
          }
          resolve(undefined);
        };
        let command;
        try {
          command = commandLine(this.executable, ["fmt", file]);
        } catch (error) {
          return failed(error as Error, "");
        }
        const child = execFile(
          command.command,
          command.args,
          {
            timeout: 10_000,
            maxBuffer: 64 * 1024 * 1024,
            shell: command.shell,
          },
          (error, stdout, stderr) => {
            cancel.dispose();
            if (token.isCancellationRequested) return resolve(undefined);
            if (error) return failed(error, stderr);
            resolve(stdout);
          },
        );
        const cancel = token.onCancellationRequested(() => child.kill());
      });
      if (formatted === "" && text.trim() !== "") {
        this.output.error(
          "stag fmt printed nothing; the document is left as is",
        );
        return [];
      }
      if (formatted === undefined || formatted === text) return [];
      const all = new vscode.Range(
        document.positionAt(0),
        document.positionAt(text.length),
      );
      return [vscode.TextEdit.replace(all, formatted)];
    } finally {
      await fs.promises.rm(dir, { recursive: true, force: true });
    }
  }
}

// A client and the server process it spawned.
class Session {
  child: ChildProcess | undefined;

  constructor(readonly client: LanguageClient) {}

  // The library can only stop a running client, so a starting one (eg. a
  // server that never answers [initialize]) is stopped by killing its process.
  async stop(): Promise<void> {
    if (this.client.state === State.Running) {
      await this.client.stop().catch(() => undefined);
    }
    if (this.child) await terminate(this.child);
  }
}

// Starts and stops run one at a time, in order. None waits for the server to
// initialize, so that a hung server can still be restarted.
class Server {
  private session: Session | undefined;
  private formatter: vscode.Disposable | undefined;
  private pending: Promise<void> = Promise.resolve();
  readonly reports: string[] = [];
  readonly liveProcesses = new Set<ChildProcess>();
  // Each start is a generation, so that a failure is reported once, and not
  // for a client that was since stopped.
  private generation = 0;

  constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly output: vscode.LogOutputChannel,
  ) {}

  start(explicit: boolean): Promise<void> {
    return this.enqueue(() => this.launch(explicit));
  }

  stop(): Promise<void> {
    return this.enqueue(() => this.shutdown());
  }

  restart(explicit: boolean): Promise<void> {
    return this.enqueue(async () => {
      await this.shutdown();
      await this.launch(explicit);
    });
  }

  private enqueue(action: () => Promise<void>): Promise<void> {
    const next = this.pending.then(action).catch((error) => {
      this.output.error(`stag: ${error}`);
    });
    this.pending = next;
    return next;
  }

  private async launch(explicit: boolean): Promise<void> {
    const generation = ++this.generation;
    if (!vscode.workspace.isTrusted) {
      this.output.info(
        "The language server and formatting are off in untrusted workspaces.",
      );
      return;
    }
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
    const isCurrent = () => generation === this.generation;
    let reported = false;
    const failed = (message: string) => {
      if (reported || !isCurrent()) return;
      reported = true;
      this.useFallbackFormatter(executable);
      void this.report(explicit, message);
    };
    const couldNotStart = `stag: the language server (\`${serverPath} lsp\`) could not start. Highlighting still works, and formatting uses \`stag fmt\`.`;
    let started = false;
    const serverOptions: ServerOptions = () =>
      new Promise<ChildProcess>((resolve, reject) => {
        if (!isCurrent()) throw new Error("stopped before it started");
        const command = commandLine(executable, ["lsp"]);
        const child = spawn(command.command, command.args, {
          shell: command.shell,
        });
        session.child = child;
        child.once("error", reject);
        child.once("spawn", () => {
          this.liveProcesses.add(child);
          resolve(child);
        });
        child.once("exit", () => this.liveProcesses.delete(child));
      });
    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ language: "stag" }],
      outputChannel: this.output,
      errorHandler: {
        error: () => ({ action: ErrorAction.Shutdown, handled: true }),
        closed: () => {
          failed(
            started
              ? `stag: the language server (\`${serverPath} lsp\`) exited. Highlighting still works, and formatting uses \`stag fmt\`; restart the server with "stag: Restart Language Server".`
              : couldNotStart,
          );
          return { action: CloseAction.DoNotRestart, handled: true };
        },
      },
      initializationFailedHandler: () => false,
    };
    const session = new Session(
      new QuietLanguageClient("stag", "stag", serverOptions, clientOptions),
    );
    this.session = session;
    session.client.start().then(
      async () => {
        started = true;
        if (!isCurrent()) return session.stop();
        if (
          !session.client.initializeResult?.capabilities
            .documentFormattingProvider
        ) {
          this.useFallbackFormatter(executable);
        }
      },
      async (error) => {
        if (!isCurrent()) return session.stop();
        this.output.error(`stag language server failed to start: ${error}`);
        failed(couldNotStart);
      },
    );
  }

  private async shutdown(): Promise<void> {
    this.generation++;
    this.formatter?.dispose();
    this.formatter = undefined;
    const session = this.session;
    this.session = undefined;
    await session?.stop();
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

// For tests.
export interface Api {
  // The failures reported to the user.
  readonly reports: readonly string[];
  liveServerProcesses(): number;
}

export function activate(context: vscode.ExtensionContext): Api {
  const output = vscode.window.createOutputChannel("stag", { log: true });
  const started = new Server(context, output);
  server = started;
  context.subscriptions.push(
    output,
    vscode.commands.registerCommand("stag.restartServer", () =>
      started.restart(true),
    ),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("stag.serverPath")) {
        void started.restart(true);
      }
    }),
    vscode.workspace.onDidGrantWorkspaceTrust(() => {
      void started.restart(false);
    }),
  );
  void started.start(false);
  return {
    reports: started.reports,
    liveServerProcesses: () => started.liveProcesses.size,
  };
}

export function deactivate(): Promise<void> | undefined {
  return server?.stop();
}
