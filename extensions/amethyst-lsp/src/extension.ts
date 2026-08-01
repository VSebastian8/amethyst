import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  const { command, args } = resolveServerCommand(context);

  // The server communicates over stdio; "run" and "debug" are identical here
  // since geode has no separate debug build variant to branch on.
  const serverOptions: ServerOptions = {
    run: { command, args, transport: TransportKind.stdio },
    debug: { command, args, transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "amethyst" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.myst"),
    },
  };

  client = new LanguageClient(
    "amethystLsp",
    "Amethyst LSP",
    serverOptions,
    clientOptions,
  );

  context.subscriptions.push({ dispose: () => client?.stop() });
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

/**
 * Resolves how to launch the LSP server as `<command> server`.
 *
 * Resolution order:
 *   1. `amethystLsp.serverPath` setting, if set — treated as a path to the
 *      `geode` binary.
 *   2. A hardcoded relative path into geode's own target directory, for
 *      local development before `geode` is installed anywhere.
 *   3. Fall back to `geode` on PATH, assuming it's installed.
 */
function resolveServerCommand(context: vscode.ExtensionContext): {
  command: string;
  args: string[];
} {
  const configured = vscode.workspace
    .getConfiguration("amethystLsp")
    .get<string>("serverPath");

  if (configured && configured.trim().length > 0) {
    return { command: configured, args: ["server"] };
  }

  const ext = process.platform === "win32" ? ".exe" : "";
  const hardcoded = context.asAbsolutePath(
    path.join("..", "..", "target", "debug", `geode${ext}`),
  );
  if (fs.existsSync(hardcoded)) {
    return { command: hardcoded, args: ["server"] };
  }

  return { command: "geode", args: ["server"] };
}
