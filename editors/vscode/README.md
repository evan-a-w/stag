# stag for VS Code

Syntax highlighting, editing support and a language server client for
stag (`.stag` files).

- Highlighting follows the lexer (`syntax/src/lexer.ml`). Quotes `.< >.` are
  `meta.quote.stag` and splices `.~` are `meta.splice.stag`, so they can be
  tinted, eg. in `settings.json`:

  ```json
  "editor.tokenColorCustomizations": {
    "textMateRules": [
      { "scope": "meta.quote.stag", "settings": { "fontStyle": "italic" } }
    ]
  }
  ```

- The language server is `stag lsp` (planned). Until it exists, the client
  reports once that it could not start, and formatting runs `stag fmt`.
- `stag.serverPath` (default `stag`, looked up on PATH) is the stag executable.
- `stag: Restart Language Server` restarts the server.

## Build and install

```sh
npm ci
npm run package     # stag-<version>.vsix
code --install-extension stag-*.vsix
```

Without node: `nix shell nixpkgs#nodejs_22 -c npm ...`.

## Test

```sh
./test.sh                # grammar tests, the grammar over all .stag files, tsc
./test.sh --integration  # also the client in VS Code, against the dune build
```

The grammar source is `syntaxes/stag.tmLanguage.yaml`; `npm run build:grammar`
regenerates the JSON, which is committed. Grammar unit tests are in
`test/grammar/` (`^` lines assert the scopes of the line above), the snapshot
in `test/snap/` (update with `npx vscode-tmgrammar-snap -u 'test/snap/*.stag'`).
`node test/tokenize.js FILE` prints the tokens of a file.
