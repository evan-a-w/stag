{
  description = "stag: OxCaml development environment";

  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    nixpkgs.follows = "opam-nix/nixpkgs";
    flake-utils.follows = "opam-nix/flake-utils";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    oxcaml-opam-repository = {
      url = "github:oxcaml/opam-repository";
      flake = false;
    };
    opam-nix.inputs.opam-repository.follows = "opam-repository";
  };

  outputs =
    {
      opam-nix,
      nixpkgs,
      flake-utils,
      opam-repository,
      oxcaml-opam-repository,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        inherit (pkgs.stdenv.hostPlatform) isDarwin;

        # The opam variables the solution is resolved for.
        opamEnv =
          if isDarwin then
            {
              arch = "arm64";
              os = "macos";
              os-distribution = "homebrew";
              os-family = "homebrew";
              os-version = "15";
            }
          else
            {
              arch = pkgs.stdenv.hostPlatform.parsed.cpu.name;
              os = "linux";
              os-distribution = "debian";
              os-family = "debian";
              os-version = "13";
            };

        # Earlier repositories take precedence.
        repos = [
          oxcaml-opam-repository
          opam-repository
        ];

        devPackages = [
          "oxcaml-compiler"
          "ocaml-variants"
          "dune"
          "ocamlfind"
          "core"
          "core_unix"
          "ppx_jane"
          "ocaml_intrinsics"
          "parsexp"
          "sexplib0"
          "time_now"
          "yojson"
          "lsp"
          "jsonrpc"
          "ocamlformat"
          "ocaml-lsp-server"
          "merlin"
          "utop"
          "ctypes"
          "ctypes-foreign"
        ];

        pins = {
          oxcaml-compiler = "5.4.0-ox9";
          ocaml-variants = "5.4.0+ox";
        };

        request = map (name: if pins ? ${name} then "${name}.${pins.${name}}" else name) devPackages;

        # Solving the request takes minutes, longer than opam-nix's solver
        # timeout allows, so the solution is checked in and regenerated with
        # [nix run .#resolve].
        lockFile = ./nix/opam-packages-${opamEnv.os}.txt;

        resolve = pkgs.writeShellApplication {
          name = "resolve";
          runtimeInputs = [ pkgs.opam ];
          text = ''
            export OPAMROOT OPAMCLI=2.0 OPAMSOLVERTIMEOUT=0
            OPAMROOT=$(mktemp -d)
            trap 'rm -rf "$OPAMROOT"' EXIT
            out=$PWD/nix/opam-packages-${opamEnv.os}.txt
            cd ${on.joinRepos repos}
            opam admin list \
              --resolve=${builtins.concatStringsSep "," request} \
              --short --columns=package \
              --environment '${
                builtins.concatStringsSep "," (pkgs.lib.mapAttrsToList (k: v: "${k}=${v}") opamEnv)
              }' \
              --criteria='-count[avoid-version,request],-count[avoid-version,changed],-count[version-lag,request],-count[version-lag,changed]' \
              > "$out"
            echo "wrote $out"
          '';
        };

        lockedQuery = on.opamListToQuery (
          builtins.filter (l: l != "") (pkgs.lib.splitString "\n" (builtins.readFile lockFile))
        );

        defs = on.queryToDefs repos lockedQuery;

        # Several ox packages (merlin, ocaml-compiler-libs, ...) are built from
        # a subdirectory of the oxcaml source tree, which opam-nix ignores.
        subpathOverlay =
          final: prev:
          builtins.mapAttrs (
            name: def:
            prev.${name}.overrideAttrs (oa: {
              postUnpack = (oa.postUnpack or "") + ''
                sourceRoot="$sourceRoot/${def.x-subpath}"
              '';
            })
          ) (pkgs.lib.filterAttrs (_: def: def ? x-subpath) defs);

        # [tools/merge_dot_a_files.sh] would merge static libraries with
        # Apple's libtool on macOS, which the sandbox does not allow.
        darwinOverlay =
          final: prev:
          {
            oxcaml-compiler = prev.oxcaml-compiler.overrideAttrs (oa: {
              patches = (oa.patches or [ ]) ++ [ ./nix/patches/merge-dot-a-files-ar.patch ];
            });
          };

        scope = on.applyOverlays (on.__overlays ++ [ subpathOverlay ] ++ (if isDarwin then [ darwinOverlay ] else [ ])) (
          on.defsToScope pkgs { } defs
        );
      in
      {
        legacyPackages = scope;

        apps.resolve = flake-utils.lib.mkApp { drv = resolve; };

        devShells.default = pkgs.mkShell {
          packages = map (name: scope.${name}) devPackages;
        };
      }
    );
}
