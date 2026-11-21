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
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

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
        ];

        pins = {
          oxcaml-compiler = "5.4.0-ox9";
          ocaml-variants = "5.4.0+ox";
        };

        request = map (name: if pins ? ${name} then "${name}.${pins.${name}}" else name) devPackages;

        # Solving the request takes minutes, longer than opam-nix's solver
        # timeout allows, so the solution is checked in and regenerated with
        # [nix run .#resolve].
        lockFile = ./nix/opam-packages.txt;

        resolve = pkgs.writeShellApplication {
          name = "resolve";
          runtimeInputs = [ pkgs.opam ];
          text = ''
            export OPAMROOT OPAMCLI=2.0 OPAMSOLVERTIMEOUT=0
            OPAMROOT=$(mktemp -d)
            trap 'rm -rf "$OPAMROOT"' EXIT
            out=$PWD/nix/opam-packages.txt
            cd ${on.joinRepos repos}
            opam admin list \
              --resolve=${builtins.concatStringsSep "," request} \
              --short --columns=package \
              --environment 'arch=${pkgs.stdenv.hostPlatform.parsed.cpu.name},os=linux,os-distribution=debian,os-family=debian,os-version=13' \
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

        scope = on.applyOverlays (on.__overlays ++ [ subpathOverlay ]) (on.defsToScope pkgs { } defs);
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
