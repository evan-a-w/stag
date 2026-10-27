{
  pkgs ? import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/e554fab72f81915600f3f449b786fd9af40439a5.tar.gz"
  ) { },
}:
let
  ocamlPackages = pkgs.ocamlPackages;
in
pkgs.mkShell {
  packages = with ocamlPackages; [
    ocaml
    dune_3
    findlib
    core
    core_unix
    ppx_jane
    ppx_optional
    ppx_expect
    ppx_inline_test
    ocaml-lsp
    utop
    pkgs.ocamlformat
  ];
}
