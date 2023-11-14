{ pkgs, ... }:

let
  deps = ppkgs: with ppkgs; [ tinydb ];
  py = pkgs.python3.withPackages deps;
in
{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.ocamlPackages.ppx_inline_test
    pkgs.opam
    pkgs.gnumake
    pkgs.sqlite
  ];

  enterShell = "";

  # https://devenv.sh/languages/
  languages.python.enable = true;
  languages.python.package = py;
  languages.java.enable = true;
  languages.ocaml = {
    enable = true;

    # packages = [ "ocaml-lsp-server" ];
  };

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
