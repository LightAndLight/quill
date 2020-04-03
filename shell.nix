{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.zlib.dev
    pkgs.postgresql
  ];
  shellHook =
    ''
    LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
    '';
}
