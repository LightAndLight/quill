{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.zlib.dev
  ];
  shellHook =
    ''
    LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
    '';
}