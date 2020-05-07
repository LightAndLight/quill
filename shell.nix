{ pkgs ? import ./nixpkgs }:
pkgs.mkShell {
  buildInputs = [
    pkgs.zlib.dev
    pkgs.postgresql96
    pkgs.capnproto
    pkgs.haskell.packages.ghc865.capnp
  ];
  shellHook = ''
    PATH=~/.cabal/bin:$PATH
    LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH

    export LANG=en_US.UTF-8 \
           PGDATA="$PWD/nix/pgdata" \
           PGHOST="$PWD/nix/sockets" \
           PGDATABASE=test_db

    trap "'$PWD/nix/client' remove" EXIT
    nix/client add
  '';
}
