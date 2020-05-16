let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc865 = pkgs.haskell.packages.ghc865.override {
            overrides = new: old: rec {
              # haskell-src = new.callHackage "haskell-src" "1.0.3.0" {};
              primitive-extras = new.callHackage "primitive-extras" "0.8" {};
              stm-hamt = new.callHackage "stm-hamt" "1.2.0.4" {};
              stm-containers = pkgs.haskell.lib.dontCheck (new.callHackage "stm-containers" "1.1.0.4" {});
              list-t = pkgs.haskell.lib.dontCheck (new.callHackage "list-t" "1.0.4" {});
              supervisors = pkgs.haskell.lib.dontCheck (new.callHackage "supervisors" "0.2.0.0" {});
              # pretty-show = new.callHackage "pretty-show" "1.9.5" {};
              # network-bsd = new.callHackage "network-bsd" "2.8.0.0" {};
              # network = pkgs.haskell.lib.dontCheck (new.callHackage "network" "2.8.0.1" {});
              # bytes = new.callHackage "bytes" "0.16" {};
              capnp = pkgs.haskell.lib.dontCheck (new.callHackage "capnp" "0.5.0.0" {});
            };
          };
        };
      };
    };
  };
  nixpkgs =
    import (builtins.fetchGit {
      name = "nixos-unstable-2020-05-06";
      url = https://github.com/nixos/nixpkgs-channels/;
      ref = "refs/heads/nixos-unstable";
      # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
      rev = "fce7562cf46727fdaf801b232116bc9ce0512049";
    })
    { inherit config; };
in
{ pkgs ? nixpkgs }:
pkgs.mkShell {
  buildInputs = [
    pkgs.zlib.dev
    pkgs.postgresql96
    pkgs.capnproto
    pkgs.haskell.compiler.ghc865
    pkgs.haskell.packages.ghc865.cabal-install
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
