let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc865 = pkgs.haskell.packages.ghc865.override {
            overrides = new: old: rec {
              haskell-src = new.callHackage "haskell-src" "1.0.3.0" {};
              primitive-extras = new.callHackage "primitive-extras" "0.8" {};
              stm-hamt = new.callHackage "stm-hamt" "1.2.0.4" {};
              # HTF == 0.13.*
              stm-containers = pkgs.haskell.lib.dontCheck (new.callHackage "stm-containers" "1.1.0.4" {});
              # HTF = new.callHackage "HTF" "0.14.0.3" {};
              list-t = pkgs.haskell.lib.dontCheck (new.callHackage "list-t" "1.0.4" {});
              # non-negotiable due to time-compat's QuickCheck lower bound
              # QuickCheck = new.callHackage "QuickCheck" "2.12.6.1" {};
              # hspec-discover = new.callHackage "hspec-discover" "2.6.1" {};
              # hspec-core = new.callHackage "hspec-core" "2.6.1" {};
              # hspec = new.callHackage "hspec" "2.6.1" {};
              supervisors = pkgs.haskell.lib.dontCheck (new.callHackage "supervisors" "0.2.0.0" {});
              pretty-show = new.callHackage "pretty-show" "1.9.5" {};
              network-bsd = new.callHackage "network-bsd" "2.8.0.0" {};
              # some sort of compile error
              network = pkgs.haskell.lib.dontCheck (new.callHackage "network" "2.8.0.1" {});
              bytes = new.callHackage "bytes" "0.16" {};
              capnp = pkgs.haskell.lib.dontCheck (new.callHackage "capnp" "0.4.0.0" {});
            };
          };
        };
      };
    };
  };
  nixpkgs =
    import (builtins.fetchGit {
      name = "nixos-unstable-2020-04-04";
      url = https://github.com/nixos/nixpkgs-channels/;
      ref = "refs/heads/nixos-unstable";
      # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
      rev = "ae6bdcc53584aaf20211ce1814bea97ece08a248";
    })
    { inherit config; };
in
{ pkgs ? nixpkgs }:
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
