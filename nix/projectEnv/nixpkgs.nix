let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc865 = pkgs.haskell.packages.ghc865.override {
            overrides = new: old: rec {

              # capnp

              haskell-src = new.callHackage "haskell-src" "1.0.3.0" {};
              primitive-extras = new.callHackage "primitive-extras" "0.8" {};
              stm-hamt = new.callHackage "stm-hamt" "1.2.0.4" {};
              stm-containers = pkgs.haskell.lib.dontCheck (new.callHackage "stm-containers" "1.1.0.4" {});
              list-t = pkgs.haskell.lib.dontCheck (new.callHackage "list-t" "1.0.4" {});
              supervisors = pkgs.haskell.lib.dontCheck (new.callHackage "supervisors" "0.2.0.0" {});
              pretty-show = new.callHackage "pretty-show" "1.9.5" {};
              network-bsd = new.callHackage "network-bsd" "2.8.0.0" {};
              network = pkgs.haskell.lib.dontCheck (new.callHackage "network" "2.8.0.1" {});
              bytes = new.callHackage "bytes" "0.16" {};
              capnp = pkgs.haskell.lib.dontCheck (new.callHackage "capnp" "0.4.0.0" {});


              # hakyll

              http2 = new.callHackage "http2" "1.6.5" {};
              warp = pkgs.haskell.lib.dontCheck (new.callHackage "warp" "3.2.28" {});
              hakyll = pkgs.haskell.lib.appendConfigureFlag old.hakyll "-fpreviewserver";

            };
          };
        };
      };
    };
  };
in
  import (builtins.fetchGit {
    name = "nixos-unstable-2020-04-04";
    url = https://github.com/nixos/nixpkgs-channels/;
    ref = "refs/heads/nixos-unstable";
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    rev = "ae6bdcc53584aaf20211ce1814bea97ece08a248";
  })
  { inherit config; }