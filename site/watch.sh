#! /usr/bin/env sh

nix-build ./compiler

exe=$(nix eval "(import ./compiler).exe")

cd ./src; nix-shell --run "$exe watch" ../compiler
