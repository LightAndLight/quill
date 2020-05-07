#! /usr/bin/env sh

nix-build ./hakyll

exe=$(nix eval "(import ./hakyll).exe")

cd ./src; nix-shell --run "$exe watch" ../hakyll
