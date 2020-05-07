/*

`site`

The compiled site


Outputs:

```
{
  out: path   # path to the generated site contents
}
```

*/


let
  stdenv = (import ../nix/projectEnv).stdenv;
  compiler = import ./compiler;
in
  stdenv.mkDerivation {
    name = "site";
    src = ./src;
    buildPhase = ''
      ${compiler.exe} clean
      ${compiler.exe} build
    '';
    installPhase = ''
      mkdir -p $out
      cp -R _site/* $out
    '';
  }
