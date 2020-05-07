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
  hakyll = import ./hakyll;
in
  stdenv.mkDerivation {
    name = "site";
    src = ./src;
    buildPhase = ''
      ${hakyll.exe} clean
      ${hakyll.exe} build
    '';
    installPhase = ''
      mkdir -p $out
      cp -R _site/* $out
    '';
  }