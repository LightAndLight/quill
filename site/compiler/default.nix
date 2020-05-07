/*

`compiler`

The static site compiler


Outputs:

```
{
  exe: path   # path to the built executable
}
```

*/


let
  haskellPackages = (import ../../nix/projectEnv).haskell;
  compiler = haskellPackages.callPackage ./compiler.nix {};
in
  compiler // {
    exe = compiler.out + "/bin/compiler";
  }
