/*

`hakyll`

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
  site = haskellPackages.callPackage ./site.nix {};
in
  site // {
    exe = site.out + "/bin/site";
  }