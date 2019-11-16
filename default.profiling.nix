{
  sources ? import ./nix/sources.nix
, compiler ? "ghc865" } :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };
in
(myHaskellPackages.callCabal2nixWithOptions "moscoviumorange" (./.) "--enable-profiling" {})
