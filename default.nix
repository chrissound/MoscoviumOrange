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
    overrides = self: super: rec {
      rainbox = self.callCabal2nix "rainbox" (builtins.fetchTarball "https://github.com/chrissound/rainbox/tarball/nix") {};
      rainbow = self.callCabal2nix "rainbow" (builtins.fetchGit {
        url = "git@github.com:massysett/rainbow.git";
        rev = "852c6b23e570ebcb190d9169a3aa24449c70e33f";
      })
      {};
    };
  };
in
(myHaskellPackages.callCabal2nixWithOptions "moscoviumorange" (./.) "--enable-profiling"{})
