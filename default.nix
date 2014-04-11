{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:
let
  inherit (haskellPackages)
    cabal cabalInstall_1_18_0_2 haskellPlatform ghcMod
    acidState
    aeson
    hscolour
    lens
    optparseApplicative
    safecopy;

in cabal.mkDerivation (self: {
  pname = "mtg";
  version = "0.1.0";
  src = ./.;
  buildDepends = [
    haskellPlatform
    ghcMod
    acidState
    aeson
    hscolour
    lens
    optparseApplicative
    safecopy
  ];
  buildTools = [ cabalInstall_1_18_0_2 ];
  enableSplitObjs = false;
})
