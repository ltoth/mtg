{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:
let
  inherit (haskellPackages)
    cabal cabalInstall_1_18_0_3 haskellPlatform ghcMod
    acidState
    aeson
    hscolour
    ipprint
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
    ipprint
    lens
    optparseApplicative
    safecopy
  ];
  buildTools = [ cabalInstall_1_18_0_3 ];
  enableSplitObjs = false;
})
