{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:
let
  inherit (haskellPackages)
    cabal cabalInstall_1_18_0_3 haskellPlatform
    acidState
    aeson
    derive
    hscolour
    ipprint
    lens
    monadLoops
    MonadRandom
    optparseApplicative
    randomShuffle
    safecopy;

in cabal.mkDerivation (self: {
  pname = "mtg";
  version = "0.1.0";
  src = ./.;
  buildDepends = [
    haskellPlatform
    acidState
    aeson
    derive
    hscolour
    ipprint
    lens
    monadLoops
    MonadRandom
    optparseApplicative
    randomShuffle
    safecopy
  ];
  buildTools = [ cabalInstall_1_18_0_3 ];
  enableSplitObjs = false;
})
