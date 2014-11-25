{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:
let
  inherit (haskellPackages)
    cabal cabalInstall
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
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
