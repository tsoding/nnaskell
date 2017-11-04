let
  nixos = import <nixpkgs> {};
  fixedOpenblas = nixos.openblas.override { blas64 = false; };
in rec {
  nnaskellEnv = nixos.stdenv.mkDerivation {
      name = "nnaskell-env";
      buildInputs = [ nixos.ghc nixos.cabal-install nixos.stack fixedOpenblas ];
      LD_LIBRARY_PATH="${fixedOpenblas}/lib";
  };
}
