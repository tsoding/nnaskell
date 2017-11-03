with import <nixpkgs> {}; {
    boidEnv = stdenv.mkDerivation {
        name = "boid-env";
        buildInputs = [ ghc cabal-install stack openblas ];
    };
}
