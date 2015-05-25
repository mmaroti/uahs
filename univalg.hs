name:			univalg
version:		0.1.1
synopsis:		Universal Algebra in Haskell
-- description:
license:		GPL-2
license-file:		LICENSE
author:			Miklos Maroti
maintainer:		mmaroti@gmail.com
-- copyright:
category:		Math
build-type:		Simple
-- extra-source-files:
cabal-version:		>=1.10

library
  build-depends:
    base >=4.6 && <4.7,
    vector
  hs-source-dirs:	src
  default-language:	Haskell2010
  ghc-options:		-Wall
  exposed-modules:
    UnivAlg.Array
    UnivAlg.Boolean
    UnivAlg.Semiring
    UnivAlg.SatSolver
    UnivAlg.DiscrMath
