{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base-unicode-symbols, bytestring
      , data-textual, deepseq, exited, finite-typelits, fluffy, fpath
      , lens, ListLike, mid, monaderror-io, more-unicode, mtl
      , optparse-applicative, QuickCheck, scientific, stdenv, tasty
      , tasty-hunit, tasty-plus, tasty-quickcheck, text, text-printer
      , tfmt, unordered-containers, vector, yaml
      }:
      mkDerivation {
        pname = "infy";
        version = "1.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base base-unicode-symbols bytestring data-textual deepseq
          finite-typelits more-unicode QuickCheck tasty tasty-hunit
          tasty-plus tasty-quickcheck text text-printer tfmt
        ];
        executableHaskellDepends = [
          aeson base base-unicode-symbols bytestring data-textual exited
          fluffy fpath lens ListLike monaderror-io more-unicode mtl
          optparse-applicative scientific tasty tasty-hunit tasty-plus text
          text-printer tfmt unordered-containers vector yaml
        ];
        testHaskellDepends = [ base mid tasty tasty-hunit ];
        description = "manage info.yaml";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
