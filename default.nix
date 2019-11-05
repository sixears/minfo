{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base-unicode-symbols
      , data-textual, exited, fpath, lens, mid, monaderror-io
      , more-unicode, mtl, optparse-applicative, path, stdenv, tasty
      , tasty-hunit, tasty-plus, text, text-printer, tfmt, yaml
      }:
      mkDerivation {
        pname = "infy";
        version = "1.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base path ];
        executableHaskellDepends = [
          aeson base base-unicode-symbols data-textual exited fpath lens
          monaderror-io more-unicode mtl optparse-applicative tasty
          tasty-hunit tasty-plus text text-printer tfmt yaml
        ];
        testHaskellDepends = [ base mid tasty tasty-hunit ];
        doHaddock = false;
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
