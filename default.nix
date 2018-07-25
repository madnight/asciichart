{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, hspec, random, raw-strings-qq
      , silently, stdenv
      }:
      mkDerivation {
        pname = "asciichart";
        version = "1.0.0";
        src = ./.;
        libraryHaskellDepends = [ array base ];
        testHaskellDepends = [ base hspec random raw-strings-qq silently ];
        homepage = "https://github.com/madnight/asciichart";
        description = "Line charts in terminal";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
