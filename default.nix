{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  fast-nats = haskellPackages.callPackage ../fast-nats { };

  f = { mkDerivation, base, fast-nats, stdenv, vector }:
      mkDerivation {
        pname = "fast-vec";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base fast-nats vector ];
        description = "Length indexed vectors with identical performance to vector";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { inherit fast-nats; };

in

  if pkgs.lib.inNixShell then drv.env else drv
