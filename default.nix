{ nixpkgs ? import <nixpkgs> {}, compiler }:
nixpkgs.haskell.packages.${compiler}.callPackage ./computational-geometry.nix { }
