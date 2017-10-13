{ mkDerivation, ansi-wl-pprint, base, containers, lens-family
, linear, protolude, stdenv, vector
}:
mkDerivation {
  pname = "computational-geometry";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base containers lens-family linear protolude vector
  ];
  description = "Collection of algorithms in Computational Geometry";
  license = stdenv.lib.licenses.bsd3;
}
