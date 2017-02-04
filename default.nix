{ mkDerivation, base, fast-nats, primitive, singletons, stdenv
, vector
}:
mkDerivation {
  pname = "fast-vec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base fast-nats primitive singletons vector
  ];
  description = "Length indexed vectors with identical performance to vector";
  license = stdenv.lib.licenses.mit;
}
