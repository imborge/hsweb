{ mkDerivation, aeson, attoparsec, base, beam-core, beam-migrate
, bytestring, case-insensitive, conduit, directory, filepath, free
, hashable, haskell-src-exts, hedgehog, lifted-base, monad-control
, mtl, network-uri, postgresql-libpq, postgresql-simple, process
, scientific, stdenv, tagged, tasty, tasty-hunit, temporary, text
, time, unordered-containers, uuid, uuid-types, vector, postgres # added postgres!
}:
mkDerivation {
  pname = "beam-postgres";
  version = "0.4.0.0";
  sha256 = "7bc8dfd34c6fc034bf951ae38f0c795cf1ef39263c96b069de6078afbfb9b637";
  libraryHaskellDepends = [
    aeson attoparsec base beam-core beam-migrate bytestring
    case-insensitive conduit free hashable haskell-src-exts lifted-base
    monad-control mtl network-uri postgresql-libpq postgresql-simple
    scientific tagged text time unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    base beam-core beam-migrate bytestring directory filepath hedgehog
    postgresql-simple process tasty tasty-hunit temporary text uuid
  ];
  buildDepends = [ postgres ]; # had to add this! (for the tests to run, and compile successfully)
  homepage = "http://tathougies.github.io/beam/user-guide/backends/beam-postgres";
  description = "Connection layer between beam and postgres";
  license = stdenv.lib.licenses.mit;
}
