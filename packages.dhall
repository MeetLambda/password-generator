let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210304/packages.dhall sha256:c88151fe7c05f05290224c9c1ae4a22905060424fb01071b691d3fe2e5bad4ca

in  upstream
  with concur-core      = ../purescript-concur-core/spago.dhall as Location
  with concur-react     = ../purescript-concur-react/spago.dhall as Location
  with formless         = ../purescript-formless-independent/lib/spago.dhall as Location
  with fortuna          = ../purescript-fortuna/spago.dhall as Location
