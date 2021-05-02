let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210419/packages.dhall sha256:d9a082ffb5c0fabf689574f0680e901ca6f924e01acdbece5eeedd951731375a

in  upstream
  with concur-core = ../purescript-concur-core/spago.dhall as Location
  with concur-react = ../purescript-concur-react/spago.dhall as Location
  with formless = ../purescript-formless-independent/lib/spago.dhall as Location
  with fortuna = ../purescript-fortuna/spago.dhall as Location
