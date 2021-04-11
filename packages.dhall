let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f

in  upstream
  with concur-core = ../purescript-concur-core/spago.dhall as Location
  with concur-react = ../purescript-concur-react/spago.dhall as Location
  with formless = ../purescript-formless-independent/lib/spago.dhall as Location
  with fortuna = ../purescript-fortuna/spago.dhall as Location
