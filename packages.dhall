let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210114/packages.dhall sha256:6c7e247826373fc2b63fbf0a28fa84d8ff4981fbac0aa087985413716cb4b286

in  upstream
    with concur-core          = ../purescript-concur-core/spago.dhall               as Location
    with concur-react         = ../purescript-concur-react/spago.dhall              as Location
    with halogen-formless     = ../purescript-formless-independent/lib/spago.dhall  as Location
    with fortuna              = ../purescript-fortuna/spago.dhall                   as Location