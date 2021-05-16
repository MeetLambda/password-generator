{ name = "password-generator"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, dependencies =
  [ "aff"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "formatters"
  , "formless"
  , "fortuna"
  , "integers"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "strings"
  , "tailrec"
  ]
, packages = ./packages.dhall
}
