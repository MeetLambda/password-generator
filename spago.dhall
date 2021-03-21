{ name = "password-generator"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, dependencies =
  [ "concur-react"
  , "console"
  , "effect"
  , "fortuna"
  , "formless"
  , "newtype"
  -- , "payload"
  , "test-unit"
  ]
, packages = ./packages.dhall
}
