{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "password-generator"
, dependencies =
    [ "console", "effect", "halogen", "random", "stringutils", "psci-support" ]
, packages =
    ./packages.dhall
}
