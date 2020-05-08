{
    sources = [ "src/**/*.purs", "test/**/*.purs" ],
    name = "password-generator",
    dependencies =  [ "console", "effect", "halogen", "halogen-formless", "random", "stringutils", "polyform", "psci-support" ],
    packages = ./packages.dhall
}
