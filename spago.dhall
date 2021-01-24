{
    name = "password-generator",
    sources = [ "src/**/*.purs", "test/**/*.purs" ],
    dependencies =  [
        "concur-react",
        "console",
        "effect",
        "fortuna",
        -- "halogen",
        -- "halogen-formless",
        -- "random",
        -- "stringutils",
        -- "polyform",
        -- "psci-support",
        "payload",
        "test-unit"
    ],
    packages = ./packages.dhall
}
