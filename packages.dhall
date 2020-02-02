let mkPackage = https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200127/packages.dhall sha256:06a623f48c49ea1c7675fdf47f81ddb02ae274558e29f511efae1df99ea92fb8
let upstream  = https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200127/packages.dhall sha256:06a623f48c49ea1c7675fdf47f81ddb02ae274558e29f511efae1df99ea92fb8

let overrides = {
    halogen         = upstream.halogen          // { version = "v5.0.0-rc.7" },
    halogen-vdom    = upstream.halogen-vdom     // { version = "v6.1.1" }
}

in  upstream // overrides --// additions
