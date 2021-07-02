{ name = "packrat-parsing"
, dependencies =
  [ "console"
  , "effect"
  , "lazy"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/purescript-packrat-parsing.git"
}
