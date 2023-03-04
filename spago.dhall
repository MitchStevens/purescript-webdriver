{ name = "purescript-webdriver"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
