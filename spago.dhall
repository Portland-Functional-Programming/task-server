{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "b64"
  , "console"
  , "debug"
  , "effect"
  , "errors"
  , "httpure"
  , "psci-support"
  , "refs"
  , "simple-json"
  , "smolder"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
