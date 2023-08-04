{ name = "webgpu"
, dependencies =
  [ "arraybuffer-types"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "integers"
  , "js-promise"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "uint"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
