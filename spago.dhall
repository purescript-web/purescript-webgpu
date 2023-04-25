{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "webgpu"
, dependencies =
  [ "arraybuffer-types"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "uint"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  , "web-promise"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
