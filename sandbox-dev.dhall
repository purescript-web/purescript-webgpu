let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "sandbox/**/*.purs" ]
        , dependencies = conf.dependencies # [ "aff", "arrays", "refs", "st", "console", "js-date", "gl-matrix", "numbers", "aff-promise", "arraybuffer", "control", "float32", "foldable-traversable", "uint", "web-dom" ]
        }