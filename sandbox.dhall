let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "sandbox/**/*.purs" ]
        , dependencies = conf.dependencies # [ "aff", "aff-promise", "arraybuffer", "control", "float32", "foldable-traversable", "uint", "web-dom" ]
        , backend = "purs-backend-es build"
        }