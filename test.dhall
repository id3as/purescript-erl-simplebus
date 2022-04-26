let conf = ./spago.dhall

in    conf
    ⫽ { sources =
          conf.sources # [ "test/**/*.purs" ]
      , dependencies =
            conf.dependencies
          # [ "assert"
            , "debug"
            , "erl-kernel"
            , "erl-test-eunit"
            , "exceptions"
            , "filterable"
            , "free"
            ]
      }
