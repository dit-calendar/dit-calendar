module Page.RegisterTest exposing (updateTest)

import Dict exposing (Dict)
import Expect
import Http exposing (Metadata)
import Http.Detailed exposing (Error(..))
import Page.Register as RegisterPage exposing (Model)
import Test exposing (Test, describe, test)
import Tuple exposing (first, second)


updateTest : Test
updateTest =
    describe "update"
        [ describe "registerResult"
            [ test "case ok" <|
                \_ ->
                    let
                        registerResult =
                            RegisterPage.RegisterResult (Ok ( Metadata "" 0 "" Dict.empty, "ok" ))

                        startModel =
                            Model (RegisterPage.RegisterModel "" "" "" "") []
                    in
                    Expect.all
                        [ first >> Expect.equal startModel
                        , second >> Expect.equal Cmd.none
                        ]
                        (RegisterPage.update registerResult startModel)
            , test "case error" <|
                \_ ->
                    let
                        registerResult =
                            RegisterPage.RegisterResult (Err (BadUrl "error1"))

                        startModel =
                            Model (RegisterPage.RegisterModel "" "" "" "") [ "start Fehler" ]

                        expectedModel =
                            Model (RegisterPage.RegisterModel "" "" "" "") [ "irgendein Fehler" ]
                    in
                    Expect.all
                        [ first >> Expect.equal expectedModel
                        , second >> Expect.equal Cmd.none
                        ]
                        (RegisterPage.update registerResult startModel)
            ]
        ]
