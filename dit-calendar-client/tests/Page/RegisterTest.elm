module Page.RegisterTest exposing (..)


import Expect
import Test exposing (Test, describe, test)

example : Test
example =
    describe "Example"
    [describe "Addition"
            [ test "two plus two equals four" <|
                \_ -> (2 + 2) |> Expect.equal 4
            , test "three plus four equals seven" <|
                \_ -> (3 + 4) |> Expect.equal 7
            ]
    , describe "Register2"
                  [ test "two plus two equals four" <|
                      \_ -> (2 + 2) |> Expect.equal 4
                  , test "three plus four equals seven" <|
                      \_ -> (3 + 4) |> Expect.equal 7
                  ]        ]

guardianNames : Test
guardianNames =
    test "only 2 guardians have names with less than 6 characters" <|
        \_ ->
            let
                guardians =
                    [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]
            in
                guardians
                    |> List.map String.length
                    |> List.filter (\x -> x < 6)
                    |> List.length
                    |> Expect.equal 2