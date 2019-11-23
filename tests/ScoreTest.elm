module ScoreTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (Player, addToPlayerScore)
import Test exposing (..)


suite : Test
suite =
    describe "Player"
        [ test "Add score to Player 0" <|
            \_ ->
                addToPlayerScore 0 1 [ Player 0 "" "" 0 ]
                    |> Expect.equal [ Player 0 "" "" 1 ]
        , test "Add score to Player 2" <|
            \_ ->
                addToPlayerScore 2 1 [ Player 0 "" "" 0, Player 2 "" "" 0 ]
                    |> Expect.equal [ Player 0 "" "" 0, Player 2 "" "" 1 ]
        , fuzz int "Add any score to Player 0" <|
            \rand ->
                addToPlayerScore 0 rand [ Player 0 "" "" 0 ]
                    |> Expect.equal [ Player 0 "" "" rand ]
        ]
