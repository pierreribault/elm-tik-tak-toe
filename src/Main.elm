module Main exposing (..)

import Html exposing (div, text)


main =
    div []
        [ text (displayPlayer Nothing)
        , text (displayPlayer (Just Player1))
        , text (displayPlayer (Just (switchPlayer Player1)))
        ]


type Player
    = Player1
    | Player2


displayPlayer : Maybe Player -> String
displayPlayer player =
    case player of
        Nothing ->
            "?"

        Just p ->
            case p of
                Player1 ->
                    "O"

                Player2 ->
                    "X"


switchPlayer : Player -> Player
switchPlayer player =
    if player == Player1 then
        Player2

    else
        Player1
