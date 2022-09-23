module Main exposing (..)

import Html exposing (..)


main =
    div []
        [ displayHtmlCell initCell
        , displayHtmlCell { player = Just Player1 }
        , displayHtmlCell { player = Just Player2 }
        ]



-- PLAYER --


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



-- CELL --


type alias Cell =
    { player : Maybe Player
    }


initCell : Cell
initCell =
    { player = Nothing
    }


displayCell : Cell -> String
displayCell cell =
    displayPlayer cell.player


displayHtmlCell : Cell -> Html ()
displayHtmlCell cell =
    button [] [ text <| displayCell cell ]
