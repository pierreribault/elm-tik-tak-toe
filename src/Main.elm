module Main exposing (..)

import Array exposing (Array, repeat)
import Html exposing (..)
import Html.Attributes exposing (id, style)


main =
    div []
        [ displayHtmlBoardgame initBoardgame
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


displayHtmlCell : Int -> Cell -> Html ()
displayHtmlCell index cell =
    button
        [ id <| "text" ++ String.fromInt index
        ]
        [ text <| displayCell cell ]



-- Boardgame --


type alias Boardgame =
    { cells : Array Cell
    }


initBoardgame : Boardgame
initBoardgame =
    { cells = repeat 9 initCell
    }


displayBoardgame : Boardgame -> List (Html ())
displayBoardgame boardgame =
    List.map (\( index, cell ) -> displayHtmlCell index cell) <| Array.toIndexedList boardgame.cells


displayHtmlBoardgame : Boardgame -> Html ()
displayHtmlBoardgame boardgame =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "grid-gap" "10px"
        , style "grid-auto-rows" "minmax(100px, auto)"
        ]
        (displayBoardgame boardgame)
