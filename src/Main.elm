module Main exposing (..)

import Array exposing (Array, repeat)
import Browser
import Html exposing (..)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox
        { init = initBoardgame
        , view = displayHtmlBoardgame
        , update = update
        }



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


displayHtmlCell : Int -> Cell -> Player -> Html Msg
displayHtmlCell index cell currentPlayer =
    let
        backgroundColor =
            case cell.player of
                Nothing ->
                    "gray"

                Just player ->
                    case player of
                        Player1 ->
                            "red"

                        Player2 ->
                            "blue"
    in
    button
        [ id <| "text" ++ String.fromInt index
        , onClick <| CellSelectedBy index currentPlayer
        , style "background-color" backgroundColor
        ]
        [ text <| displayCell cell ]


modifyCellOwner : Int -> Player -> Boardgame -> Array Cell
modifyCellOwner id player boardgame =
    Array.set id { player = Just player } boardgame.cells


cellIsOwnable : Int -> Boardgame -> Bool
cellIsOwnable id boardgame =
    case Array.get id boardgame.cells of
        Just cell ->
            cell.player == Nothing

        Nothing ->
            False



-- Boardgame --


type alias Boardgame =
    { cells : Array Cell
    , currentPlayer : Player
    }


initBoardgame : Boardgame
initBoardgame =
    { cells = repeat 9 initCell
    , currentPlayer = Player1
    }


displayBoardgame : Boardgame -> List (Html Msg)
displayBoardgame boardgame =
    List.map (\( index, cell ) -> displayHtmlCell index cell boardgame.currentPlayer) <| Array.toIndexedList boardgame.cells


displayHtmlBoardgame : Boardgame -> Html Msg
displayHtmlBoardgame boardgame =
    let
        player =
            if boardgame.currentPlayer == Player1 then
                "Player 1"

            else
                "Player 2"
    in
    div []
        [ div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(3, 1fr)"
            , style "grid-gap" "10px"
            , style "grid-auto-rows" "minmax(100px, auto)"
            ]
            (displayBoardgame boardgame)
        , p
            [ style "margin-top" "2em"
            , style "text-align" "center"
            , style "font-size" "30px"
            ]
            [ text <| "It's turn of " ++ player ]
        ]



-- Event --


type Msg
    = CellSelectedBy Int Player


update : Msg -> Boardgame -> Boardgame
update msg boardgame =
    case msg of
        CellSelectedBy id player ->
            if cellIsOwnable id boardgame then
                { boardgame
                    | cells = modifyCellOwner id player boardgame
                    , currentPlayer = switchPlayer boardgame.currentPlayer
                }

            else
                boardgame
