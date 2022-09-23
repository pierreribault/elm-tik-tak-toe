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


doesPlayerGetVictoryCombinaison : List Int -> Player -> Array Cell -> Bool
doesPlayerGetVictoryCombinaison combinaison player cells =
    let
        howManyPlayerGetCombinaisonCells =
            Array.fromList combinaison
                |> Array.map (\id -> Maybe.withDefault initCell (Array.get id cells))
                |> Array.filter (\cell -> cellIsOwnedBy cell player)
                |> Array.length
    in
    howManyPlayerGetCombinaisonCells == 3


doesPlayerWin : Player -> Array Cell -> Bool
doesPlayerWin player cells =
    victoryPossibilities
        |> List.map (\combinaison -> doesPlayerGetVictoryCombinaison combinaison player cells)
        |> List.filter ((==) True)
        |> List.isEmpty
        |> not



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


cellIsOwnedBy : Cell -> Player -> Bool
cellIsOwnedBy cell currentPlayer =
    case cell.player of
        Nothing ->
            False

        Just player ->
            player == currentPlayer



-- Boardgame --


type alias Boardgame =
    { cells : Array Cell
    , currentPlayer : Player
    , winner : Maybe Player
    }


initBoardgame : Boardgame
initBoardgame =
    { cells = repeat 9 initCell
    , currentPlayer = Player1
    , winner = Nothing
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


victoryPossibilities : List (List Int)
victoryPossibilities =
    [ [ 0, 1, 2 ]
    , [ 0, 3, 6 ]
    , [ 0, 4, 8 ]
    , [ 3, 4, 5 ]
    , [ 1, 4, 7 ]
    , [ 6, 4, 2 ]
    , [ 6, 7, 8 ]
    , [ 2, 5, 8 ]
    ]


isThereWinner : Boardgame -> Boardgame
isThereWinner boardgame =
    if doesPlayerWin boardgame.currentPlayer boardgame.cells then
        { boardgame | winner = Just boardgame.currentPlayer }

    else
        { boardgame | currentPlayer = switchPlayer boardgame.currentPlayer }



-- Event --


type Msg
    = CellSelectedBy Int Player


update : Msg -> Boardgame -> Boardgame
update msg boardgame =
    case msg of
        CellSelectedBy id player ->
            if cellIsOwnable id boardgame then
                isThereWinner { boardgame | cells = modifyCellOwner id player boardgame }

            else
                boardgame
