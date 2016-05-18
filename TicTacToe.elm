module TicTacToe exposing (..)

import TicTacToe.Model exposing (..)
import TicTacToe.VictoryConditions exposing (..)

import Debug
import Array exposing(Array)
import String
import Char

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

-- Msg
type Msg = Reset | Play Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset -> 
            (newModel, Cmd.none)
        Play position -> 
            case model.state of
                Won p -> (model, Cmd.none)
                Stalled -> (model, Cmd.none)
                GameOn ->
                    (play model position, Cmd.none)

play : Model -> Position -> Model
play model position =
    case get model.board position of
        Nothing -> model
        Just Empty -> 
            let --TODO: change functions to use Model instead and pipe through them all
                boardAfterPlay = set position model.turn model.board
                (stateAfterPlay, winningMove) = detectState model.turn boardAfterPlay
                newTurn = switchTurns model.turn
            in
                {model | board = boardAfterPlay, turn = newTurn, state = stateAfterPlay, winningMove = winningMove}
        Just _ ->
            model

-- VIEW
view : Model -> Html Msg
view model =
    div [] [
        h1 [] [text (stateDescription model), resetButton model.state],
        table [] (Array.indexedMap (toTableRow model) model.board |> Array.toList)
    ]

toTableRow : Model -> Int -> Array Slot -> Html Msg
toTableRow model rowIdx row =
    tr [] (Array.indexedMap (toTableCell model rowIdx) row |> Array.toList)

toTableCell : Model -> Int -> Int -> Slot -> Html Msg
toTableCell model rowIdx colIdx slot =
    td [class (cellClasses (rowIdx, colIdx) model slot), onClick (Play (rowIdx, colIdx))] []

cellClasses : Position -> Model -> Slot -> String
cellClasses pos model slot =
    let
        winClass = pos |> isWinningMove model.winningMove |> cellColor
        playerClass = cellPlayer slot
    in
       if winClass /= "" then playerClass ++ " " ++ winClass else playerClass

cellColor : Bool -> String
cellColor b =
    case b of
        False -> ""
        True -> "winning-move"

cellPlayer : Slot -> String
cellPlayer s =
    case s of 
        Empty -> ""
        PlayedBy p -> toString p

stateDescription : Model -> String
stateDescription model =
    case model.state of
        GameOn ->
            (toString model.turn) ++ "'s turn"
        Stalled -> "It's a Draw"
        Won p ->
            (toString p) ++ " Won"

resetButton : BoardState -> Html Msg
resetButton state =
    a [href "javascript:void(0)", onClick Reset, hidden (state == GameOn)] [text (0x27f2 |> Char.fromCode |> String.fromChar)]