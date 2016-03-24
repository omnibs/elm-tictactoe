module TicTacToe where

import TicTacToe.Model exposing (..)
import TicTacToe.VictoryConditions exposing (..)

import Debug
import Array exposing(Array)
import String
import Char

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

-- ACTION
type Action = Reset | Play Position


update : Action -> Model -> Model
update action model =
    case action of
        Reset -> 
            newModel
        Play position -> 
            case model.state of
                Won p -> model
                Stalled -> model
                GameOn ->
                    play model position

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
view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        h1 [] [text (stateDescription model)],
        table [] (Array.indexedMap (toTableRow address model) model.board |> Array.toList)
    ]

toTableRow : Signal.Address Action -> Model -> Int -> Array Slot -> Html
toTableRow address model rowIdx row =
    tr [] (Array.indexedMap (toTableCell address model rowIdx) row |> Array.toList)

toTableCell : Signal.Address Action -> Model -> Int -> Int -> Slot -> Html
toTableCell address model rowIdx colIdx slot =
    td [class (cellClasses (rowIdx, colIdx) model slot), onClick address (Play (rowIdx, colIdx))] []

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