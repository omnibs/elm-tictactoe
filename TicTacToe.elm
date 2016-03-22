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
    let --TODO: change functions to use Model instead and pipe through them all
        boardAfterPlay = set position model.turn model.board
        stateAfterPlay = detectState model.turn boardAfterPlay
        newTurn = switchTurns model.turn
    in
        {model | board = boardAfterPlay, turn = newTurn, state = stateAfterPlay}

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        h1 [] [text (stateDescription model)],
        table [style [("border", "1px solid black"), ("text-align", "center"), ("font-size", "70px")]] (Array.indexedMap (toTableRow address) model.board |> Array.toList)
    ]

toTableRow : Signal.Address Action -> Int -> Array Slot -> Html
toTableRow address rowIdx row =
    tr [] (Array.indexedMap (toTableCell address rowIdx) row |> Array.toList)

toTableCell : Signal.Address Action -> Int -> Int -> Slot -> Html
toTableCell address rowIdx colIdx slot =
    td [style [("border", "1px solid black"), ("width", "100px"), ("height", "100px")], onClick address (Play (rowIdx, colIdx))] 
        [text (case slot of 
                PlayedBy PlayerOne -> Char.fromCode 10060 |> String.fromChar
                PlayedBy PlayerTwo -> Char.fromCode 8413 |> String.fromChar
                Empty -> "")]

stateDescription : Model -> String
stateDescription model =
    case model.state of
        GameOn ->
            (toString model.turn) ++ "'s turn"
        Stalled -> "It's a Draw"
        Won p ->
            (toString p) ++ " Won"