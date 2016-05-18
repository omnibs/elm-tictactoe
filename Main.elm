module Main exposing (..)

import TicTacToe.Model
import TicTacToe exposing (update, view)
import Html.App as Html


main =
  Html.program
    { init = (TicTacToe.Model.newModel, Cmd.none)
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }