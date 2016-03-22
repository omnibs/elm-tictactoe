import TicTacToe.Model
import TicTacToe exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = TicTacToe.Model.newModel
    , update = update
    , view = view
    }