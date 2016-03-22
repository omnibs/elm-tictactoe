import TicTacToe exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = TicTacToe.newModel
    , update = update
    , view = view
    }