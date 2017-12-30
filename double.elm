import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String



main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { content : String
  }


model : Model
model =
  Model ""



-- UPDATE


type Msg
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      case String.toInt newContent of
        Ok n -> { model | content = toString (2 * n) }
        _ -> { model | content = "Invalid" }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Number to double", onInput Change ] []
    , div [] [ text (model.content) ]
    ]
