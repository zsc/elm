import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  , content2 : String
  , content3 : String
  }


init : Model
init =
  { content = "", content2 = "", content3 = "" }

f x y z =x +y-z

-- UPDATE


type Msg
  = Change String
  | Change2 String
  | Change3 String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    Change2 newContent ->
      { model | content2 = newContent }
    Change3 newContent ->
      { model | content3 = newContent }

-- VIEW


view : Model -> Html Msg
view model =
  div [style "font-size" "48px"]
    [ input [ placeholder "Input a number", value model.content, onInput Change ] []
    , input [ placeholder "Input a number", value model.content2, onInput Change2 ] []
    , input [ placeholder "Input a number", value model.content3, onInput Change3 ] []
    , div [] [ text (String.fromInt (
        f (Maybe.withDefault 0 (String.toInt model.content))
          (Maybe.withDefault 0 (String.toInt model.content2))
          (Maybe.withDefault 0 (String.toInt model.content3))          
        )) ]
    ]
