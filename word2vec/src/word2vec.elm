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

interpolate x y z = "unknown" -- x + y - z

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
    [ text "Word2vec demo"
    , div [] []
    , input [ placeholder "Input a word", value model.content, onInput Change ] []
    , div [style "font-size" "24px", style "display" "inline"] [text " - "]
    , input [ placeholder "Input a word", value model.content2, onInput Change2 ] []
    , div [] []
    , div [style "font-size" "16px", style "display" "inline"] [text " + "]
    , input [ placeholder "Input a word", value model.content3, onInput Change3 ] []
    , div [] []
    , div [style "font-size" "24px", style "display" "inline"] [text " = ", text (
        interpolate model.content model.content2 model.content3
        ) ]
    ]
