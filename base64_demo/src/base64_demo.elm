import Browser
import Html exposing (Html, Attribute, div, input, textarea, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Base64
import Result



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String,
    base64 : String
  }


init : Model
init =
  { content = "" , base64 = ""}



-- UPDATE


type Msg
  = Change String
  | Change2 String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent , base64 = Base64.encode newContent}
    Change2 newContent ->
      { model | base64 = newContent , content = Result.withDefault "Invalid base64 string(校验失败)" (Base64.decode newContent)}


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ textarea [rows 20, cols 40, placeholder "Text to encode in base64 (禁止中文)", value model.content, onInput Change ] []
    --, div [] [ text ("Base64 encoded") ]
    --, div [] [ text (model.base64) ]
    ,textarea [rows 20, cols 40, placeholder "Base64 text to decode", value model.base64, onInput Change ] []
    --, div [] [ text ("Base64 decoded") ]
    --, div [] [ text model.content ]
    ]
