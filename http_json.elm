import Html exposing (Html, div, input, text, h2, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Http
import Json.Decode as Decode


main =
  Html.program
    { init = (model, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
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
  = Change (Result Http.Error String)
  | MorePlease

parseResult : Result Http.Error String -> String
parseResult result =
  case result of
    Ok str -> str
    Err _ -> "error"

jsonDecoder : Decode.Decoder String
jsonDecoder =
  Decode.field "category_list" (Decode.field "draft_key" Decode.string)

url = ""

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease -> 
        (model, Http.send Change (Http.get url jsonDecoder))

    Change result ->
        ({ model | content = parseResult result }, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text model.content ]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    ]

