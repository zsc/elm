import Set
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value , rows, cols)
import List
import Maybe
import Random
import Time
import Json.Decode as JD
import Json.Print


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Model =
  { fieldA : String
  , fieldB : String
  , level : Int
  }

fromJust : Maybe a -> a
fromJust m_a = case m_a of
  Just v -> v
  Nothing -> Debug.todo "fromJust"

sampleIn = """[{
    "cmd": "sync",
    "id": 1,
    "ids": [
        [
            [
                0
            ],
            "ld",
            0
        ]
    ],
    "task_id": 0
}]"""

init : () -> (Model, Cmd Msg)
init _ =
  ( { fieldA = sampleIn
    , fieldB = processString sampleIn
    , level = 1
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | ChangeLevel Int
  | ChangeA String
  | ChangeB String
  | Tick Time.Posix

type alias Summary =
  { id : Int
  , cmd : String
  --, eu_idx : Maybe String
  --, is_param : Maybe Bool
  --, size : Maybe Int
  --, layer_name : Maybe String
  }

jsonDecoder : JD.Decoder (List Summary)
jsonDecoder = JD.list (JD.map2 Summary
  (JD.field "id" JD.int)
  (JD.field "cmd" JD.string)
  )
  --(JD.field "eu_idx" (JD.string))
  --(JD.field "is_param" (JD.bool))
  --(JD.field "size" (JD.int))
  --(JD.field "_layer_name" (JD.string))

getSummary : String -> String
getSummary x = case JD.decodeString jsonDecoder x of
  Ok summaries -> String.join "\n" (List.map (\summary -> (String.fromInt summary.id) ++ " " ++ summary.cmd) summaries)
  Err z -> (JD.errorToString z)  --"Error parsing"

processString : String -> String
processString x = "[details = " ++ getSummary x
    ++ "]\n" ++ (Result.withDefault "" (Json.Print.prettyString {indent = 4, columns = 60} x)) ++ "\n[/details]"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    ChangeA value ->
      ( {model | fieldA = value, fieldB = processString value}
      , Cmd.none
      )
    ChangeB value ->
      ( model 
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

levelDescription level = case level of
  1 -> "论坛格式"
  2 -> "矩阵减"
  3 -> "矩阵乘"
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "24px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (levelDescription l)])
  [1]

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    --, div [style "font-size" "32px", style "text-align" "center"]
    --      [ text ("功能 " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    --, div [style "font-size" "24px"] [text "名单A"]
    , div [style "font-size" "32px"] ([textarea [cols 60, rows (max 10 (List.length (String.split "\n" model.fieldA))), placeholder "名单A", value (model.fieldA) , onInput ChangeA] []])
    , div [style "font-size" "16px"] [textarea [cols 60, rows 20, value model.fieldB, onInput ChangeB] []]
    --, div [style "font-size" "24px"] [text "名单B"]
    --, div [style "font-size" "32px"] ()
    --, div [style "font-size" "24px"] [text "名单A - 名单B"]
    --, div [style "font-size" "16px"] [text (diff_string model.fieldA model.fieldB)]
    --, div [style "font-size" "24px"] [text "名单B - 名单A"]
    --, div [style "font-size" "16px"] [text (diff_string model.fieldB model.fieldA)]
    ]
    )
