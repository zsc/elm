import Set
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value , rows)
import List
import Maybe
import Random
import Time
import Data exposing (..)


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
  { field : String
  , explain : List String
  , level : Int
  }

fromJust : Maybe a -> a
fromJust m_a = case m_a of
  Just v -> v
  Nothing -> Debug.todo "fromJust"

init : () -> (Model, Cmd Msg)
init _ =
  ( { field = ""
    , explain = []
    , level = 1
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | ChangeLevel Int
  | Change String
  | Tick Time.Posix

update_explain str = List.take 1000 (List.filter (\x -> String.contains (String.toLower str) (String.toLower x)) my_list)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    Change value ->
      ( {model | field = value, explain = update_explain value}
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

levelDescription level = case level of
  1 -> "词汇表"
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
    ([
      div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px"] [input [ placeholder "What to lookup?", value model.field, onInput Change ] []]
    ] ++ List.map (\x -> div [style "font-size" "24px"] [text x]) model.explain
    )
