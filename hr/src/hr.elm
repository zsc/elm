import Set
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value , rows)
import List
import Maybe
import Random
import Time


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

init : () -> (Model, Cmd Msg)
init _ =
  ( { fieldA = "张三\n李四"
    , fieldB = "李四\n王五"
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

explode str = String.words str

diff_string strA strB =
   let lstA = explode strA in 
   let lstB = explode strB in 
   String.join " " (Set.toList (Set.diff (Set.fromList lstA) (Set.fromList lstB)))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    ChangeA value ->
      ( {model | fieldA = value}
      , Cmd.none
      )
    ChangeB value ->
      ( {model | fieldB = value}
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

levelDescription level = case level of
  1 -> "比名单"
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
    , div [style "font-size" "32px"] ([textarea [ rows (max 10 (List.length (String.split "\n" model.fieldA))), placeholder "名单A", value (model.fieldA) , onInput ChangeA] []] ++ [textarea [ rows (max 10 (List.length (String.split "\n" model.fieldB))), placeholder "名单B", value (model.fieldB) , onInput ChangeB] []])
    --, div [style "font-size" "24px"] [text "名单B"]
    --, div [style "font-size" "32px"] ()
    , div [style "font-size" "24px"] [text "名单A - 名单B"]
    , div [style "font-size" "16px"] [text (diff_string model.fieldA model.fieldB)]
    , div [style "font-size" "24px"] [text "名单B - 名单A"]
    , div [style "font-size" "16px"] [text (diff_string model.fieldB model.fieldA)]
    ]
    )
