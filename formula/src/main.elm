import Set
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value , rows)
import List
import Maybe
import Random
import Svg exposing (svg, circle, line)
import Svg.Attributes exposing (fill, r, cx, cy, viewBox, x1, y1, x2, y2, stroke)
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
  { control_point1_a : Float
  , control_point1_b : Float
  , control_point2_a : Float
  , control_point2_b : Float
  , level : Int
  }

fromJust : Maybe a -> a
fromJust m_a = case m_a of
  Just v -> v
  Nothing -> Debug.todo "fromJust"

init : () -> (Model, Cmd Msg)
init _ =
  ( { control_point1_a = 0
    , control_point1_b = 0
    , control_point2_a = 0
    , control_point2_b = 0
    , level = 1
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | ChangeLevel Int
  | Change1A String
  | Change1B String
  | Change2A String
  | Change2B String
  | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    Change1A value ->
      ( {model | control_point1_a = strToFloat value}
      , Cmd.none
      )
    Change1B value ->
      ( {model | control_point1_b = strToFloat value}
      , Cmd.none
      )
    Change2A value ->
      ( {model | control_point2_a = strToFloat value}
      , Cmd.none
      )
    Change2B value ->
      ( {model | control_point2_b = strToFloat value}
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

levelDescription level = case level of
  1 -> "分段映射"
  2 -> "矩阵减"
  3 -> "矩阵乘"
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "24px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (levelDescription l)])
  [1]

strToFloat str = 0.01 --str |> String.toFloat |> fromJust

transform model x =
  if x <= model.control_point1_a then model.control_point1_b * (model.control_point1_a - x) + x else
  if x <= model.control_point2_a then model.control_point2_b * (model.control_point2_a - x) + x else x

lines model = 
  List.map (\l_in -> 
    let l = toFloat l_in in
    let val = String.fromFloat (600 - 10 * transform model l) in
    let x = String.fromFloat (10 * (l - 20)) in
    line [x1 x, y1 "600", x2 x, y2 val, fill "#0B79CE", width 10, stroke "#023963"] []) (List.range 20 45)

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [] [text "control point 1a: ", input [ placeholder "Input a number", value (String.fromFloat (model.control_point1_a)), onInput Change1A] []]
    , div [] [text "control point 1b: ", input [ placeholder "Input a number", value (String.fromFloat (model.control_point1_b)), onInput Change1B] []]
    , div [] [text "control point 2a: ", input [ placeholder "Input a number", value (String.fromFloat (model.control_point2_a)), onInput Change2A] []]
    , div [] [text "control point 2b: ", input [ placeholder "Input a number", value (String.fromFloat (model.control_point2_b)), onInput Change2B] []]
    , svg [ viewBox "0 0 1000 1000", Svg.Attributes.width "300px" ]
      ([ 
      --, circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      --, line [ x1 "50", y1 "50", x2 "100", y2 "100", stroke "#023963" ] []
      ] ++ lines model)
    ]
    )
