import Array exposing ( Array )
import Dict exposing (Dict)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value, height )
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
  { question : String
  , level : Int
  , pool : List Int
  , last : Int
  , ignored : List Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { question = toString 0
    , level = 1
    , pool = []
    , last = 0
    , ignored = []
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | Ignore
  -- | Show
  | ChangeLevel Int
  | NewFace (Int, Int)
  | Tick Time.Posix

toString i =
  let s = String.fromInt i in
  String.padLeft 5 '0' s 

fromJust x = case x of
  Just v -> v
  Nothing -> Debug.todo "impossible" 

nth n lst = fromJust (Array.get n (Array.fromList lst))

updateModel (i, j) model =
  if i >= 0 && i /= model.last && not (List.member i model.ignored) then {model | question = toString (nth i model.pool), last = i} else {model | pool = j :: model.pool, question = toString j, last = j}

genNewFace model = Random.generate NewFace (Random.pair (Random.int -1 (List.length model.pool - 1)) (Random.int 0 984))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Ignore ->
      ( {model | ignored = model.last :: model.ignored}
      , genNewFace model
      )
    Roll ->
      ( model
      , genNewFace model
      )
    NewFace newFace ->
      ( updateModel newFace model, Cmd.none)
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    _ -> 
      ( model
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 3000 Tick


-- VIEW

levelDescription level = case level of
  1 -> "Top 100"
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "32px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (String.fromInt l)])
  [1]

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text ("关卡 " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center", style "font-size" "32px"]
          [img [src ("list/" ++ model.question ++ ".jpg"), height 300] []]
    , div [style "text-align" "center"] [button [ onClick Ignore , style "font-size" "32px"] [ text "Ignore" ], button [ onClick Roll , style "font-size" "32px"] [ text "Next" ]]
    --, button [ onClick Show ] [ text "Answer" ]
    ])
