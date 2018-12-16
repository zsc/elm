import Array
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder )
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

type Expr
 = Plus Int Int
 | Minus Int Int
 
eval : Expr -> Int
eval expr = case expr of
    Plus a b -> a + b
    Minus a b -> a - b

repr : Expr -> String
repr expr = case expr of
    Plus a b -> String.fromInt a ++ " + " ++ String.fromInt b
    Minus a b -> String.fromInt a ++ " - " ++ String.fromInt b

toExpr : Int -> Int -> Int -> Expr
toExpr op1 op2 op =
    if op == 0 then Plus op1 op2 else Minus op1 op2

defaultLevel = 1

filterZero : Random.Generator Expr -> Random.Generator Expr
filterZero g =
   g |> Random.andThen 
        (\expr ->
             if eval expr < 0 then g
             else Random.uniform expr []
        )

levelDescription : Int -> String
levelDescription level = case level of
  1 -> "Plus/minus under 10."
  2 -> "Plus/minus under 100."
  _ -> Debug.todo "Unknown level"

rand : Int -> Random.Generator Expr
rand level =
    case level of
      1 -> filterZero (Random.map3 toExpr (Random.int 0 9) (Random.int 0 9) (Random.int 0 1))
      2 -> filterZero (Random.map3 toExpr (Random.int 0 99) (Random.int 0 99) (Random.int 0 1))
      _ -> Debug.todo "Unknown level"

type alias Click = 
  { time : Time.Posix
  , expr : Expr
  }


type alias Model =
  { dieFace : Expr
  , time : Time.Posix
  , clicks : List Click
  , level : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {dieFace = Plus 1 1, time = Time.millisToPosix 0, clicks = [], level = defaultLevel}
  , Cmd.none
  )

worst : List Click -> List (Html msg)
worst clicks =
  if List.length clicks < 2 then [] else
      let except_last = List.take (List.length clicks - 1) clicks in
      let diffs = List.map2 (\a b -> (a.expr, Time.posixToMillis a.time - Time.posixToMillis b.time)) except_last (List.drop 1 clicks) in
      let top = List.take 3 (List.reverse (List.sortBy (\(a,b) -> b) diffs)) in
      (List.map (\(a, b) -> text (String.fromInt (round (toFloat b / 100.0)) ++ ", " ++ repr a)) top)

stat : List Click -> String
stat clicks =
    if List.length clicks < 2 then "Click \"Next\"" else
        let cs = List.map (\h -> Time.posixToMillis h.time) clicks in
        let delta = (Maybe.withDefault 0 (List.head cs)) - (Maybe.withDefault 0 (List.head (List.drop (List.length cs - 1) cs))) in
        let qps = round (60000.0 / (toFloat delta) * (toFloat (List.length cs - 1))) in
        "Total: " ++ String.fromInt (List.length cs) ++ ", per minute: " ++ String.fromInt qps

-- UPDATE


type Msg
  = Roll
  | NewFace Expr
  | Tick Time.Posix
  | Change String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( { model | clicks = {time = model.time, expr = model.dieFace} :: model.clicks}
      , Random.generate NewFace (rand model.level)
      )

    NewFace newFace ->
      ( { model | dieFace = newFace }
      , Cmd.none
      )

    Tick posix ->
      ( { model | time = posix }
      , Cmd.none
      )

    Change level ->
      ( { model | level = Maybe.withDefault defaultLevel (String.toInt level) }
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

buttonN = button [ onClick Roll ] [div [style "font-size" "32px", style "height" "48px"] [text "Next" ]]

view : Model -> Html Msg
view model =
  div []
    ([input [ placeholder "Level", onInput Change ] []
    , div [style "font-size" "32px"] [ text ("Level " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "font-size" "64px", style "text-align" "center"] 
          [ text (repr model.dieFace) ]
    , div [style "text-align" "center", style "height" "48px"] [buttonN]
    , div [style "font-size" "32px"] [ text (stat model.clicks)]
    , br [] []
    ] ++ (List.map (\x -> div [style "font-size" "32px"] [x]) (worst model.clicks)))
