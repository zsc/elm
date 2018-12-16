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
 | Times Int Int
 | Div Int Int
 | Exp Int Int
 
eval : Expr -> Int
eval expr = case expr of
    Plus a b -> a + b
    Minus a b -> a - b
    Times a b -> a * b
    Div a b -> a // b
    Exp a b -> a ^ b

repr : Expr -> String
repr expr = case expr of
    Plus a b -> String.fromInt a ++ " + " ++ String.fromInt b
    Minus a b -> String.fromInt a ++ " - " ++ String.fromInt b
    Times a b -> String.fromInt a ++ " × " ++ String.fromInt b
    Div a b -> String.fromInt a ++ " ÷ " ++ String.fromInt b
    Exp a b -> String.fromInt a ++ " ^ " ++ String.fromInt b

toExpr : Int -> Int -> Int -> Expr
toExpr op1 op2 op =
  case op of
      0 -> Plus op1 op2
      1 -> Minus op1 op2
      2 -> Times op1 op2
      3 -> Div op1 op2
      _ -> Debug.todo ("Unknown operator: " ++ String.fromInt op)

defaultLevel = 1

filterNegative : Random.Generator Expr -> Random.Generator Expr
filterNegative g =
   g |> Random.andThen 
        (\expr ->
             if eval expr < 0 then filterNegative g
             else Random.constant expr
        )

levelDescription : Int -> String
levelDescription level = case level of
  1 -> "Plus/minus under 10."
  2 -> "Plus/minus under 100."
  3 -> "Plus/minus under 100 and times/divsion under 10."
  4 -> "Plus/minus under 100, times/divsion under 10 and exponentation of 1 ~ 3."
  _ -> Debug.todo "Unknown level"

genLevel2 : Random.Generator Expr
genLevel2 = filterNegative (Random.map3 toExpr (Random.int 0 99) (Random.int 0 99) (Random.int 0 1))

genLevel3 : Random.Generator Expr
genLevel3 =
  Random.int 0 2 |> Random.andThen
      (\i -> case i of
           0 -> Random.map2 (\a b -> Div (a * b) a) (Random.int 1 9) (Random.int 0 9)
           1 -> Random.map3 toExpr (Random.int 0 9) (Random.int 0 9) (Random.constant 2)
           _ -> genLevel2)

genLevel4 = 
  Random.int 0 3 |> Random.andThen
      (\i -> case i of
           0 -> Random.map2 Exp (Random.int 1 3) (Random.int 0 9)
           _ -> genLevel3)

rand : Int -> Random.Generator Expr
rand level =
    case level of
      1 -> filterNegative (Random.map3 toExpr (Random.int 0 9) (Random.int 0 9) (Random.int 0 1))
      2 -> genLevel2
      3 -> genLevel3
      4 -> genLevel4
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
    ([div [style "text-align" "center"] [input [ placeholder "Level", onInput Change ] []]
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text ("Level " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "font-size" "64px", style "text-align" "center"] 
          [ text (repr model.dieFace) ]
    , div [style "text-align" "center", style "height" "48px"] [buttonN]
    , div [style "font-size" "32px"] [ text (stat model.clicks)]
    , br [] []
    ] ++ (List.map (\x -> div [style "font-size" "32px"] [x]) (worst model.clicks)))
