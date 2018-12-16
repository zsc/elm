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

filterTooLarge : Random.Generator Expr -> Random.Generator Expr
filterTooLarge g =
   g |> Random.andThen 
        (\expr ->
             if eval expr > 1000 then filterTooLarge g else Random.constant expr)

filterNegative : Random.Generator Expr -> Random.Generator Expr
filterNegative g =
   g |> Random.andThen 
        (\expr ->
             if eval expr < 0 then filterNegative g
             else Random.constant expr
        )

levelDescription : Language -> Int -> String
levelDescription lang level = case level of
  1 -> case lang of
      LEngish -> "Plus/minus under 10"
      LChinese -> "十以下加减法"
  2 -> case lang of
      LEngish -> "Plus/minus under 100"
      LChinese -> "百以下加减法"
  3 -> case lang of
      LEngish -> "Plus/minus/times/division under 100"
      LChinese -> "百以下加减乘除"
  4 -> case lang of
      LEngish -> "Plus/minus/time/divsion under 100 and exponentation under 10."
      LChinese -> "百以下加减乘除和十以下指数"
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
           0 -> filterTooLarge (Random.map2 Exp (Random.int 0 9) (Random.int 0 9))
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

type Language =
  LEngish
  | LChinese

type alias Model =
  { dieFace : Expr
  , time : Time.Posix
  , clicks : List Click
  , level : Int
  , lang : Language
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {dieFace = Plus 1 1, time = Time.millisToPosix 0, clicks = [], level = defaultLevel, lang = LChinese}
  , Cmd.none
  )

worst : List Click -> List (Html msg)
worst clicks =
  if List.length clicks < 2 then [] else
      let except_last = List.take (List.length clicks - 1) clicks in
      let diffs = List.map2 (\a b -> (a.expr, Time.posixToMillis a.time - Time.posixToMillis b.time)) except_last (List.drop 1 clicks) in
      let top = List.take 3 (List.reverse (List.sortBy (\(a,b) -> b) diffs)) in
      (List.map (\(a, b) -> text (String.fromInt (round (toFloat b / 100.0)) ++ ", " ++ repr a)) top)

stat : List Click -> (Int, Int)
stat clicks =
    if List.length clicks < 2 then (0, 0) else
        let cs = List.map (\h -> Time.posixToMillis h.time) clicks in
        let delta = (Maybe.withDefault 0 (List.head cs)) - (Maybe.withDefault 0 (List.head (List.drop (List.length cs - 1) cs))) in
        (List.length cs, round (60000.0 / (toFloat delta) * (toFloat (List.length cs - 1))))

-- UPDATE


type Msg
  = Roll
  | NewFace Expr
  | Tick Time.Posix
  | Change Int


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
      ( { model | level = level }
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

strStat lang (total, qps) = case lang of
  LEngish -> "Total: " ++ String.fromInt total ++ ", per minute: " ++ String.fromInt qps
  LChinese -> "共：" ++ String.fromInt total ++ "，每分钟：" ++ String.fromInt qps

strLevel lang = case lang of
  LEngish -> "Level"
  LChinese -> "关卡"

buttonN = button [ onClick Roll ] [div [style "font-size" "32px", style "height" "48px"] [text "Next" ]]

levelButtons = List.map (\l -> button [onClick (Change l)] [div [style "font-size" "32px", style "height" "48px"] [text (String.fromInt l) ]]) [1, 2, 3, 4]

view : Model -> Html Msg
view model =
  div [style "font-size" "32px"]
    ([div [style "text-align" "center", style "font-size" "32px"] levelButtons
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text (strLevel model.lang ++ " " ++ String.fromInt model.level ++ ": " ++ levelDescription model.lang model.level)]
    , div [style "font-size" "64px", style "text-align" "center"] 
          [ text (repr model.dieFace) ]
    , div [style "text-align" "center", style "height" "48px"] [buttonN]
    , div [style "font-size" "32px"] [ text (strStat model.lang (stat model.clicks))]
    , br [] []
    ] ++ (List.map (\x -> div [style "font-size" "32px"] [x]) (worst model.clicks)))
