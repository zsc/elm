import Array
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, rowspan, id, style, tabindex)
import List
import Maybe
import Random
import Time
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)


-- MAIN

number_limit = 30

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Expr
 = Plus Float Float
 | Minus Float Float
 | Times Int Int
 | Div Int Int
 | MulFrac Int Int
 | FracMul Int Int
 | Exp Int Int
 | Log Int Int
 | Sqrt Int
 | PlusSqrt Int Int

eval : Expr -> Float
eval expr = toFloat (round ((eval1 expr) * 100.0)) / 100.0
eval1 expr = case expr of
    Plus a b -> a + b
    Minus a b -> a - b
    Times a b -> toFloat (a * b)
    Div a b -> toFloat (a // b)
    MulFrac a b -> toFloat (a // b)
    FracMul a b -> toFloat (a // b)
    Exp a b -> toFloat (a ^ b)
    Log 1 1 -> 0.0
    Log a b -> logBase (toFloat a) (toFloat b)
    Sqrt a -> sqrt (toFloat a)
    PlusSqrt a b -> sqrt (toFloat a) + sqrt (toFloat b)

reprEval : Expr -> String
reprEval e = case e of
    PlusSqrt a b -> repr e ++ " = sqrt(" ++ String.fromInt a ++ ") + sqrt(" ++ String.fromInt b ++ ")"
    _ -> repr e ++ " = " ++ String.fromFloat (eval e)

repr : Expr -> String
repr expr = case expr of
    Plus a b -> if isInfinite a || isInfinite b then "+∞ + +∞ = +∞" else String.fromFloat a ++ " + " ++ String.fromFloat b
    Minus a b -> String.fromFloat a ++ " - " ++ String.fromFloat b
    Times a b -> String.fromInt a ++ " × " ++ String.fromInt b
    Div a b -> String.fromInt a ++ " ÷ " ++ String.fromInt b
    MulFrac a b -> String.fromInt a ++ " ÷ " ++ String.fromInt b
    FracMul a b -> String.fromInt a ++ " ÷ " ++ String.fromInt b
    Exp a b -> String.fromInt a ++ " ^ " ++ String.fromInt b
    Log a b -> "log(" ++ String.fromInt a ++ ", " ++ String.fromInt b ++ ")"
    Sqrt a -> "sqrt(" ++ String.fromInt a ++ ")"
    PlusSqrt a b -> "sqrt(" ++ String.fromInt (a + b) ++ " + " ++ "sqrt(" ++ String.fromInt (4 * a * b) ++ "))"

fracExpr pre a b post = table [style "font-size" "72px", style "width" "20%", style "margin-left" "auto", style "margin-right" "auto"] [
        tr [] [td [rowspan 2] pre, td [style "border-bottom" "solid 1px"] [text (String.fromInt a)], td [rowspan 2] post]
        ,tr [] [td [] [text (String.fromInt b)]]
    ]

textExpr expr = case expr of
    Log a b -> div [style "font-size" "96px", style "text-align" "center"] 
                   [text ("log "), sub [style "font-size" "72px"] [text (String.fromInt a)], text (" " ++ String.fromInt b)]
    Exp a b -> div [style "font-size" "96px", style "text-align" "center"]
                   [text (String.fromInt a), sup [style "font-size" "72px"] [text (String.fromInt b)]]
    Sqrt a -> div [style "font-size" "96px", style "text-align" "center"]
                  [span [style "font-size" "72px"] [text "√"], span [style "text-decoration" "overline"] [text (String.fromInt a)]]
    FracMul a b -> div [style "font-size" "96px", style "text-align" "center"]
                   [fracExpr [] 1 b [text (" × " ++ String.fromInt a)]]
    MulFrac a b -> div [style "font-size" "96px", style "text-align" "center"]
                   [fracExpr [text (String.fromInt a ++ " × ")] 1 b []]
    _ -> div [style "font-size" "96px", style "text-align" "center"] [text (repr expr)]

toExpr : Int -> Int -> Int -> Expr
toExpr op1 op2 op =
  case op of
      0 -> Plus (toFloat op1) (toFloat op2)
      1 -> Minus (toFloat op1) (toFloat op2)
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
      LChinese -> "百以下加减乘除（含分数）"
  4 -> case lang of
      LEngish -> "Plus/minus/time/divsion under 100 and exponentation under 10."
      LChinese -> "百以下加减乘除和十以下指数"
  5 -> case lang of
      LEngish -> "Plus/minus/time/divsion under 100, exponentation under 10 and square root under 20."
      LChinese -> "百以下加减乘除、十以下指数对数和二十以下平方根"
  6 -> case lang of
      LEngish -> "Experimental"
      LChinese -> "表示成sqrt(a) + sqrt(b)"
  _ -> case lang of
      LEngish -> "Plus/minus/time under 1000."
      LChinese -> "千以下加减乘"

genLevel2 : Random.Generator Expr
genLevel2 = filterNegative (Random.map3 toExpr (Random.int 0 99) (Random.int 0 99) (Random.int 0 1))

toFloatExpr : Random.Generator Expr -> Random.Generator Expr
toFloatExpr exprs =
  let trans e f = case e of
        Plus a b -> Plus (a / f) (b / f)
        Minus a b -> Minus (a / f) (b / f)
        _ -> e in
  Random.map2 trans exprs (Random.uniform 1.0 [10.0, 100.0])

genLevel7 : Random.Generator Expr
genLevel7 =
  Random.int 0 1 |> Random.andThen
        (\i -> case i of
             0 -> filterNegative (Random.map3 toExpr (Random.int 0 999) (Random.int 0 999) (Random.int 0 1))
             _ -> filterTooLarge (Random.map3 toExpr (Random.int 10 99) (Random.int 10 99) (Random.int 2 2))
        )

genLevel3 : Random.Generator Expr
genLevel3 =
  Random.int 0 3 |> Random.andThen
      (\i -> case i of
           0 -> Random.map2 (\a b -> Div (a * b) a) (Random.int 1 9) (Random.int 0 9)
           1 -> Random.map3 toExpr (Random.int 0 9) (Random.int 0 9) (Random.constant 2)
           2 -> Random.int 0 1 |> Random.andThen (\j -> case j of
                 0 -> Random.map2 (\a b -> MulFrac (a * b) a) (Random.int 1 9) (Random.int 0 9)
                 _ -> Random.map2 (\a b -> FracMul (a * b) a) (Random.int 1 9) (Random.int 0 9)
                 )
           _ -> genLevel2)
  |> toFloatExpr

genLevel4 = 
  Random.int 0 3 |> Random.andThen
      (\i -> case i of
           0 -> filterTooLarge (Random.map2 Exp (Random.int 0 9) (Random.int 0 9))
           _ -> genLevel3)

genLogPair =
  Random.pair (Random.int 1 9) (Random.int 0 9) |> Random.andThen
      (\(a, b) -> if a ^ b > 1000 then genLogPair else Random.constant (a, b))

genLevel5 =
  Random.int 0 5 |> Random.andThen
      (\i -> case i of
           0 -> filterTooLarge (Random.map2 Exp (Random.int 0 9) (Random.int 0 9))
           1 -> Random.map (\(a, b) -> Log a (a ^ b)) genLogPair 
           2 -> Random.map (\a -> Sqrt (a * a)) (Random.int 1 19)
           _ -> genLevel3)

rand : Int -> Random.Generator Expr
rand level =
    case level of
      1 -> filterNegative (Random.map3 toExpr (Random.int 0 9) (Random.int 0 9) (Random.int 0 1))
      2 -> genLevel2
      3 -> genLevel3
      4 -> genLevel4
      5 -> genLevel5
      6 -> Random.map2 PlusSqrt (Random.int 0 9) (Random.int 0 9)
      _ -> genLevel7

type alias Click = 
  { time : Time.Posix
  , expr : Expr
  }

type Language =
  LEngish
  | LChinese

type alias Model =
  { dieFace : Expr
  , input : String
  , time : Time.Posix
  , clicks : List Click
  , level : Int
  , lang : Language
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {dieFace = Plus 1 1, time = Time.millisToPosix 0, clicks = [], level = defaultLevel, lang = LChinese, input = "　"}
  , Cmd.none
  )

worst : List Click -> List (Html msg)
worst clicks =
  if List.length clicks < 2 then [] else
      let except_last = List.take (List.length clicks - 1) clicks in
      let diffs = List.map2 (\a b -> (a.expr, Time.posixToMillis a.time - Time.posixToMillis b.time)) except_last (List.drop 1 clicks) in
      let top = List.take 3 (List.reverse (List.sortBy (\(a,b) -> b) diffs)) in
      (List.map (\(a, b) -> text (String.fromInt (round (toFloat b / 100.0)) ++ ", " ++ reprEval a)) top)

stat : List Click -> (Int, Float)
stat clicks =
    if List.length clicks < 2 then (0, 0) else
        let cs = List.map (\h -> Time.posixToMillis h.time) clicks in
        let delta = (Maybe.withDefault 0 (List.head cs)) - (Maybe.withDefault 0 (List.head (List.drop (List.length cs - 1) cs))) in
        let precision = 10.0 in 
        (List.length cs, toFloat (round (precision * 60000.0 / (toFloat delta) * (toFloat (List.length cs - 1)))) / precision)

-- UPDATE


type Msg
  = Roll
  | NewFace Expr
  | Tick Time.Posix
  | Change Int
  | Input String
  | HandleKeyboardEvent KeyboardEvent

merge_input c str_in =
  let str = if str_in == "　" then "" else str_in in
  case c of
    "+/-" -> if String.startsWith "-" str then String.dropLeft 1 str else "-" ++ str
    "." -> if String.contains "." str then str else str ++ c
    "AC" -> "　"
    _ -> str ++ c

is_match str val =
  case String.toFloat str of
    Nothing -> False
    Just val2 -> val2 == val

proc_input c model =
  let input = merge_input c model.input in
  if is_match input (eval model.dieFace) then (model, Random.generate (\_-> Roll) (rand 1)) else ( { model | input = input}, Cmd.none)

keys = [".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", "c"]
translate_key c =
  case c of
    "-" -> "+/-"
    "c" -> "AC"
    _ -> c

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HandleKeyboardEvent event ->
        case event.key of
            Just c -> if List.member c keys then (model, Random.generate (\_-> Input (translate_key c)) (rand 1)) else (model, Cmd.none)
            Nothing -> (model, Cmd.none)
    Input c -> if List.length model.clicks < number_limit then proc_input c model
        else ({model | input = "　", dieFace = Plus (1/0) (1/0)}, Cmd.none)
    Roll ->
      ( { model | input = "　", clicks = {time = model.time, expr = model.dieFace} :: model.clicks}
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
  LEngish -> "Total: " ++ String.fromInt total ++ ", per minute: " ++ String.fromFloat qps
  LChinese -> "共：" ++ String.fromInt total ++ "，每分钟：" ++ String.fromFloat qps

strLevel lang = case lang of
  LEngish -> "Level"
  LChinese -> "关卡"

strWorst lang = case lang of
  LEngish -> "Time, Expr"
  LChinese -> "时间，式子"

buttonN = button [ onClick Roll, style "font-size" "32px"] [text "Next"]

levelButtons curLevel = List.map 
  (\l -> button ([onClick (Change l), style "font-size" "32px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (String.fromInt l)]) 
  [1, 2, 3, 4, 5, 6, 7]

inputButtons = List.map
  (\l -> button ([onClick (Input l), style "font-size" "32px"]) [text l]) 
  (["AC", ".", "+/-"] ++ List.map String.fromInt (List.range 0 9))

view : Model -> Html Msg
view model =
  div [tabindex 0, on "keydown" <| Json.map HandleKeyboardEvent decodeKeyboardEvent] [ div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text (strLevel model.lang ++ " " ++ String.fromInt model.level ++ ": " ++ levelDescription model.lang model.level)]
    --, div [style "font-size" "32px"] [text "　"]
    --, div [style "text-align" "center"] [buttonN]
    , div [style "text-align" "center", style "font-size" "32px"] [text model.input]
    , div [style "text-align" "center"] inputButtons
    , textExpr model.dieFace
    , div [style "font-size" "32px"] [ text (strStat model.lang (stat model.clicks))]
    , div [style "font-size" "24px"] [text (strWorst model.lang)]
    ] ++ (List.map (\x -> div [style "font-size" "32px"] [x]) (worst model.clicks)) ++ [
      div [style "text-align" "right"] 
          [a [style "font-size" "24px", href "https://github.com/zsc/elm/tree/master/elm_0.19"] [text "GitHub"]]]
    )]
