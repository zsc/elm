import Array exposing ( Array )
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value )
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

type alias Matrix = Array (Array Int)

type alias Model =
  { matA : Matrix
  , matB : Matrix
  , level : Int
  }

getVal : Int -> Int -> Matrix -> Int
getVal row col mat = Array.get col (Array.get row mat |> fromJust) |> fromJust

fromJust : Maybe a -> a
fromJust m_a = case m_a of
  Just v -> v
  Nothing -> Debug.todo "fromJust"

setVal : Int -> Int -> Matrix -> Int -> Matrix
setVal row col mat val = Array.set row (Array.set col val (Array.get row mat |> fromJust)) mat

zipWith : (a -> b -> c) -> Array a -> Array b -> Array c
zipWith op arrA arrB =
  let len = min (Array.length arrA) (Array.length arrB) in
  Array.initialize len (\i -> op (fromJust (Array.get i arrA)) (fromJust (Array.get i arrB)))

vecMatMul : Array Int -> Matrix -> Array Int
vecMatMul vec mat = Array.map (\a -> Array.foldl (+) 0 (zipWith (*) vec a)) mat

matLift : (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
matLift op matA matB = zipWith (zipWith op) matA matB

numCols : Matrix -> Int
numCols mat = Array.length (fromJust (Array.get 0 mat))

matMul : Matrix -> Matrix -> Matrix
matMul matA matB = 
  Array.initialize (Array.length matA) (\i ->
    Array.initialize (numCols matB) (\k ->
      Array.foldl (+) 0 (Array.initialize (Array.length matB) (\j -> (getVal i j matA) * (getVal j k matB)))))

rowRepr : Array Int -> String
rowRepr row = Array.foldl (\i s -> s ++ String.fromInt i ++ " ") "" row

repr : Matrix -> List (Html msg)
repr mat = List.map (\i -> div [style "font-size" "24px"] [text (if i == 0 then "⎡" else "⎣")
                                 , text (rowRepr (fromJust (Array.get i mat)))
                                 , text (if i == 0 then "⎤" else "⎦")]) (List.range 0 (Array.length mat - 1))

init : () -> (Model, Cmd Msg)
init _ =
  ( { matA = Array.fromList (List.map Array.fromList [[0, 1], [2, 3]])
    , matB = Array.fromList (List.map Array.fromList [[0, 1], [2, 3]])
    , level = 1
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | ChangeLevel Int
  | ChangeA Int Int String
  | ChangeB Int Int String
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    ChangeA row col value ->
      ( {model | matA = setVal row col model.matA (Maybe.withDefault (getVal row col model.matA) (String.toInt value))}
      , Cmd.none
      )
    ChangeB row col value ->
      ( {model | matB = setVal row col model.matB (Maybe.withDefault (getVal row col model.matB) (String.toInt value))}
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

levelDescription level = case level of
  1 -> "矩阵加"
  2 -> "矩阵减"
  3 -> "矩阵乘"
  _ -> Debug.todo "Unknown level"

levelOperatorRepr level = case level of
  1 -> text " + "
  2 -> text " - "
  3 -> text " × "
  _ -> Debug.todo "Unknown level"

levelOperator level = case level of
  1 -> matLift (+)
  2 -> matLift (-)
  3 -> matMul
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "32px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (String.fromInt l)])
  [1, 2, 3]

numButton op row cols mat =
    List.concat (List.map (
        \col -> [input [ placeholder "Enter a number", value (String.fromInt (getVal row col mat))
                       , onInput (op row col)] [], text " "]) (List.range 0 (cols - 1)))

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text ("实验 " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "font-size" "32px"] ([text "⎡"] ++ numButton ChangeA 0 2 model.matA ++ [text "⎤"])
    , div [style "font-size" "32px"] ([text "⎣"] ++ numButton ChangeA 1 2 model.matA ++ [text "⎦"])
    , div [style "font-size" "32px"] [levelOperatorRepr model.level]
    , div [style "font-size" "32px"] ([text "⎡"] ++ numButton ChangeB 0 2 model.matB ++ [text "⎤"])
    , div [style "font-size" "32px"] ([text "⎣"] ++ numButton ChangeB 1 2 model.matB ++ [text "⎦"])
    , div [style "font-size" "32px"] [text " = "]
    ] ++ repr ((levelOperator model.level) model.matA model.matB))
