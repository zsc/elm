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

type alias Matrix =
  { data : List Int
  , rows : Int
  , cols : Int
  }

type alias Model =
  { matA : Matrix
  , matB : Matrix
  }

nth : Int -> List Int -> Int
nth n lst =
  Maybe.withDefault -1 (List.head (List.drop n lst))

getVal : Int -> Int -> Matrix -> Int
getVal row col mat = nth (row * mat.cols + col) mat.data

setVal : Int -> Int -> Matrix -> Int -> Matrix
setVal row col mat val =
  {mat | data = List.map2
       (\v i -> if i == row * mat.cols + col then val else v) mat.data (List.range 0 (mat.cols * mat.rows - 1))}

vecMatMul : List Int -> Matrix -> List Int
vecMatMul vec mat =
    List.map (\k -> List.sum (List.map (\j -> (nth j vec) * (getVal j k) mat) (List.range 0 (mat.rows - 1)))) (List.range 0 (mat.cols - 1))

matmul : Matrix -> Matrix -> Matrix
matmul matA matB =
    { data = List.concat (List.map (\i -> vecMatMul (getRow i matA) matB) (List.range 0 (matA.rows - 1)))
    , rows = matA.rows
    , cols = matB.cols
    }

getRow : Int -> Matrix -> List Int
getRow i mat =
  List.drop (mat.cols * i) (List.take (mat.cols * (i + 1)) mat.data)

rowRepr : List Int -> String
rowRepr row =
  List.foldl (\i s -> s ++ String.fromInt i ++ " ") "" row

repr : Matrix -> List (Html msg)
repr mat = List.map (\i -> div [style "font-size" "24px"] [text (if i == 0 then "⎡" else "⎣")
                                 , text (rowRepr (getRow i mat))
                                 , text (if i == 0 then "⎤" else "⎦")]) (List.range 0 (mat.rows - 1))

init : () -> (Model, Cmd Msg)
init _ =
  ( { matA = {data = List.range 0 3 , rows = 2 , cols = 2 }
    , matB = {data = List.range 0 3 , rows = 2 , cols = 2 }}
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | ChangeA Int Int String
  | ChangeB Int Int String
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeA row col value ->
      ( {model | matA = setVal row col model.matA (Maybe.withDefault -1 (String.toInt value))}
      , Cmd.none
      )
    ChangeB row col value ->
      ( {model | matB = setVal row col model.matB (Maybe.withDefault -1 (String.toInt value))}
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

numButton op row cols mat =
    List.concat (List.map (
        \col -> [input [ placeholder "Enter a number", value (String.fromInt (getVal row col mat))
                       , onInput (op row col)] [], text " "]) (List.range 0 (cols - 1)))

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "font-size" "32px"] ([text "⎡"] ++ numButton ChangeA 0 2 model.matA ++ [text "⎤"])
    , div [style "font-size" "32px"] ([text "⎣"] ++ numButton ChangeA 1 2 model.matA ++ [text "⎦"])
    , div [style "font-size" "32px"] [text " × "]
    , div [style "font-size" "32px"] ([text "⎡"] ++ numButton ChangeB 0 2 model.matB ++ [text "⎤"])
    , div [style "font-size" "32px"] ([text "⎣"] ++ numButton ChangeB 1 2 model.matB ++ [text "⎦"])
    , div [style "font-size" "32px"] [text " = "]
    ] ++ repr (matmul model.matA model.matB))
