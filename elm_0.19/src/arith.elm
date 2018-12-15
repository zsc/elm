import Array
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style )
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

qps : List Time.Posix -> Int
qps clicks =
    if List.length clicks < 2 then 0 else
        let cs = List.map Time.posixToMillis clicks in
        let delta = (Maybe.withDefault 0 (List.head cs)) - (Maybe.withDefault 0 (List.head (List.drop (List.length cs - 1) cs))) in
        round (60000.0 / (toFloat delta) * (toFloat (List.length cs - 1)))

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

func : Int -> Int -> Int -> Expr
func op1 op2 op =
    if op == 0 then Plus op1 op2 else Minus op1 op2

rand : Random.Generator Expr
rand =
    Random.map3 func (Random.int 0 9) (Random.int 0 9) (Random.int 0 1)
        |> Random.andThen
            (\expr ->
                 if eval expr < 0 then
                     rand
                 else
                     Random.uniform expr []
            )

type alias Model =
  { dieFace : Expr
  , time : Time.Posix
  , clicks : List Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {dieFace = Plus 1 1, time = Time.millisToPosix 0, clicks = []}
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace Expr
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( { model | clicks = List.take 20 (model.time :: model.clicks)}
      , Random.generate NewFace rand
      )

    NewFace newFace ->
      ( { model | dieFace = newFace }
      , Cmd.none
      )

    Tick posix ->
      ( { model | time = posix }
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
    [ div [style "font-size" "64px", style "text-align" "center"] 
          [ text (repr model.dieFace) ]
    , div [style "text-align" "center"] [buttonN]
    , div [style "font-size" "32px"] [ text ("per minute: " ++ String.fromInt (qps model.clicks)) ]
    ]
