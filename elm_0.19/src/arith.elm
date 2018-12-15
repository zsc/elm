import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style )
import Random



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
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Plus 1 1)
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace Expr


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace rand
      )

    NewFace newFace ->
      ( Model newFace
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

buttonN = button [ onClick Roll ] [div [style "font-size" "32px", style "height" "48px"] [text "Next" ]]

view : Model -> Html Msg
view model =
  div []
    [ div [style "font-size" "64px", style "text-align" "center"] 
          [ text (repr model.dieFace) ]
    , div [style "text-align" "center"] [buttonN]
    ]
