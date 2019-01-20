import Array exposing ( Array )
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value, height )
import List
import Maybe
import Random
import Time


-- MAIN

syllable x = case x of
  "C4" -> "do" 
  "D4" -> "re" 
  "E4" -> "mi" 
  "F4" -> "fa" 
  "G4" -> "sol" 
  "A4" -> "la" 
  "B4" -> "si" 
  _ -> ""

fnames = Array.fromList [ "PrimNote11A3-midi.pdf.svg.png" , "PrimNote11A4-midi.pdf.svg.png" , "PrimNote11B3-midi.pdf.svg.png" , "PrimNote11B4-midi.pdf.svg.png" , "PrimNote11C3-midi.pdf.svg.png" , "PrimNote11C4-midi.pdf.svg.png" , "PrimNote11D3-midi.pdf.svg.png" , "PrimNote11D4-midi.pdf.svg.png" , "PrimNote11E3-midi.pdf.svg.png" , "PrimNote11E4-midi.pdf.svg.png" , "PrimNote11F3-midi.pdf.svg.png" , "PrimNote11F4-midi.pdf.svg.png" , "PrimNote11G3-midi.pdf.svg.png" , "PrimNote11G4-midi.pdf.svg.png" , "PrimNote12A3-midi.pdf.svg.png" , "PrimNote12A4-midi.pdf.svg.png" , "PrimNote12B3-midi.pdf.svg.png" , "PrimNote12B4-midi.pdf.svg.png" , "PrimNote12C3-midi.pdf.svg.png" , "PrimNote12C4-midi.pdf.svg.png" , "PrimNote12D3-midi.pdf.svg.png" , "PrimNote12D4-midi.pdf.svg.png" , "PrimNote12E3-midi.pdf.svg.png" , "PrimNote12E4-midi.pdf.svg.png" , "PrimNote12F3-midi.pdf.svg.png" , "PrimNote12F4-midi.pdf.svg.png" , "PrimNote12G3-midi.pdf.svg.png" , "PrimNote12G4-midi.pdf.svg.png" , "PrimNote14A3-midi.pdf.svg.png" , "PrimNote14A4-midi.pdf.svg.png" , "PrimNote14B3-midi.pdf.svg.png" , "PrimNote14B4-midi.pdf.svg.png" , "PrimNote14C3-midi.pdf.svg.png" , "PrimNote14C4-midi.pdf.svg.png" , "PrimNote14D3-midi.pdf.svg.png" , "PrimNote14D4-midi.pdf.svg.png" , "PrimNote14E3-midi.pdf.svg.png" , "PrimNote14E4-midi.pdf.svg.png" , "PrimNote14F3-midi.pdf.svg.png" , "PrimNote14F4-midi.pdf.svg.png" , "PrimNote14G3-midi.pdf.svg.png" , "PrimNote14G4-midi.pdf.svg.png" , "PrimNote18A3-midi.pdf.svg.png" , "PrimNote18A4-midi.pdf.svg.png" , "PrimNote18B3-midi.pdf.svg.png" , "PrimNote18B4-midi.pdf.svg.png" , "PrimNote18C3-midi.pdf.svg.png" , "PrimNote18C4-midi.pdf.svg.png" , "PrimNote18D3-midi.pdf.svg.png" , "PrimNote18D4-midi.pdf.svg.png" , "PrimNote18E3-midi.pdf.svg.png" , "PrimNote18E4-midi.pdf.svg.png" , "PrimNote18F3-midi.pdf.svg.png" , "PrimNote18F4-midi.pdf.svg.png" , "PrimNote18G3-midi.pdf.svg.png" , "PrimNote18G4-midi.pdf.svg.png" ]

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Model =
  { dice : Int
  , answer : String
  , level : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { dice = 0
    , answer = ""
    , level = 1
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | Show
  | ChangeLevel Int
  | NewFace Int
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( { model | answer = "" }
      , Random.generate NewFace (Random.int 0 55)
      )
    NewFace newFace ->
      ( {model | dice = newFace}, Cmd.none)
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    _ -> 
      ( { model | answer = let note = String.slice 10 12 (getFileName model.dice) in note ++ " " ++ syllable note}
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 4000 Tick


-- VIEW

levelDescription level = case level of
  1 -> "简单谱"
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "32px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (String.fromInt l)])
  [1]

getFileName n = Maybe.withDefault "" (Array.get n fnames)

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text ("关卡 " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "font-size" "32px"]
          [img [src ("notes/" ++ getFileName model.dice), height 100] [], text model.answer]
    , button [ onClick Roll ] [ text "Next" ]
    --, button [ onClick Show ] [ text "Answer" ]
    ])
