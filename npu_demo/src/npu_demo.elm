import Array exposing ( Array )
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value )
import List
import Maybe
import Random
import Time
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Color
import TypedSvg exposing (circle, g, line, svg, title)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Fill(..))


--
netGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Data"
        , "Conv1"
        ]
        [ (1, 0)
        ]

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL
type Location = 
    Loc_Ddr
  | Loc_Ocm

type alias Chunk =
  { addr : Int
  , num_col : Int
  , num_row : Int
  , num_page : Int
  , row_stride : Int
  , page_stride : Int
  , location : Location
  }

type Net =
    Net_Chunk Chunk
  | Net_UnPool Net
  | Net_Conv ConvParam (List Net)

type alias ConvParam = 
  { oc : Int
  , of_bit : Int
  , stride : Int
  }

type Instruction =
    Ins_Conv ConvParam
  | Ins_Dma Location Location Chunk -- src * dst

type alias Ddr =
  { burst_length : Int
  , num_bit : Int
  }

type alias Model =
  { ddr : Ddr
  , level : Int
  , graph : Graph String ()
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { ddr = {burst_length = 16 * 64, num_bit = 64}
    , level = 1
    , graph = netGraph
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | ChangeLevel Int
  | ChangeDdr Ddr
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    ChangeDdr newDdr ->
      ( {model | ddr = newDdr}
      , Cmd.none
      )
    _ -> Debug.todo ""

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Tick


-- VIEW

levelDescription level = case level of
  1 -> "Eval efficiency"
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "32px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (String.fromInt l)])
  [1]

view : Model -> Html Msg
view model =
  div []
    ([ div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px"] [text "　"]
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text ("Experiment " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "font-size" "32px"] [text ("DDR: " ++ Debug.toString model.ddr)]
    , div [] [text (Debug.toString netGraph)]
    ])
