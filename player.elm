import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Json.Decode exposing (string, map)
import String
import List

import Html exposing (..)
import Color exposing (..)
--import Collage exposing (..)
import Element exposing (..)
import Time exposing (Time)
import AnimationFrame

type alias Model = { progress : Float, images : List String, speed : Int}

max_progress = 100

type Msg
    = Update String
      | Increment
      | Decrement
      | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Update v ->
      let progress = String.toFloat v |> Result.withDefault 0
      in ({ progress = progress , images = [], speed = 0}, Cmd.none)
  Increment -> ({ model | speed = model.speed + 1 }, Cmd.none)
  Decrement -> ({ model | speed = model.speed - 1 }, Cmd.none)
  Tick _ -> ({ model | progress = Basics.min max_progress (model.progress + toFloat model.speed)}, Cmd.none)

view model =
  let progress = model.progress
  in
  div []
    [
    -- a [href "https://github.com/zsc/elm"] [img [style [("position", "absolute"), ("top", "0"), ("left", "0"), ("border", "0")], src "https://camo.githubusercontent.com/121cd7cbdc3e4855075ea8b558508b91ac463ac2/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f6c6566745f677265656e5f3030373230302e706e67"] []]
    br [] []
    , input
      [ type_ "range"
      , H.min "0"
      , H.max (toString max_progress)
      , value <| toString progress
      , onInput Update
      ] []
    , text <| toString progress
    , br [] []
    , button [ onClick Increment ] [ text " + " ]
    , text <| " " ++ toString model.speed ++ " "
    , button [ onClick Decrement ] [ text " - " ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    -- NOTE: The "Time.every millisecond Tick" is not as much smooth as "AnimationFrame.times Tick" is.
    AnimationFrame.times Tick

main =
  Html.program
    { init = ({ progress = 0, images = [], speed = 0 }, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
