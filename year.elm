import Html exposing (Attribute, div, text, input, img, br)
import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode exposing (string, map)
import String

type alias Model = { year : Int, url : (String, String)}

type Msg
    = Update String
    
tsinghuaUrl = ("http://www.tsinghua.edu.cn/publish/newthuen/images/logo.png", "year/56238459.jpg")
googleUrl = ("year/googlelogo_color_272x92dp.png", "year/IMG_20131126_192441.jpg")
megviiUrl = ("https://avatars2.githubusercontent.com/u/11012279?s=200&v=4", "year/IMG_4032.jpg")
casUrl = ("https://upload.wikimedia.org/wikipedia/en/thumb/b/bd/CAS_logo_2.png/180px-CAS_logo_2.png", "year/DSC_3550.jpg")

getUrl year = if 2014 < year then megviiUrl else if 2011 < year then googleUrl else if 2004 < year then casUrl else tsinghuaUrl

update : Msg -> Model -> Model
update (Update v) model =
  let year = String.toInt v |> Result.withDefault 2018
  in { year = year , url = getUrl year}

view model =
  div []
    [img [src (Tuple.first model.url), width 200] []
    , br [] []
    , img [src (Tuple.second model.url), height 400] []
    , br [] []
    , input
      [ type_ "range"
      , H.min "2000"
      , H.max "2018"
      , value <| toString model.year
      , onInput Update
      ] []
    , text <| toString model.year
    ]

main =
  Html.beginnerProgram
    { model = { year = 2018, url = megviiUrl}
    , view = view
    , update = update
    }
