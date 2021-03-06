import Html exposing (Attribute, div, text, input, img, br, button, a)
import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Json.Decode exposing (string, map)
import String

type alias Model = { year : Int, url : (String, String)}

type Msg
    = Update String
    
tsinghuaUrl = ("http://www.tsinghua.edu.cn/publish/newthuen/images/logo.png", "year/56238459.jpg")
googleUrl = ("year/googlelogo_color_272x92dp.png", "year/IMG_20131126_192441.jpg")
megviiUrl = ("https://avatars2.githubusercontent.com/u/11012279?s=200&v=4", "year/IMG_4032.jpg")
casUrl = ("https://upload.wikimedia.org/wikipedia/en/thumb/b/bd/CAS_logo_2.png/180px-CAS_logo_2.png", "year/DSC_3550.jpg")

getUrl year = if 2014 < year then megviiUrl else if 2010 < year then googleUrl else if 2004 < year then casUrl else tsinghuaUrl

update : Msg -> Model -> Model
update (Update v) model =
  let year = String.toInt v |> Result.withDefault 2016
  in { year = year , url = getUrl year}

view model =
  let style_ x = if model.year == x then style [("fontWeight", "bold")] else style []
  in
  div []
    ([
    a [href "https://github.com/zsc/elm"] [img [style [("position", "absolute"), ("top", "0"), ("left", "0"), ("border", "0")], src "https://camo.githubusercontent.com/121cd7cbdc3e4855075ea8b558508b91ac463ac2/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f6c6566745f677265656e5f3030373230302e706e67"] []]
    , img [src (Tuple.second model.url), height 400] []
    --, br [] []
    , img [src (Tuple.first model.url), width 200] []
    , br [] []
--    , input
--      [ type_ "range"
--      , H.min "2000"
--      , H.max "2018"
--      , value <| toString model.year
--      , onInput Update
--      ] []
    --, text <| toString model.year
    , br [] []
    ] ++ List.map (\x -> button [onClick (Update (toString x)), style_ x] [text (toString x)]) [2000, 2007, 2011, 2016])

main =
  Html.beginnerProgram
    { model = { year = 2016, url = megviiUrl}
    , view = view
    , update = update
    }
