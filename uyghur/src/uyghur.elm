import Array exposing ( Array )
import Dict exposing (Dict)
import Tuple exposing (first)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( style, placeholder, src, width, href, value, height )
import List
import Maybe
import Random
import Time


-- MAIN

fromJust x = case x of
  Just v -> v
  Nothing -> Debug.todo "impossible"

qa_dict = Dict.fromList qa_list
qa_list = 
  [ ("ۋە‎", "we")
  , ("بۇ‎", "bu")
  , ("بىلەن‎", "bilen")
  , ("بىر‎", "bir")
  , ("بولۇپ‎", "bolup")
  , ("قىلىش‎", "qilish")
  , ("بولىدۇ‎", "bolidu")
  , ("قىلىپ‎", "qilip")
  , ("بولغان‎", "bolghan")
  , ("ئۈچۈن‎", "üchün")
  , ("مۇنداق‎", "mundaq")
  , ("ئۇ‎", "u")
  , ("بويىچە‎", "boyiche")
  , ("يەنە‎", "yene")
  , ("قاتارلىق‎", "qatarliq")
  , ("كېيىن‎", "këyin")
  , ("كېرەك‎", "kërek")
  , ("دېدى‎", "dëdi")
  , ("ئارقىلىق‎", "arqiliq")
  , ("دەپ‎", "dep")
  , ("ئەمدى‎", "emdi")
  , ("يۇقىرى‎", "yuqiri")
  , ("قىلغان‎", "qilghan")
  , ("قىلىدۇ‎", "qilidu")
  , ("مۇشۇ‎", "mushu")
  , ("قىلدى‎", "qildi")
  , ("چوڭ‎", "chong")
  , ("مەن‎", "men")
  , ("ئۇنىڭ‎", "uning")
  , ("دېگەن‎", "dëgen")
  , ("ئېلىپ‎", "ëlip")
  , ("بېرىش‎", "bërish")
  , ("ياكى‎", "yaki")
  , ("شۇ‎", "shu")
  , ("بولسا‎", "bolsa")
  , ("يېقىندا‎", "yëqinda")
  , ("سۇ‎", "su")
  , ("بۇيان‎", "buyan")
  , ("ھەمدە‎", "hemde")
  , ("باشقا‎", "bashqa")
  , ("بولسۇن‎", "bolsun")
  , ("بولدى‎", "boldi")
  , ("ئىچىدە‎", "ichide")
  , ("تېخىمۇ‎", "tëximu")
  , ("ھەم‎", "hem")
  , ("بولۇش‎", "bolush")
  , ("قارىغاندا‎", "qarighanda")
  , ("شۇڭا‎", "shunga")
  , ("قايسى‎", "qaysi")
  , ("سىز‎", "siz")
  , ("نۆۋەتتە‎", "nöwette")
  , ("باشلاپ‎", "bashlap")
  , ("كەلگەن‎", "kelgen")
  , ("ئىكەن‎", "iken")
  , ("ئەمما‎", "emma")
  , ("ئۇلارنىڭ‎", "ularning")
  , ("نۇرغۇن‎", "nurghun")
  , ("ئۇلار‎", "ular")
  , ("ھازىر‎", "hazir")
  , ("يېتىپ‎", "yëtip")
  , ("نەچچە‎", "nechche")
  , ("شۇنداقلا‎", "shundaqla")
  , ("قىلىنغان‎", "qilinghan")
  , ("ئۇنى‎", "uni")
  , ("ئوخشاش‎", "oxshash")
  , ("توغرىسىدا‎", "toghrisida")
  , ("ئەمەس‎", "emes")
  , ("ھالدا‎", "halda")
  , ("ئىدى‎", "idi")
  , ("قالغان‎", "qalghan")
  , ("بۇنىڭدىن‎", "buningdin")
  , ("شۇنداق‎", "shundaq")
  , ("ناھايىتى‎", "nahayiti")
  , ("لېكىن‎", "lëkin")
  , ("ئەگەر‎", "eger")
  , ("بەردى‎", "berdi")
  , ("بۇنداق‎", "bundaq")
  , ("يوق‎", "yoq")
  , ("ئۆزىنىڭ‎", "özining")
  , ("كىشىلەر‎", "kishiler")
  , ("بولسىمۇ‎", "bolsimu")
  , ("كېلىپ‎", "këlip")
  , ("داۋاملىق‎", "dawamliq")
  , ("بەرگەن‎", "bergen")
  , ("ۋاقىتتا‎", "waqitta")
  , ("ئارتۇق‎", "artuq")
  , ("مۇمكىن‎", "mumkin")
  , ("تۇرۇپ‎", "turup")
  , ("قاراپ‎", "qarap")
  , ("يەردە‎", "yerde")
  , ("چىقىپ‎", "chiqip")
  , ("تولۇق‎", "toluq")
  , ("تۇرسۇن‎", "tursun")
  , ("يەنى‎", "yeni")
  , ("ئالدى‎", "aldi")
  , ("قىلىمىز‎", "qilimiz")
  , ("ئىبارەت‎", "ibaret")]

getAnswer x = case Dict.get x qa_dict of
    Just v -> v
    Nothing -> "　"

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Model =
  { question : String
  , answer : String
  , level : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { question = "ۋە‎"
    , answer = "　"
    , level = 1
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | Show
  | ChangeLevel Int
  | NewFace (String, String)
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( { model | answer = "　" }
      , Random.generate NewFace (Random.uniform (fromJust (List.head qa_list)) (List.drop 1 qa_list))
      )
    NewFace newFace ->
      ( {model | question = first newFace}, Cmd.none)
    ChangeLevel lv ->
      ( {model | level = lv}
      , Cmd.none
      )
    _ -> 
      ( { model | answer = getAnswer model.question}
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 3000 Tick


-- VIEW

levelDescription level = case level of
  1 -> "Top 100"
  _ -> Debug.todo "Unknown level"

levelButtons curLevel = List.map
  (\l -> button ([onClick (ChangeLevel l), style "font-size" "32px"] ++ (if curLevel == l then [style "font-weight" "bold"] else []))
                [text (String.fromInt l)])
  [1]

view : Model -> Html Msg
view model =
  div []
    ([ div [style "text-align" "center"] [a [style "font-size" "24px", href "https://zsc.github.io/637913017.jpg"] [text "打赏"]]
    , div [style "font-size" "32px"] [text "　"]
    , div [style "text-align" "center"] (levelButtons model.level)
    , div [style "font-size" "32px", style "text-align" "center"]
          [ text ("关卡 " ++ String.fromInt model.level ++ ": " ++ levelDescription model.level)]
    , div [style "text-align" "center", style "font-size" "32px"] [text model.question]
    , div [style "text-align" "center", style "font-size" "32px"] [text model.answer]
    , div [style "text-align" "center"] [button [ onClick Roll , style "font-size" "32px"] [ text "Next" ]]
    --, button [ onClick Show ] [ text "Answer" ]
    ])
