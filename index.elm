module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Random
import Time exposing (Time, second, millisecond)

-- Model

type alias FileList = List String

type State = Resetting | Playing
type alias Model = { position : Int
                   , state : State
                   , fileList : FileList
                   , delay : Float
                   }
type ShowStatus = Vanish | Show | Other

choiceNumber : Int -> Cmd Msg
choiceNumber n = Random.generate Select (Random.int 0 (n - 1))

init : ( Model, Cmd Msg )
init =
    let
        initial =
            { position = 1
            , state = Playing
            , fileList = ["./img/01.jpg", "./img/02.jpg", "./img/03.jpg"]
            , delay = 4
            }
    in
        ( initial, choiceNumber (List.length initial.fileList) )


-- Message

type Msg
    = Select Int
    | Tick Time
    | Start

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [ css [ position relative
                    , Css.width (px 300)
                    , Css.height (px 420)
                    ]
              ] (imageList model)
        , progress model
        , div [] (radioList model)
        ]
        
progress : Model -> Html Msg
progress model =  
    div [ style [("transition", (if model.state == Resetting then "0" else toString model.delay)++"s linear")]
        , css [ backgroundImage
                    (linearGradient2
                         toRight
                         (stop2 (rgba 0 0 0 0) <| pct 50)
                         (stop2 (rgba 255 30 70 100) <| pct 50) [])
              , Css.height (px 3)
              , Css.width (px 300)
              , backgroundSize2 (pct 200) auto
              , if model.state == Resetting
                then backgroundPosition2 zero zero
                else backgroundPosition2 (pct -100) zero
              ]
        ][]


showStatus : Int -> Int -> Int -> ShowStatus
showStatus n show all =
    if show == n
    then Show
    else if (show - n + all) % all == 1
         then Vanish
         else Other

imageList : Model -> List (Html Msg)
imageList model =
    let
        image (n, name) =
            let
                state = showStatus n model.position (List.length model.fileList)
            in
                div
                [ id ("image" ++ toString n)
                , style [("transition", "0.5s")]
                , css [ backgroundImage (url name)
                      , backgroundSize cover
                      , Css.width (px 300)
                      , Css.height (px 400)
                      , position absolute
                      , left zero
                      , top zero
                      , zIndex (if state == Show
                                then int 10
                                else if state == Vanish
                                     then int 20
                                     else int 0)
                      , opacity (if state == Show
                                 then int 100
                                 else int 0)
                      ]
                ] []
    in
        List.map image (List.indexedMap (,) model.fileList)
        
radioList : Model -> List (Html Msg)
radioList model =
    let
        radio n = input [ type_ "radio"
                        , name "position"
                        , Html.Styled.Attributes.checked (n == model.position)
                        , onClick ( Select n ) ] []
    in
       List.map radio ( List.range 0 ( (List.length model.fileList) - 1 ) )
        
-- Update

         
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            { model | position = ( model.position + 1 ) % (List.length model.fileList)
            , state = Resetting } ! []
        Select n ->
            { model | position = n , state = Resetting } ! []
        Start ->
            { model | state = Playing } ! []

-- Subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing ->
            Time.every ( second * model.delay ) Tick
        Resetting ->
            Time.every ( millisecond * 100 ) (\_ -> Start)

-- MAIN

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
