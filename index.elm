module Main exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Random
import Time exposing (Time, second, millisecond)
import Task

-- Model

fileList : List String
fileList = ["./img/01.jpg", "./img/02.jpg", "./img/03.jpg"]

fileCounts : Int
fileCounts = List.length fileList
           
choiceFile : Cmd Msg
choiceFile = Random.generate Select (Random.int 0 (fileCounts - 1))

type State = Resetting | Playing
type alias Model = { position : Int
                   , state : State
                   }


init : ( Model, Cmd Msg )
init =
    ( {position = 1, state = Playing}, choiceFile )


-- Message


type Msg
    = Select Int
    | Tick Time
    | Start



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString model.position)
        , div [ css [ position relative
                    , Css.width (px 300)
                    , Css.height (px 430)
                    ]
              ] (imageList model)
        , div [] (radioList model)
        ]

imageList : Model -> List (Html Msg)
imageList model =
    let
        image (n, name) = div
                          [ id ("image" ++ toString n)
                          , css [ backgroundImage (url name)
                                , backgroundSize cover
                                , Css.width (px 300)
                                , Css.height (px 430)
                                , position absolute
                                , left zero
                                , top zero
                                , zIndex (if model.position == n
                                          then int 10
                                          else int 0)
                                ]
                          ] []
    in
        List.map image (List.indexedMap (,) fileList)
        
radioList : Model -> List (Html Msg)
radioList model =
    let
        radio n = input [ type_ "radio"
                        , name "position"
                        , Html.Styled.Attributes.checked (n == model.position)
                        , onClick ( Select n ) ] []
    in
       List.map radio ( List.range 0 ( fileCounts - 1 ) )
        
-- Update

         
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            { model | position = ( model.position + 1 ) % fileCounts
            , state = Playing } ! []
        Select n ->
            { model | position = n , state = Resetting } ! []
        Start ->
            { model | state = Playing } ! []

-- Subscription

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing ->
            Time.every ( second ) Tick
        Resetting ->
            Time.every ( millisecond ) (\_ -> Start)

-- MAIN

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
