module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import List
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace : Int
    , circles : List (Svg Msg)
    , generator : CircleGenerator
    }


init : ( Model, Cmd Msg )
init =
    ( Model 2 [] defaultCircleGenerator, Cmd.none )



-- UPDATE


type Msg
    = Roll
    | Tick Time
    | NewFace Int
    | GenCircle CircleGenerator
    | NewCircle Circle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        Tick time ->
            update (GenCircle model.generator) model

        GenCircle generator ->
            ( model, Random.generate NewCircle generator )

        NewCircle circle ->
            let
                newCircles =
                    circleToSvg circle :: model.circles
            in
            ( { model | circles = newCircles }, Cmd.none )

        NewFace newFace ->
            ( { model | dieFace = newFace }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "500"
            , height "500"
            , viewBox "0 0 500 500"
            , fill "white"
            , stroke "black"
            , strokeWidth "3"
            , Html.Attributes.style [ ( "padding-left", "20px" ), ( "padding-top", "20px" ) ]
            ]
            (List.append [ rect [ x "1", y "1", width "400", height "400" ] [] ] model.circles)
        , button [ onClick (GenCircle defaultCircleGenerator) ] [ Html.text "Roll" ]
        ]


type alias Circle =
    { x : Float
    , y : Float
    , r : Float
    }


type alias CircleGenerator =
    Random.Generator Circle


circleGenerator : Float -> Float -> Float -> Float -> Float -> Float -> Random.Generator Circle
circleGenerator minX maxX minY maxY minR maxR =
    Random.map3 Circle (Random.float minX maxX) (Random.float minY maxY) (Random.float minR maxR)


defaultCircleGenerator =
    circleGenerator 21 379 21 379 2 20


circleToSvg : Circle -> Svg Msg
circleToSvg circ =
    circle [ cx (toString circ.x), cy (toString circ.y), r (toString circ.r), fill "black", fillOpacity "0.4" ] []
