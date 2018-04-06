module Main exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import List
import Maybe exposing (withDefault)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, millisecond, second)


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
    , shouldGenerate : Bool
    , circles : List Circle
    , generator : CircleGenerator
    }


init : ( Model, Cmd Msg )
init =
    ( Model 2 True [] defaultCircleGenerator, Cmd.none )



-- UPDATE


type Msg
    = Roll
      --| Tick Time
    | ToggleGeneration
    | NewFace Int
    | GenCircle CircleGenerator
    | NewCircle Circle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        ToggleGeneration ->
            update (GenCircle model.generator) model

        --Tick time ->
        --    ( model, Cmd.none )
        GenCircle generator ->
            if shouldKeepGenerating model.circles 0.19 then
                ( model, Random.generate NewCircle generator )
            else
                ( model, Cmd.none )

        NewCircle circle ->
            let
                circleOverlaps =
                    List.any (doCirclesOverlap circle) model.circles
            in
            if circleOverlaps then
                update (GenCircle model.generator) model
            else
                let
                    newModel =
                        { model | circles = circle :: model.circles }
                in
                update (GenCircle model.generator) newModel

        NewFace newFace ->
            ( { model | dieFace = newFace }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--every (1 * millisecond) Tick


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "900"
            , height "900"
            , viewBox "0 0 1200 1200"
            , fill "white"
            , stroke "black"
            , Html.Attributes.style [ ( "padding-left", "20px" ), ( "padding-top", "20px" ) ]
            ]
            (List.append [ rect [ x "1", y "1", width "900", height "900" ] [] ] (List.map circleToSvg model.circles))
        , button [ onClick ToggleGeneration ] [ Html.text "Toggle Generation" ]
        ]


type alias Circle =
    { color : Maybe String
    , x : Float
    , y : Float
    , r : Float
    }


type alias CircleGenerator =
    Random.Generator Circle


circleGenerator : Float -> Float -> Float -> Float -> Float -> Float -> Random.Generator Circle
circleGenerator minX maxX minY maxY minR maxR =
    let
        colorlessCircle =
            Circle Nothing
    in
    Random.map3 colorlessCircle (Random.float minX maxX) (Random.float minY maxY) (Random.float minR maxR)



--generateCircleWithSize : Circle -> Float -> Circle
--genrateCircleWithSize spawn


defaultCircleGenerator =
    circleGenerator 21 879 21 879 10 20


getAreaForCircle : Circle -> Float
getAreaForCircle circle =
    0.5 * pi * circle.r ^ 2


shouldKeepGenerating : List Circle -> Float -> Bool
shouldKeepGenerating circles maxFillRatio =
    let
        maxArea =
            900 * 900

        currentArea =
            List.map getAreaForCircle circles
                |> List.sum

        ratio =
            currentArea / maxArea
    in
    log (toString ratio)
        ratio
        < maxFillRatio


doCirclesOverlap : Circle -> Circle -> Bool
doCirclesOverlap c1 c2 =
    let
        lhs =
            sqrt ((c2.x - c1.x) ^ 2 + (c2.y - c1.y) ^ 2)

        rhs =
            c1.r + c2.r + 3

        overlap =
            lhs <= rhs
    in
    overlap


getCircleWithColor : Circle -> List Circle -> Circle
getCircleWithColor circle circles =
    if List.any (doCirclesOverlap circle) circles then
        { circle | color = Just "red" }
    else
        { circle | color = Just "black" }


circleToSvg : Circle -> Svg Msg
circleToSvg circ =
    let
        color =
            withDefault "black" circ.color
    in
    circle [ cx (toString circ.x), cy (toString circ.y), r (toString circ.r), fill color, fillOpacity "0.4" ] []
