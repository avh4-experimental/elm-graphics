module Sketch
    exposing
        ( scene
        , rectangle
        , rotate
        , move
        , random
        , hsla
        , always
        )

import Color exposing (Color)
import Html.App
import Html exposing (Html)
import Graphics.Render as Render
import Random
import Time


type Shape
    = Rectangle
        { width : SketchNumber
        , height : SketchNumber
        , color : SketchColor
        }
    | Rotate SketchNumber Shape
    | Move SketchNumber SketchNumber Shape


type SketchNumber
    = ConstantNumber Float
    | RandomNumber { min : Float, max : Float }


type SketchColor
    = SketchColor
        { hue : SketchNumber
        , saturation : SketchNumber
        , lightness : SketchNumber
        , alpha : SketchNumber
        }


type alias SceneState =
    { seeds : List Random.Seed }


type SceneMsg
    = NewRandomSeed


scene : Color -> Shape -> Program Never
scene backgroundColor shape =
    let
        sceneSize =
            { width = 500, height = 500 }

        initialModel : SceneState
        initialModel =
            { seeds = [] }

        update : SceneMsg -> SceneState -> ( SceneState, Cmd SceneMsg )
        update msg model =
            case msg of
                NewRandomSeed ->
                    let
                        currentSeed =
                            case model.seeds of
                                [] ->
                                    Random.initialSeed 0

                                first :: rest ->
                                    first

                        ( _, newSeed ) =
                            renderShape shape currentSeed
                    in
                        ( { model | seeds = newSeed :: model.seeds }
                        , Cmd.none
                        )

        view model =
            renderScene sceneSize backgroundColor shape model.seeds
    in
        Html.App.program
            { init = ( initialModel, Cmd.none )
            , subscriptions = \_ -> Time.every 100 (Basics.always NewRandomSeed)
            , update = update
            , view = view
            }


renderScene :
    { width : Float, height : Float }
    -> Color
    -> Shape
    -> List Random.Seed
    -> Html msg
renderScene sceneSize backgroundColor shape seeds =
    Render.group
        [ Render.rectangle sceneSize.width sceneSize.height
            |> Render.solidFill backgroundColor
        , seeds
            |> List.map (\seed -> renderShape shape seed |> fst)
            |> Render.group
        ]
        |> Render.svg sceneSize.width sceneSize.height


renderShape : Shape -> Random.Seed -> ( Render.Form msg, Random.Seed )
renderShape shape seed =
    case shape of
        Rectangle { width, height, color } ->
            let
                ( widthValue, seed1 ) =
                    toFloat width seed

                ( heightValue, seed2 ) =
                    toFloat height seed1

                ( colorValue, seed3 ) =
                    toColor color seed2
            in
                ( Render.rectangle widthValue heightValue
                    |> Render.solidFill colorValue
                , seed3
                )

        Rotate angle innerShape ->
            let
                ( angleValue, seed1 ) =
                    toFloat angle seed

                ( innerShapeValue, seed2 ) =
                    renderShape innerShape seed1
            in
                ( Render.rotate angleValue innerShapeValue
                , seed2
                )

        Move x y innerShape ->
            let
                ( xValue, seed1 ) =
                    toFloat x seed

                ( yValue, seed2 ) =
                    toFloat y seed1

                ( innerShapeValue, seed3 ) =
                    renderShape innerShape seed2
            in
                ( Render.move xValue yValue innerShapeValue
                , seed2
                )



-- Shapes


rectangle :
    { width : SketchNumber
    , height : SketchNumber
    , color : SketchColor
    }
    -> Shape
rectangle =
    Rectangle


rotate : SketchNumber -> Shape -> Shape
rotate angle shape =
    Rotate angle shape


move : SketchNumber -> SketchNumber -> Shape -> Shape
move x y shape =
    Move x y shape



-- Values


random : Float -> Float -> SketchNumber
random min max =
    RandomNumber { min = min, max = max }


always : Float -> SketchNumber
always value =
    ConstantNumber value


toFloat : SketchNumber -> Random.Seed -> ( Float, Random.Seed )
toFloat number seed =
    case number of
        ConstantNumber value ->
            ( value, seed )

        RandomNumber { min, max } ->
            Random.step (Random.float min max) seed


hsla :
    SketchNumber
    -> SketchNumber
    -> SketchNumber
    -> SketchNumber
    -> SketchColor
hsla hue saturation lightness alpha =
    SketchColor
        { hue = hue
        , saturation = saturation
        , lightness = lightness
        , alpha = alpha
        }


toColor : SketchColor -> Random.Seed -> ( Color, Random.Seed )
toColor color seed =
    case color of
        SketchColor { hue, saturation, lightness, alpha } ->
            let
                ( hueValue, seed1 ) =
                    toFloat hue seed

                ( saturationValue, seed2 ) =
                    toFloat saturation seed1

                ( lightnessValue, seed3 ) =
                    toFloat lightness seed2

                ( alphaValue, seed4 ) =
                    toFloat alpha seed3
            in
                ( Color.hsla
                    (degrees <| hueValue)
                    saturationValue
                    lightnessValue
                    alphaValue
                , seed4
                )
