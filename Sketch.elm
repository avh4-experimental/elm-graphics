module Sketch
    exposing
        ( scene
        , rectangle
        , rotate
        , move
        , named
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
import Dict exposing (Dict)


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
    | NamedNumber String SketchNumber


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

                        ( _, newContext ) =
                            renderShape shape { seed = currentSeed, floatCache = Dict.empty }
                    in
                        ( { model | seeds = newContext.seed :: model.seeds }
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
            |> List.map
                (\seed ->
                    renderShape shape
                        { seed = seed, floatCache = Dict.empty }
                        |> fst
                )
            |> Render.group
        ]
        |> Render.svg sceneSize.width sceneSize.height


renderShape : Shape -> Context -> ( Render.Form msg, Context )
renderShape shape context =
    case shape of
        Rectangle { width, height, color } ->
            let
                ( widthValue, context1 ) =
                    toFloat width context

                ( heightValue, context2 ) =
                    toFloat height context1

                ( colorValue, context3 ) =
                    toColor color context2
            in
                ( Render.rectangle widthValue heightValue
                    |> Render.solidFill colorValue
                , context3
                )

        Rotate angle innerShape ->
            let
                ( angleValue, context1 ) =
                    toFloat angle context

                ( innerShapeValue, context2 ) =
                    renderShape innerShape context1
            in
                ( Render.rotate angleValue innerShapeValue
                , context2
                )

        Move x y innerShape ->
            let
                ( xValue, context1 ) =
                    toFloat x context

                ( yValue, context2 ) =
                    toFloat y context1

                ( innerShapeValue, context3 ) =
                    renderShape innerShape context2
            in
                ( Render.move xValue yValue innerShapeValue
                , context2
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


named : String -> SketchNumber -> SketchNumber
named name child =
    NamedNumber name child


type alias Context =
    { seed : Random.Seed
    , floatCache : Dict String Float
    }


toFloat : SketchNumber -> Context -> ( Float, Context )
toFloat number context =
    case number of
        ConstantNumber value ->
            ( value, context )

        RandomNumber { min, max } ->
            let
                ( value, newSeed ) =
                    Random.step (Random.float min max) context.seed
            in
                ( value, { context | seed = newSeed } )

        NamedNumber name child ->
            case Dict.get name context.floatCache of
                Nothing ->
                    let
                        ( value, newContext ) =
                            toFloat child context
                    in
                        ( value
                        , { newContext
                            | floatCache =
                                Dict.insert name value newContext.floatCache
                          }
                        )

                Just value ->
                    ( value, context )


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


toColor : SketchColor -> Context -> ( Color, Context )
toColor color context =
    case color of
        SketchColor { hue, saturation, lightness, alpha } ->
            let
                ( hueValue, context1 ) =
                    toFloat hue context

                ( saturationValue, context2 ) =
                    toFloat saturation context1

                ( lightnessValue, context3 ) =
                    toFloat lightness context2

                ( alphaValue, context4 ) =
                    toFloat alpha context3
            in
                ( Color.hsla
                    (degrees <| hueValue)
                    saturationValue
                    lightnessValue
                    alphaValue
                , context4
                )
