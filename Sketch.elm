module Sketch
    exposing
        ( scene
        , rectangle
        , rotate
        , move
        , named
        , random
        , always
        , calculate
        , combine
        , hsla
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
    | CalculatedNumber SketchNumber (Float -> Float)
    | CombinedNumber (Float -> Float -> Float) SketchNumber SketchNumber


type SketchColor
    = SketchColor
        { hue : SketchNumber
        , saturation : SketchNumber
        , lightness : SketchNumber
        , alpha : SketchNumber
        }


type alias SceneState =
    { seed : Random.Seed
    , pastShapes : List (Render.Form Never)
    }


type SceneMsg
    = NewRandomSeed


scene : Color -> Shape -> Program Never
scene backgroundColor shape =
    let
        sceneSize =
            { width = 500, height = 500 }

        initialModel : SceneState
        initialModel =
            { seed = Random.initialSeed 0
            , pastShapes = []
            }

        update : SceneMsg -> SceneState -> ( SceneState, Cmd SceneMsg )
        update msg model =
            case msg of
                NewRandomSeed ->
                    let
                        ( newShape, newContext ) =
                            renderShape shape { seed = model.seed, floatCache = Dict.empty }
                    in
                        ( { model
                            | seed = newContext.seed
                            , pastShapes = newShape :: model.pastShapes
                          }
                        , Cmd.none
                        )

        view model =
            renderScene sceneSize backgroundColor shape model.seed model.pastShapes
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
    -> Random.Seed
    -> List (Render.Form Never)
    -> Html msg
renderScene sceneSize backgroundColor shape seed pastShapes =
    Render.group
        [ Render.rectangle sceneSize.width sceneSize.height
            |> Render.solidFill backgroundColor
        , pastShapes
            |> Render.group
        , renderShape shape { seed = seed, floatCache = Dict.empty } |> fst
        ]
        |> Render.svg sceneSize.width sceneSize.height
        |> Html.App.map (\x -> Debug.crash "got a Never value")


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


calculate : SketchNumber -> (Float -> Float) -> SketchNumber
calculate base fn =
    CalculatedNumber base fn


combine : (Float -> Float -> Float) -> SketchNumber -> SketchNumber -> SketchNumber
combine fn a b =
    CombinedNumber fn a b


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

        CalculatedNumber base fn ->
            let
                ( value, newContext ) =
                    toFloat base context
            in
                ( fn value, newContext )

        CombinedNumber fn a b ->
            let
                ( aValue, context1 ) =
                    toFloat a context

                ( bValue, context2 ) =
                    toFloat b context1
            in
                ( fn aValue bValue, context2 )


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
