module Main exposing (..)

import Color exposing (rgb)
import Sketch


lineLen =
    80


backgroundColor =
    Color.hsl (degrees 220) 0.5 0.6


main =
    let
        x =
            Sketch.named "x" <| Sketch.random -250 250

        baseAngle =
            Sketch.calculate x
                (\v -> (sin (10 * v / 500)) / 3)
    in
        Sketch.scene backgroundColor
            (Sketch.rectangle
                { width = Sketch.random lineLen (lineLen + 70)
                , height = Sketch.always 3
                , color =
                    Sketch.hsla
                        (Sketch.random 200 240)
                        (Sketch.random 0.0 1.0)
                        (Sketch.random 0.4 0.6)
                        (Sketch.random 0.0 0.4)
                }
                |> Sketch.rotate
                    (Sketch.combine (+) baseAngle (Sketch.random -0.5 0.5))
                |> Sketch.move
                    x
                    (Sketch.random -250 250)
            )



--
--
-- main =
--     Render.group
--         [ Render.rectangle 500 500
--             |> Render.solidFill backgroundColor
--         , riverLine
--         ]
--         |> Render.svg 500 500
--
--
-- riverLine =
--     let
--         len =
--             80
--
--         fillColor =
--             Color.hsla (degrees <| 200 + 20) 0.5 (0.4 + 1.5) 0.2
--     in
--         Render.rectangle len 3
--             |> Render.solidFill fillColor
