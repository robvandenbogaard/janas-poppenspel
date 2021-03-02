module Cartoon.Doll exposing
    ( attribution
    , drawing
    , girl
    )

import Cartoon.Fabric as Fabric exposing (Fabric(..))
import Cartoon.Part as Part exposing (Part(..))
import Playground
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


attribution =
    { title = "Doll"
    , author = "Rob van den Bogaard"
    , url = "https://www.compurob.nl"
    }


coords ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


startFrom ( x, y ) =
    "M " ++ coords ( x, y )


moveTo ( x, y ) path =
    path ++ " " ++ startFrom ( x, y )


moveToRel ( x, y ) path =
    path ++ " " ++ "m " ++ coords ( x, y )


lineTo points path =
    "L"
        :: List.map coords points
        |> String.join " "
        |> (++) path


lineToRel points path =
    "l"
        :: List.map coords points
        |> String.join " "
        |> (++) path


cubic points path =
    "C"
        :: List.map coords points
        |> String.join " "
        |> (++) path


cubicRel points path =
    "c"
        :: List.map coords points
        |> String.join " "
        |> (++) path


build =
    { head =
        startFrom ( 40, -10 )
            |> cubic [ ( 40, -35 ), ( 20, -50 ), ( 0, -50 ), ( -20, -50 ), ( -40, -35 ), ( -40, -10 ), ( -40, 20 ), ( -15, 50 ), ( 0, 50 ), ( 15, 50 ), ( 40, 20 ), ( 40, -10 ) ]
    , upperBody =
        startFrom ( -10, -150 )
            |> lineTo [ ( -10, -132 ), ( -50, -130 ), ( -40, -20 ), ( -40, 0 ), ( 40, 0 ), ( 40, -20 ), ( 50, -130 ), ( 10, -132 ), ( 10, -150 ) ]
    , lowerBody =
        startFrom ( -40, 0 )
            |> lineTo
                ([ ( -60, 50 ), ( -60, 60 ), ( -40, 180 ), ( -40, 190 ), ( -45, 200 ), ( -48, 210 ), ( -50, 230 ), ( -52, 280 ), ( -54, 295 ), ( -52, 320 ) ]
                    ++ [ ( -60, 360 ), ( -55, 380 ), ( -40, 395 ), ( -30, 350 ), ( -35, 330 ), ( -7, 230 ) ]
                    ++ [ ( 2, 340 ), ( 0, 400 ), ( 5, 410 ), ( 25, 390 ), ( 20, 345 ) ]
                    ++ [ ( 30, 300 ), ( 33, 230 ), ( 31, 210 ), ( 28, 200 ), ( 23, 190 ), ( 23, 180 ), ( 63, 60 ), ( 60, 50 ), ( 40, 0 ) ]
                )
    , body =
        startFrom ( -10, 45 )
            |> lineToRel
                ([ ( 0, 16 ), ( -20, 0 ), ( 14, 76 ), ( -11, 34 ), ( -1, 30 ), ( 4, 40 ), ( 10, 40 ), ( 4, 16 ), ( -4, 18 ), ( -2, 22 ), ( 0, 32 ) ]
                    ++ [ ( 2, 22 ), ( -6, 22 ), ( 2, 18 ), ( 6, 10 ), ( 8, -16 ), ( -2, -20 ), ( -2, -12 ), ( 6, -52 ), ( 4, 32 ), ( 2, 24 ), ( 2, 20 ), ( -2, 16 ) ]
                    ++ [ ( 6, 24 ), ( 12, -14 ), ( -6, -26 ), ( -4, -12 ), ( 2, -10 ), ( 4, -28 ), ( 2, -14 ), ( -2, -16 ), ( -4, -22 ), ( 16, -66 ), ( 6, -30 ) ]
                    ++ [ ( 2, -32 ), ( -6, -22 ), ( -10, -22 ), ( -2, -8 ), ( 8, -70 ), ( -22, -4 ), ( -2, -12 ) ]
                )
            |> moveTo ( 4, 202 )
            |> lineToRel [ ( -4, 42 ), ( -2, 52 ), ( 0, 44 ) ]
            |> moveTo ( -14, 183 )
            |> lineToRel [ ( 18, 18 ), ( 16, -16 ) ]
    , breasts =
        startFrom ( -24, 82 )
            |> cubicRel [ ( -10, 22 ), ( 10, 30 ), ( 20, 25 ) ]
            |> moveTo ( 2, 106 )
            |> cubicRel [ ( 12, 8 ), ( 32, 6 ), ( 22, -22 ) ]
    , leftArm =
        startFrom ( -28, 60 )
            |> lineToRel
                [ ( -12, 10 ), ( -6, 50 ), ( -4, 8 ), ( -8, 10 ), ( -12, 20 ), ( -12, 14 ), ( -16, 10 ), ( -2, 14 ), ( 6, 4 ), ( 12, -8 ), ( 6, -8 ), ( -2, -6 ), ( 20, -18 ), ( 16, -18 ), ( 4, -8 ), ( 12, -46 ) ]
    , rightArm =
        startFrom ( 26, 64 )
            |> lineToRel [ ( 8, 6 ), ( 6, 12 ), ( 4, 24 ), ( 10, 24 ), ( 10, 16 ), ( 10, 16 ), ( 8, 14 ), ( 6, 12 ), ( 10, 2 ), ( 6, 10 ), ( -2, 10 ), ( -10, -4 ), ( -12, -16 ), ( -26, -34 ), ( -6, -6 ), ( -20, -50 ) ]
    , shirt =
        startFrom ( -37, -75 )
            |> lineTo [ ( 45, -70 ), ( 30, 10 ), ( 55, 60 ), ( -30, 85 ), ( -25, 15 ), ( -37, -75 ) ]
    , skirt =
        startFrom ( -23, 1 )
            |> lineToRel [ ( 60, -4 ), ( 6, 24 ) ]
            |> cubicRel [ ( 24, 80 ), ( 50, 170 ), ( 84, 222 ), ( -60, 12 ), ( -114, 10 ), ( -172, 6 ), ( 10, -68 ), ( 22, -150 ), ( 20, -228 ) ]
    }


drawing : ( Fabric, Part ) -> Svg (Playground.Msg (Part.Msg a))
drawing ( fabric, p ) =
    let
        group strokeColor attributes =
            Svg.g
                ([ onClick <| Playground.Clicked (Part.Clicked fabric p)
                 , Fabric.fill fabric "none"
                 , stroke strokeColor
                 ]
                    ++ attributes
                )
    in
    case p of
        Patch ->
            group
                "none"
                []
                [ Svg.rect [ Fabric.fill fabric "none", x "-25", y "-25", width "50", height "50" ] []
                ]

        Girl parts ->
            group
                "none"
                []
                [ Fabric.defs, girl fabric parts ]

        Body ->
            group
                "#331122"
                [ transform "translate(5,-160)" ]
                [ Svg.path [ d build.body ] []
                , Svg.path [ transform "scale(0.7) translate(0,27)", d build.head ] []
                , Svg.path [ d build.leftArm ] []
                , Svg.path [ d build.rightArm ] []
                , Svg.path [ d build.breasts ] []
                ]

        Shirt ->
            group
                "none"
                [ transform "translate(0,-30)" ]
                [ Svg.path [ d build.shirt ] [] ]

        Sleeves ->
            group "black" [] []

        Skirt ->
            group "none"
                []
                [ Svg.path [ d build.skirt ] [] ]

        Boots ->
            group "black" [] []

        HairClip ->
            group "black" [] []


girl : Fabric -> List ( Fabric, Part ) -> Svg (Playground.Msg (Part.Msg a))
girl fabric parts =
    List.map drawing (( fabric, Body ) :: parts)
        |> Svg.g [ Svg.Attributes.title <| String.join "" [ "Drawing by ", attribution.author, ": ", attribution.url ] ]
