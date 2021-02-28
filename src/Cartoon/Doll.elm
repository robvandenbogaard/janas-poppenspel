module Cartoon.Doll exposing
    ( attribution
    , girl
    , part
    )

import Cartoon.Fabric as Fabric exposing (Fabric(..))
import Cartoon.Part as Part exposing (Part(..))
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
            |> cubicRel [ ( -16, 22 ), ( 10, 40 ), ( 20, 18 ) ]
            |> moveTo ( 2, 102 )
            |> cubicRel [ ( 12, 20 ), ( 38, 6 ), ( 22, -16 ) ]
    , leftArm =
        startFrom ( -28, 60 )
            |> lineToRel
                [ ( -12, 10 ), ( -6, 50 ), ( -4, 8 ), ( -8, 10 ), ( -12, 20 ), ( -12, 14 ), ( -16, 10 ), ( -2, 14 ), ( 6, 4 ), ( 12, -8 ), ( 6, -8 ), ( -2, -6 ), ( 20, -18 ), ( 16, -18 ), ( 4, -8 ), ( 12, -46 ) ]
    , rightArm =
        startFrom ( 26, 64 )
            |> lineToRel [ ( 8, 6 ), ( 6, 12 ), ( 4, 24 ), ( 10, 24 ), ( 10, 16 ), ( 10, 16 ), ( 8, 14 ), ( 6, 12 ), ( 10, 2 ), ( 6, 10 ), ( -2, 10 ), ( -10, -4 ), ( -12, -16 ), ( -26, -34 ), ( -6, -6 ), ( -20, -50 ) ]
    }


part : (Fabric -> Part -> msg) -> Part -> Fabric.Coloring -> Fabric -> Svg msg
part msg p coloring fabric =
    let
        group fillColor strokeColor attributes =
            Svg.g
                ([ onClick <| msg fabric p
                 , Fabric.fill fabric fillColor
                 , stroke strokeColor
                 ]
                    ++ attributes
                )
    in
    case p of
        Cloth color ->
            group color
                "none"
                []
                [ Svg.rect [ Fabric.fill fabric color, x "-25", y "-25", width "50", height "50" ] []
                ]

        Girl parts ->
            group coloring.skirt.base
                "none"
                []
                [ Fabric.defs, girl msg parts coloring fabric ]

        Body ->
            group "white"
                "black"
                []
                [ group "brown"
                    "#331122"
                    [ transform "translate(5,-160)" ]
                    [ Svg.path [ d build.body ] []
                    , Svg.path [ transform "scale(0.7) translate(0,27)", d build.head ] []
                    , Svg.path [ d build.leftArm ] []
                    , Svg.path [ d build.rightArm ] []
                    , Svg.path [ d build.breasts ] []
                    ]
                , Svg.rect [ x "0", y "0", width "10", height "10" ] []
                ]

        Shirt ->
            group "white" "black" [] []

        Sleeves ->
            group "white" "black" [] []

        Skirt ->
            group "white" "black" [] []

        Boots ->
            group "white" "black" [] []

        HairClip ->
            group "white" "black" [] []


girl : (Fabric -> Part -> msg) -> List Part -> Fabric.Coloring -> Fabric -> Svg msg
girl msg parts coloring fabric =
    Svg.g [ Svg.Attributes.title <| String.join "" [ "Drawing by ", attribution.author, ": ", attribution.url ] ]
        (List.map
            (\p -> part msg p coloring fabric)
            (Body :: parts)
        )
