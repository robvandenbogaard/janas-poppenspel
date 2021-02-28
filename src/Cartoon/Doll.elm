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
        startFrom ( 71, 47 )
            |> lineToRel
                ([ ( 0, 8 ), ( -9, 0 ), ( 7, 38 ), ( -4, 17 ), ( -3, 15 ), ( 2, 20 ), ( 5, 20 ), ( 2, 8 ), ( -2, 9 ), ( -1, 11 ), ( 0, 15.54974 ) ]
                    ++ [ ( 1, 11 ), ( -3, 11 ), ( 1, 9 ), ( 3, 5 ), ( 4, -8 ), ( -1, -10 ), ( -1, -6 ), ( 3, -26 ), ( 2, 16 ), ( 1, 12 ), ( 1, 10 ), ( -1, 8 ) ]
                    ++ [ ( 3, 12 ), ( 6, -7 ), ( -3, -13 ), ( -2, -6 ), ( 1, -5 ), ( 2, -14 ), ( 1, -7 ), ( -1, -8 ), ( -2, -11 ), ( 8, -33 ), ( 3, -15 ) ]
                    ++ [ ( 1, -16 ), ( -5, -11 ), ( -6, -11 ), ( 1, -4 ), ( 4, -35 ), ( -10, -2 ), ( -1, -6 ) ]
                )
            |> moveTo ( 78, 125 )
            |> lineToRel [ ( -2, 21 ), ( -1, 26 ), ( 0, 22 ) ]
            |> moveTo ( 69, 116 )
            |> lineToRel [ ( 9, 9 ), ( 8, -8 ) ]
    , leftArm =
        startFrom ( 62, 55 )
            |> lineToRel
                [ ( -6, 5 ), ( -3, 25 ), ( -2, 4 ), ( -4, 5 ), ( -6, 10 ), ( -6, 7 ), ( -8, 5 ), ( -1, 7 ), ( 3, 2 ), ( 6, -4 ), ( 3, -4 ), ( -1, -3 ), ( 10, -9 ), ( 8, -9 ), ( 2, -4 ), ( 6, -23 ) ]
    , rightArm =
        startFrom ( 90, 55 )
            |> lineToRel [ ( 4, 3 ), ( 3, 6 ), ( 2, 12 ), ( 5, 12 ), ( 5, 8 ), ( 5, 8 ), ( 4, 7 ), ( 3, 6 ), ( 5, 1 ), ( 3, 5 ), ( -1, 5 ), ( -5, -2 ), ( -6, -8 ), ( -13, -17 ), ( -3, -3 ), ( -10, -25 ) ]
    }


part : (Fabric -> Part -> msg) -> Part -> Fabric.Coloring -> Fabric -> Svg msg
part msg p coloring fabric =
    let
        group fillColor strokeColor =
            Svg.g
                [ onClick <| msg fabric p
                , Fabric.fill fabric fillColor
                , stroke strokeColor
                ]
    in
    case p of
        Cloth color ->
            group color
                "none"
                [ Svg.rect [ Fabric.fill fabric color, x "-25", y "-25", width "50", height "50" ] []
                ]

        Girl parts ->
            group coloring.skirt.base
                "none"
                [ Fabric.defs, girl msg parts coloring fabric ]

        Body ->
            group "brown"
                "black"
                [ Svg.path [ d build.body ] []
                , Svg.path [ d build.head ] []
                , Svg.path [ d build.leftArm ] []
                , Svg.path [ d build.rightArm ] []
                ]

        Shirt ->
            group "white" "black" []

        Sleeves ->
            group "white" "black" []

        Skirt ->
            group "white" "black" []

        Boots ->
            group "white" "black" []

        HairClip ->
            group "white" "black" []


girl : (Fabric -> Part -> msg) -> List Part -> Fabric.Coloring -> Fabric -> Svg msg
girl msg parts coloring fabric =
    Svg.g [ Svg.Attributes.title <| String.join "" [ "Drawing by ", attribution.author, ": ", attribution.url ] ]
        (List.map
            (\p -> part msg p coloring fabric)
            (Body :: parts)
        )
