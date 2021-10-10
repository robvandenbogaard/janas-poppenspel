module Cartoon exposing
    ( Location
    , cubic
    , cubicRel
    , line
    , lineTo
    , lineToRel
    , moveTo
    , moveToRel
    , startFrom
    )

import Playground
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Location =
    ( Float, Float )


line color path =
    Svg.g
        [ stroke (Playground.renderColor color)
        , fill "none"
        ]
        [ Svg.path
            [ d path ]
            []
        ]


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
