module Cartoon.Part exposing
    ( Part(..)
    , list
    , show
    )

import Svg.Attributes


type Part
    = Cloth String
    | Girl (List Part)
    | Body
    | Skirt
    | Shirt
    | Sleeves
    | HairClip
    | Boots


list =
    [ Skirt, Shirt, Sleeves, HairClip, Boots ]


show p parts =
    if List.member p parts then
        Svg.Attributes.style ""

    else
        Svg.Attributes.style "visibility: hidden"
