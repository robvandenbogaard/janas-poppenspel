module Cartoon.Part exposing
    ( Msg(..)
    , Part(..)
    , list
    , show
    )

import Cartoon.Fabric exposing (Fabric)
import Svg.Attributes


type Part
    = Patch
    | Girl (List ( Fabric, Part ))
    | Body
    | Skirt
    | Shirt
    | Sleeves
    | HairClip
    | Boots


type Msg id
    = Clicked Fabric Part
    | ClickedGroup id


list =
    [ Skirt, Shirt, Sleeves, HairClip, Boots ]


show p parts =
    if List.member p (List.map Tuple.second parts) then
        Svg.Attributes.style ""

    else
        Svg.Attributes.style "visibility: hidden"
