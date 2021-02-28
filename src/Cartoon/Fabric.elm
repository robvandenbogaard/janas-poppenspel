module Cartoon.Fabric exposing
    ( Coloring
    , Colors
    , Fabric(..)
    , colors
    , defs
    , fill
    , shadow
    )

import Svg
import Svg.Attributes exposing (..)


type alias Coloring =
    { skin : { light : String }
    , skirt : { base : String, lower : String, low : String, mid : String, light : String, shade : String }
    , boots : String
    , hairclip : { base : String, shade : String }
    }


type alias Colors =
    { system9tan : Coloring
    , system9tanGreen : Coloring
    }


colors : Colors
colors =
    { system9tan =
        { skin = { light = "#FFE6D5" }
        , skirt = { base = "#3771C8", lower = "#1091FF", low = "#80B3FF", mid = "#8FBAFF", light = "#D5E5FF", shade = "#5A6F90" }
        , boots = "#F4EED7"
        , hairclip = { base = "#AAEEFF", shade = "#0D7AD6" }
        }
    , system9tanGreen =
        { skin = { light = "#FFE6D5" }
        , skirt = { base = "#71C837", lower = "#91FF10", low = "#B3FF80", mid = "#BAFF8F", light = "#E5FFD5", shade = "#6F905A" }
        , boots = "#F4EED7"
        , hairclip = { base = "#EEFFAA", shade = "#7AD60D" }
        }
    }


type Fabric
    = Solid
    | Flower
    | Shawl
    | Shawl2


defs =
    Svg.defs
        []
        [ Svg.pattern
            [ id "cloth-flower", patternUnits "objectBoundingBox", patternContentUnits "objectBoundingBox", width "1", height "1" ]
            [ Svg.image [ xlinkHref "/assets/cloth-flower.png", x "0", y "0", width "1", height "1", preserveAspectRatio "xMidYMid slice" ] [] ]
        , Svg.pattern
            [ id "cloth-shawl", patternUnits "objectBoundingBox", patternContentUnits "objectBoundingBox", width "1", height "1" ]
            [ Svg.image [ xlinkHref "/assets/cloth-shawl.jpg", x "0", y "0", width "1", height "1", preserveAspectRatio "xMidYMid slice" ] [] ]
        , Svg.pattern
            [ id "cloth-shawl-detail", patternUnits "objectBoundingBox", patternContentUnits "objectBoundingBox", width "1", height "1" ]
            [ Svg.image [ xlinkHref "/assets/cloth-shawl-detail.jpg", x "0", y "0", width "1", height "1", preserveAspectRatio "xMidYMid slice" ] [] ]
        , Svg.pattern
            [ id "cloth-shawl-detail-2", patternUnits "objectBoundingBox", patternContentUnits "objectBoundingBox", width "1", height "1" ]
            [ Svg.image [ xlinkHref "/assets/cloth-shawl-detail-2.jpg", x "0", y "0", width "1", height "1", preserveAspectRatio "xMidYMid slice" ] [] ]
        , Svg.filter
            [ id "shadow" ]
            [ Svg.feColorMatrix
                [ in_ "SourceGraphic"
                , type_ "matrix"
                , values "0.7 0 0 0 0   0 0.7 0 0 0   0 0 0.7 0 0   0 0 0 1 0"
                ]
                []
            ]
        ]


fill : Fabric -> String -> Svg.Attribute msg
fill fabric color =
    case fabric of
        Solid ->
            Svg.Attributes.fill color

        Flower ->
            Svg.Attributes.fill "url(#cloth-flower)"

        Shawl ->
            Svg.Attributes.fill "url(#cloth-shawl-detail)"

        Shawl2 ->
            Svg.Attributes.fill "url(#cloth-shawl-detail-2)"


shadow =
    Svg.Attributes.filter "url(#shadow)"
