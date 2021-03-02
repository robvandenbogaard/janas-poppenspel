module Cartoon.Fabric exposing
    ( Fabric(..)
    , defs
    , fill
    , list
    , shadow
    )

import Svg
import Svg.Attributes exposing (..)


type Fabric
    = Default
    | Solid String
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
fill fabric defaultColor =
    case fabric of
        Default ->
            Svg.Attributes.fill defaultColor

        Solid color ->
            Svg.Attributes.fill color

        Flower ->
            Svg.Attributes.fill "url(#cloth-flower)"

        Shawl ->
            Svg.Attributes.fill "url(#cloth-shawl-detail)"

        Shawl2 ->
            Svg.Attributes.fill "url(#cloth-shawl-detail-2)"


shadow =
    Svg.Attributes.filter "url(#shadow)"


list =
    List.reverse
        [ Solid "lightGreen"
        , Solid "lightBlue"
        , Solid "pink"
        , Flower
        , Shawl
        , Shawl2
        ]
