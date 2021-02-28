module Cartoon exposing
    ( Fabric
    , Part
    , drawing
    )

import Cartoon.Doll as Doll
import Cartoon.Fabric
import Cartoon.Part
import Cartoon.System9Tan as System9Tan
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


drawing =
    --System9Tan.drawing
    Doll.drawing


type alias Fabric =
    Cartoon.Fabric.Fabric


type alias Part =
    Cartoon.Part.Part


type alias Location =
    ( Float, Float )


type alias Doll =
    { shoulders : Location
    , hips : Location
    , feet : Location
    }
