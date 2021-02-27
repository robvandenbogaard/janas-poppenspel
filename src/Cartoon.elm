module Cartoon exposing
    ( Fabric
    , Part
    , colors
    , part
    )

import Cartoon.Fabric
import Cartoon.Part
import Cartoon.System9Tan as System9Tan
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


colors =
    Cartoon.Fabric.colors


part =
    System9Tan.part


type alias Fabric =
    Cartoon.Fabric.Fabric


type alias Part =
    Cartoon.Part.Part
