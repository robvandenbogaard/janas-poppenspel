module Main exposing (main)

import Browser
import Cartoon
import Cartoon.Fabric
import Cartoon.Part
import Html exposing (Html)
import Html.Attributes as Attr
import Playground exposing (..)


type alias Cloth =
    Maybe ( Cartoon.Fabric, String )


type alias Model =
    { playground : Playground.Game {}
    , parts : List ( Cartoon.Part, Cloth )
    , cloth : Cloth
    }


type Msg
    = PlaygroundMsg Playground.Msg


main =
    let
        ( p, cmd ) =
            Playground.componentInit {}
    in
    Browser.document
        { init = \() -> ( { playground = p, parts = [], cloth = Nothing }, Cmd.map PlaygroundMsg cmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


pink =
    rgb 255 230 230


view : Model -> Browser.Document Msg
view model =
    { title = "Jana's Poppenspel"
    , body =
        [ background pink
            [ Html.map PlaygroundMsg <| Playground.componentView model.playground (scene model.cloth model.parts)
            ]
        ]
    }


scene geselecteerdeStof ongedragen computer memory =
    let
        metKleren =
            List.filter
                (\part -> not <| List.member ( part, Nothing ) ongedragen)
                Cartoon.Part.list
    in
    [ bed, kast (kleding ongedragen), stoffen geselecteerdeStof, girl metKleren, rectangle red 1 1 ]


clicked memory =
    moveUp 50 <|
        words red <|
            case memory.clicked of
                Nothing ->
                    "No click"

                Just p ->
                    case p of
                        Cartoon.Part.Skirt ->
                            "Skirt"

                        Cartoon.Part.Shirt ->
                            "Shirt"

                        Cartoon.Part.Boots ->
                            "Boots"

                        _ ->
                            "Other"


bed =
    group
        [ oval lightBlue 360 50
            |> moveRight 20
            |> moveUp 20
        , circle yellow 30
            |> moveLeft 180
            |> moveUp 20
        , rectangle brown 400 30
        , rectangle brown 20 110
            |> moveLeft 200
            |> moveUp 20
        , rectangle brown 20 70
            |> moveRight 200
        ]
        |> moveRight 200
        |> moveDown 250
        |> scale 1.4


kast inhoud =
    group
        [ rectangle brown 300 400
        , group [ oval brown 300 100, oval pink 70 30 |> moveUp 50 ]
            |> moveUp 200
        , rectangle darkBrown 280 380
        , inhoud
        ]
        |> moveRight 300
        |> moveUp 80



--winkel =
--    een plek waar je spulen kan koopen


girl kledingSelectie =
    drawing (Cartoon.Part.Girl kledingSelectie) Cartoon.Fabric.Solid
        |> moveLeft 300
        |> scale 1.2


stof fabric color currentCloth =
    case currentCloth of
        Nothing ->
            drawing (Cartoon.Part.Cloth color) fabric

        Just ( f, c ) ->
            if f == fabric && c == color then
                group
                    [ drawing (Cartoon.Part.Cloth "salmon") Cartoon.Fabric.Solid
                        |> scale 1.1
                    , drawing (Cartoon.Part.Cloth color) fabric
                    ]

            else
                drawing (Cartoon.Part.Cloth color) fabric


stoffen geselecteerdeStof =
    group
        (List.indexedMap
            (\i s ->
                s |> moveDown (60 * toFloat i)
            )
            [ stof Cartoon.Fabric.Solid "lightGreen" geselecteerdeStof
            , stof Cartoon.Fabric.Solid "lightBlue" geselecteerdeStof
            , stof Cartoon.Fabric.Solid "pink" geselecteerdeStof
            , stof Cartoon.Fabric.Flower "white" geselecteerdeStof
            , stof Cartoon.Fabric.Shawl "red" geselecteerdeStof
            , stof Cartoon.Fabric.Shawl2 "yellow" geselecteerdeStof
            ]
        )
        |> moveRight 115
        |> moveUp 200


kleding selectie =
    group
        (List.filterMap
            (\( part, maybeCloth ) ->
                let
                    ( fabric, color ) =
                        case maybeCloth of
                            Nothing ->
                                ( Cartoon.Fabric.Solid, "none" )

                            Just c ->
                                c
                in
                case part of
                    Cartoon.Part.Skirt ->
                        Just
                            (drawing Cartoon.Part.Skirt fabric
                                |> moveLeft 50
                                |> moveUp 20
                            )

                    Cartoon.Part.Boots ->
                        Just
                            (drawing Cartoon.Part.Boots fabric
                                |> moveRight 100
                                |> moveUp 10
                            )

                    Cartoon.Part.Shirt ->
                        Just
                            (drawing Cartoon.Part.Shirt fabric
                                |> moveRight 50
                                |> moveUp 70
                            )

                    _ ->
                        Nothing
            )
            selectie
        )
        |> scale 0.8


menu =
    Html.nav
        [ Attr.style "position" "absolute"
        , Attr.style "width" "6rem"
        , Attr.style "top" "0"
        , Attr.style "right" "0"
        , Attr.style "height" "100vh"
        , Attr.style "margin-top" "1rem"
        , Attr.style "margin-right" "1rem"
        , Attr.style "text-align" "right"
        ]
        [ Html.button [ Attr.style "height" "5rem", Attr.style "width" "5rem" ] [ Html.text "mens" ]
        , Html.button [ Attr.style "height" "5rem", Attr.style "width" "5rem" ] [ Html.text "haren" ]
        , Html.button [ Attr.style "height" "5rem", Attr.style "width" "5rem" ] [ Html.text "kleren" ]
        , Html.button [ Attr.style "height" "2rem", Attr.style "width" "5rem" ] [ Html.text "ok" ]
        ]


background color =
    Html.div
        [ Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "background-color" (renderColor color)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaygroundMsg pmsg ->
            let
                ( cloth, parts ) =
                    case pmsg of
                        Playground.Clicked fabric part ->
                            case part of
                                Cartoon.Part.Cloth color ->
                                    ( Just ( fabric, color ), model.parts )

                                Cartoon.Part.Girl _ ->
                                    ( model.cloth, Debug.log "closet" model.parts )

                                _ ->
                                    if List.member part (List.map Tuple.first model.parts) then
                                        ( model.cloth, List.filter (\( p, _ ) -> p /= part) model.parts )

                                    else
                                        ( Nothing, ( part, model.cloth ) :: model.parts )

                        _ ->
                            ( model.cloth, model.parts )

                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { model | playground = playground, parts = parts, cloth = cloth }, Cmd.map PlaygroundMsg cmd )


updatePlayground computer memory =
    memory
