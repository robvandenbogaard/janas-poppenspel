module Main exposing (main)

import Browser
import Cartoon
import Cartoon.Fabric exposing (Fabric(..))
import Cartoon.Part exposing (Part)
import Html exposing (Html)
import Html.Attributes as Attr
import Playground exposing (..)


type alias Model =
    { playground : Playground.Game {}
    , clothesInCloset : List ( Fabric, Part )
    , clothesBeingWorn : List ( Fabric, Part )
    , selectedFabric : Maybe Fabric
    }


type Msg
    = PlaygroundMsg (Playground.Msg Cartoon.Part.Msg)


main =
    let
        ( p, cmd ) =
            Playground.componentInit {}
    in
    Browser.document
        { init =
            \() ->
                ( { playground = p
                  , clothesInCloset = []
                  , clothesBeingWorn =
                        [ ( Flower, Cartoon.Part.Skirt )
                        , ( Solid "#AAAAFF", Cartoon.Part.Shirt )
                        ]
                  , selectedFabric = Nothing
                  }
                , Cmd.map PlaygroundMsg cmd
                )
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
            [ Html.map PlaygroundMsg <| Playground.componentView model.playground (scene model.selectedFabric model.clothesInCloset model.clothesBeingWorn)
            ]
        ]
    }


scene selectedFabric clothesInCloset clothesBeingWorn computer memory =
    [ bed, kast (kleding clothesInCloset), stoffen selectedFabric, girl clothesBeingWorn, rectangle red 1 1 ]


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


availableClothingVariants clothing =
    Cartoon.Fabric.list
        |> List.map (Tuple.pair clothing)


availableCothes =
    Cartoon.Part.list
        |> List.map availableClothingVariants
        |> List.concat


girl kledingSelectie =
    Cartoon.drawing ( Solid "brown", Cartoon.Part.Girl kledingSelectie )
        |> drawing
        |> moveLeft 270
        |> scale 1.2


stof currentFabric fabric =
    case currentFabric of
        Nothing ->
            drawing (Cartoon.drawing ( fabric, Cartoon.Part.Patch ))

        Just f ->
            if f == fabric then
                group
                    [ drawing (Cartoon.drawing ( Cartoon.Fabric.Solid "salmon", Cartoon.Part.Patch ))
                        |> scale 1.1
                    , drawing
                        (Cartoon.drawing ( fabric, Cartoon.Part.Patch ))
                    ]

            else
                drawing (Cartoon.drawing ( fabric, Cartoon.Part.Patch ))


stoffen geselecteerdeStof =
    group
        (Cartoon.Fabric.list
            |> List.map (stof geselecteerdeStof)
            |> List.indexedMap
                (\i s ->
                    s |> moveDown (60 * toFloat i)
                )
        )
        |> moveRight 115
        |> moveUp 200


kleding selectie =
    group
        (List.filterMap
            (\( fabric, part ) ->
                case part of
                    Cartoon.Part.Skirt ->
                        Just
                            (drawing (Cartoon.drawing ( fabric, Cartoon.Part.Skirt ))
                                |> moveLeft 50
                                |> moveUp 20
                            )

                    Cartoon.Part.Boots ->
                        Just
                            (drawing (Cartoon.drawing ( fabric, Cartoon.Part.Boots ))
                                |> moveRight 100
                                |> moveUp 10
                            )

                    Cartoon.Part.Shirt ->
                        Just
                            (drawing (Cartoon.drawing ( fabric, Cartoon.Part.Shirt ))
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
                ( selectedFabric, clothesInCloset, clothesBeingWorn ) =
                    case pmsg of
                        Playground.Clicked (Cartoon.Part.Clicked clickedFabric clickedPart) ->
                            case clickedPart of
                                Cartoon.Part.Patch ->
                                    ( Just clickedFabric, model.clothesInCloset, model.clothesBeingWorn )

                                Cartoon.Part.Girl _ ->
                                    ( model.selectedFabric, model.clothesInCloset, model.clothesBeingWorn )

                                _ ->
                                    if List.member ( clickedFabric, clickedPart ) model.clothesInCloset then
                                        ( model.selectedFabric
                                        , List.filter (\( _, p ) -> p /= clickedPart) model.clothesInCloset
                                        , ( clickedFabric, clickedPart ) :: model.clothesBeingWorn
                                        )

                                    else
                                        case model.selectedFabric of
                                            Nothing ->
                                                if List.member clickedPart (List.map Tuple.second model.clothesInCloset) then
                                                    ( Nothing, model.clothesInCloset, ( clickedFabric, clickedPart ) :: model.clothesBeingWorn )

                                                else
                                                    ( Nothing
                                                    , ( clickedFabric, clickedPart ) :: model.clothesInCloset
                                                    , List.filter (\( _, p ) -> p /= clickedPart) model.clothesBeingWorn
                                                    )

                                            Just c ->
                                                ( Nothing
                                                , ( c, clickedPart ) :: model.clothesInCloset
                                                , List.filter (\( _, p ) -> p /= clickedPart) model.clothesBeingWorn
                                                )

                        _ ->
                            ( model.selectedFabric, model.clothesInCloset, model.clothesBeingWorn )

                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { model
                | playground = playground
                , clothesInCloset = clothesInCloset
                , clothesBeingWorn = clothesBeingWorn
                , selectedFabric = selectedFabric
              }
            , Cmd.map PlaygroundMsg cmd
            )


updatePlayground computer memory =
    memory
