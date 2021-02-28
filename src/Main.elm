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
                  , clothesInCloset = wardrobe
                  , clothesBeingWorn =
                        [ ( Flower, Cartoon.Part.Skirt )
                        , ( Solid "#7755CC", Cartoon.Part.Shirt )
                        ]
                  }
                , Cmd.map PlaygroundMsg cmd
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


wardrobe =
    List.concatMap (\f -> List.map (Tuple.pair f) Cartoon.Part.list) Cartoon.Fabric.list


pink =
    rgb 255 235 235


view : Model -> Browser.Document Msg
view model =
    { title = "Jana's Poppenspel"
    , body =
        [ background pink
            [ Html.h1
                [ Attr.style "color" "salmon"
                , Attr.style "position" "fixed"
                , Attr.style "margin-left" "1em"
                ]
                [ Html.text "Jana's poppenspel" ]
            , Playground.componentView model.playground (scene model.clothesInCloset model.clothesBeingWorn)
                |> Html.map PlaygroundMsg
            ]
        ]
    }


scene clothesInCloset clothesBeingWorn computer memory =
    [ bed, kast (kleding clothesInCloset), girl clothesBeingWorn, rectangle red 1 1 ]


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


darkerBrown =
    rgb 70 35 30


kast inhoud =
    group
        [ rectangle brown 400 500
        , group [ oval brown 400 100, oval pink 70 30 |> moveUp 50 ]
            |> moveUp 250
        , rectangle darkBrown 380 480
        , rectangle darkerBrown 320 420
            |> moveUp 5
        , inhoud
        ]
        |> moveRight 300
        |> moveUp 120


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
    let
        ox =
            8
    in
    group
        (List.foldl
            (\( fabric, part ) ( i, parts ) ->
                case part of
                    Cartoon.Part.Skirt ->
                        ( i + 1
                        , (drawing (Cartoon.drawing ( fabric, Cartoon.Part.Skirt ))
                            |> moveLeft (50 + i * ox)
                            |> moveUp 20
                          )
                            :: parts
                        )

                    Cartoon.Part.Boots ->
                        ( i + 1
                        , (drawing (Cartoon.drawing ( fabric, Cartoon.Part.Boots ))
                            |> moveRight (100 + i * ox)
                            |> moveUp 10
                          )
                            :: parts
                        )

                    Cartoon.Part.Shirt ->
                        ( i + 1
                        , (drawing (Cartoon.drawing ( fabric, Cartoon.Part.Shirt ))
                            |> moveRight (50 + i * ox)
                            |> moveUp 70
                          )
                            :: parts
                        )

                    _ ->
                        ( i, parts )
            )
            ( 0, [] )
            selectie
            |> Tuple.second
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
        [ Attr.style "font-family" "girlNextDoor"
        , Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "background-color" (renderColor color)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaygroundMsg pmsg ->
            let
                ( clothesInCloset, clothesBeingWorn ) =
                    case pmsg of
                        Playground.Clicked (Cartoon.Part.Clicked clickedFabric clickedPart) ->
                            case clickedPart of
                                Cartoon.Part.Patch ->
                                    ( model.clothesInCloset, model.clothesBeingWorn )

                                Cartoon.Part.Girl _ ->
                                    ( model.clothesInCloset, model.clothesBeingWorn )

                                _ ->
                                    if List.member ( clickedFabric, clickedPart ) model.clothesInCloset then
                                        ( List.filter (\c -> c /= ( clickedFabric, clickedPart )) model.clothesInCloset
                                        , Cartoon.addClothes ( clickedFabric, clickedPart ) model.clothesBeingWorn
                                        )

                                    else
                                        ( ( clickedFabric, clickedPart ) :: model.clothesInCloset
                                        , List.filter (\( _, p ) -> p /= clickedPart) model.clothesBeingWorn
                                        )

                        _ ->
                            ( model.clothesInCloset, model.clothesBeingWorn )

                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { model
                | playground = playground
                , clothesInCloset = clothesInCloset
                , clothesBeingWorn = clothesBeingWorn
              }
            , Cmd.map PlaygroundMsg cmd
            )


updatePlayground computer memory =
    memory
