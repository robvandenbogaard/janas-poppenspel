module Main exposing (main)

import Browser
import Cartoon
import Cartoon.Fabric exposing (Fabric(..))
import Cartoon.Part exposing (Part)
import Html exposing (Html)
import Html.Attributes as Attr
import Playground exposing (..)
import Zomaar


type alias Model =
    { playground : Playground.Game {}
    , clothesInCloset : List ( Fabric, Part )
    , clothesBeingWorn : List ( Fabric, Part )
    , kamer : Kamer
    , schilderij : Expression
    , pruiken : Expression
    }


type Kamer
    = Slaapkamer
    | Atelier
    | Haarstudio


type Expression
    = Assignment ClickableThing Expression
    | Value ClickableThing
    | Many (List Expression)
    | Zilch


type Msg
    = PlaygroundMsg (Playground.Msg (Cartoon.Part.Msg ClickableThing))


type ClickableThing
    = Schilderij
    | Pruiken
    | Walvis
    | Toekan
    | ToekanTak
    | AndereToekan
    | AndereToekanTak
    | GaNaar Kamer


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
                  , kamer = Slaapkamer
                  , schilderij =
                        Assignment Schilderij
                            (Many
                                [ Value Walvis
                                , Value Toekan
                                , Value ToekanTak
                                , Value AndereToekan
                                , Value AndereToekanTak
                                ]
                            )
                  , pruiken =
                        Assignment Pruiken
                            (Many [])
                  }
                , Cmd.map PlaygroundMsg cmd
                )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.map PlaygroundMsg <| Playground.componentSubscriptions p
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
            , Playground.componentView model.playground (scene model)
                |> Html.map PlaygroundMsg
            ]
        ]
    }


scene model computer memory =
    let
        when x y =
            if x then
                y

            else
                identity

        atelier =
            clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| GaNaar Atelier)
                [ rectangle pink 1000 1000
                , schilderij model.schilderij computer.time
                    |> scale 2
                    |> moveRight 190
                    |> moveUp 50
                , laptop
                    |> moveDown 200
                ]

        slaapkamer =
            clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| GaNaar Slaapkamer)
                [ rectangle pink 1000 1000
                , bed
                , kast (kleding model.clothesInCloset)
                , schilderij model.schilderij computer.time
                ]

        haarstudio =
            clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| GaNaar Haarstudio)
                [ rectangle pink 1000 1000
                , stoel
                , koppen -- keuze (koppen |> met model.pruiken)
                ]

        kamerkiezer =
            group
                [ slaapkamer |> when (model.kamer == Slaapkamer) (fade 0.2)
                , haarstudio |> moveDown 1000 |> when (model.kamer == Haarstudio) (fade 0.2)
                , atelier |> moveDown 2000 |> when (model.kamer == Atelier) (fade 0.2)
                ]
                |> scale 0.2
                |> moveRight 600
                |> moveUp 300
    in
    case model.kamer of
        Atelier ->
            [ atelier
            , programmeerVakMet model.schilderij
                |> moveDown 200
            , slimmeJana model.clothesBeingWorn
                |> moveLeft 100
            , kamerkiezer
            ]

        Slaapkamer ->
            [ slaapkamer
            , slimmeJana model.clothesBeingWorn
            , kamerkiezer
            ]

        Haarstudio ->
            [ haarstudio
            , slimmeJana model.clothesBeingWorn
            , kamerkiezer
            ]


programmeerVakMet expressie =
    group
        [ rectangle darkGreen 450 250
        , programmeerBlokMet 0 expressie
        ]


programmeerBlokVastMet hoogte label =
    group
        [ rectangle darkBlue 150 50
        , words white label
        ]
        |> duwNeer hoogte


programmeerBlokMet hoogte expressie =
    case expressie of
        Assignment ding waarde ->
            group
                [ programmeerBlokVastMet hoogte <| naamVan ding ++ " ="
                , programmeerBlokMet (hoogte + 1) waarde
                ]
                |> duwNeer hoogte

        Value waarde ->
            clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup waarde)
                [ rectangle blue 150 50
                , words white <| naamVan waarde
                ]
                |> duwNeer hoogte

        Many moreExpressions ->
            group
                [ programmeerBlokVastMet hoogte "["
                , stapel (hoogte + 1) (List.map (programmeerBlokMet 0) moreExpressions)
                , programmeerBlokVastMet (hoogte + toFloat (List.length moreExpressions)) "]"
                ]

        Zilch ->
            group []


duwNeer hoogte =
    moveDown (60 * hoogte)


stapel hoogte blokken =
    group
        (List.indexedMap (\aantal blok -> blok |> moveDown (toFloat aantal * 60)) blokken)
        |> duwNeer hoogte


naamVan ding =
    case ding of
        Schilderij ->
            "schilderij"

        Pruiken ->
            "pruiken"

        Walvis ->
            "walvis"

        Toekan ->
            "toekan"

        ToekanTak ->
            "toekanTak"

        AndereToekan ->
            "andereToekan"

        AndereToekanTak ->
            "andereToekanTak"

        GaNaar kamer ->
            "gaNaar" ++ naamVanKamer kamer


naamVanKamer kamer =
    case kamer of
        Slaapkamer ->
            "slaapkamer"

        Haarstudio ->
            "haarstudio"

        Atelier ->
            "atelier"


bevat waarde expressie =
    case expressie of
        Assignment _ nogEenExpressie ->
            bevat waarde nogEenExpressie

        Value ookEenWaarde ->
            waarde == ookEenWaarde

        Many expressies ->
            [] /= List.filter (bevat waarde) expressies

        Zilch ->
            False


zonder waarde expressie =
    case expressie of
        Assignment ding nogEenExpressie ->
            if ding == waarde then
                Zilch

            else
                zonder waarde nogEenExpressie
                    |> Assignment ding

        Value ookEenWaarde ->
            if ookEenWaarde == waarde then
                Zilch

            else
                expressie

        Many expressies ->
            expressies
                |> List.map (zonder waarde)
                |> List.filter ((/=) Zilch)
                |> Many

        Zilch ->
            Zilch


met waarde expressie =
    case expressie of
        Assignment ding nogEenExpressie ->
            Assignment ding (met waarde nogEenExpressie)

        Value _ ->
            expressie

        Many expressies ->
            Many (Value waarde :: expressies)

        Zilch ->
            Zilch


laptop =
    group
        [ rectangle darkGray 500 50
            |> moveDown (25 + 150 + 1)
        , rectangle darkGrey 500 300
        , rectangle darkGreen 450 250
        ]


schilderij expressie tijd =
    let
        weglaten =
            group []
    in
    clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup Schilderij)
        [ rectangle darkBrown 340 290
        , rectangle white 300 250

        --, rectangle red 10 10
        , if bevat Walvis expressie then
            walvis
                |> moveDown 80
                |> moveRight 70

          else
            weglaten
        , group
            [ if bevat Toekan expressie then
                toekan
                    |> rotate (wave 0 10 10 tijd)

              else
                weglaten
            , if bevat ToekanTak expressie then
                toekanTak

              else
                weglaten
            ]
            |> moveUp 60
            |> moveLeft 133
        , group
            [ if bevat AndereToekan expressie then
                andereToekan

              else
                weglaten
            , if bevat AndereToekanTak expressie then
                andereToekanTak

              else
                weglaten
            ]
            |> moveUp 45
            |> moveRight 150
        ]
        |> scale 0.75
        |> moveUp 180
        |> moveLeft 300


walvis =
    drawing Zomaar.walvis
        |> moveUp 200
        |> moveLeft 200


toekan =
    drawing Zomaar.toekan
        |> moveUp 60
        |> moveLeft 30


toekanTak =
    drawing Zomaar.toekanTak
        |> moveUp 60
        |> moveLeft 30


andereToekan =
    drawing Zomaar.toekan2
        |> moveUp 70
        |> moveLeft 265


andereToekanTak =
    drawing Zomaar.toekanTak2
        |> moveUp 70
        |> moveLeft 265


stoel =
    group
        [ rectangle purple 10 100
        ]


koppen =
    group
        [ circle white 20 ]


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


slimmeJana =
    girl


girl kledingSelectie =
    Cartoon.drawing ( Cartoon.Fabric.Solid "brown", Cartoon.Part.Girl kledingSelectie )
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
                    , drawing (Cartoon.drawing ( fabric, Cartoon.Part.Patch ))
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
                newmodel =
                    case pmsg of
                        Playground.Clicked (Cartoon.Part.ClickedGroup clickedThing) ->
                            let
                                _ =
                                    Debug.log "playground clicked:" clickedThing
                            in
                            case clickedThing of
                                GaNaar kamer ->
                                    { model | kamer = kamer }

                                _ ->
                                    case model.kamer of
                                        Atelier ->
                                            if model.schilderij |> bevat clickedThing then
                                                { model
                                                    | schilderij = model.schilderij |> zonder clickedThing
                                                }

                                            else
                                                { model
                                                    | schilderij = model.schilderij |> met clickedThing
                                                }

                                        _ ->
                                            model

                        Playground.Clicked (Cartoon.Part.Clicked clickedFabric clickedPart) ->
                            case model.kamer of
                                Slaapkamer ->
                                    case clickedPart of
                                        Cartoon.Part.Patch ->
                                            model

                                        Cartoon.Part.Girl _ ->
                                            model

                                        _ ->
                                            if List.member ( clickedFabric, clickedPart ) model.clothesInCloset then
                                                { model
                                                    | clothesInCloset =
                                                        List.filter (\c -> c /= ( clickedFabric, clickedPart )) model.clothesInCloset
                                                    , clothesBeingWorn =
                                                        Debug.log "clothes" <|
                                                            Cartoon.addClothes ( clickedFabric, clickedPart ) model.clothesBeingWorn
                                                }

                                            else
                                                { model
                                                    | clothesInCloset =
                                                        ( clickedFabric, clickedPart ) :: model.clothesInCloset
                                                    , clothesBeingWorn =
                                                        List.filter (\( _, p ) -> p /= clickedPart) model.clothesBeingWorn
                                                }

                                _ ->
                                    model

                        _ ->
                            model

                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { newmodel
                | playground = playground
              }
            , Cmd.map PlaygroundMsg cmd
            )


updatePlayground computer memory =
    memory
