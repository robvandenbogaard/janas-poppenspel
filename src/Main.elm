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
    , modus : Modus
    }


type Modus
    = Aankleden Expression
    | Schilderen Expression


type Expression
    = Assignment ClickableThing Expression
    | Value ClickableThing
    | Many (List Expression)
    | Zilch


type Msg
    = PlaygroundMsg (Playground.Msg (Cartoon.Part.Msg ClickableThing))


type ClickableThing
    = Schilderij
    | Walvis
    | Toekan
    | ToekanTak
    | AndereToekan
    | AndereToekanTak


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
                  , modus =
                        Aankleden <|
                            Assignment Schilderij
                                (Many
                                    [ Value Walvis
                                    , Value Toekan
                                    , Value ToekanTak
                                    , Value AndereToekan
                                    , Value AndereToekanTak
                                    ]
                                )
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
            , Playground.componentView model.playground (scene model.modus model.clothesInCloset model.clothesBeingWorn)
                |> Html.map PlaygroundMsg
            ]
        ]
    }


scene modus clothesInCloset clothesBeingWorn computer memory =
    case modus of
        Aankleden expressie ->
            [ schilderij expressie computer.time
            , bed
            , kast (kleding clothesInCloset)
            , slimmeJana clothesBeingWorn
            ]

        Schilderen expressie ->
            [ bed
            , kast (kleding clothesInCloset)
            , slimmeJana clothesBeingWorn
            , schilderij expressie computer.time |> scale 2 |> moveRight 100
            , programmeerVakMet expressie
            ]


programmeerVakMet expressie =
    programmeerBlokMet 0 expressie
        |> moveUp 360
        |> moveRight 115


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
                ( modus, clothesInCloset, clothesBeingWorn ) =
                    case pmsg of
                        Playground.Clicked (Cartoon.Part.ClickedGroup Schilderij) ->
                            case model.modus of
                                Aankleden code ->
                                    ( Schilderen code, model.clothesInCloset, model.clothesBeingWorn )

                                Schilderen code ->
                                    ( Aankleden code, model.clothesInCloset, model.clothesBeingWorn )

                        Playground.Clicked (Cartoon.Part.ClickedGroup clickedThing) ->
                            case model.modus of
                                Aankleden _ ->
                                    ( model.modus, model.clothesInCloset, model.clothesBeingWorn )

                                Schilderen code ->
                                    if bevat clickedThing code then
                                        ( Schilderen <| zonder clickedThing code
                                        , model.clothesInCloset
                                        , model.clothesBeingWorn
                                        )

                                    else
                                        ( Schilderen <| met clickedThing code
                                        , model.clothesInCloset
                                        , model.clothesBeingWorn
                                        )

                        Playground.Clicked (Cartoon.Part.Clicked clickedFabric clickedPart) ->
                            case model.modus of
                                Aankleden _ ->
                                    case clickedPart of
                                        Cartoon.Part.Patch ->
                                            ( model.modus, model.clothesInCloset, model.clothesBeingWorn )

                                        Cartoon.Part.Girl _ ->
                                            ( model.modus, model.clothesInCloset, model.clothesBeingWorn )

                                        _ ->
                                            if List.member ( clickedFabric, clickedPart ) model.clothesInCloset then
                                                ( model.modus
                                                , List.filter (\c -> c /= ( clickedFabric, clickedPart )) model.clothesInCloset
                                                , Cartoon.addClothes ( clickedFabric, clickedPart ) model.clothesBeingWorn
                                                )

                                            else
                                                ( model.modus
                                                , ( clickedFabric, clickedPart ) :: model.clothesInCloset
                                                , List.filter (\( _, p ) -> p /= clickedPart) model.clothesBeingWorn
                                                )

                                _ ->
                                    ( model.modus, model.clothesInCloset, model.clothesBeingWorn )

                        _ ->
                            ( model.modus, model.clothesInCloset, model.clothesBeingWorn )

                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { model
                | playground = playground
                , clothesInCloset = clothesInCloset
                , clothesBeingWorn = clothesBeingWorn
                , modus = modus
              }
            , Cmd.map PlaygroundMsg cmd
            )


updatePlayground computer memory =
    memory
