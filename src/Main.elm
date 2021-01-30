module Main exposing (main)

import Browser
import Cartoon
import Html exposing (Html)
import Html.Attributes as Attr
import Playground exposing (..)


type alias Model =
    { playground : Playground.Game {}
    , parts : List Cartoon.Part
    }


type Msg
    = PlaygroundMsg Playground.Msg


main =
    let
        ( p, cmd ) =
            Playground.componentInit {}
    in
    Browser.document
        { init = \() -> ( { playground = p, parts = [] }, Cmd.map PlaygroundMsg cmd )
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
            [ Html.map PlaygroundMsg <| Playground.componentView model.playground (scene model.parts)
            ]
        ]
    }


scene ongedragen computer memory =
    let
        metKleren =
            List.filter
                (\part -> not <| List.member part ongedragen)
                Cartoon.allParts
    in
    [ bed, kast (kleding ongedragen), girl metKleren, rectangle red 10 10 ]


clicked memory =
    moveUp 50 <|
        words red <|
            case memory.clicked of
                Nothing ->
                    "No click"

                Just p ->
                    case p of
                        Cartoon.Skirt ->
                            "Skirt"

                        Cartoon.Shirt ->
                            "Shirt"

                        Cartoon.Boots ->
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


girl kledingSelectie =
    drawing (Cartoon.Girl kledingSelectie)
        |> moveLeft 300
        |> scale 1.2


kleding selectie =
    let
        alles =
            [ ( Cartoon.Skirt
              , drawing Cartoon.Skirt
                    |> moveLeft 50
                    |> moveUp 20
              )
            , ( Cartoon.Boots
              , drawing Cartoon.Boots
                    |> moveRight 100
                    |> moveUp 10
              )
            , ( Cartoon.Shirt
              , drawing Cartoon.Shirt
                    |> moveRight 50
                    |> moveUp 70
              )
            ]
    in
    group
        (List.filterMap
            (\( p, d ) ->
                if List.member p selectie then
                    Just d

                else
                    Nothing
            )
            alles
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
                parts =
                    case pmsg of
                        Playground.Clicked part ->
                            case part of
                                Cartoon.Girl _ ->
                                    Debug.log "closet" model.parts

                                _ ->
                                    if List.member part model.parts then
                                        List.filter (\p -> p /= part) model.parts

                                    else
                                        part :: model.parts

                        _ ->
                            model.parts

                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { model | playground = playground, parts = parts }, Cmd.map PlaygroundMsg cmd )


updatePlayground computer memory =
    memory
