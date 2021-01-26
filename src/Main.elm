module Main exposing (main)

import Browser
import Cartoon
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Playground exposing (..)


type alias Model =
    { playground : Playground.Game {} }


type Msg
    = PlaygroundMsg Playground.Msg


main =
    let
        ( p, cmd ) =
            Playground.componentInit {}
    in
    Browser.document
        { init = \() -> ( { playground = p }, Cmd.map PlaygroundMsg cmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


dark color =
    let
        hsla =
            Color.toHsla color
    in
    Color.fromHsla { hsla | lightness = hsla.lightness * 0.5 }


pink =
    Color.rgb 1.0 0.9 0.9


view : Model -> Browser.Document Msg
view model =
    { title = "Jana's Poppenspel"
    , body =
        [ background pink
            [ Html.map PlaygroundMsg <| Playground.componentView model.playground scene
            , girl
            , menu
            , Html.nav
                [ Attr.style "position" "absolute"
                , Attr.style "width" "6rem"
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
            ]
        ]
    }


scene computer memory =
    [ bed ]


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
        |> moveDown 200


girl =
    Cartoon.girl Cartoon.allParts Cartoon.colors.girl


menu =
    Html.nav
        []
        []


background color =
    Html.div
        [ Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "background-color" (Color.toCssString color)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaygroundMsg pmsg ->
            let
                ( playground, cmd ) =
                    Playground.componentUpdate updatePlayground pmsg model.playground
            in
            ( { model | playground = playground }, Cmd.map PlaygroundMsg cmd )


updatePlayground computer memory =
    memory
