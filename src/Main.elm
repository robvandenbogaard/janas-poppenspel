module Main exposing (main)

import Browser
import Browser.Events
import Cartoon
import Cartoon.Doll as Doll
import Cartoon.Fabric exposing (Fabric(..))
import Cartoon.Part exposing (Part)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Playground exposing (..)
import Zomaar


type alias Model =
    { playground : Playground.Game {}
    , clothesInCloset : List ( Fabric, Part )
    , clothesBeingWorn : List ( Fabric, Part )
    , kamer : Kamer
    , schilderij : Expression
    , pruiken : Expression
    , pruik : Int
    , lipstick : Color
    , eyeshadow : Color
    , typen : Typen
    }


type alias Typen =
    { position : ( Int, Int, Int )
    , tekst : String
    }


type Kamer
    = Slaapkamer
    | Atelier
    | Haarstudio


type Expression
    = Definition ClickableThing (List Expression) Expression
    | Value ClickableThing
    | Many (List Expression)
    | Zilch
    | Application Expression Expression
    | Pipeline Expression Expression


type Msg
    = PlaygroundMsg (Playground.Msg (Cartoon.Part.Msg ClickableThing))
    | OnKey Key


type ClickableThing
    = Schilderij
    | Pruiken
    | Pruik Int
    | Lipstick Color
    | EyeShadow Color
    | Walvis
    | Toekan
    | ToekanTak
    | AndereToekan
    | AndereToekanTak
    | GaNaar Kamer
    | Beweeg
    | Tijd


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
                        Definition Schilderij
                            [ Value Tijd ]
                            (Many
                                [ Value Walvis
                                , Application (Value Beweeg) (Many [ Value Tijd, Value Toekan ])
                                , Pipeline (Value Toekan) (Application (Value Beweeg) (Value Tijd))
                                , Value ToekanTak
                                , Value AndereToekan
                                , Value AndereToekanTak
                                ]
                            )
                  , pruiken =
                        Definition Pruiken
                            []
                            (Many [])
                  , pruik = 1
                  , lipstick = red
                  , eyeshadow = black
                  , typen = Typen ( 0, 0, 0 ) ""
                  }
                , Cmd.map PlaygroundMsg cmd
                )
        , view = view
        , update = update
        , subscriptions = subscriptions p
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
                , Attr.style "margin" "0"
                , Attr.style "padding" "0.5em 1em 0 1em"
                , Attr.style "position" "fixed"
                , Attr.style "top" "0"
                ]
                [ Html.text "Jana's poppenspel" ]
            , Playground.componentView model.playground (scene model)
                |> Html.map PlaygroundMsg
            ]
        ]
    }


when x y =
    if x then
        y

    else
        identity


scene model computer memory =
    let
        kamer k =
            case k of
                Atelier ->
                    [ rectangle pink 1000 1000
                    , schilderij model.schilderij computer.time
                        |> scale 1.5
                        |> moveRight 100
                        |> moveUp 180
                    , laptop
                        |> moveDown 200
                    ]

                Slaapkamer ->
                    [ rectangle pink 1000 1000
                    , bed
                    , kast (kleding model.clothesInCloset)
                        |> moveRight 300
                        |> moveUp 120
                    , schilderij model.schilderij computer.time
                        |> scale 0.75
                        |> moveUp 180
                        |> moveLeft 300
                    ]

                Haarstudio ->
                    [ rectangle pink 1000 1000
                    , stoel
                    , makeUpDoos
                        |> moveUp 250
                        |> moveRight 200
                    , [ koppen
                      , tafel
                            |> moveDown 20
                      ]
                        |> group
                        |> moveRight 300

                    -- keuze (koppen |> met model.pruiken)
                    ]

        kamerkiezer =
            group
                [ [ kast (group []) ]
                    |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| GaNaar Slaapkamer)
                    |> when (model.kamer == Slaapkamer) (fade 0.2)
                , [ kop |> scale 5 |> moveUp 50 ]
                    |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| GaNaar Haarstudio)
                    |> moveDown 500
                    |> when (model.kamer == Haarstudio) (fade 0.2)
                , [ schilderij model.schilderij computer.time ]
                    |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| GaNaar Atelier)
                    |> moveDown 1000
                    |> when (model.kamer == Atelier) (fade 0.2)
                ]
                |> scale 0.25
                |> moveRight 600
                |> moveUp 300
    in
    case model.kamer of
        Atelier ->
            [ kamer Atelier
                |> group
            , programmeerVakMet model.schilderij model.typen computer.time
                |> moveDown 200
            , slimmeJana model.pruik model.lipstick model.eyeshadow model.clothesBeingWorn
                |> moveLeft 100
            , kamerkiezer
            ]

        Slaapkamer ->
            [ kamer Slaapkamer
                |> group
            , slimmeJana model.pruik model.lipstick model.eyeshadow model.clothesBeingWorn
            , kamerkiezer
            ]

        Haarstudio ->
            [ kamer Haarstudio
                |> group
            , slimmeJana model.pruik model.lipstick model.eyeshadow model.clothesBeingWorn
            , kamerkiezer
            ]


programmeerVakMet expressie typen tijd =
    group
        [ rectangle darkGreen 450 250
        , codeMet expressie ( ( 0, 0, 0 ), [] )
            |> plaatsCodeMetCursor typen.position tijd
            |> moveLeft 200
            |> moveUp 100
        ]


codeTekst tekst ( ( x, y, z ), blokken ) =
    let
        ( positie, positieNa ) =
            plaatsVoorEnNa tekst ( x, y, z )
    in
    ( positieNa, ( positie, code lightGreen tekst ) :: blokken )


codeWaarde waarde ( ( x, y, z ), blokken ) =
    let
        waardeNaam =
            naamVan waarde

        ( positie, positieNa ) =
            plaatsVoorEnNa waardeNaam ( x, y, z )
    in
    ( positieNa
    , ( positie
      , clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup waarde)
            [ code white waardeNaam ]
      )
        :: blokken
    )


codeBlokMet expressies ( ( x, y, z ), blokken ) =
    List.foldl codeMet ( ( x, y, z ), blokken ) expressies


codePipeline expressies ( ( x, y, z ), blokken ) =
    let
        pipeline expressie code =
            code
                |> meerInspringen
                |> codeTekst "|>"
                |> codeMet expressie
                |> minderInspringen
    in
    List.foldl pipeline
        ( ( x, y, z ), blokken )
        (case expressies of
            Many parameters ->
                parameters

            expressie ->
                [ expressie ]
        )


codeMet expressie ( ( x, y, z ), blokken ) =
    case expressie of
        Definition ding parameters waarde ->
            ( ( x, y, z ), blokken )
                |> codeTekst (naamVan ding)
                |> when (parameters /= []) (codeTekst "met")
                |> codeBlokMet parameters
                |> codeTekst "="
                |> codeMet waarde

        Value waarde ->
            ( ( x, y, z ), blokken )
                |> codeWaarde waarde

        Many moreExpressions ->
            ( ( x, y, z ), blokken )
                |> meerInspringen
                |> codeTekst "["
                |> codeBlokMet moreExpressions
                |> codeTekst "]"
                |> minderInspringen

        Zilch ->
            ( ( x, y, z ), blokken )

        Application functie parameterExpression ->
            let
                parameters =
                    case parameterExpression of
                        Many params ->
                            params

                        param ->
                            [ param ]
            in
            ( ( x, y, z ), blokken )
                |> codeTekst "("
                |> codeMet functie
                |> when (parameters /= []) (codeTekst "met")
                |> codeBlokMet parameters
                |> codeTekst ")"

        Pipeline expression parameters ->
            ( ( x, y, z ), blokken )
                |> codeTekst "("
                |> codeMet expression
                |> codePipeline parameters
                |> codeTekst ")"


plaatsVoorEnNa tekst ( x, y, z ) =
    let
        length =
            1 + String.length tekst
    in
    if x + length > 30 then
        ( ( 0, y + 1, z ), ( length, y + 1, z ) )

    else
        ( ( x, y, z ), ( x + length, y, z ) )


enter ( ( x, y, z ), code ) =
    ( ( 0, y + 1, z ), code )


meerInspringen ( ( x, y, z ), code ) =
    ( ( 0, y + 1, z + 1 ), code )


minderInspringen ( ( x, y, z ), code ) =
    ( ( 0
      , y + 1
      , if z > 0 then
            z - 1

        else
            0
      )
    , code
    )


plaats : ( Int, Int, Int ) -> Shape msg -> Shape msg
plaats ( x, y, z ) =
    move (20 * toFloat z + 12 * toFloat x) (-22 * toFloat y)


plaatsCodeEnVoegToe : ( ( Int, Int, Int ), Shape msg ) -> List (Shape msg) -> List (Shape msg)
plaatsCodeEnVoegToe ( ( x, y, z ), blok ) blokken =
    plaats ( x, y, z ) blok :: blokken


plaatsCode =
    Tuple.second >> List.foldl plaatsCodeEnVoegToe [] >> group


plaatsCodeMetCursor ( x, y, z ) tijd ( ( lx, ly, lz ), blokken ) =
    group
        (List.foldl plaatsCodeEnVoegToe [] blokken
            ++ [ rectangle lightGreen 12 20
                    |> moveRight 6
                    |> fade (zigzag 0 1 1 tijd)
                    |> plaats ( x, y, z )
               ]
        )


naamVan ding =
    case ding of
        Schilderij ->
            "schilderij"

        Pruiken ->
            "pruiken"

        Pruik i ->
            "pruik " ++ String.fromInt i

        Lipstick _ ->
            "lipstick"

        EyeShadow _ ->
            "eye shadow"

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

        Beweeg ->
            "beweeg"

        Tijd ->
            "tijd"


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
        Definition _ _ nogEenExpressie ->
            bevat waarde nogEenExpressie

        Value ookEenWaarde ->
            waarde == ookEenWaarde

        Many expressies ->
            [] /= List.filter (bevat waarde) expressies

        Zilch ->
            False

        Application nogEenExpressie parameters ->
            bevat waarde nogEenExpressie || bevat waarde parameters

        Pipeline nogEenExpressie parameters ->
            bevat waarde nogEenExpressie || bevat waarde parameters


zonder waarde expressie =
    case expressie of
        Definition ding parameters nogEenExpressie ->
            if ding == waarde then
                Zilch

            else
                zonder waarde nogEenExpressie
                    |> Definition ding parameters

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

        Application nogEenExpressie parameters ->
            let
                z =
                    zonder waarde nogEenExpressie
            in
            if z == Zilch then
                Zilch

            else
                Application z (zonder waarde parameters)

        Pipeline nogEenExpressie parameters ->
            let
                z =
                    zonder waarde nogEenExpressie
            in
            if z == Zilch then
                Zilch

            else
                Pipeline z (zonder waarde parameters)


met waarde expressie =
    case expressie of
        Definition ding parameters nogEenExpressie ->
            Definition ding parameters (met waarde nogEenExpressie)

        Value _ ->
            expressie

        Many expressies ->
            Many (Value waarde :: expressies)

        Zilch ->
            Zilch

        Application _ _ ->
            expressie

        Pipeline _ _ ->
            expressie


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
        [ oval purple 98 40 |> moveUp 40
        , rectangle purple 100 80
        , rectangle purple 100 20 |> moveDown 55
        , rectangle grey 20 100 |> moveDown 120
        , rectangle grey 100 20 |> moveDown 160
        ]


makeUpDoos =
    group
        [ rectangle brown 600 250
        , rectangle darkBrown 580 230
        , rectangle darkerBrown 570 220 |> move -1 -1
        , [ lipstick red
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| Lipstick red)
          , lipstick orange
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| Lipstick orange)
          , lipstick darkBlue
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| Lipstick darkBlue)
          ]
            |> List.indexedMap
                (\i shape ->
                    shape
                        |> scale 2
                        |> moveRight (toFloat i * 60)
                )
            |> group
            |> move 120 -30
        , [ [ eyeshadow black ]
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| EyeShadow black)
          , [ eyeshadow darkBlue ]
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| EyeShadow darkBlue)
          , [ eyeshadow lightBlue ]
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| EyeShadow lightBlue)
          , [ eyeshadow orange ]
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| EyeShadow orange)
          , [ eyeshadow green ]
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| EyeShadow green)
          , [ eyeshadow darkGreen ]
                |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| EyeShadow darkGreen)
          ]
            |> List.indexedMap
                (\i shape ->
                    shape
                        |> scale 4
                        |> move (toFloat (modBy 3 i * 90)) -(toFloat (i // 3 * 90))
                )
            |> group
            |> move -220 40
        ]


kop =
    group
        [ rectangle black 30 15 |> moveDown 52
        , rectangle white 29 15 |> moveDown 52
        , Doll.drawing ( Cartoon.Fabric.Solid "white", Cartoon.Part.Head ) |> drawing
        ]


koppen =
    group
        [ [ kop ]
            |> between (hairdo 0) identity
            |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| Pruik 0)
            |> moveLeft 100
        , [ kop ]
            |> between (hairdo 1) identity
            |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| Pruik 1)
            |> moveLeft 20
        , [ kop ]
            |> between (hairdo 2) identity
            |> clickableGroup (Playground.Clicked <| Cartoon.Part.ClickedGroup <| Pruik 2)
            |> moveRight 80
        ]


tafel =
    group
        [ rectangle grey 20 100 |> moveLeft 135
        , rectangle grey 20 100 |> moveRight 135
        , rectangle purple 300 20 |> moveUp 50
        ]
        |> moveDown 100


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


availableClothingVariants clothing =
    Cartoon.Fabric.list
        |> List.map (Tuple.pair clothing)


availableCothes =
    Cartoon.Part.list
        |> List.map availableClothingVariants
        |> List.concat


slimmeJana =
    girl


girl pruik lipcolor eyecolor kledingSelectie =
    [ Doll.drawing ( Cartoon.Fabric.Solid "brown", Cartoon.Part.Girl kledingSelectie )
        |> drawing
    , group
        [ eye eyecolor -0.1 2.5
            |> moveLeft 12
            |> rotate -5
        , eye eyecolor -0.5 3
            |> moveRight 12
            |> rotate 10
        ]
        |> moveUp 145
        |> moveRight 10
    , nose
        |> moveUp 140
        |> moveRight 10
    , mouth lipcolor
        |> moveUp 115
        |> moveRight 10

    --, baret
    --    |> rotate 10
    --    |> moveUp 180
    --    |> moveLeft 5
    ]
        |> between (hairdo pruik) (moveUp 155)
        |> group
        |> moveLeft 270
        |> scale 1.2


with something existing =
    existing ++ [ something ]


between ( back, front ) mutation existing =
    (mutation back :: existing) ++ [ mutation front ]


hairdo i =
    case i of
        0 ->
            ( group []
            , group
                [ baret |> move -5 15 |> rotate 10
                ]
            )

        1 ->
            ( group
                [ circle black 20
                    |> moveDown 35
                    |> moveRight 20
                , circle black 10
                    |> moveDown 55
                    |> moveRight 20
                , circle black 5
                    |> moveDown 65
                    |> moveRight 16
                , circle black 45
                    --oval black 90 80
                    |> moveRight 5
                ]
            , group
                [ oval black 60 23
                    |> moveUp 10
                    |> moveRight 2
                    |> rotate 2
                ]
            )

        2 ->
            ( group
                [ oval black 90 80
                    |> moveRight 5
                ]
            , group
                [ oval black 60 23
                    |> moveUp 10
                    |> moveRight 2
                    |> rotate 2
                ]
            )

        _ ->
            ( group []
            , group []
            )


baret =
    group
        [ oval lightBlue 60 25
            |> rotate 15
        , oval blue 6 15
            |> moveUp 9
            |> moveLeft 2
        ]


nose =
    group
        [ Cartoon.startFrom ( 0, 0 )
            |> Cartoon.cubicRel [ ( 0, 10 ), ( 3, 12 ), ( 3, 9 ) ]
            |> Cartoon.cubicRel [ ( 1, -1 ), ( 5, 4 ), ( 0, 7 ) ]
            |> Cartoon.line black
            |> drawing
        ]


mouth lipcolor =
    group
        [ oval lipcolor 15 8
        , group
            [ circle lipcolor 3.5
                |> moveLeft 3
            , circle lipcolor 3.5
                |> moveRight 3
            ]
            |> moveUp 3.5
        , oval darkRed 15 3
            |> moveUp 1
        ]


eye color lookUp lookRight =
    group
        [ eyeshadow color
        , if color == black then
            eyeball
                |> moveDown 0.3

          else
            oval white 20 9
                |> moveDown 0.8
        , group
            [ iris
            , pupil
            ]
            |> move lookRight lookUp
        ]
        |> scale 0.85


eyeshadow color =
    if color == black then
        oval black 20 12
            |> moveUp 0.5

    else
        oval color 20 15
            |> moveUp 1


eyeball =
    oval white 20 10


iris =
    circle blue 5


pupil =
    circle black 3.5


lipstick color =
    [ oval color 17 60
        |> moveUp 20
    , rectangle white 20 50
    ]


stof currentFabric fabric =
    case currentFabric of
        Nothing ->
            drawing (Doll.drawing ( fabric, Cartoon.Part.Patch ))

        Just f ->
            if f == fabric then
                group
                    [ drawing (Doll.drawing ( Cartoon.Fabric.Solid "salmon", Cartoon.Part.Patch ))
                        |> scale 1.1
                    , drawing (Doll.drawing ( fabric, Cartoon.Part.Patch ))
                    ]

            else
                drawing (Doll.drawing ( fabric, Cartoon.Part.Patch ))


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
                        , (drawing (Doll.drawing ( fabric, Cartoon.Part.Skirt ))
                            |> moveLeft (50 + i * ox)
                            |> moveUp 20
                          )
                            :: parts
                        )

                    Cartoon.Part.Boots ->
                        ( i + 1
                        , (drawing (Doll.drawing ( fabric, Cartoon.Part.Boots ))
                            |> moveRight (100 + i * ox)
                            |> moveUp 10
                          )
                            :: parts
                        )

                    Cartoon.Part.Shirt ->
                        ( i + 1
                        , (drawing (Doll.drawing ( fabric, Cartoon.Part.Shirt ))
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

                                Pruik i ->
                                    { model | pruik = i }

                                Lipstick c ->
                                    { model | lipstick = c }

                                EyeShadow c ->
                                    { model | eyeshadow = c }

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

                                        Cartoon.Part.Head ->
                                            model

                                        _ ->
                                            if List.member ( clickedFabric, clickedPart ) model.clothesInCloset then
                                                { model
                                                    | clothesInCloset =
                                                        List.filter (\c -> c /= ( clickedFabric, clickedPart )) model.clothesInCloset
                                                    , clothesBeingWorn =
                                                        Debug.log "clothes" <|
                                                            Doll.addClothes ( clickedFabric, clickedPart ) model.clothesBeingWorn
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

        OnKey key ->
            case key of
                Character char ->
                    let
                        ( x, y, z ) =
                            model.typen.position

                        position =
                            if z * 4 + x + 1 > 30 then
                                ( 0, y + 1, z )

                            else
                                ( x + 1, y, z )
                    in
                    ( { model | typen = Typen position (model.typen.tekst ++ String.fromChar char) }
                    , Cmd.none
                    )

                Control "Enter" ->
                    let
                        ( x, y, z ) =
                            model.typen.position

                        position =
                            ( 0, y + 1, z )
                    in
                    ( { model | typen = Typen position model.typen.tekst }
                    , Cmd.none
                    )

                Control "Tab" ->
                    let
                        ( x, y, z ) =
                            model.typen.position

                        position =
                            ( x, y, z + 1 )
                    in
                    ( { model | typen = Typen position model.typen.tekst }
                    , Cmd.none
                    )

                Control "Shift Tab" ->
                    let
                        ( x, y, z ) =
                            model.typen.position

                        position =
                            if z > 0 then
                                ( x, y, z - 1 )

                            else
                                ( x, y, z )
                    in
                    ( { model | typen = Typen position model.typen.tekst }
                    , Cmd.none
                    )

                Control "Backspace" ->
                    let
                        ( x, y, z ) =
                            model.typen.position

                        position =
                            if x > 0 then
                                ( x - 1, y, z )

                            else if z > 0 then
                                ( 0, y, z - 1 )

                            else if y > 0 then
                                ( x, y - 1, z )

                            else
                                ( 0, 0, 0 )
                    in
                    ( { model
                        | typen = Typen position (String.dropRight 1 model.typen.tekst)
                      }
                    , Cmd.none
                    )

                Control c ->
                    let
                        _ =
                            Debug.log "key" c
                    in
                    ( model, Cmd.none )


updatePlayground computer memory =
    memory


type Key
    = Character Char
    | Control String


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


subscriptions p model =
    Sub.batch
        [ Sub.map PlaygroundMsg <| Playground.componentSubscriptions p
        , Browser.Events.onKeyPress <| Decode.map OnKey keyDecoder
        ]
