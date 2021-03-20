module Playground exposing
    ( componentInit, componentView, componentUpdate, componentSubscriptions, Game, Msg(..)
    , Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon, drawing
    , words
    , image
    , move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY
    , scale, rotate, fade, tilt, turn
    , group, clickableGroup
    , Time, spin, wave, zigzag
    , Computer, Mouse, Screen, Keyboard, toX, toY, toXY
    , Color, rgb, red, orange, yellow, green, blue, purple, brown
    , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
    , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
    , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
    , lightGray, gray, darkGray
    , Number
    , renderColor
    )

{-|


# Playgrounds


# Playground component parts

@docs componentInit, componentView, componentUpdate, componentSubscriptions, Game, Msg


# Shapes

@docs Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon, drawing


# Words

@docs words


# Images

@docs image


# Move Shapes

@docs move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY


# Customize Shapes

@docs scale, rotate, fade, tilt, turn


# Groups

@docs group, clickableGroup


# Time

@docs Time, spin, wave, zigzag


# Computer

@docs Computer, Mouse, Screen, Keyboard, toX, toY, toXY


# Colors

@docs Color, rgb, red, orange, yellow, green, blue, purple, brown


### Light Colors

@docs lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown


### Dark Colors

@docs darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown


### Shades of Grey

@docs white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black


### Alternate Spellings of Gray

@docs lightGray, gray, darkGray


### Numbers

@docs Number

-}

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html
import Html.Attributes as H
import Json.Decode as D
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Task
import Time



-- COMPUTER


{-| When writing a [`game`](#game), you can look up all sorts of information
about your computer:

  - [`Mouse`](#Mouse) - Where is the mouse right now?
  - [`Keyboard`](#Keyboard) - Are the arrow keys down?
  - [`Screen`](#Screen) - How wide is the screen?
  - [`Time`](#Time) - What time is it right now?

So you can use expressions like `computer.mouse.x` and `computer.keyboard.enter`
in games where you want some mouse or keyboard interaction.

-}
type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }



-- MOUSE


{-| Figure out what is going on with the mouse.

You could draw a circle around the mouse with a program like this:

    import Playground exposing (..)

    main =
        game view update 0

    view computer memory =
        [ circle yellow 40
            |> moveX computer.mouse.x
            |> moveY computer.mouse.y
        ]

    update computer memory =
        memory

You could also use `computer.mouse.down` to change the color of the circle
while the mouse button is down.

-}
type alias Mouse =
    { x : Number
    , y : Number
    , down : Bool
    , click : Bool
    }


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float



-- KEYBOARD


{-| Figure out what is going on with the keyboard.

If someone is pressing the UP and RIGHT arrows, you will see a value like this:

    { up = True
    , down = False
    , left = False
    , right = True
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.fromList [ "ArrowUp", "ArrowRight" ]
    }

So if you want to move a character based on arrows, you could write an update
like this:

    update computer y =
        if computer.keyboard.up then
            y + 1

        else
            y

Check out [`toX`](#toX) and [`toY`](#toY) which make this even easier!

**Note:** The `keys` set will be filled with the name of all keys which are
down right now. So you will see things like `"a"`, `"b"`, `"c"`, `"1"`, `"2"`,
`"Space"`, and `"Control"` in there. Check out [this list][list] to see the
names used for all the different special keys! From there, you can use
[`Set.member`][member] to check for whichever key you want. E.g.
`Set.member "Control" computer.keyboard.keys`.

[list]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
[member]: /packages/elm/core/latest/Set#member

-}
type alias Keyboard =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , enter : Bool
    , shift : Bool
    , backspace : Bool
    , keys : Set.Set String
    }


{-| Turn the LEFT and RIGHT arrows into a number.

    toX { left = False, right = False, ... } == 0
    toX { left = True , right = False, ... } == -1
    toX { left = False, right = True , ... } == 1
    toX { left = True , right = True , ... } == 0

So to make a square move left and right based on the arrow keys, we could say:

    import Playground exposing (..)

    main =
        game view update 0

    view computer x =
        [ square green 40
            |> moveX x
        ]

    update computer x =
        x + toX computer.keyboard

-}
toX : Keyboard -> Number
toX keyboard =
    (if keyboard.right then
        1

     else
        0
    )
        - (if keyboard.left then
            1

           else
            0
          )


{-| Turn the UP and DOWN arrows into a number.

    toY { up = False, down = False, ... } == 0
    toY { up = True , down = False, ... } == 1
    toY { up = False, down = True , ... } == -1
    toY { up = True , down = True , ... } == 0

This can be used to move characters around in games just like [`toX`](#toX):

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square blue 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

-}
toY : Keyboard -> Number
toY keyboard =
    (if keyboard.up then
        1

     else
        0
    )
        - (if keyboard.down then
            1

           else
            0
          )


{-| If you just use `toX` and `toY`, you will move diagonal too fast. You will go
right at 1 pixel per update, but you will go up/right at 1.41421 pixels per
update.

So `toXY` turns the arrow keys into an `(x,y)` pair such that the distance is
normalized:

    toXY { up = True , down = False, left = False, right = False, ... } == (1, 0)
    toXY { up = True , down = False, left = False, right = True , ... } == (0.707, 0.707)
    toXY { up = False, down = False, left = False, right = True , ... } == (0, 1)

Now when you go up/right, you are still going 1 pixel per update.

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square green 40
            |> move x y
        ]

    update computer ( x, y ) =
        let
            ( dx, dy ) =
                toXY computer.keyboard
        in
        ( x + dx, y + dy )

-}
toXY : Keyboard -> ( Number, Number )
toXY keyboard =
    let
        x =
            toX keyboard

        y =
            toY keyboard
    in
    if x /= 0 && y /= 0 then
        ( x / squareRootOfTwo, y / squareRootOfTwo )

    else
        ( x, y )


squareRootOfTwo : Number
squareRootOfTwo =
    sqrt 2



-- SCREEN


{-| Get the dimensions of the screen. If the screen is 800 by 600, you will see
a value like this:

    { width = 800
    , height = 600
    , top = 300
    , left = -400
    , right = 400
    , bottom = -300
    }

This can be nice when used with [`moveY`](#moveY) if you want to put something
on the bottom of the screen, no matter the dimensions.

-}
type alias Screen =
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }



-- TIME


{-| The current time.

Helpful when making an [`animation`](#animation) with functions like
[`spin`](#spin), [`wave`](#wave), and [`zigzag`](#zigzag).

-}
type Time
    = Time Time.Posix


{-| Create an angle that cycles from 0 to 360 degrees over time.

Here is an [`animation`](#animation) with a spinning triangle:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ triangle orange 50
            |> rotate (spin 8 time)
        ]

It will do a full rotation once every eight seconds. Try changing the `8` to
a `2` to make it do a full rotation every two seconds. It moves a lot faster!

-}
spin : Number -> Time -> Number
spin period time =
    360 * toFrac period time


{-| Smoothly wave between two numbers.

Here is an [`animation`](#animation) with a circle that resizes:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ circle lightBlue (wave 50 90 7 time)
        ]

The radius of the circle will cycles between 50 and 90 every seven seconds.
It kind of looks like it is breathing.

-}
wave : Number -> Number -> Number -> Time -> Number
wave lo hi period time =
    lo + (hi - lo) * (1 + cos (turns (toFrac period time))) / 2


{-| Zig zag between two numbers.

Here is an [`animation`](#animation) with a rectangle that tips back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ rectangle lightGreen 20 100
            |> rotate (zigzag -20 20 4 time)
        ]

It gets rotated by an angle. The angle cycles from -20 degrees to 20 degrees
every four seconds.

-}
zigzag : Number -> Number -> Number -> Time -> Number
zigzag lo hi period time =
    lo + (hi - lo) * abs (2 * toFrac period time - 1)


toFrac : Float -> Time -> Float
toFrac period (Time posix) =
    let
        ms =
            Time.posixToMillis posix

        p =
            period * 1000
    in
    toFloat (modBy (round p) ms) / p


componentInit : memory -> ( Game memory, Cmd (Msg msg) )
componentInit initialMemory =
    ( Game E.Visible initialMemory initialComputer
    , Task.perform GotViewport Dom.getViewport
    )


componentView : Game memory -> (Computer -> memory -> List (Shape (Msg msg))) -> Html.Html (Msg msg)
componentView (Game _ memory computer) viewMemory =
    render computer.screen (viewMemory computer memory)


componentUpdate : (Computer -> memory -> memory) -> Msg msg -> Game memory -> ( Game memory, Cmd (Msg msg) )
componentUpdate updateMemory msg model =
    ( gameUpdate updateMemory msg model
    , Cmd.none
    )


componentSubscriptions : Game memory -> Sub (Msg msg)
componentSubscriptions (Game visibility _ _) =
    case visibility of
        E.Hidden ->
            E.onVisibilityChange VisibilityChanged

        E.Visible ->
            gameSubscriptions


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard = emptyKeyboard
    , screen = toScreen 600 600
    , time = Time (Time.millisToPosix 0)
    }



-- SUBSCRIPTIONS


gameSubscriptions : Sub (Msg msg)
gameSubscriptions =
    Sub.batch
        [ E.onResize Resized
        , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , E.onAnimationFrame Tick
        , E.onVisibilityChange VisibilityChanged
        , E.onClick (D.succeed MouseClick)
        , E.onMouseDown (D.succeed (MouseButton True))
        , E.onMouseUp (D.succeed (MouseButton False))
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        ]



-- GAME HELPERS


type Game memory
    = Game E.Visibility memory Computer


type Msg clickPayload
    = KeyChanged Bool String
    | Tick Time.Posix
    | GotViewport Dom.Viewport
    | Resized Int Int
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool
    | Clicked clickPayload


gameUpdate : (Computer -> memory -> memory) -> Msg msg -> Game memory -> Game memory
gameUpdate updateMemory msg (Game vis memory computer) =
    case msg of
        Tick time ->
            Game vis (updateMemory computer memory) <|
                if computer.mouse.click then
                    { computer | time = Time time, mouse = mouseClick False computer.mouse }

                else
                    { computer | time = Time time }

        GotViewport { viewport } ->
            Game vis memory { computer | screen = toScreen viewport.width viewport.height }

        Resized w h ->
            Game vis memory { computer | screen = toScreen (toFloat w) (toFloat h) }

        KeyChanged isDown key ->
            Game vis memory { computer | keyboard = updateKeyboard isDown key computer.keyboard }

        MouseMove pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top - pageY
            in
            Game vis memory { computer | mouse = mouseMove x y computer.mouse }

        MouseClick ->
            Game vis memory { computer | mouse = mouseClick True computer.mouse }

        MouseButton isDown ->
            Game vis memory { computer | mouse = mouseDown isDown computer.mouse }

        VisibilityChanged visibility ->
            Game visibility
                memory
                { computer
                    | keyboard = emptyKeyboard
                    , mouse = Mouse computer.mouse.x computer.mouse.y False False
                }

        Clicked m ->
            Game vis memory computer



-- SCREEN HELPERS


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }



-- MOUSE HELPERS


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
    { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
    { mouse | down = bool }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | x = x, y = y }



-- KEYBOARD HELPERS


emptyKeyboard : Keyboard
emptyKeyboard =
    { up = False
    , down = False
    , left = False
    , right = False
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.empty
    }


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
    let
        keys =
            if isDown then
                Set.insert key keyboard.keys

            else
                Set.remove key keyboard.keys
    in
    case key of
        " " ->
            { keyboard | keys = keys, space = isDown }

        "Enter" ->
            { keyboard | keys = keys, enter = isDown }

        "Shift" ->
            { keyboard | keys = keys, shift = isDown }

        "Backspace" ->
            { keyboard | keys = keys, backspace = isDown }

        "ArrowUp" ->
            { keyboard | keys = keys, up = isDown }

        "ArrowDown" ->
            { keyboard | keys = keys, down = isDown }

        "ArrowLeft" ->
            { keyboard | keys = keys, left = isDown }

        "ArrowRight" ->
            { keyboard | keys = keys, right = isDown }

        _ ->
            { keyboard | keys = keys }



-- SHAPES


{-| Shapes help you make a `picture`, `animation`, or `game`.

Read on to see examples of [`circle`](#circle), [`rectangle`](#rectangle),
[`words`](#words), [`image`](#image), and many more!

-}
type Shape msg
    = Shape
        Number
        -- x
        Number
        -- y
        Number
        -- angle
        Number
        -- scale
        Number
        -- alpha
        Number
        -- tilt
        Number
        -- turn
        (Form msg)


type Form msg
    = Circle Color Number
    | Oval Color Number Number
    | Rectangle Color Number Number
    | Ngon Color Int Number
    | Polygon Color (List ( Number, Number ))
    | Image Number Number String
    | Words Color String
    | Group (List (Shape msg))
    | ClickableGroup msg (List (Shape msg))
    | Drawing (Svg msg)


{-| Make circles:

    dot =
        circle red 10

    sun =
        circle yellow 300

You give a color and then the radius. So the higher the number, the larger
the circle.

-}
circle : Color -> Number -> Shape msg
circle color radius =
    Shape 0 0 0 1 1 0 0 (Circle color radius)


{-| Make ovals:

    football =
        oval brown 200 100

You give the color, and then the width and height. So our `football` example
is 200 pixels wide and 100 pixels tall.

-}
oval : Color -> Number -> Number -> Shape msg
oval color width height =
    Shape 0 0 0 1 1 0 0 (Oval color width height)


{-| Make squares. Here are two squares combined to look like an empty box:

    import Playground exposing (..)

    main =
        picture
            [ square purple 80
            , square white 60
            ]

The number you give is the dimension of each side. So that purple square would
be 80 pixels by 80 pixels.

-}
square : Color -> Number -> Shape msg
square color n =
    Shape 0 0 0 1 1 0 0 (Rectangle color n n)


{-| Make rectangles. This example makes a red cross:

    import Playground exposing (..)

    main =
        picture
            [ rectangle red 20 60
            , rectangle red 60 20
            ]

You give the color, width, and then height. So the first shape is vertical
part of the cross, the thinner and taller part.

-}
rectangle : Color -> Number -> Number -> Shape msg
rectangle color width height =
    Shape 0 0 0 1 1 0 0 (Rectangle color width height)


{-| Make triangles. So if you wanted to draw the Egyptian pyramids, you could
do a simple version like this:

    import Playground exposing (..)

    main =
        picture
            [ triangle darkYellow 200
            ]

The number is the "radius", so the distance from the center to each point of
the pyramid is `200`. Pretty big!

-}
triangle : Color -> Number -> Shape msg
triangle color radius =
    Shape 0 0 0 1 1 0 0 (Ngon color 3 radius)


{-| Make pentagons:

    import Playground exposing (..)

    main =
        picture
            [ pentagon darkGrey 100
            ]

You give the color and then the radius. So the distance from the center to each
of the five points is 100 pixels.

-}
pentagon : Color -> Number -> Shape msg
pentagon color radius =
    Shape 0 0 0 1 1 0 0 (Ngon color 5 radius)


{-| Make hexagons:

    import Playground exposing (..)

    main =
        picture
            [ hexagon lightYellow 50
            ]

The number is the radius, the distance from the center to each point.

If you made more hexagons, you could [`move`](#move) them around to make a
honeycomb pattern!

-}
hexagon : Color -> Number -> Shape msg
hexagon color radius =
    Shape 0 0 0 1 1 0 0 (Ngon color 6 radius)


{-| Make octogons:

    import Playground exposing (..)

    main =
        picture
            [ octagon red 100
            ]

You give the color and radius, so each point of this stop sign is 100 pixels
from the center.

-}
octagon : Color -> Number -> Shape msg
octagon color radius =
    Shape 0 0 0 1 1 0 0 (Ngon color 8 radius)


{-| Make any shape you want! Here is a very thin triangle:

    import Playground exposing (..)

    main =
        picture
            [ polygon black [ ( -10, -20 ), ( 0, 100 ), ( 10, -20 ) ]
            ]

**Note:** If you [`rotate`](#rotate) a polygon, it will always rotate around
`(0,0)`. So it is best to build your shapes around that point, and then use
[`move`](#move) or [`group`](#group) so that rotation makes more sense.

-}
polygon : Color -> List ( Number, Number ) -> Shape msg
polygon color points =
    Shape 0 0 0 1 1 0 0 (Polygon color points)


{-| Add some image from the internet:

    import Playground exposing (..)

    main =
        picture
            [ image 96 96 "https://elm-lang.org/images/turtle.gif"
            ]

You provide the width, height, and then the URL of the image you want to show.

-}
image : Number -> Number -> String -> Shape msg
image w h src =
    Shape 0 0 0 1 1 0 0 (Image w h src)


{-| Show some words!

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello! How are you?"
            ]

You can use [`scale`](#scale) to make the words bigger or smaller.

-}
words : Color -> String -> Shape msg
words color string =
    Shape 0 0 0 1 1 0 0 (Words color string)


{-| Put shapes together so you can [`move`](#move) and [`rotate`](#rotate)
them as a group. Maybe you want to put a bunch of stars in the sky:

    import Playground exposing (..)

    main =
        picture
            [ star
                |> move 100 100
                |> rotate 5
            , star
                |> move -120 40
                |> rotate 20
            , star
                |> move 80 -150
                |> rotate 32
            , star
                |> move -90 -30
                |> rotate -16
            ]

    star =
        group
            [ triangle yellow 20
            , triangle yellow 20
                |> rotate 180
            ]

-}
group : List (Shape msg) -> Shape msg
group shapes =
    Shape 0 0 0 1 1 0 0 (Group shapes)


clickableGroup : msg -> List (Shape msg) -> Shape msg
clickableGroup msg shapes =
    Shape 0 0 0 1 1 0 0 (ClickableGroup msg shapes)


drawing : Svg msg -> Shape msg
drawing d =
    Shape 0 0 0 1 1 0 0 (Drawing d)



-- TRANSFORMS


{-| Move a shape by some number of pixels:

    import Playground exposing (..)

    main =
        picture
            [ square red 100
                |> move -60 60
            , square yellow 100
                |> move 60 60
            , square green 100
                |> move 60 -60
            , square blue 100
                |> move -60 -60
            ]

-}
move : Number -> Number -> Shape msg -> Shape msg
move dx dy (Shape x y a s o ti tu f) =
    Shape (x + dx) (y + dy) a s o ti tu f


{-| Move a shape up by some number of pixels. So if you wanted to make a tree
you could move the leaves up above the trunk:

    import Playground exposing (..)

    main =
        picture
            [ rectangle brown 40 200
            , circle green 100
                |> moveUp 180
            ]

-}
moveUp : Number -> Shape msg -> Shape msg
moveUp =
    moveY


{-| Move a shape down by some number of pixels. So if you wanted to put the sky
above the ground, you could move the sky up and the ground down:

    import Playground exposing (..)

    main =
        picture
            [ rectangle lightBlue 200 100
                |> moveUp 50
            , rectangle lightGreen 200 100
                |> moveDown 50
            ]

-}
moveDown : Number -> Shape msg -> Shape msg
moveDown dy (Shape x y a s o ti tu f) =
    Shape x (y - dy) a s o ti tu f


{-| Move shapes to the left.

    import Playground exposing (..)

    main =
        picture
            [ circle yellow 10
                |> moveLeft 80
                |> moveUp 30
            ]

-}
moveLeft : Number -> Shape msg -> Shape msg
moveLeft dx (Shape x y a s o ti tu f) =
    Shape (x - dx) y a s o ti tu f


{-| Move shapes to the right.

    import Playground exposing (..)

    main =
        picture
            [ square purple 20
                |> moveRight 80
                |> moveDown 100
            ]

-}
moveRight : Number -> Shape msg -> Shape msg
moveRight =
    moveX


{-| Move the `x` coordinate of a shape by some amount. Here is a square that
moves back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square purple 20
            |> moveX (wave 4 -200 200 time)
        ]

Using `moveX` feels a bit nicer here because the movement may be positive or negative.

-}
moveX : Number -> Shape msg -> Shape msg
moveX dx (Shape x y a s o ti tu f) =
    Shape (x + dx) y a s o ti tu f


{-| Move the `y` coordinate of a shape by some amount. Maybe you want to make
grass along the bottom of the screen:

    import Playground exposing (..)

    main =
        game view update 0

    update computer memory =
        memory

    view computer count =
        [ rectangle green computer.screen.width 100
            |> moveY computer.screen.bottom
        ]

Using `moveY` feels a bit nicer when setting things relative to the bottom or
top of the screen, since the values are negative sometimes.

-}
moveY : Number -> Shape msg -> Shape msg
moveY dy (Shape x y a s o ti tu f) =
    Shape x (y + dy) a s o ti tu f


{-| Make a shape bigger or smaller. So if you wanted some [`words`](#words) to
be larger, you could say:

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello, nice to see you!"
                |> scale 3
            ]

-}
scale : Number -> Shape msg -> Shape msg
scale ns (Shape x y a s o ti tu f) =
    Shape x y a (s * ns) o ti tu f


{-| Rotate shapes in degrees.

    import Playground exposing (..)

    main =
        picture
            [ words black "These words are tilted!"
                |> rotate 10
            ]

The degrees go **counter-clockwise** to match the direction of the
[unit circle](https://en.wikipedia.org/wiki/Unit_circle).

-}
rotate : Number -> Shape msg -> Shape msg
rotate da (Shape x y a s o ti tu f) =
    Shape x y (a + da) s o ti tu f


{-| Fade a shape. This lets you make shapes see-through or even completely
invisible. Here is a shape that fades in and out:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square orange 30
        , square blue 200
            |> fade (zigzag 0 1 3 time)
        ]

The number has to be between `0` and `1`, where `0` is totally transparent
and `1` is completely solid.

-}
fade : Number -> Shape msg -> Shape msg
fade o (Shape x y a s _ ti tu f) =
    Shape x y a s o ti tu f


tilt : Number -> Shape msg -> Shape msg
tilt ti (Shape x y a s o _ tu f) =
    Shape x y a s o ti tu f


turn : Number -> Shape msg -> Shape msg
turn tu (Shape x y a s o ti _ f) =
    Shape x y a s o ti tu f



-- COLOR


{-| Represents a color.

The colors below, like `red` and `green`, come from the [Tango palette][tango].
It provides a bunch of aesthetically reasonable colors. Each color comes with a
light and dark version, so you always get a set like `lightYellow`, `yellow`,
and `darkYellow`.

[tango]: https://en.wikipedia.org/wiki/Tango_Desktop_Project

-}
type Color
    = Hex String
    | Rgb Int Int Int


{-| -}
lightYellow : Color
lightYellow =
    Hex "#fce94f"


{-| -}
yellow : Color
yellow =
    Hex "#edd400"


{-| -}
darkYellow : Color
darkYellow =
    Hex "#c4a000"


{-| -}
lightOrange : Color
lightOrange =
    Hex "#fcaf3e"


{-| -}
orange : Color
orange =
    Hex "#f57900"


{-| -}
darkOrange : Color
darkOrange =
    Hex "#ce5c00"


{-| -}
lightBrown : Color
lightBrown =
    Hex "#e9b96e"


{-| -}
brown : Color
brown =
    Hex "#c17d11"


{-| -}
darkBrown : Color
darkBrown =
    Hex "#8f5902"


{-| -}
lightGreen : Color
lightGreen =
    Hex "#8ae234"


{-| -}
green : Color
green =
    Hex "#73d216"


{-| -}
darkGreen : Color
darkGreen =
    Hex "#4e9a06"


{-| -}
lightBlue : Color
lightBlue =
    Hex "#729fcf"


{-| -}
blue : Color
blue =
    Hex "#3465a4"


{-| -}
darkBlue : Color
darkBlue =
    Hex "#204a87"


{-| -}
lightPurple : Color
lightPurple =
    Hex "#ad7fa8"


{-| -}
purple : Color
purple =
    Hex "#75507b"


{-| -}
darkPurple : Color
darkPurple =
    Hex "#5c3566"


{-| -}
lightRed : Color
lightRed =
    Hex "#ef2929"


{-| -}
red : Color
red =
    Hex "#cc0000"


{-| -}
darkRed : Color
darkRed =
    Hex "#a40000"


{-| -}
lightGrey : Color
lightGrey =
    Hex "#eeeeec"


{-| -}
grey : Color
grey =
    Hex "#d3d7cf"


{-| -}
darkGrey : Color
darkGrey =
    Hex "#babdb6"


{-| -}
lightCharcoal : Color
lightCharcoal =
    Hex "#888a85"


{-| -}
charcoal : Color
charcoal =
    Hex "#555753"


{-| -}
darkCharcoal : Color
darkCharcoal =
    Hex "#2e3436"


{-| -}
white : Color
white =
    Hex "#FFFFFF"


{-| -}
black : Color
black =
    Hex "#000000"



-- ALTERNATE SPELLING GREYS


{-| -}
lightGray : Color
lightGray =
    Hex "#eeeeec"


{-| -}
gray : Color
gray =
    Hex "#d3d7cf"


{-| -}
darkGray : Color
darkGray =
    Hex "#babdb6"



-- CUSTOM COLORS


{-| RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:

    brightBlue =
        rgb 18 147 216

    brightGreen =
        rgb 119 244 8

    brightPurple =
        rgb 94 28 221

Each number needs to be between 0 and 255.

It can be hard to figure out what numbers to pick, so try using a color picker
like [paletton] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/

-}
rgb : Number -> Number -> Number -> Color
rgb r g b =
    Rgb (colorClamp r) (colorClamp g) (colorClamp b)


colorClamp : Number -> Int
colorClamp number =
    clamp 0 255 (round number)



-- RENDER


render : Screen -> List (Shape msg) -> Html.Html msg
render screen shapes =
    let
        ( w, h ) =
            if screen.height > screen.width then
                ( 1000, 1000 * screen.height / screen.width )

            else
                ( 1000 * screen.width / screen.height, 1000 )

        x =
            -500

        y =
            -500
    in
    svg
        [ viewBox (String.fromFloat x ++ " " ++ String.fromFloat y ++ " " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%"
        ]
        (List.map renderShape shapes)



-- TODO try adding Svg.Lazy to renderShape
--


renderShape : Shape msg -> Svg msg
renderShape (Shape x y angle s alpha ti tu form) =
    case form of
        Circle color radius ->
            renderCircle color radius x y angle s alpha ti tu

        Oval color width height ->
            renderOval color width height x y angle s alpha ti tu

        Rectangle color width height ->
            renderRectangle color width height x y angle s alpha ti tu

        Ngon color n radius ->
            renderNgon color n radius x y angle s alpha ti tu

        Polygon color points ->
            renderPolygon color points x y angle s alpha ti tu

        Image width height src ->
            renderImage width height src x y angle s alpha ti tu

        Words color string ->
            renderWords color string x y angle s alpha ti tu

        Group shapes ->
            g (transform (renderTransform x y angle s) :: renderAlpha alpha ++ renderTransform3D ti tu)
                (List.map renderShape shapes)

        ClickableGroup msg shapes ->
            g (Svg.Events.onClick msg :: transform (renderTransform x y angle s) :: renderAlpha alpha ++ renderTransform3D ti tu)
                (List.map renderShape shapes)

        Drawing d ->
            g (transform (renderTransform x y angle s) :: renderAlpha alpha ++ renderTransform3D ti tu)
                [ d ]



-- RENDER CIRCLE AND OVAL


renderCircle : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderCircle color radius x y angle s alpha ti tu =
    Svg.circle
        (r (String.fromFloat radius)
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        []


renderOval : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderOval color width height x y angle s alpha ti tu =
    ellipse
        (rx (String.fromFloat (width / 2))
            :: ry (String.fromFloat (height / 2))
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        []



-- RENDER RECTANGLE AND IMAGE


renderRectangle : Color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderRectangle color w h x y angle s alpha ti tu =
    rect
        (width (String.fromFloat w)
            :: height (String.fromFloat h)
            :: fill (renderColor color)
            :: transform (renderRectTransform w h x y angle s ti tu)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        []


renderRectTransform : Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> String
renderRectTransform width height x y angle s ti tu =
    renderTransform x y angle s
        ++ " translate("
        ++ String.fromFloat (-width / 2)
        ++ ","
        ++ String.fromFloat (-height / 2)
        ++ ")"


renderImage : Number -> Number -> String -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderImage w h src x y angle s alpha ti tu =
    Svg.image
        (xlinkHref src
            :: width (String.fromFloat w)
            :: height (String.fromFloat h)
            :: transform (renderRectTransform w h x y angle s ti tu)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        []



-- RENDER NGON


renderNgon : Color -> Int -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderNgon color n radius x y angle s alpha ti tu =
    Svg.polygon
        (points (toNgonPoints 0 n radius "")
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        []


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n radius string =
    if i == n then
        string

    else
        let
            a =
                turns (toFloat i / toFloat n - 0.25)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")



-- RENDER POLYGON


renderPolygon : Color -> List ( Number, Number ) -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderPolygon color coordinates x y angle s alpha ti tu =
    Svg.polygon
        (points (List.foldl addPoint "" coordinates)
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        []


addPoint : ( Float, Float ) -> String -> String
addPoint ( x, y ) str =
    str ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ " "



-- RENDER WORDS


renderWords : Color -> String -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Svg msg
renderWords color string x y angle s alpha ti tu =
    text_
        (textAnchor "middle"
            :: dominantBaseline "central"
            :: fill (renderColor color)
            :: transform (renderTransform x y angle s)
            :: renderAlpha alpha
            ++ renderTransform3D ti tu
        )
        [ text string
        ]



-- RENDER COLOR


renderColor : Color -> String
renderColor color =
    case color of
        Hex str ->
            str

        Rgb r g b ->
            "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"



-- RENDER ALPHA


renderAlpha : Number -> List (Svg.Attribute msg)
renderAlpha alpha =
    if alpha == 1 then
        []

    else
        [ opacity (String.fromFloat (clamp 0 1 alpha)) ]



-- RENDER TRANFORMS


renderTransform : Number -> Number -> Number -> Number -> String
renderTransform x y a s =
    if a == 0 then
        if s == 1 then
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ")"

        else
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") scale(" ++ String.fromFloat s ++ ")"

    else if s == 1 then
        "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromFloat -a ++ ")"

    else
        "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromFloat -a ++ ") scale(" ++ String.fromFloat s ++ ")"


renderTransform3D : Number -> Number -> List (Svg.Attribute msg)
renderTransform3D ti tu =
    if tu == 0 then
        if ti == 0 then
            []

        else
            [ Svg.Attributes.style <| "transform: rotate3d(1, 0, 0, " ++ String.fromFloat ti ++ "deg)" ]

    else if ti == 0 then
        [ Svg.Attributes.style <| "transform: rotate3d(0, 1, 0, " ++ String.fromFloat tu ++ "deg)" ]

    else
        [ Svg.Attributes.style <| "transform: rotate3d(1, 0, 0, " ++ String.fromFloat ti ++ "deg) rotate3d(0, 1, 0, " ++ String.fromFloat tu ++ "deg)" ]
