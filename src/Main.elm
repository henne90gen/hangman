module Main exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (cx, cy, r, stroke, strokeWidth, viewBox, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


type Letter
    = Shown Char
    | Hidden Char


type AlphabetLetter
    = Used Char
    | Unused Char


type alias Model =
    { shownWord : List Letter
    , alphabet : List AlphabetLetter
    , errorCounter : Int
    , hasWon : Bool
    }


type Msg
    = NewGame
    | GuessLetter Char


init : flags -> ( Model, Cmd msg )
init _ =
    ( startNewGame
    , Cmd.none
    )


createAlphabet : String -> List AlphabetLetter
createAlphabet str =
    List.map createAlphabetLetter (String.toList str)


createAlphabetLetter : Char -> AlphabetLetter
createAlphabetLetter c =
    Unused c


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( startNewGame, Cmd.none )

        GuessLetter letter ->
            ( guessLetter model letter, Cmd.none )


startNewGame : Model
startNewGame =
    let
        newWord =
            "cookie"
    in
    { shownWord = List.map toLetter (String.toList newWord)
    , alphabet = createAlphabet "abcdefghijklmnopqrstuvwxyzäöüß"
    , errorCounter = 0
    , hasWon = False
    }


toLetter : Char -> Letter
toLetter c =
    Hidden c


guessLetter : Model -> Char -> Model
guessLetter model c =
    let
        ( hasFoundLetter, newShownWord ) =
            guessLetterShownWord model.shownWord c

        hasWon =
            checkForWin newShownWord
    in
    { shownWord = newShownWord
    , alphabet = guessLetterAlphabet model.alphabet c
    , errorCounter = updateErrorCounter model.errorCounter hasFoundLetter
    , hasWon = hasWon
    }


guessLetterShownWord : List Letter -> Char -> ( Bool, List Letter )
guessLetterShownWord shownWord c =
    let
        temp =
            List.map (matchLetter c) shownWord

        hasFoundLetter =
            List.foldl (\( b, _ ) r -> b || r) False temp

        result =
            List.foldl (\( _, letter ) r -> r ++ [ letter ]) [] temp
    in
    ( hasFoundLetter, result )


matchLetter : Char -> Letter -> ( Bool, Letter )
matchLetter matchChar letter =
    case letter of
        Hidden c ->
            if c == matchChar then
                ( True, Shown c )

            else
                ( False, Hidden c )

        Shown c ->
            ( False, Shown c )


guessLetterAlphabet : List AlphabetLetter -> Char -> List AlphabetLetter
guessLetterAlphabet alphabet c =
    List.map (guessSingleLetterAlphabet c) alphabet


guessSingleLetterAlphabet : Char -> AlphabetLetter -> AlphabetLetter
guessSingleLetterAlphabet c letter =
    case letter of
        Used a ->
            Used a

        Unused a ->
            if a == c then
                Used a

            else
                Unused a


checkForWin : List Letter -> Bool
checkForWin word =
    word
        |> List.map checkForWinLetter
        |> List.foldl (&&) True


checkForWinLetter : Letter -> Bool
checkForWinLetter letter =
    case letter of
        Shown _ ->
            True

        Hidden _ ->
            False


updateErrorCounter : Int -> Bool -> Int
updateErrorCounter errorCounter hasFoundLetter =
    if hasFoundLetter then
        errorCounter

    else
        errorCounter + 1


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Hangman"
    , body =
        [ viewNewGame
        , viewAlphabet model.alphabet model.hasWon
        , viewWord model.shownWord
        , viewErrorCounter model.errorCounter
        , viewHasWon model.hasWon
        ]
    }


viewNewGame : Html Msg
viewNewGame =
    button
        [ onClick NewGame
        , class "px-4"
        , class "py-2"
        , class "bg-blue-700"
        , class "rounded"
        , class "text-white"
        , class "m-5"
        ]
        [ text "Start New Game" ]


viewAlphabet : List AlphabetLetter -> Bool -> Html Msg
viewAlphabet alphabet hasWon =
    div
        [ class "flex"
        , class "items-center"
        ]
        [ div [ class "flex-1" ] []
        , div
            [ class "flex-1"
            ]
            (List.map (viewAlphabetLetter hasWon) alphabet)
        , div [ class "flex-1" ] []
        ]


viewAlphabetLetter : Bool -> AlphabetLetter -> Html Msg
viewAlphabetLetter hasWon letter =
    case letter of
        Used c ->
            viewAlphabetLetterButton c True

        Unused c ->
            viewAlphabetLetterButton c (False || hasWon)


viewAlphabetLetterButton : Char -> Bool -> Html Msg
viewAlphabetLetterButton c disabled_ =
    let
        classes =
            getClassesForAlphabetLetterButton disabled_
    in
    button
        ([ onClick (GuessLetter c)
         , disabled disabled_
         , class "px-4"
         , class "py-2"
         , class "bg-gray-300"
         , class "mt-1"
         , class "mb-1"
         ]
            ++ classes
        )
        [ text (String.fromList [ c ])
        ]


getClassesForAlphabetLetterButton : Bool -> List (Html.Attribute msg)
getClassesForAlphabetLetterButton disabled_ =
    if disabled_ then
        [ class "opacity-50", class "cursor-not-allowed" ]

    else
        []


viewWord : List Letter -> Html msg
viewWord letters =
    div
        [ class "text-4xl"
        , class "m-5"
        ]
        (List.map viewLetter letters)


viewLetter : Letter -> Html msg
viewLetter letter =
    case letter of
        Shown c ->
            text (String.fromList [ c ])

        Hidden _ ->
            text "_"


viewErrorCounter : Int -> Html msg
viewErrorCounter counter =
    div []
        [ text (String.fromInt counter)
        , viewHangman counter
        ]


viewHangman : Int -> Html msg
viewHangman counter =
    let
        temp =
            List.range 0 (counter - 1)
    in
    svg [ viewBox 0 0 100 100 ] (List.map viewHangmanLine temp)


viewHangmanLine : Int -> Svg msg
viewHangmanLine index =
    if index == 0 then
        line
            [ x1 (px 10)
            , y1 (px 30)
            , x2 (px 20)
            , y2 (px 40)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 1 then
        line
            [ x1 (px 0)
            , y1 (px 40)
            , x2 (px 10)
            , y2 (px 30)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 2 then
        line
            [ x1 (px 10)
            , y1 (px 30)
            , x2 (px 10)
            , y2 (px 20)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 3 then
        line
            [ x1 (px 10)
            , y1 (px 20)
            , x2 (px 10)
            , y2 (px 10)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 4 then
        line
            [ x1 (px 10)
            , y1 (px 10)
            , x2 (px 10)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 5 then
        line
            [ x1 (px 10)
            , y1 (px 0)
            , x2 (px 20)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 6 then
        line
            [ x1 (px 10)
            , y1 (px 10)
            , x2 (px 20)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 7 then
        line
            [ x1 (px 20)
            , y1 (px 0)
            , x2 (px 30)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 8 then
        line
            [ x1 (px 30)
            , y1 (px 0)
            , x2 (px 30)
            , y2 (px 10)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            ]
            []

    else if index == 9 then
        g []
            [ circle
                [ cx (px 30)
                , cy (px 10)
                , r (px 3)
                ]
                []
            , line
                [ x1 (px 30)
                , y1 (px 10)
                , x2 (px 30)
                , y2 (px 25)
                , strokeWidth (px 1)
                , stroke <| Paint <| Color.rgba 0 0 0 1
                ]
                []
            , line
                [ x1 (px 30)
                , y1 (px 17)
                , x2 (px 25)
                , y2 (px 14)
                , strokeWidth (px 1)
                , stroke <| Paint <| Color.rgba 0 0 0 1
                ]
                []
            , line
                [ x1 (px 30)
                , y1 (px 17)
                , x2 (px 35)
                , y2 (px 14)
                , strokeWidth (px 1)
                , stroke <| Paint <| Color.rgba 0 0 0 1
                ]
                []
            , line
                [ x1 (px 30)
                , y1 (px 25)
                , x2 (px 25)
                , y2 (px 30)
                , strokeWidth (px 1)
                , stroke <| Paint <| Color.rgba 0 0 0 1
                ]
                []
            , line
                [ x1 (px 30)
                , y1 (px 25)
                , x2 (px 35)
                , y2 (px 30)
                , strokeWidth (px 1)
                , stroke <| Paint <| Color.rgba 0 0 0 1
                ]
                []
            ]

    else
        line [] []


viewHasWon : Bool -> Html msg
viewHasWon hasWon =
    if hasWon then
        div [] [ text "You have won!" ]

    else
        div [] []
