module Main exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List
import List.Extra
import Random
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (cx, cy, height, r, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)
import WordList exposing (wordList_de, wordList_en)


type Language
    = DE
    | EN


type Letter
    = Shown Char
    | Hidden Char


type AlphabetLetter
    = Used Char
    | Unused Char


type GameState
    = Playing
    | HasWon
    | HasLost


type alias Model =
    { shownWord : List Letter
    , alphabet : List AlphabetLetter
    , errorCounter : Int
    , gameState : GameState
    , language : Language
    }


type Msg
    = NewGame
    | NewRandomNumber Int
    | GuessLetter Char
    | ChangeLanguage Language


init : flags -> ( Model, Cmd Msg )
init _ =
    ( startNewGame DE ""
    , generateRandomNumber DE
    )


generateRandomNumber : Language -> Cmd Msg
generateRandomNumber language =
    let
        wordList =
            getWordList language
    in
    Random.generate NewRandomNumber (Random.int 0 (List.length wordList - 1))


createAlphabet : String -> Char -> List AlphabetLetter
createAlphabet str firstLetter =
    List.map (createAlphabetLetter firstLetter) (String.toList str)


createAlphabetLetter : Char -> Char -> AlphabetLetter
createAlphabetLetter firstLetter c =
    if firstLetter == c then
        Used c

    else
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
            ( startNewGame model.language "", generateRandomNumber model.language )

        NewRandomNumber num ->
            let
                wordList =
                    getWordList model.language

                newWord =
                    Maybe.withDefault "" (List.Extra.getAt num wordList)
            in
            ( startNewGame model.language newWord, Cmd.none )

        GuessLetter letter ->
            ( guessLetter model letter, Cmd.none )

        ChangeLanguage newLanguage ->
            ( startNewGame newLanguage "", generateRandomNumber newLanguage )


getWordList : Language -> List String
getWordList language =
    case language of
        DE ->
            wordList_de

        EN ->
            wordList_en


startNewGame : Language -> String -> Model
startNewGame language newWord =
    let
        characters =
            String.toList newWord

        firstLetter =
            Char.toLower (Maybe.withDefault ' ' (List.head characters))
    in
    { shownWord = List.indexedMap (toLetter firstLetter) characters
    , alphabet = createAlphabet "abcdefghijklmnopqrstuvwxyzäöüß" firstLetter
    , errorCounter = 0
    , gameState = Playing
    , language = language
    }


toLetter : Char -> Int -> Char -> Letter
toLetter firstLetter index c =
    if index == 0 then
        Shown c

    else if firstLetter == c then
        Shown c

    else
        Hidden c


guessLetter : Model -> Char -> Model
guessLetter model c =
    let
        ( hasFoundLetter, newShownWord ) =
            guessLetterShownWord model.shownWord c

        newErrorCounter =
            updateErrorCounter model.errorCounter hasFoundLetter

        gameState =
            checkGameState model.gameState newShownWord newErrorCounter
    in
    { shownWord = newShownWord
    , alphabet = guessLetterAlphabet model.alphabet c
    , errorCounter = newErrorCounter
    , gameState = gameState
    , language = model.language
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


checkGameState : GameState -> List Letter -> Int -> GameState
checkGameState gameState word errorCounter =
    case gameState of
        Playing ->
            let
                hasLost =
                    errorCounter >= 10

                hasWon =
                    word
                        |> List.map checkForWinLetter
                        |> List.foldl (&&) True
            in
            if hasLost then
                HasLost

            else if hasWon then
                HasWon

            else
                Playing

        HasWon ->
            HasWon

        HasLost ->
            HasLost


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
        [ viewNewGame model.language
        , viewLanguageSelect
        , viewAlphabet model.alphabet model.gameState
        , viewWord model.shownWord model.gameState
        , viewErrorCounter model.errorCounter
        , viewHasWon model.gameState model.language
        ]
    }


viewNewGame : Language -> Html Msg
viewNewGame language =
    button
        [ onClick NewGame
        , class "px-4"
        , class "py-2"
        , class "bg-blue-700"
        , class "rounded"
        , class "text-white"
        , class "m-5"
        ]
        [ text (getNewGameText language) ]


getNewGameText : Language -> String
getNewGameText language =
    case language of
        DE ->
            "Neues Spiel starten"

        EN ->
            "Start New Game"


viewLanguageSelect : Html Msg
viewLanguageSelect =
    select
        [ class "appearance-none"
        , class "bg-gray-200"
        , class "border"
        , class "border-gray-200"
        , class "text-gray-700"
        , class "py-3"
        , class "px-4"
        , class "rounded"
        , class "leading-tight"
        , class "focus:outline-none"
        , class "focus:bg-white"
        , class "focus:border-gray-500"
        ]
        [ option [ onClick (ChangeLanguage DE) ] [ text "DE" ]
        , option [ onClick (ChangeLanguage EN) ] [ text "EN" ]
        ]


viewAlphabet : List AlphabetLetter -> GameState -> Html Msg
viewAlphabet alphabet gameState =
    div
        [ class "flex"
        , class "items-center"
        ]
        [ div [ class "flex-1" ] []
        , div
            [ class "flex-1"
            ]
            (List.map (viewAlphabetLetter gameState) alphabet)
        , div [ class "flex-1" ] []
        ]


viewAlphabetLetter : GameState -> AlphabetLetter -> Html Msg
viewAlphabetLetter gameState letter =
    case letter of
        Used c ->
            viewAlphabetLetterButton c True

        Unused c ->
            viewAlphabetLetterButton c (False || isGameOver gameState)


isGameOver : GameState -> Bool
isGameOver gameState =
    case gameState of
        Playing ->
            False

        HasLost ->
            True

        HasWon ->
            True


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


viewWord : List Letter -> GameState -> Html msg
viewWord letters gameState =
    div
        [ class "text-4xl"
        , class "m-5"
        ]
        (List.map (viewLetter gameState) letters)


viewLetter : GameState -> Letter -> Html msg
viewLetter gameState letter =
    let
        gameOver =
            isGameOver gameState
    in
    case letter of
        Shown c ->
            text (String.fromList [ c ])

        Hidden c ->
            if gameOver then
                text (String.fromList [ c ])

            else
                text " _ "


viewErrorCounter : Int -> Html msg
viewErrorCounter counter =
    div
        [ class "flex"
        , class "flex-col"
        , class "items-center"
        ]
        [ div [ class "flex-1" ] [ viewHangman counter ]
        ]


viewHangman : Int -> Html msg
viewHangman counter =
    let
        temp =
            List.range 0 (counter - 1)
    in
    svg
        [ viewBox 0 0 40 40
        , width (px 400)
        , height (px 400)
        ]
        (List.map viewHangmanLine temp)


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


viewHasWon : GameState -> Language -> Html msg
viewHasWon gameState language =
    case gameState of
        Playing ->
            div [] []

        HasWon ->
            div [ class "mt-5" ] [ text (getWonText language) ]

        HasLost ->
            div [ class "mt-5" ] [ text (getLostText language) ]


getWonText : Language -> String
getWonText language =
    case language of
        DE ->
            "Du hast gewonnen!"

        EN ->
            "You have won!"


getLostText : Language -> String
getLostText language =
    case language of
        DE ->
            "Du hast verloren!"

        EN ->
            "You have lost!"
