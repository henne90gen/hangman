module Main exposing (main)

import Browser
import Color
import Html exposing (Html, a, button, div, option, select, span, text)
import Html.Attributes exposing (class, disabled, href, target, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import List
import List.Extra
import Random
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (cx, cy, height, r, stroke, strokeWidth, viewBox, visibility, width, x1, x2, y1, y2)
import TypedSvg.Core
import TypedSvg.Types exposing (Paint(..), px)
import Url
import WordList exposing (wordList_de, wordList_en)


type Language
    = DE
    | EN


type Letter
    = Shown Char
    | Hidden Char


type AlphabetLetter
    = CorrectlyUsed Char
    | IncorrectlyUsed Char
    | Disabled Char
    | Unused Char


type GameState
    = Playing
    | HasWon
    | HasLost


type alias Statistics =
    { mostCorrectWordsOverall : Int
    , mostCorrectWordsCurrent : Int
    , mostIncorrectWordsOverall : Int
    , mostIncorrectWordsCurrent : Int
    , mostCorrectLettersOverall : Int
    , mostCorrectLettersCurrent : Int
    , mostIncorrectLettersOverall : Int
    , mostIncorrectLettersCurrent : Int
    , correctWordsTotal : Int
    , incorrectWordsTotal : Int
    , correctLettersTotal : Int
    , incorrectLettersTotal : Int
    }


type alias Model =
    { shownWord : List Letter
    , alphabet : List AlphabetLetter
    , errorCounter : Int
    , gameState : GameState
    , language : Language
    , showStatistics : Bool
    , statistics : Statistics
    }


type Msg
    = NewGame
    | NewRandomNumber Int
    | GuessLetter Char
    | ChangeLanguage String
    | ToggleStatistics


init : flags -> ( Model, Cmd Msg )
init _ =
    ( startNewGame emptyScores False DE ""
    , generateRandomNumber DE
    )


emptyScores : Statistics
emptyScores =
    { mostCorrectWordsOverall = 0
    , mostCorrectWordsCurrent = 0
    , mostIncorrectWordsOverall = 0
    , mostIncorrectWordsCurrent = 0
    , mostCorrectLettersOverall = 0
    , mostCorrectLettersCurrent = 0
    , mostIncorrectLettersOverall = 0
    , mostIncorrectLettersCurrent = 0
    , correctWordsTotal = 0
    , incorrectWordsTotal = 0
    , correctLettersTotal = 0
    , incorrectLettersTotal = 0
    }


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
        CorrectlyUsed c

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
            ( startNewGame model.statistics model.showStatistics model.language "", generateRandomNumber model.language )

        NewRandomNumber num ->
            let
                wordList =
                    getWordList model.language

                newWord =
                    Maybe.withDefault "" (List.Extra.getAt num wordList)
            in
            ( startNewGame model.statistics model.showStatistics model.language newWord, Cmd.none )

        GuessLetter letter ->
            ( guessLetter model letter, Cmd.none )

        ChangeLanguage newLanguageStr ->
            let
                newLanguage =
                    languageFromString newLanguageStr
            in
            ( startNewGame model.statistics model.showStatistics newLanguage "", generateRandomNumber newLanguage )

        ToggleStatistics ->
            ( { model | showStatistics = not model.showStatistics }, Cmd.none )


languageFromString : String -> Language
languageFromString str =
    if str == "DE" then
        DE

    else if str == "EN" then
        EN

    else
        -- return default language
        DE


getWordList : Language -> List String
getWordList language =
    case language of
        DE ->
            wordList_de

        EN ->
            wordList_en


startNewGame : Statistics -> Bool -> Language -> String -> Model
startNewGame statistics showStatistics language newWord =
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
    , showStatistics = showStatistics
    , statistics = statistics
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

        statistics =
            updateWordHighScores model.statistics gameState
    in
    { shownWord = newShownWord
    , alphabet = guessLetterAlphabet model.alphabet c hasFoundLetter gameState
    , errorCounter = newErrorCounter
    , gameState = gameState
    , language = model.language
    , showStatistics = model.showStatistics
    , statistics = updateLetterStatistics statistics hasFoundLetter
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


guessLetterAlphabet : List AlphabetLetter -> Char -> Bool -> GameState -> List AlphabetLetter
guessLetterAlphabet alphabet c hasFoundLetter gameState =
    List.map (guessSingleLetterAlphabet c hasFoundLetter gameState) alphabet


guessSingleLetterAlphabet : Char -> Bool -> GameState -> AlphabetLetter -> AlphabetLetter
guessSingleLetterAlphabet c hasFoundLetter gameState letter =
    if isGameOver gameState then
        guessLetterGameOver letter

    else
        guessLetterGameRunning c hasFoundLetter letter


guessLetterGameOver : AlphabetLetter -> AlphabetLetter
guessLetterGameOver letter =
    case letter of
        CorrectlyUsed a ->
            Disabled a

        IncorrectlyUsed a ->
            Disabled a

        Disabled a ->
            Disabled a

        Unused a ->
            Disabled a


guessLetterGameRunning : Char -> Bool -> AlphabetLetter -> AlphabetLetter
guessLetterGameRunning c hasFoundLetter letter =
    case letter of
        CorrectlyUsed a ->
            CorrectlyUsed a

        IncorrectlyUsed a ->
            IncorrectlyUsed a

        Disabled a ->
            Disabled a

        Unused a ->
            if a == c then
                if hasFoundLetter then
                    CorrectlyUsed a

                else
                    IncorrectlyUsed a

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


updateWordHighScores : Statistics -> GameState -> Statistics
updateWordHighScores statistics gameState =
    case gameState of
        HasWon ->
            let
                mostCorrectWordsCurrent =
                    statistics.mostCorrectWordsCurrent + 1
            in
            { statistics
                | incorrectWordsTotal = statistics.incorrectWordsTotal + 1
                , mostCorrectWordsCurrent = mostCorrectWordsCurrent
                , mostCorrectWordsOverall = max mostCorrectWordsCurrent statistics.mostCorrectWordsOverall
            }

        HasLost ->
            let
                mostIncorrectWordsCurrent =
                    statistics.mostIncorrectWordsCurrent + 1
            in
            { statistics
                | incorrectWordsTotal = statistics.incorrectWordsTotal + 1
                , mostIncorrectWordsCurrent = mostIncorrectWordsCurrent
                , mostIncorrectWordsOverall = max mostIncorrectWordsCurrent statistics.mostIncorrectWordsOverall
            }

        Playing ->
            statistics


updateLetterStatistics : Statistics -> Bool -> Statistics
updateLetterStatistics statistics hasFoundLetter =
    if hasFoundLetter then
        let
            mostCorrectLettersCurrent =
                statistics.mostCorrectLettersCurrent + 1
        in
        { statistics
            | correctLettersTotal = statistics.correctLettersTotal + 1
            , mostCorrectLettersCurrent = mostCorrectLettersCurrent
            , mostCorrectLettersOverall = max mostCorrectLettersCurrent statistics.mostCorrectLettersOverall
        }

    else
        let
            mostIncorrectLettersCurrent =
                statistics.mostIncorrectLettersCurrent + 1
        in
        { statistics
            | incorrectLettersTotal = statistics.incorrectLettersTotal + 1
            , mostIncorrectLettersCurrent = mostIncorrectLettersCurrent
            , mostIncorrectLettersOverall = max mostIncorrectLettersCurrent statistics.mostIncorrectLettersOverall
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = getTitle model.language
    , body =
        [ div [ class "my-2" ]
            [ viewNewGame model.language
            , viewLanguageSelect
            ]
        , viewWord model.shownWord model.gameState model.language
        , viewAlphabet model.alphabet
        , viewHasWon model.gameState model.language
        , viewHangman model.errorCounter
        , viewStatistics model.statistics model.showStatistics model.language
        ]
    }


getTitle : Language -> String
getTitle language =
    case language of
        DE ->
            "Galgenraten"

        EN ->
            "Hangman"


viewNewGame : Language -> Html Msg
viewNewGame language =
    button
        [ onClick NewGame
        , class "px-4"
        , class "py-2"
        , class "bg-blue-700"
        , class "rounded"
        , class "text-white"
        , class "mx-5"
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
        , onChange ChangeLanguage
        ]
        [ option [ value "DE" ] [ text "DE" ]
        , option [ value "EN" ] [ text "EN" ]
        ]


viewAlphabet : List AlphabetLetter -> Html Msg
viewAlphabet alphabet =
    div
        [ class "flex"
        , class "items-center"
        , class "mx-5"
        , class "mb-3"
        ]
        [ div
            [ class "flex-1"
            ]
            (List.map viewAlphabetLetter alphabet)
        ]


viewAlphabetLetter : AlphabetLetter -> Html Msg
viewAlphabetLetter letter =
    let
        ( classes, disabled_, c ) =
            getClassesAndDisabledForAlphabetLetterButton letter
    in
    button
        ([ onClick (GuessLetter c)
         , disabled disabled_
         , class "px-4"
         , class "py-2"
         , class "my-1"
         ]
            ++ classes
        )
        [ text (String.fromList [ c ])
        ]


getClassesAndDisabledForAlphabetLetterButton : AlphabetLetter -> ( List (Html.Attribute msg), Bool, Char )
getClassesAndDisabledForAlphabetLetterButton letter =
    case letter of
        Unused c ->
            ( [ class "bg-gray-300" ], False, c )

        CorrectlyUsed c ->
            ( [ class "bg-green-400", class "opacity-75", class "cursor-not-allowed" ], True, c )

        IncorrectlyUsed c ->
            ( [ class "bg-red-500", class "opacity-75", class "cursor-not-allowed" ], True, c )

        Disabled c ->
            ( [ class "bg-gray-300", class "opacity-50", class "cursor-not-allowed" ], True, c )


viewWord : List Letter -> GameState -> Language -> Html msg
viewWord letters gameState language =
    let
        classes =
            getWordClasses gameState
    in
    div
        ([ class "text-4xl"
         , class "m-5"
         , class "mt-0"
         , class "mb-2"
         , class "pt-2"
         , class "rounded"
         , class "flex"
         , class "flex-col"
         , class "items-center"
         ]
            ++ classes
        )
        [ div [ class "flex-1" ] (List.map (viewLetter gameState) letters)
        , div [ class "flex-1" ]
            [ viewGoogleLink letters gameState language
            , viewWikipediaLink letters gameState language
            ]
        ]


viewSearchLink : GameState -> Html.Attribute msg -> String -> Html msg
viewSearchLink gameState link linkText =
    if isGameOver gameState then
        a
            [ target "_blank"
            , link
            , class "text-sm"
            , class "mx-2"
            , class "py-1"
            , class "px-2"
            , class "bg-gray-600"
            , class "text-white"
            , class "rounded"
            ]
            [ text linkText ]

    else
        div [] []


viewGoogleLink : List Letter -> GameState -> Language -> Html msg
viewGoogleLink letters gameState language =
    let
        link =
            getGoogleLink letters

        linkText =
            getGoogleLinkText language
    in
    viewSearchLink gameState link linkText


getGoogleLink : List Letter -> Html.Attribute msg
getGoogleLink letters =
    let
        word =
            letters
                |> List.map getCharacter
                |> String.fromList
    in
    href ("http://www.google.com/search?q=" ++ Url.percentEncode word)


getCharacter : Letter -> Char
getCharacter letter =
    case letter of
        Shown c ->
            c

        Hidden c ->
            c


getGoogleLinkText : Language -> String
getGoogleLinkText language =
    case language of
        DE ->
            "Google Suche"

        EN ->
            "Google Search"


viewWikipediaLink : List Letter -> GameState -> Language -> Html msg
viewWikipediaLink letters gameState language =
    let
        link =
            getWikipediaLink letters language

        linkText =
            getWikipediaLinkText language
    in
    viewSearchLink gameState link linkText


getWikipediaLink : List Letter -> Language -> Html.Attribute msg
getWikipediaLink letters language =
    let
        langPrefix =
            getWikipediaLanguagePrefix language

        word =
            letters
                |> List.map getCharacter
                |> String.fromList
    in
    href
        ("https://"
            ++ langPrefix
            ++ ".wikipedia.org/wiki/Special:Search/"
            ++ Url.percentEncode word
        )


getWikipediaLanguagePrefix : Language -> String
getWikipediaLanguagePrefix language =
    case language of
        DE ->
            "de"

        EN ->
            "en"


getWikipediaLinkText : Language -> String
getWikipediaLinkText language =
    case language of
        DE ->
            "Wikipedia Suche"

        EN ->
            "Wikipedia Search"


getWordClasses : GameState -> List (Html.Attribute msg)
getWordClasses gameState =
    case gameState of
        Playing ->
            []

        HasWon ->
            [ class "bg-green-400" ]

        HasLost ->
            [ class "bg-red-400" ]


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


isGameOver : GameState -> Bool
isGameOver gameState =
    case gameState of
        Playing ->
            False

        HasLost ->
            True

        HasWon ->
            True


viewHangman : Int -> Html msg
viewHangman counter =
    div
        [ class "flex"
        , class "flex-col"
        , class "items-center"
        ]
        [ div [ class "flex-1" ] [ viewHangmanSvg counter ]
        ]


viewHangmanSvg : Int -> Html msg
viewHangmanSvg counter =
    svg
        [ viewBox 0 0 40 40
        , width (px 400)
        , height (px 400)
        ]
        [ line
            [ x1 (px 10)
            , y1 (px 30)
            , x2 (px 20)
            , y2 (px 40)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 1)
            ]
            []
        , line
            [ x1 (px 0)
            , y1 (px 40)
            , x2 (px 10)
            , y2 (px 30)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 2)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 30)
            , x2 (px 10)
            , y2 (px 20)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 3)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 20)
            , x2 (px 10)
            , y2 (px 10)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 4)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 10)
            , x2 (px 10)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 5)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 0)
            , x2 (px 20)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 6)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 10)
            , x2 (px 20)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 7)
            ]
            []
        , line
            [ x1 (px 20)
            , y1 (px 0)
            , x2 (px 30)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 8)
            ]
            []
        , line
            [ x1 (px 30)
            , y1 (px 0)
            , x2 (px 30)
            , y2 (px 10)
            , strokeWidth (px 1)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , isVisible (counter >= 9)
            ]
            []
        , g [ isVisible (counter >= 10) ]
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
        ]


isVisible : Bool -> TypedSvg.Core.Attribute msg
isVisible b =
    if b then
        visibility "visible"

    else
        visibility "hidden"


viewHasWon : GameState -> Language -> Html msg
viewHasWon gameState language =
    case gameState of
        Playing ->
            div [] []

        HasWon ->
            div [ class "my-3" ] [ text (getWonText language) ]

        HasLost ->
            div [ class "my-3" ] [ text (getLostText language) ]


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


viewStatistics : Statistics -> Bool -> Language -> Html Msg
viewStatistics statistics showStatistics language =
    div []
        [ button
            [ onClick ToggleStatistics
            , class "py-2"
            , class "px-4"
            , class "rounded"
            , class "bg-gray-500"
            , class "text-white"
            ]
            [ text <| getShowStatisticsButtonText language ]
        , viewStatisticsPane statistics showStatistics language
        ]


getShowStatisticsButtonText : Language -> String
getShowStatisticsButtonText language =
    case language of
        DE ->
            "Statistiken anzeigen"

        EN ->
            "Show Statistics"


viewStatisticsPane : Statistics -> Bool -> Language -> Html msg
viewStatisticsPane statistics showStatistics language =
    div [ getStatisticsVisibilityClass showStatistics ]
        [ div [] [ text <| getCorrectWordsTotalText language ++ String.fromInt statistics.correctWordsTotal ]
        , div [] [ text <| getIncorrectWordsTotalText language ++ String.fromInt statistics.incorrectWordsTotal ]
        , div [] [ text <| getCorrectLettersTotalText language ++ String.fromInt statistics.correctLettersTotal ]
        , div [] [ text <| getIncorrectLettersTotalText language ++ String.fromInt statistics.incorrectLettersTotal ]
        , div [] [ text <| "Current Correct Word Streak: " ++ String.fromInt statistics.mostCorrectWordsCurrent ]
        , div [] [ text <| "Best Correct Word Streak: " ++ String.fromInt statistics.mostCorrectWordsOverall ]
        , div [] [ text <| "Current Incorrect Word Streak: " ++ String.fromInt statistics.mostIncorrectWordsCurrent ]
        , div [] [ text <| "Best Incorrect Word Streak: " ++ String.fromInt statistics.mostIncorrectWordsOverall ]
        , div [] [ text <| "Current Correct Letter Streak: " ++ String.fromInt statistics.mostCorrectLettersCurrent ]
        , div [] [ text <| "Best Correct Letter Streak: " ++ String.fromInt statistics.mostCorrectLettersOverall ]
        , div [] [ text <| "Current Incorrect Word Streak: " ++ String.fromInt statistics.mostIncorrectLettersCurrent ]
        , div [] [ text <| "Best Incorrect Word Streak: " ++ String.fromInt statistics.mostIncorrectLettersOverall ]
        ]


getCorrectWordsTotalText : Language -> String
getCorrectWordsTotalText language =
    case language of
        DE ->
            "Korrekte Worte: "

        EN ->
            "Correct Words: "


getIncorrectWordsTotalText : Language -> String
getIncorrectWordsTotalText language =
    "Incorrect Words: "


getCorrectLettersTotalText : Language -> String
getCorrectLettersTotalText language =
    "Correct Letters: "


getIncorrectLettersTotalText : Language -> String
getIncorrectLettersTotalText language =
    "Incorrect Letters: "


getStatisticsVisibilityClass : Bool -> Html.Attribute msg
getStatisticsVisibilityClass showStatistics =
    if showStatistics then
        class "block"

    else
        class "hidden"
