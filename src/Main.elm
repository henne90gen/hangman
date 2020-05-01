port module Main exposing (main)

import Browser
import Color
import Html exposing (Html, a, button, div, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, href, target, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import List
import Translations
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (cx, cy, height, r, stroke, strokeWidth, viewBox, visibility, width, x1, x2, y1, y2)
import TypedSvg.Core
import TypedSvg.Types exposing (Paint(..), px)
import Url


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
    , language : Translations.Language
    , statistics : Statistics
    }


type Msg
    = NewGameButtonPressed
    | GuessLetter Char
    | ChangeLanguage String
    | GotWord Translations.Language String


type alias Flags =
    { statistics : Maybe Statistics }


port saveStatistics : Statistics -> Cmd msg


port requestWord : String -> Cmd msg


port receiveWord : (( String, String ) -> msg) -> Sub msg



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( createInitialModel flags.statistics
    , requestWord (Translations.languageToString Translations.defaultLanguage)
    )


alphabetString : String
alphabetString =
    "abcdefghijklmnopqrstuvwxyzäöüß"


createInitialModel : Maybe Statistics -> Model
createInitialModel statistics =
    { shownWord = []
    , alphabet = List.map Unused (String.toList alphabetString)
    , errorCounter = 0
    , gameState = Playing
    , language = Translations.defaultLanguage
    , statistics = Maybe.withDefault emptyStatistics statistics
    }


emptyStatistics : Statistics
emptyStatistics =
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


createAlphabet : String -> Char -> List AlphabetLetter
createAlphabet str firstLetter =
    List.map (createAlphabetLetter firstLetter) (String.toList str)


createAlphabetLetter : Char -> Char -> AlphabetLetter
createAlphabetLetter firstLetter c =
    if firstLetter == c then
        CorrectlyUsed c

    else
        Unused c



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGameButtonPressed ->
            ( model
            , Cmd.batch
                [ requestWord (Translations.languageToString model.language)
                , saveStatistics model.statistics
                ]
            )

        GuessLetter letter ->
            let
                newModel =
                    guessLetter model letter
            in
            ( newModel, saveStatistics newModel.statistics )

        ChangeLanguage newLanguageStr ->
            let
                newLanguage =
                    Translations.languageFromString newLanguageStr
            in
            ( { model | language = newLanguage, alphabet = List.map Disabled (String.toList alphabetString) }
            , Cmd.batch
                [ requestWord (Translations.languageToString newLanguage)
                , saveStatistics model.statistics
                ]
            )

        GotWord language word ->
            ( startNewGame model language word, Cmd.none )


startNewGame : Model -> Translations.Language -> String -> Model
startNewGame model language newWord =
    let
        characters =
            String.toList newWord

        firstLetter =
            Char.toLower (Maybe.withDefault ' ' (List.head characters))
    in
    { model
        | shownWord = List.indexedMap (toLetter firstLetter) characters
        , alphabet = createAlphabet alphabetString firstLetter
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

        statistics =
            updateWordHighScores model.statistics gameState
    in
    { shownWord = newShownWord
    , alphabet = guessLetterAlphabet model.alphabet c hasFoundLetter gameState
    , errorCounter = newErrorCounter
    , gameState = gameState
    , language = model.language
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
                | correctWordsTotal = statistics.correctWordsTotal + 1
                , mostCorrectWordsCurrent = mostCorrectWordsCurrent
                , mostCorrectWordsOverall = max mostCorrectWordsCurrent statistics.mostCorrectWordsOverall
                , mostIncorrectWordsCurrent = 0
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
                , mostCorrectWordsCurrent = 0
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
            , mostIncorrectLettersCurrent = 0
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
            , mostCorrectLettersCurrent = 0
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveWord (\( lang, word ) -> GotWord (Translations.languageFromString lang) word)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = Translations.getTitle model.language
    , body =
        [ div [ class "my-2" ]
            [ viewNewGameButton model.language
            , viewLanguageSelect
            ]
        , viewWord model.shownWord model.gameState model.language
        , viewAlphabet model.alphabet
        , viewHasWon model.gameState model.language
        , viewHangmanAndStatistics model.errorCounter model.statistics model.language
        ]
    }


viewNewGameButton : Translations.Language -> Html Msg
viewNewGameButton language =
    button
        [ onClick NewGameButtonPressed
        , class "px-4"
        , class "py-2"
        , class "bg-blue-700"
        , class "rounded"
        , class "text-white"
        , class "mx-5"
        ]
        [ text (Translations.getNewGameText language) ]


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


viewWord : List Letter -> GameState -> Translations.Language -> Html msg
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
        , div [ class "flex-1", class "mb-2" ]
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


viewGoogleLink : List Letter -> GameState -> Translations.Language -> Html msg
viewGoogleLink letters gameState language =
    let
        link =
            getGoogleLink letters

        linkText =
            Translations.getGoogleLinkText language
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


viewWikipediaLink : List Letter -> GameState -> Translations.Language -> Html msg
viewWikipediaLink letters gameState language =
    let
        link =
            getWikipediaLink letters language

        linkText =
            Translations.getWikipediaLinkText language
    in
    viewSearchLink gameState link linkText


getWikipediaLink : List Letter -> Translations.Language -> Html.Attribute msg
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


getWikipediaLanguagePrefix : Translations.Language -> String
getWikipediaLanguagePrefix language =
    case language of
        Translations.DE ->
            "de"

        Translations.EN ->
            "en"


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


viewHangmanAndStatistics : Int -> Statistics -> Translations.Language -> Html msg
viewHangmanAndStatistics counter statistics language =
    div
        [ class "grid"
        , class "grid-cols-1"
        , class "lg:grid-cols-2"
        , class "xl:grid-cols-2"
        , class "bg-gray-200"
        , class "py-5"
        , class "mx-5"
        , class "mb-5"
        , class "rounded"
        ]
        [ viewHangman counter
        , viewStatistics statistics language
        ]


viewHangman : Int -> Html msg
viewHangman counter =
    div
        [ class "flex"
        , class "justify-center"
        , class "lg:justify-end"
        , class "xl:justify-end"
        ]
        [ viewHangmanSvg counter ]


viewHangmanSvg : Int -> Html msg
viewHangmanSvg counter =
    svg
        [ viewBox 0 0 40 40
        , width (px 400)
        , height (px 400)
        ]
        [ g [ isVisible (counter >= 10) ]
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
        , line
            [ x1 (px 30)
            , y1 (px 0)
            , x2 (px 30)
            , y2 (px 10)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 9)
            ]
            []
        , line
            [ x1 (px 20)
            , y1 (px 0)
            , x2 (px 30)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 8)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 10)
            , x2 (px 20)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 7)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 0)
            , x2 (px 20)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 6)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 10)
            , x2 (px 10)
            , y2 (px 0)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 5)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 20)
            , x2 (px 10)
            , y2 (px 10)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 4)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 30)
            , x2 (px 10)
            , y2 (px 20)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 3)
            ]
            []
        , line
            [ x1 (px 0)
            , y1 (px 40)
            , x2 (px 10)
            , y2 (px 30)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 2)
            ]
            []
        , line
            [ x1 (px 10)
            , y1 (px 30)
            , x2 (px 20)
            , y2 (px 40)
            , strokeWidth (px 1)
            , stroke <| Paint <| getLineColor (counter >= 1)
            ]
            []
        ]


isVisible : Bool -> TypedSvg.Core.Attribute msg
isVisible b =
    if b then
        visibility "visible"

    else
        visibility "hidden"


getLineColor : Bool -> Color.Color
getLineColor active =
    if active then
        Color.rgba 0 0 0 1

    else
        Color.rgba 0.9 0.9 0.9 1


viewStatistics : Statistics -> Translations.Language -> Html msg
viewStatistics statistics language =
    div
        [ class "mt-5"
        , class "flex"
        , class "flex-col"
        , class "items-center"
        ]
        [ viewStatisticsPane statistics language
        ]


viewStatisticsPane : Statistics -> Translations.Language -> Html msg
viewStatisticsPane statistics language =
    table
        [ class "my-2"
        , class "table-auto"
        , class "border-collapse"
        ]
        [ thead []
            [ tr []
                [ th [ class "px-4", class "py-1" ] []
                , th [ class "px-4", class "py-1" ] [ text <| Translations.getStatisticsTableHeaderCorrect language ]
                , th [ class "px-4", class "py-1" ] [ text <| Translations.getStatisticsTableHeaderIncorrect language ]
                ]
            ]
        , tbody []
            [ tr []
                [ td [ class "px-4", class "py-1", class "text-right" ] [ text <| Translations.getWordsTotalText language ]
                , td [ class "px-4", class "py-1" ] [ text <| String.fromInt statistics.correctWordsTotal ]
                , td [ class "px-4", class "py-1" ] [ text <| String.fromInt statistics.incorrectWordsTotal ]
                ]
            , tr []
                [ td [ class "px-4", class "pb-2", class "text-right" ] [ text <| Translations.getLettersTotalText language ]
                , td [ class "px-4", class "pb-2" ] [ text <| String.fromInt statistics.correctLettersTotal ]
                , td [ class "px-4", class "pb-2" ] [ text <| String.fromInt statistics.incorrectLettersTotal ]
                ]
            , tr []
                [ td [ class "px-4", class "pt-2", class "text-right" ] [ text <| Translations.getCurrentWordStreakText language ]
                , td [ class "px-4", class "pt-2" ] [ text <| String.fromInt statistics.mostCorrectWordsCurrent ]
                , td [ class "px-4", class "pt-2" ] [ text <| String.fromInt statistics.mostIncorrectWordsCurrent ]
                ]
            , tr []
                [ td [ class "px-4", class "pb-2", class "text-right" ] [ text <| Translations.getBestWordStreakText language ]
                , td [ class "px-4", class "pb-2" ] [ text <| String.fromInt statistics.mostCorrectWordsOverall ]
                , td [ class "px-4", class "pb-2" ] [ text <| String.fromInt statistics.mostIncorrectWordsOverall ]
                ]
            , tr []
                [ td [ class "px-4", class "pt-2", class "text-right" ] [ text <| Translations.getCurrentLetterStreakText language ]
                , td [ class "px-4", class "pt-2" ] [ text <| String.fromInt statistics.mostCorrectLettersCurrent ]
                , td [ class "px-4", class "pt-2" ] [ text <| String.fromInt statistics.mostIncorrectLettersCurrent ]
                ]
            , tr []
                [ td [ class "px-4", class "text-right" ] [ text <| Translations.getBestLetterStreakText language ]
                , td [ class "px-4" ] [ text <| String.fromInt statistics.mostCorrectLettersOverall ]
                , td [ class "px-4" ] [ text <| String.fromInt statistics.mostIncorrectLettersOverall ]
                ]
            ]
        ]


viewHasWon : GameState -> Translations.Language -> Html msg
viewHasWon gameState language =
    case gameState of
        Playing ->
            div [] []

        HasWon ->
            div [ class "my-3" ] [ text (Translations.getWonText language) ]

        HasLost ->
            div [ class "my-3" ] [ text (Translations.getLostText language) ]
