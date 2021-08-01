port module Main exposing (main)

import Browser
import Color
import File
import Html exposing (Html, a, button, div, h1, input, label, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, disabled, href, selected, target, type_, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Html.Keyed
import Json.Decode as Decode
import Round
import Task
import Translations
import TypedSvg exposing (circle, g, line, path, rect, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, transform, viewBox, visibility, width, x1, x2, y1, y2)
import TypedSvg.Core
import TypedSvg.Types exposing (Paint(..), StrokeLinejoin(..), Transform(..), percent, px)
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


type alias GameData =
    { shownWord : List Letter
    , alphabet : List AlphabetLetter
    , errorCounter : Int
    , gameState : GameState
    }


type ColorTheme
    = LightTheme
    | DarkTheme


type alias Settings =
    { language : Translations.Language
    , theme : ColorTheme
    , activeWordPacks : List Int
    }


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


type alias FileWordPack =
    { name : String
    , words : List String
    }


type alias Model =
    { gameData : GameData
    , settings : Settings
    , statistics : Statistics
    , fileInputIdx : Int -- NOTE this is a hack to clear the file input after uploading a file
    , wordPacks : List WordPackInfo
    , isSettingsPanelOpen : Bool
    , useDefaultPack : Bool
    }


type Msg
    = NewGameButtonPressed
    | GuessLetter Char
    | ChangeLanguage String
    | GotWord String
    | GotWordPackInfos (List WordPackInfo)
    | ToggleDarkMode
    | ToggleSettingsPanel
    | ToggleUseDefaultPack
    | GotCustomWordFiles (List File.File)
    | GotCustomWordContent String String
    | RemoveCustomWordPack Int
    | ToggleCustomWordPackActive Int


type alias SettingsFlags =
    { language : String
    , theme : String
    , activeWordPacks : List Int
    }


type alias LetterFlags =
    { shown : Bool
    , letter : String
    }


type alias AlphabetLetterFlags =
    { state : String
    , letter : String
    }


type alias GameDataFlags =
    { shownWord : List LetterFlags
    , alphabet : List AlphabetLetterFlags
    , errorCounter : Int
    , gameState : String
    }


type alias Flags =
    { statistics : Maybe Statistics
    , settings : Maybe SettingsFlags
    , gameData : Maybe GameDataFlags
    , wordPackInfos : List WordPackInfo
    }


type alias WordPackInfo =
    { id : Int
    , name : String
    , isDefault : Bool
    , wordCount : Int
    }


port saveSettings : SettingsFlags -> Cmd msg


port saveStatistics : Statistics -> Cmd msg


port saveGameData : GameDataFlags -> Cmd msg


port saveFileWordPack : FileWordPack -> Cmd msg


port deleteFileWordPack : Int -> Cmd msg


port requestWord : List Int -> Cmd msg


port receiveWord : (String -> msg) -> Sub msg


port receiveWordPackInfos : (List WordPackInfo -> msg) -> Sub msg



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            createInitialModel flags

        alreadyHasWord =
            List.length model.gameData.shownWord > 0

        cmd =
            if alreadyHasWord then
                Cmd.none

            else
                requestWord model.settings.activeWordPacks
    in
    ( model, cmd )


alphabetString : String
alphabetString =
    "abcdefghijklmnopqrstuvwxyzäöüß"


createInitialModel : Flags -> Model
createInitialModel flags =
    let
        gameData =
            Maybe.withDefault emptyGameData (Maybe.map convertGameDataFlags flags.gameData)

        settings =
            Maybe.withDefault
                (defaultSettings flags)
                (Maybe.map convertSettingsFlags flags.settings)
    in
    { gameData = gameData
    , settings = settings
    , statistics = Maybe.withDefault emptyStatistics flags.statistics
    , wordPacks = flags.wordPackInfos
    , fileInputIdx = 0
    , isSettingsPanelOpen = False
    , useDefaultPack = True
    }


emptyGameData : GameData
emptyGameData =
    { shownWord = []
    , alphabet = List.map Unused (String.toList alphabetString)
    , errorCounter = 0
    , gameState = Playing
    }


defaultSettings : Flags -> Settings
defaultSettings flags =
    { language = Translations.defaultLanguage
    , theme = LightTheme
    , activeWordPacks = List.map .id flags.wordPackInfos
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
                [ requestWord model.settings.activeWordPacks
                , model.gameData |> convertGameData |> saveGameData
                ]
            )

        GuessLetter letter ->
            let
                newModel =
                    guessLetter model letter
            in
            ( newModel
            , Cmd.batch
                [ newModel.gameData |> convertGameData |> saveGameData
                , saveStatistics newModel.statistics
                ]
            )

        ChangeLanguage newLanguageStr ->
            let
                newLanguage =
                    Translations.languageFromString newLanguageStr

                newSettings =
                    updateLanguage model.settings newLanguage
            in
            ( { model
                | settings = newSettings
                , gameData = clearAlphabet model.gameData
              }
            , newSettings |> convertSettings |> saveSettings
            )

        GotWord word ->
            let
                newModel =
                    startNewGame model word
            in
            ( newModel, newModel.gameData |> convertGameData |> saveGameData )

        GotWordPackInfos infos ->
            ( { model | wordPacks = infos }, Cmd.none )

        ToggleDarkMode ->
            let
                newSettings =
                    updateTheme model.settings
            in
            ( { model | settings = newSettings }, newSettings |> convertSettings |> saveSettings )

        ToggleSettingsPanel ->
            ( { model | isSettingsPanelOpen = not model.isSettingsPanelOpen }, Cmd.none )

        ToggleUseDefaultPack ->
            ( { model | useDefaultPack = not model.useDefaultPack }, Cmd.none )

        GotCustomWordFiles files ->
            ( model
            , files
                |> List.map (\f -> ( File.name f, File.toString f ))
                |> List.map (\( n, t ) -> Task.perform (GotCustomWordContent n) t)
                |> Cmd.batch
            )

        GotCustomWordContent fileName content ->
            ( { model | fileInputIdx = model.fileInputIdx + 1 }
            , saveFileWordPack (FileWordPack fileName (content |> String.split "\n" |> List.filter (\s -> not <| String.isEmpty s)))
            )

        RemoveCustomWordPack id ->
            removeCustomWordPack model id

        ToggleCustomWordPackActive id ->
            let
                newSettings =
                    toggleActiveWordPack model.settings id
            in
            ( { model | settings = newSettings }, saveSettings <| convertSettings <| newSettings )


startNewGame : Model -> String -> Model
startNewGame model newWord =
    let
        characters =
            String.toList newWord

        firstLetter =
            Char.toLower (Maybe.withDefault ' ' (List.head characters))
    in
    { model
        | gameData =
            { shownWord = List.indexedMap (toLetter firstLetter) characters
            , alphabet = createAlphabet alphabetString firstLetter
            , errorCounter = 0
            , gameState = Playing
            }
    }


updateLanguage : Settings -> Translations.Language -> Settings
updateLanguage settings language =
    { settings | language = language }


toggleActiveWordPack : Settings -> Int -> Settings
toggleActiveWordPack settings id =
    let
        activeWordPacks =
            settings.activeWordPacks

        isContained =
            case
                activeWordPacks
                    |> List.filter (\i -> i == id)
                    |> List.head
            of
                Nothing ->
                    False

                Just _ ->
                    True

        isLastId =
            List.length activeWordPacks == 1

        newActiveWordPacks =
            if isContained then
                if isLastId then
                    activeWordPacks

                else
                    List.filter (\i -> i /= id) activeWordPacks

            else
                id :: activeWordPacks
    in
    { settings | activeWordPacks = newActiveWordPacks }


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
        gameData =
            model.gameData

        ( hasFoundLetter, newShownWord ) =
            guessLetterShownWord gameData.shownWord c

        newErrorCounter =
            updateErrorCounter gameData.errorCounter hasFoundLetter

        gameState =
            checkGameState gameData.gameState newShownWord newErrorCounter

        statistics =
            updateWordHighScores model.statistics gameState
    in
    { model
        | gameData =
            { shownWord = newShownWord
            , alphabet = guessLetterAlphabet gameData.alphabet c hasFoundLetter gameState
            , errorCounter = newErrorCounter
            , gameState = gameState
            }
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


clearAlphabet : GameData -> GameData
clearAlphabet gameData =
    { gameData | alphabet = List.map Disabled (String.toList alphabetString) }


updateTheme : Settings -> Settings
updateTheme settings =
    case settings.theme of
        DarkTheme ->
            { settings | theme = LightTheme }

        LightTheme ->
            { settings | theme = DarkTheme }


removeCustomWordPack : Model -> Int -> ( Model, Cmd msg )
removeCustomWordPack model id =
    let
        settings =
            model.settings

        newActiveWordPacks =
            List.filter (\i -> i /= id) settings.activeWordPacks

        newNewActiveWordPacks =
            if List.length newActiveWordPacks == 0 then
                model.wordPacks
                    |> List.filter (\wp -> wp.isDefault)
                    |> List.map .id

            else
                newActiveWordPacks

        newSettings =
            { settings | activeWordPacks = newNewActiveWordPacks }
    in
    ( { model | settings = newSettings }, Cmd.batch [ deleteFileWordPack id, saveSettings <| convertSettings newSettings ] )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveWord GotWord
        , receiveWordPackInfos GotWordPackInfos
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        language =
            model.settings.language

        theme =
            model.settings.theme
    in
    { title = Translations.getTitle language
    , body =
        [ div [ class "h-full", getBackgroundColor theme ]
            [ viewSettingsButton theme model.isSettingsPanelOpen
            , viewActivePage model
            ]
        ]
    }


viewActivePage : Model -> Html Msg
viewActivePage model =
    let
        language =
            model.settings.language

        theme =
            model.settings.theme

        gameData =
            model.gameData
    in
    if model.isSettingsPanelOpen then
        viewSettingsPanel model

    else
        div []
            [ viewWord gameData.shownWord gameData.gameState language theme
            , viewAlphabet gameData.gameState gameData.alphabet theme
            , viewGameOverText gameData.gameState language theme
            , viewNewGameButton gameData.gameState language theme
            , viewHangmanAndStatistics gameData.errorCounter theme
            ]


viewSettingsPanel : Model -> Html Msg
viewSettingsPanel model =
    let
        language =
            model.settings.language

        theme =
            model.settings.theme

        wordPacks =
            model.wordPacks

        fileInputIdx =
            model.fileInputIdx

        statistics =
            model.statistics
    in
    div [ class "flex justify-center" ]
        [ div [ class "grid gap-8 grid-cols-1 auto-rows-auto w-2/3" ]
            [ viewSettingsTitle theme language
            , viewColorThemeSelector theme language
            , viewLanguageSelector language theme
            , viewStatistics statistics language theme
            , viewCustomWordsFileUpload theme model.settings.activeWordPacks wordPacks fileInputIdx
            ]
        ]


viewSettingsTitle : ColorTheme -> Translations.Language -> Html msg
viewSettingsTitle theme language =
    h1
        [ getTextColor theme
        , class "text-2xl pt-8 pb-5"
        ]
        [ text (Translations.getSettingsTitle language) ]


viewSettingsButton : ColorTheme -> Bool -> Html Msg
viewSettingsButton theme isSettingsPanelOpen =
    let
        buttonClasses =
            "absolute top-8 right-10"

        accentColor =
            case theme of
                DarkTheme ->
                    Paint Color.white

                LightTheme ->
                    Paint Color.black
    in
    if isSettingsPanelOpen then
        button
            [ onClick ToggleSettingsPanel
            , class buttonClasses
            ]
            [ svg
                [ TypedSvg.Attributes.width (px 32)
                , TypedSvg.Attributes.height (px 32)
                , TypedSvg.Attributes.viewBox 0 0 50 50
                ]
                [ TypedSvg.circle
                    [ TypedSvg.Attributes.cx (px 25)
                    , TypedSvg.Attributes.cy (px 25)
                    , TypedSvg.Attributes.r (px 24)
                    , TypedSvg.Attributes.fill PaintNone
                    , TypedSvg.Attributes.stroke accentColor
                    , TypedSvg.Attributes.strokeWidth (px 1.5)
                    ]
                    []
                , TypedSvg.text_
                    [ TypedSvg.Attributes.fill accentColor
                    , TypedSvg.Attributes.stroke PaintNone
                    , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
                    , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineMiddle
                    , TypedSvg.Attributes.x (px 25)
                    , TypedSvg.Attributes.y (px 27)
                    ]
                    [ TypedSvg.Core.text "ESC" ]
                ]
            ]

    else
        button
            [ onClick ToggleSettingsPanel
            , class buttonClasses
            ]
            [ svg
                [ TypedSvg.Attributes.width (px 32)
                , TypedSvg.Attributes.height (px 32)
                , TypedSvg.Attributes.viewBox 0 0 24 24
                ]
                [ path
                    [ TypedSvg.Attributes.fill accentColor
                    , TypedSvg.Attributes.stroke PaintNone
                    , TypedSvg.Attributes.d "M12 8.666c-1.838 0-3.333 1.496-3.333 3.334s1.495 3.333 3.333 3.333 3.333-1.495 3.333-3.333-1.495-3.334-3.333-3.334m0 7.667c-2.39 0-4.333-1.943-4.333-4.333s1.943-4.334 4.333-4.334 4.333 1.944 4.333 4.334c0 2.39-1.943 4.333-4.333 4.333m-1.193 6.667h2.386c.379-1.104.668-2.451 2.107-3.05 1.496-.617 2.666.196 3.635.672l1.686-1.688c-.508-1.047-1.266-2.199-.669-3.641.567-1.369 1.739-1.663 3.048-2.099v-2.388c-1.235-.421-2.471-.708-3.047-2.098-.572-1.38.057-2.395.669-3.643l-1.687-1.686c-1.117.547-2.221 1.257-3.642.668-1.374-.571-1.656-1.734-2.1-3.047h-2.386c-.424 1.231-.704 2.468-2.099 3.046-.365.153-.718.226-1.077.226-.843 0-1.539-.392-2.566-.893l-1.687 1.686c.574 1.175 1.251 2.237.669 3.643-.571 1.375-1.734 1.654-3.047 2.098v2.388c1.226.418 2.468.705 3.047 2.098.581 1.403-.075 2.432-.669 3.643l1.687 1.687c1.45-.725 2.355-1.204 3.642-.669 1.378.572 1.655 1.738 2.1 3.047m3.094 1h-3.803c-.681-1.918-.785-2.713-1.773-3.123-1.005-.419-1.731.132-3.466.952l-2.689-2.689c.873-1.837 1.367-2.465.953-3.465-.412-.991-1.192-1.087-3.123-1.773v-3.804c1.906-.678 2.712-.782 3.123-1.773.411-.991-.071-1.613-.953-3.466l2.689-2.688c1.741.828 2.466 1.365 3.465.953.992-.412 1.082-1.185 1.775-3.124h3.802c.682 1.918.788 2.714 1.774 3.123 1.001.416 1.709-.119 3.467-.952l2.687 2.688c-.878 1.847-1.361 2.477-.952 3.465.411.992 1.192 1.087 3.123 1.774v3.805c-1.906.677-2.713.782-3.124 1.773-.403.975.044 1.561.954 3.464l-2.688 2.689c-1.728-.82-2.467-1.37-3.456-.955-.988.41-1.08 1.146-1.785 3.126"
                    ]
                    []
                ]
            ]


viewNewGameButton : GameState -> Translations.Language -> ColorTheme -> Html Msg
viewNewGameButton gameState language theme =
    case gameState of
        Playing ->
            div [] []

        _ ->
            div [ class "pb-2" ]
                [ button
                    ([ onClick NewGameButtonPressed
                     , class "px-4 py-2 rounded"
                     ]
                        ++ getNewGameButtonColors theme
                    )
                    [ text (Translations.getNewGameText language) ]
                ]


viewColorThemeSelector : ColorTheme -> Translations.Language -> Html Msg
viewColorThemeSelector theme language =
    let
        isChecked =
            case theme of
                DarkTheme ->
                    True

                LightTheme ->
                    False
    in
    div
        [ class "flex items-center justify-center px-5 py-3 shadow rounded-xl"
        , getHighlightedBackgroundColor theme
        ]
        [ div [ class "flex-1 text-xl", getTextColor theme ] [ text (Translations.getSettingsColorTheme language) ]
        , div
            [ class "flex-1 w-full flex items-center justify-center py-3" ]
            [ sunIcon theme
            , label
                [ class "switch" ]
                [ input [ type_ "checkbox", checked isChecked, onClick ToggleDarkMode ] []
                , span [ class "slider" ] []
                ]
            , moonIcon theme
            ]
        ]


sunIcon : ColorTheme -> Html msg
sunIcon theme =
    let
        accentColor =
            case theme of
                LightTheme ->
                    Paint Color.black

                DarkTheme ->
                    Paint Color.white
    in
    div [ class "px-2" ]
        [ svg
            [ TypedSvg.Attributes.width (px 32)
            , TypedSvg.Attributes.height (px 32)
            , TypedSvg.Attributes.viewBox 0 0 24 24
            , TypedSvg.Attributes.fill PaintNone
            , TypedSvg.Attributes.strokeWidth (px 2)
            , TypedSvg.Attributes.stroke accentColor
            , TypedSvg.Attributes.strokeLinecap TypedSvg.Types.StrokeLinecapRound
            , TypedSvg.Attributes.strokeLinejoin StrokeLinejoinRound
            ]
            [ circle [ cx (px 12), cy (px 12), r (px 5) ] []
            , line [ x1 (px 12), y1 (px 1), x2 (px 12), y2 (px 3) ] []
            , line [ x1 (px 12), y1 (px 21), x2 (px 12), y2 (px 23) ] []
            , line [ x1 (px 4.22), y1 (px 4.22), x2 (px 5.64), y2 (px 5.64) ] []
            , line [ x1 (px 18.36), y1 (px 18.36), x2 (px 19.78), y2 (px 19.78) ] []
            , line [ x1 (px 1), y1 (px 12), x2 (px 3), y2 (px 12) ] []
            , line [ x1 (px 21), y1 (px 12), x2 (px 23), y2 (px 12) ] []
            , line [ x1 (px 4.22), y1 (px 19.78), x2 (px 5.64), y2 (px 18.36) ] []
            , line [ x1 (px 18.36), y1 (px 5.64), x2 (px 19.78), y2 (px 4.22) ] []
            ]
        ]


moonIcon : ColorTheme -> Html Msg
moonIcon theme =
    let
        accentColor =
            case theme of
                LightTheme ->
                    Paint Color.black

                DarkTheme ->
                    Paint Color.white
    in
    div [ class "px-2" ]
        [ svg
            [ TypedSvg.Attributes.width (px 32)
            , TypedSvg.Attributes.height (px 32)
            , TypedSvg.Attributes.viewBox 0 0 24 24
            , fill accentColor
            ]
            [ TypedSvg.mask [ TypedSvg.Attributes.id "moon-mask" ]
                [ rect
                    [ TypedSvg.Attributes.x (px 0)
                    , TypedSvg.Attributes.y (px 0)
                    , width (percent 100)
                    , height (percent 100)
                    , fill (Paint Color.white)
                    ]
                    []
                , circle [ cx (px 19), cy (px 8), r (px 9), fill (Paint Color.black) ] []
                ]
            , circle [ cx (px 13), cy (px 12), r (px 7), TypedSvg.Attributes.mask "url(#moon-mask)" ] []
            ]
        ]


viewLanguageSelector : Translations.Language -> ColorTheme -> Html Msg
viewLanguageSelector language theme =
    div
        [ class "flex items-center justify-center px-5 py-3 shadow rounded-xl"
        , getHighlightedBackgroundColor theme
        ]
        [ div [ class "flex-1 text-xl", getTextColor theme ] [ text (Translations.getSettingsLanguage language) ]
        , div [ class "flex-1" ]
            [ select
                ([ class "appearance-none border py-3 px-4 rounded leading-tight focus:outline-none"
                 , onChange ChangeLanguage
                 , value <| Translations.languageToString language
                 ]
                    ++ getLanguageSelectColor theme
                )
                [ option [ selected (language == Translations.DE), value "DE" ] [ text "DE" ]
                , option [ selected (language == Translations.EN), value "EN" ] [ text "EN" ]
                ]
            ]
        ]


viewCustomWordsFileUpload : ColorTheme -> List Int -> List WordPackInfo -> Int -> Html Msg
viewCustomWordsFileUpload theme activeWordPacks wordPacks fileInputIdx =
    let
        invisibleTextColor =
            case theme of
                LightTheme ->
                    class "text-gray-100"

                DarkTheme ->
                    class "text-gray-700"
    in
    Html.Keyed.node "div"
        [ class "rounded-xl shadow p-5"
        , getHighlightedBackgroundColor theme
        ]
        [ ( "file-input" ++ String.fromInt fileInputIdx
          , input
                [ type_ "file"
                , Html.Attributes.multiple True
                , Html.Events.on "change" (Decode.map GotCustomWordFiles filesDecoder)
                , invisibleTextColor
                ]
                []
          )
        , ( "custom-words-files"
          , div [ class "bg-green-700" ]
                (List.map (viewWordPackInfo theme activeWordPacks) wordPacks)
          )
        ]


filesDecoder : Decode.Decoder (List File.File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


isWordPackActive : List Int -> WordPackInfo -> Bool
isWordPackActive activeWordPacks wordPack =
    case
        activeWordPacks
            |> List.filter (\i -> i == wordPack.id)
            |> List.head
    of
        Nothing ->
            False

        Just _ ->
            True


viewWordPackInfo : ColorTheme -> List Int -> WordPackInfo -> Html Msg
viewWordPackInfo theme activeWordPacks f =
    div [ getTextColor theme ]
        [ input [ type_ "checkbox", checked (isWordPackActive activeWordPacks f), onClick (ToggleCustomWordPackActive f.id) ] []
        , text f.name
        , button
            [ onClick (RemoveCustomWordPack f.id)
            , class "px-2 py-1 bg-red-600"
            , getTextColor theme
            , disabled f.isDefault
            ]
            [ text "X" ]
        ]


viewWord : List Letter -> GameState -> Translations.Language -> ColorTheme -> Html msg
viewWord letters gameState language theme =
    let
        classes =
            getWordBackgroundColor gameState theme

        renderedLetters =
            if List.isEmpty letters then
                [ div [ class "text-transparent" ] [ text "_" ] ]

            else
                List.map (viewLetter gameState) letters
    in
    div [ class "p-5 pb-3" ]
        [ div
            (class "text-4xl pb-4 pt-2 rounded flex flex-col items-center"
                :: classes
            )
            [ div [ class "flex-1" ] renderedLetters
            , div [ class "flex-1" ]
                [ viewGoogleLink letters gameState language theme
                , viewWikipediaLink letters gameState language theme
                ]
            ]
        ]


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


viewSearchLink : GameState -> Html.Attribute msg -> String -> ColorTheme -> Html msg
viewSearchLink gameState link linkText theme =
    if isGameOver gameState then
        a
            ([ target "_blank"
             , link
             , class "text-sm"
             , class "mx-2"
             , class "px-3"
             , class "py-2"
             , class "rounded"
             ]
                ++ getSearchLinkColor theme
            )
            [ text linkText ]

    else
        div [] []


viewGoogleLink : List Letter -> GameState -> Translations.Language -> ColorTheme -> Html msg
viewGoogleLink letters gameState language theme =
    let
        link =
            getGoogleLink letters

        linkText =
            Translations.getGoogleLinkText language
    in
    viewSearchLink gameState link linkText theme


getGoogleLink : List Letter -> Html.Attribute msg
getGoogleLink letters =
    let
        word =
            letters
                |> List.map getCharacter
                |> String.fromList
    in
    href ("http://www.google.com/search?q=" ++ Url.percentEncode word)


viewWikipediaLink : List Letter -> GameState -> Translations.Language -> ColorTheme -> Html msg
viewWikipediaLink letters gameState language theme =
    let
        link =
            getWikipediaLink letters language

        linkText =
            Translations.getWikipediaLinkText language
    in
    viewSearchLink gameState link linkText theme


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


viewAlphabet : GameState -> List AlphabetLetter -> ColorTheme -> Html Msg
viewAlphabet gameState alphabet theme =
    case gameState of
        Playing ->
            div
                [ class "flex"
                , class "items-center"
                , class "px-5"
                , class "py-2"
                , getBackgroundColor theme
                ]
                [ div
                    [ class "flex-1"
                    ]
                    (List.map (viewAlphabetLetter theme) alphabet)
                ]

        _ ->
            div [] []


viewAlphabetLetter : ColorTheme -> AlphabetLetter -> Html Msg
viewAlphabetLetter theme letter =
    let
        ( classes, disabled_, c ) =
            getClassesAndDisabledForAlphabetLetterButton letter theme
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


viewGameOverText : GameState -> Translations.Language -> ColorTheme -> Html msg
viewGameOverText gameState language theme =
    case gameState of
        Playing ->
            div [] []

        HasWon ->
            div [ class "pb-2", getTextColor theme ] [ text (Translations.getWonText language) ]

        HasLost ->
            div [ class "pb-2", getTextColor theme ] [ text (Translations.getLostText language) ]


viewHangmanAndStatistics : Int -> ColorTheme -> Html msg
viewHangmanAndStatistics counter theme =
    div
        [ class "px-5 pt-2 pb-5"
        , getBackgroundColor theme
        ]
        [ div
            [ class "grid grid-cols-1 lg:grid-cols-2 xl:grid-cols-2 py-5 rounded-xl shadow"
            , getHighlightedBackgroundColor theme
            ]
            [ viewHangman counter theme
            ]
        ]


viewHangman : Int -> ColorTheme -> Html msg
viewHangman counter theme =
    div
        [ class "flex justify-center lg:justify-end xl:justify-end"
        ]
        [ viewHangmanSvg counter theme ]


viewHangmanSvg : Int -> ColorTheme -> Html msg
viewHangmanSvg counter theme =
    let
        lineColor =
            getLineColor theme
    in
    svg
        [ viewBox 0 0 36 40
        , width (px 360)
        , height (px 400)
        ]
        [ g [ transform [ Scale 0.98 0.98, Translate 1 0.5 ] ]
            [ viewHangmanPerson counter
            , line
                [ x1 (px 30)
                , y1 (px 0)
                , x2 (px 30)
                , y2 (px 10)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 9)
                ]
                []
            , line
                [ x1 (px 20)
                , y1 (px 0)
                , x2 (px 30.5)
                , y2 (px 0)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 8)
                ]
                []
            , line
                [ x1 (px 10)
                , y1 (px 10)
                , x2 (px 20)
                , y2 (px 0)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 7)
                ]
                []
            , line
                [ x1 (px 10)
                , y1 (px 0)
                , x2 (px 20.3)
                , y2 (px 0)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 6)
                ]
                []
            , line
                [ x1 (px 10)
                , y1 (px 10)
                , x2 (px 10)
                , y2 (px -1)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 5)
                ]
                []
            , line
                [ x1 (px 10)
                , y1 (px 20)
                , x2 (px 10)
                , y2 (px 10)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 4)
                ]
                []
            , line
                [ x1 (px 10)
                , y1 (px 30)
                , x2 (px 10)
                , y2 (px 20)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 3)
                ]
                []
            , line
                [ x1 (px -5)
                , y1 (px 45)
                , x2 (px 10.35)
                , y2 (px 29.65)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 2)
                ]
                []
            , line
                [ x1 (px 10)
                , y1 (px 30)
                , x2 (px 25)
                , y2 (px 45)
                , strokeWidth (px 1)
                , stroke <| Paint <| lineColor (counter >= 1)
                ]
                []
            ]
        ]


viewHangmanPerson : Int -> TypedSvg.Core.Svg msg
viewHangmanPerson counter =
    g [ isVisible (counter >= 10) ]
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


viewStatistics : Statistics -> Translations.Language -> ColorTheme -> Html msg
viewStatistics statistics language theme =
    div
        [ class "flex flex-col items-center p-4 rounded-xl shadow"
        , getHighlightedBackgroundColor theme
        , getStatisticsTextColor theme
        ]
        [ viewStatisticsPane statistics language ]


viewStatisticsPane : Statistics -> Translations.Language -> Html msg
viewStatisticsPane statistics language =
    table
        [ class "table-auto"
        , class "border-collapse"
        ]
        [ viewStatisticsTableHeader language
        , tbody []
            [ viewStatisticsWords statistics language
            , viewStatisticsLetters statistics language
            , viewStatisticsWordSeriesCurrent statistics language
            , viewStatisticsWordSeriesOverall statistics language
            , viewStatisticsLetterSeriesCurrent statistics language
            , viewStatisticsLetterSeriesOverall statistics language
            ]
        ]


viewStatisticsTableHeader : Translations.Language -> Html msg
viewStatisticsTableHeader language =
    thead []
        [ tr []
            [ th [ class "px-0 sm:px-2 lg:px-4", class "py-1" ] []
            , th [ class "px-0 sm:px-2 lg:px-4", class "py-1" ] (viewAlternateText Translations.getStatisticsTableHeaderCorrect language)
            , th [ class "px-0 sm:px-2 lg:px-4", class "py-1" ] (viewAlternateText Translations.getStatisticsTableHeaderIncorrect language)
            , th [ class "px-0 sm:px-2 lg:px-4", class "py-1" ] (viewAlternateText Translations.getStatisticsTableHeaderTotal language)
            , th [ class "px-0 sm:px-2 lg:px-4", class "py-1" ] (viewAlternateText Translations.getStatisticsTableHeaderRatio language)
            ]
        ]


viewStatisticsWords : Statistics -> Translations.Language -> Html msg
viewStatisticsWords statistics language =
    tr []
        [ td [ class "px-2", class "py-1", class "text-right" ] [ span [] [ text <| Translations.getWordsTotalText language ] ]
        , td [ class "px-2", class "py-1" ] [ text <| String.fromInt statistics.correctWordsTotal ]
        , td [ class "px-2", class "py-1" ] [ text <| String.fromInt statistics.incorrectWordsTotal ]
        , td [ class "px-2", class "py-1" ] [ text <| String.fromInt (statistics.correctWordsTotal + statistics.incorrectWordsTotal) ]
        , td [ class "px-2", class "py-1" ] [ text <| Round.round 2 <| calculateRatio statistics.correctWordsTotal statistics.incorrectWordsTotal ]
        ]


viewStatisticsLetters : Statistics -> Translations.Language -> Html msg
viewStatisticsLetters statistics language =
    tr []
        [ td [ class "px-2", class "pb-2", class "text-right" ] (viewAlternateText Translations.getLettersTotalText language)
        , td [ class "px-2", class "pb-2" ] [ text <| String.fromInt statistics.correctLettersTotal ]
        , td [ class "px-2", class "pb-2" ] [ text <| String.fromInt statistics.incorrectLettersTotal ]
        , td [ class "px-2", class "pb-2" ] [ text <| String.fromInt (statistics.correctLettersTotal + statistics.incorrectLettersTotal) ]
        , td [ class "px-2", class "pb-2" ] [ text <| Round.round 2 <| calculateRatio statistics.correctLettersTotal statistics.incorrectLettersTotal ]
        ]


viewStatisticsWordSeriesCurrent : Statistics -> Translations.Language -> Html msg
viewStatisticsWordSeriesCurrent statistics language =
    tr []
        [ td [ class "px-2", class "pt-2", class "text-right" ] (viewAlternateText Translations.getCurrentWordStreakText language)
        , td [ class "px-2", class "pt-2" ] [ text <| String.fromInt statistics.mostCorrectWordsCurrent ]
        , td [ class "px-2", class "pt-2" ] [ text <| String.fromInt statistics.mostIncorrectWordsCurrent ]
        , td [ class "px-2", class "pt-2" ] []
        , td [ class "px-2", class "pt-2" ] []
        ]


viewStatisticsWordSeriesOverall : Statistics -> Translations.Language -> Html msg
viewStatisticsWordSeriesOverall statistics language =
    tr []
        [ td [ class "px-2", class "pb-2", class "text-right" ] (viewAlternateText Translations.getBestWordStreakText language)
        , td [ class "px-2", class "pb-2" ] [ text <| String.fromInt statistics.mostCorrectWordsOverall ]
        , td [ class "px-2", class "pb-2" ] [ text <| String.fromInt statistics.mostIncorrectWordsOverall ]
        , td [ class "px-2", class "pb-2" ] [ text <| String.fromInt (statistics.mostCorrectWordsOverall + statistics.mostIncorrectWordsOverall) ]
        , td [ class "px-2", class "pb-2" ] [ text <| Round.round 2 <| calculateRatio statistics.mostCorrectWordsOverall statistics.mostIncorrectWordsOverall ]
        ]


viewStatisticsLetterSeriesCurrent : Statistics -> Translations.Language -> Html msg
viewStatisticsLetterSeriesCurrent statistics language =
    tr []
        [ td [ class "px-2", class "pt-2", class "text-right" ] (viewAlternateText Translations.getCurrentLetterStreakText language)
        , td [ class "px-2", class "pt-2" ] [ text <| String.fromInt statistics.mostCorrectLettersCurrent ]
        , td [ class "px-2", class "pt-2" ] [ text <| String.fromInt statistics.mostIncorrectLettersCurrent ]
        , td [ class "px-2", class "pt-2" ] []
        , td [ class "px-2", class "pt-2" ] []
        ]


viewStatisticsLetterSeriesOverall : Statistics -> Translations.Language -> Html msg
viewStatisticsLetterSeriesOverall statistics language =
    tr []
        [ td [ class "px-2", class "text-right" ] (viewAlternateText Translations.getBestLetterStreakText language)
        , td [ class "px-2" ] [ text <| String.fromInt statistics.mostCorrectLettersOverall ]
        , td [ class "px-2" ] [ text <| String.fromInt statistics.mostIncorrectLettersOverall ]
        , td [ class "px-2" ] [ text <| String.fromInt (statistics.mostCorrectLettersOverall + statistics.mostIncorrectLettersOverall) ]
        , td [ class "px-2" ] [ text <| Round.round 2 <| calculateRatio statistics.mostCorrectLettersOverall statistics.mostIncorrectLettersOverall ]
        ]



-- HELPER


getNewGameButtonColors : ColorTheme -> List (Html.Attribute msg)
getNewGameButtonColors theme =
    case theme of
        DarkTheme ->
            [ class "text-white"
            , class "bg-blue-700"
            ]

        LightTheme ->
            [ class "text-white"
            , class "bg-blue-700"
            ]


getLanguageSelectColor : ColorTheme -> List (Html.Attribute msg)
getLanguageSelectColor theme =
    case theme of
        DarkTheme ->
            [ class "bg-blue-800"
            , getTextColor theme
            , class "border-blue-800"
            , class "focus:bg-blue-600"
            , class "focus:border-blue-700"
            ]

        LightTheme ->
            [ class "bg-gray-300"
            , getTextColor theme
            , class "border-gray-300"
            , class "focus:border-gray-400"
            ]


getClassesAndDisabledForAlphabetLetterButton : AlphabetLetter -> ColorTheme -> ( List (Html.Attribute msg), Bool, Char )
getClassesAndDisabledForAlphabetLetterButton letter theme =
    case theme of
        DarkTheme ->
            case letter of
                Unused c ->
                    ( [ class "text-gray-200", class "bg-gray-600" ], False, c )

                CorrectlyUsed c ->
                    ( [ class "text-gray-200", class "bg-green-700", class "opacity-75", class "cursor-not-allowed" ], True, c )

                IncorrectlyUsed c ->
                    ( [ class "text-gray-200", class "bg-red-600", class "opacity-75", class "cursor-not-allowed" ], True, c )

                Disabled c ->
                    ( [ class "text-gray-200", class "bg-gray-700", class "opacity-50", class "cursor-not-allowed" ], True, c )

        LightTheme ->
            case letter of
                Unused c ->
                    ( [ class "bg-gray-300" ], False, c )

                CorrectlyUsed c ->
                    ( [ class "bg-green-400", class "opacity-75", class "cursor-not-allowed" ], True, c )

                IncorrectlyUsed c ->
                    ( [ class "bg-red-500", class "opacity-75", class "cursor-not-allowed" ], True, c )

                Disabled c ->
                    ( [ class "bg-gray-300", class "opacity-50", class "cursor-not-allowed" ], True, c )


getCharacter : Letter -> Char
getCharacter letter =
    case letter of
        Shown c ->
            c

        Hidden c ->
            c


getWordBackgroundColor : GameState -> ColorTheme -> List (Html.Attribute msg)
getWordBackgroundColor gameState theme =
    let
        textColor =
            getTextColor theme
    in
    case gameState of
        Playing ->
            [ textColor ]

        HasWon ->
            [ textColor, class "bg-green-700" ]

        HasLost ->
            [ textColor, class "bg-red-700" ]


isGameOver : GameState -> Bool
isGameOver gameState =
    case gameState of
        Playing ->
            False

        HasLost ->
            True

        HasWon ->
            True


getSearchLinkColor : ColorTheme -> List (Html.Attribute msg)
getSearchLinkColor theme =
    case theme of
        DarkTheme ->
            [ class "bg-gray-700"
            , class "text-gray-300"
            ]

        LightTheme ->
            [ class "bg-gray-600"
            , class "text-white"
            ]


isVisible : Bool -> TypedSvg.Core.Attribute msg
isVisible b =
    if b then
        visibility "visible"

    else
        visibility "hidden"


getLineColor : ColorTheme -> Bool -> Color.Color
getLineColor theme active =
    case theme of
        DarkTheme ->
            getLineColorDarkTheme active

        LightTheme ->
            getLineColorLightTheme active


getLineColorDarkTheme : Bool -> Color.Color
getLineColorDarkTheme active =
    if active then
        Color.rgba 0 0 0 1

    else
        Color.rgba 0.3 0.36 0.45 1


getLineColorLightTheme : Bool -> Color.Color
getLineColorLightTheme active =
    if active then
        Color.rgba 0 0 0 1

    else
        Color.rgba 0.9 0.9 0.9 1


getBackgroundColor : ColorTheme -> Html.Attribute msg
getBackgroundColor theme =
    case theme of
        DarkTheme ->
            class "bg-gray-800"

        LightTheme ->
            class "bg-white"


getHighlightedBackgroundColor : ColorTheme -> Html.Attribute msg
getHighlightedBackgroundColor theme =
    case theme of
        DarkTheme ->
            class "bg-gray-700"

        LightTheme ->
            class "bg-gray-100"


getTextColor : ColorTheme -> Html.Attribute msg
getTextColor theme =
    case theme of
        DarkTheme ->
            class "text-white"

        LightTheme ->
            class "text-black"


getStatisticsTextColor : ColorTheme -> Html.Attribute msg
getStatisticsTextColor theme =
    case theme of
        DarkTheme ->
            class "text-gray-300"

        LightTheme ->
            class "text-black"


calculateRatio : Int -> Int -> Float
calculateRatio i1 i2 =
    if i2 == 0 then
        toFloat i1

    else
        let
            ratio =
                toFloat i1 / toFloat i2
        in
        if isInfinite ratio then
            0

        else if isNaN ratio then
            0

        else
            ratio


viewAlternateText : (Translations.Language -> ( String, String )) -> Translations.Language -> List (Html msg)
viewAlternateText textFunc language =
    let
        ( bigText, smallText ) =
            textFunc language
    in
    [ span [ class "hidden", class "sm:inline-block" ] [ text bigText ]
    , span [ class "inline-block", class "sm:hidden" ] [ text smallText ]
    ]


convertSettings : Settings -> SettingsFlags
convertSettings settings =
    let
        convertedLanguage =
            Translations.languageToString settings.language

        convertedTheme =
            case settings.theme of
                DarkTheme ->
                    "DarkTheme"

                LightTheme ->
                    "LightTheme"
    in
    { language = convertedLanguage
    , theme = convertedTheme
    , activeWordPacks = settings.activeWordPacks
    }


convertSettingsFlags : SettingsFlags -> Settings
convertSettingsFlags settingsFlags =
    let
        convertedLanguage =
            Translations.languageFromString settingsFlags.language

        convertedTheme =
            if settingsFlags.theme == "DarkTheme" then
                DarkTheme

            else if settingsFlags.theme == "LightTheme" then
                LightTheme

            else
                LightTheme
    in
    { language = convertedLanguage
    , theme = convertedTheme
    , activeWordPacks = settingsFlags.activeWordPacks
    }


convertGameData : GameData -> GameDataFlags
convertGameData gameData =
    let
        convertedWord =
            List.map convertLetter gameData.shownWord

        convertedAlphabet =
            List.map convertAlphabetLetter gameData.alphabet

        convertedGameState =
            case gameData.gameState of
                Playing ->
                    "Playing"

                HasWon ->
                    "HasWon"

                HasLost ->
                    "HasLost"
    in
    { shownWord = convertedWord
    , alphabet = convertedAlphabet
    , errorCounter = gameData.errorCounter
    , gameState = convertedGameState
    }


convertLetter : Letter -> LetterFlags
convertLetter letter =
    case letter of
        Shown c ->
            { shown = True, letter = String.fromList [ c ] }

        Hidden c ->
            { shown = False, letter = String.fromList [ c ] }


convertAlphabetLetter : AlphabetLetter -> AlphabetLetterFlags
convertAlphabetLetter letter =
    case letter of
        Unused c ->
            { state = "Unused", letter = String.fromList [ c ] }

        CorrectlyUsed c ->
            { state = "CorrectlyUsed", letter = String.fromList [ c ] }

        IncorrectlyUsed c ->
            { state = "IncorrectlyUsed", letter = String.fromList [ c ] }

        Disabled c ->
            { state = "Disabled", letter = String.fromList [ c ] }


convertGameDataFlags : GameDataFlags -> GameData
convertGameDataFlags flags =
    let
        convertedWord =
            List.map convertLetterFlags flags.shownWord

        convertedAlphabet =
            List.map convertAlphabetLetterFlags flags.alphabet

        convertedGameState =
            if flags.gameState == "Playing" then
                Playing

            else if flags.gameState == "HasWon" then
                HasWon

            else if flags.gameState == "HasLost" then
                HasLost

            else
                Playing
    in
    { shownWord = convertedWord
    , alphabet = convertedAlphabet
    , errorCounter = flags.errorCounter
    , gameState = convertedGameState
    }


convertLetterFlags : LetterFlags -> Letter
convertLetterFlags letter =
    if letter.shown then
        Shown <| convertStringToChar <| letter.letter

    else
        Hidden <| convertStringToChar <| letter.letter


convertAlphabetLetterFlags : AlphabetLetterFlags -> AlphabetLetter
convertAlphabetLetterFlags letter =
    if letter.state == "Unused" then
        Unused <| convertStringToChar <| letter.letter

    else if letter.state == "CorrectlyUsed" then
        CorrectlyUsed <| convertStringToChar <| letter.letter

    else if letter.state == "IncorrectlyUsed" then
        IncorrectlyUsed <| convertStringToChar <| letter.letter

    else if letter.state == "Disabled" then
        Disabled <| convertStringToChar <| letter.letter

    else
        Unused <| convertStringToChar <| letter.letter


convertStringToChar : String -> Char
convertStringToChar str =
    str
        |> String.toList
        |> List.head
        |> Maybe.withDefault ' '
