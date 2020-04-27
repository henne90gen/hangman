module Translations exposing (..)

import WordList exposing (wordList_de, wordList_en)


type Language
    = DE
    | EN


getTitle : Language -> String
getTitle language =
    case language of
        DE ->
            "Galgenraten"

        EN ->
            "Hangman"


getNewGameText : Language -> String
getNewGameText language =
    case language of
        DE ->
            "Neues Spiel starten"

        EN ->
            "Start New Game"


getWordList : Language -> List String
getWordList language =
    case language of
        DE ->
            wordList_de

        EN ->
            wordList_en


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


getGoogleLinkText : Language -> String
getGoogleLinkText language =
    case language of
        DE ->
            "Google Suche"

        EN ->
            "Google Search"


getWikipediaLinkText : Language -> String
getWikipediaLinkText language =
    case language of
        DE ->
            "Wikipedia Suche"

        EN ->
            "Wikipedia Search"


getShowStatisticsButtonText : Language -> String
getShowStatisticsButtonText language =
    case language of
        DE ->
            "Statistiken anzeigen"

        EN ->
            "Show Statistics"


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
