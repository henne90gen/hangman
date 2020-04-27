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
            "Richtige Worte: "

        EN ->
            "Correct Words: "


getIncorrectWordsTotalText : Language -> String
getIncorrectWordsTotalText language =
    case language of
        DE ->
            "Falsche Worte: "

        EN ->
            "Incorrect Words: "


getCorrectLettersTotalText : Language -> String
getCorrectLettersTotalText language =
    case language of
        DE ->
            "Richtige Buchstaben: "

        EN ->
            "Correct Letters: "


getIncorrectLettersTotalText : Language -> String
getIncorrectLettersTotalText language =
    case language of
        DE ->
            "Falsche Buchstaben: "

        EN ->
            "Incorrect Letters: "


getCurrentCorrectWordStreakText : Language -> String
getCurrentCorrectWordStreakText language =
    case language of
        DE ->
            "Aktuelle Serie richtiger Worte: "

        EN ->
            "Current Correct Word Streak: "


getBestCorrectWordStreakText : Language -> String
getBestCorrectWordStreakText language =
    case language of
        DE ->
            "Längste Serie richtiger Worte: "

        EN ->
            "Longest Correct Word Streak: "


getCurrentIncorrectWordStreakText : Language -> String
getCurrentIncorrectWordStreakText language =
    case language of
        DE ->
            "Aktuelle Serie falscher Worte: "

        EN ->
            "Current Incorrect Word Streak: "


getBestIncorrectWordStreakText : Language -> String
getBestIncorrectWordStreakText language =
    case language of
        DE ->
            "Längste Serie falscher Worte: "

        EN ->
            "Longest Incorrect Word Streak: "


getCurrentCorrectLetterStreakText : Language -> String
getCurrentCorrectLetterStreakText language =
    case language of
        DE ->
            "Aktuelle Serie richtiger Buchstaben: "

        EN ->
            "Current Correct Letter Streak: "


getBestCorrectLetterStreakText : Language -> String
getBestCorrectLetterStreakText language =
    case language of
        DE ->
            "Längste Serie richtiger Buchstaben: "

        EN ->
            "Longest Correct Letter Streak: "


getCurrentIncorrectLetterStreakText : Language -> String
getCurrentIncorrectLetterStreakText language =
    case language of
        DE ->
            "Aktuelle Serie falscher Worte: "

        EN ->
            "Current Incorrect Letter Streak: "


getBestIncorrectLetterStreakText : Language -> String
getBestIncorrectLetterStreakText language =
    case language of
        DE ->
            "Längste Serie falscher Worte: "

        EN ->
            "Longest Incorrect Letter Streak: "
