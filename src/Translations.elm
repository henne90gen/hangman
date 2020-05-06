module Translations exposing (..)


type Language
    = DE
    | EN


defaultLanguage : Language
defaultLanguage =
    DE


languageFromString : String -> Language
languageFromString str =
    if str == "DE" then
        DE

    else if str == "EN" then
        EN

    else
        -- return default language
        defaultLanguage


languageToString : Language -> String
languageToString language =
    case language of
        DE ->
            "DE"

        EN ->
            "EN"


toDirectory : Language -> String
toDirectory language =
    case language of
        DE ->
            "de/"

        EN ->
            "en/"


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
            "Nächstes Wort"

        EN ->
            "Next Word"


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


getWordsTotalText : Language -> String
getWordsTotalText language =
    case language of
        DE ->
            "Worte"

        EN ->
            "Words"


getLettersTotalText : Language -> String
getLettersTotalText language =
    case language of
        DE ->
            "Buchstaben"

        EN ->
            "Letters"


getLettersTotalTextShort : Language -> String
getLettersTotalTextShort language =
    case language of
        DE ->
            "Buchst."

        EN ->
            "Letters"


getCurrentWordStreakText : Language -> String
getCurrentWordStreakText language =
    case language of
        DE ->
            "Aktuelle Wortserie"

        EN ->
            "Current Word Streak"


getCurrentWordStreakTextShort : Language -> String
getCurrentWordStreakTextShort language =
    case language of
        DE ->
            "Aktuelle Worts."

        EN ->
            "Current W. Streak"


getBestWordStreakText : Language -> String
getBestWordStreakText language =
    case language of
        DE ->
            "Längste Wortserie"

        EN ->
            "Longest Word Streak"


getBestWordStreakTextShort : Language -> String
getBestWordStreakTextShort language =
    case language of
        DE ->
            "Längste Worts."

        EN ->
            "Longest W. Streak"


getCurrentLetterStreakText : Language -> String
getCurrentLetterStreakText language =
    case language of
        DE ->
            "Aktuelle Buchstabenserie"

        EN ->
            "Current Letter Streak"


getCurrentLetterStreakTextShort : Language -> String
getCurrentLetterStreakTextShort language =
    case language of
        DE ->
            "Aktuelle Buchst.s."

        EN ->
            "Current L. Streak"


getBestLetterStreakText : Language -> String
getBestLetterStreakText language =
    case language of
        DE ->
            "Längste Buchstabenserie"

        EN ->
            "Longest Letter Streak"


getBestLetterStreakTextShort : Language -> String
getBestLetterStreakTextShort language =
    case language of
        DE ->
            "Längste Buchst.s."

        EN ->
            "Longest L. Streak"


getStatisticsTableHeaderCorrect : Language -> String
getStatisticsTableHeaderCorrect language =
    case language of
        DE ->
            "Richtig"

        EN ->
            "Correct"


getStatisticsTableHeaderCorrectShort : Language -> String
getStatisticsTableHeaderCorrectShort language =
    case language of
        DE ->
            "R"

        EN ->
            "C"


getStatisticsTableHeaderIncorrect : Language -> String
getStatisticsTableHeaderIncorrect language =
    case language of
        DE ->
            "Falsch"

        EN ->
            "Incorrect"


getStatisticsTableHeaderIncorrectShort : Language -> String
getStatisticsTableHeaderIncorrectShort language =
    case language of
        DE ->
            "F"

        EN ->
            "I"


getStatisticsTableHeaderTotal : Language -> String
getStatisticsTableHeaderTotal language =
    case language of
        DE ->
            "Summe"

        EN ->
            "Total"


getStatisticsTableHeaderTotalShort : Language -> String
getStatisticsTableHeaderTotalShort language =
    case language of
        DE ->
            "S"

        EN ->
            "T"


getStatisticsTableHeaderRatio : Language -> String
getStatisticsTableHeaderRatio language =
    case language of
        DE ->
            "Verhältnis"

        EN ->
            "Ratio"


getStatisticsTableHeaderRatioShort : Language -> String
getStatisticsTableHeaderRatioShort language =
    case language of
        DE ->
            "R/F"

        EN ->
            "C/I"
