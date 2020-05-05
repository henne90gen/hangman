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


getCurrentWordStreakText : Language -> String
getCurrentWordStreakText language =
    case language of
        DE ->
            "Aktuelle Wortserie"

        EN ->
            "Current Word Streak"


getBestWordStreakText : Language -> String
getBestWordStreakText language =
    case language of
        DE ->
            "Längste Wortserie"

        EN ->
            "Longest Word Streak"


getCurrentLetterStreakText : Language -> String
getCurrentLetterStreakText language =
    case language of
        DE ->
            "Aktuelle Buchstabenserie"

        EN ->
            "Current Letter Streak"


getBestLetterStreakText : Language -> String
getBestLetterStreakText language =
    case language of
        DE ->
            "Längste Buchstabenserie"

        EN ->
            "Longest Letter Streak"


getStatisticsTableHeaderCorrect : Language -> String
getStatisticsTableHeaderCorrect language =
    case language of
        DE ->
            "Richtig"

        EN ->
            "Correct"


getStatisticsTableHeaderIncorrect : Language -> String
getStatisticsTableHeaderIncorrect language =
    case language of
        DE ->
            "Falsch"

        EN ->
            "Incorrect"


getStatisticsTableHeaderTotal : Language -> String
getStatisticsTableHeaderTotal language =
    case language of
        DE ->
            "Summe"

        EN ->
            "Total"


getStatisticsTableHeaderRatio : Language -> String
getStatisticsTableHeaderRatio language =
    case language of
        DE ->
            "Verhältnis"

        EN ->
            "Ratio"
