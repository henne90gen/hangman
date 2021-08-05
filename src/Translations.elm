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


getLettersTotalText : Language -> ( String, String )
getLettersTotalText language =
    case language of
        DE ->
            ( "Buchstaben", "Buchst." )

        EN ->
            ( "Letters", "Letters" )


getCurrentWordStreakText : Language -> ( String, String )
getCurrentWordStreakText language =
    case language of
        DE ->
            ( "Aktuelle Wortserie", "Aktuelle Worts." )

        EN ->
            ( "Current Word Streak", "Current W. Streak" )


getBestWordStreakText : Language -> ( String, String )
getBestWordStreakText language =
    case language of
        DE ->
            ( "Längste Wortserie", "Längste Worts." )

        EN ->
            ( "Longest Word Streak", "Longest W. Streak" )


getCurrentLetterStreakText : Language -> ( String, String )
getCurrentLetterStreakText language =
    case language of
        DE ->
            ( "Aktuelle Buchstabenserie", "Aktuelle Buchst.s." )

        EN ->
            ( "Current Letter Streak", "Current L. Streak" )


getBestLetterStreakText : Language -> ( String, String )
getBestLetterStreakText language =
    case language of
        DE ->
            ( "Längste Buchstabenserie", "Längste Buchst.s." )

        EN ->
            ( "Longest Letter Streak", "Longest L. Streak" )


getStatisticsTableHeaderCorrect : Language -> ( String, String )
getStatisticsTableHeaderCorrect language =
    case language of
        DE ->
            ( "Richtig", "R" )

        EN ->
            ( "Correct", "C" )


getStatisticsTableHeaderIncorrect : Language -> ( String, String )
getStatisticsTableHeaderIncorrect language =
    case language of
        DE ->
            ( "Falsch", "F" )

        EN ->
            ( "Incorrect", "I" )


getStatisticsTableHeaderTotal : Language -> ( String, String )
getStatisticsTableHeaderTotal language =
    case language of
        DE ->
            ( "Summe", "S" )

        EN ->
            ( "Total", "T" )


getStatisticsTableHeaderRatio : Language -> ( String, String )
getStatisticsTableHeaderRatio language =
    case language of
        DE ->
            ( "Verhältnis", "R/F" )

        EN ->
            ( "Ratio", "C/I" )


getSettingsButtonOpen : Language -> String
getSettingsButtonOpen language =
    case language of
        DE ->
            "Einstellungen öffnen"

        EN ->
            "Open Settings"


getSettingsButtonClose : Language -> String
getSettingsButtonClose language =
    case language of
        DE ->
            "Einstellungen schließen"

        EN ->
            "Close Settings"


getSettingsTitle : Language -> String
getSettingsTitle language =
    case language of
        DE ->
            "Einstellungen"

        EN ->
            "Settings"


getSettingsColorTheme : Language -> String
getSettingsColorTheme language =
    case language of
        DE ->
            "Farbschema"

        EN ->
            "Color Theme"


getSettingsLanguage : Language -> String
getSettingsLanguage language =
    case language of
        DE ->
            "Sprache"

        EN ->
            "Language"


getSettingsWordPacks : Language -> String
getSettingsWordPacks language =
    case language of
        DE ->
            -- TODO find a better translation
            "Wort Pakete"

        EN ->
            "Word Packs"


getSettingsUpload : Language -> String
getSettingsUpload language =
    case language of
        DE ->
            -- TODO find a better translation
            "Hochladen"

        EN ->
            "Upload"


getSettingsWPActive : Language -> String
getSettingsWPActive language =
    case language of
        DE ->
            "Aktiv"

        EN ->
            "Active"


getSettingsWPName : Language -> String
getSettingsWPName language =
    case language of
        DE ->
            "Name"

        EN ->
            "Name"


getSettingsReset : Language -> String
getSettingsReset language =
    case language of
        DE ->
            "Zurücksetzen"

        EN ->
            "Reset"


getSettingsResetSettings : Language -> String
getSettingsResetSettings language =
    case language of
        DE ->
            "Einstellungen"

        EN ->
            "Settings"


getSettingsResetGameData : Language -> String
getSettingsResetGameData language =
    case language of
        DE ->
            "Spieldaten"

        EN ->
            "Game Data"


getSettingsResetStatistics : Language -> String
getSettingsResetStatistics language =
    case language of
        DE ->
            "Statistiken"

        EN ->
            "Statistics"


getSettingsGame : Language -> String
getSettingsGame language =
    case language of
        DE ->
            "Spiel"

        EN ->
            "Game"


getSettingsGameShowWrongLetters : Language -> String
getSettingsGameShowWrongLetters language =
    case language of
        DE ->
            "Falsche Buchstaben zeigen"

        EN ->
            "Show Wrong Letters"
