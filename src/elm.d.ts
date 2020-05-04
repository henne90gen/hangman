type MainAppType = {
    ports: {
        receiveWord: { send: (args: string[]) => void };
        requestWord: any;
        saveStatistics: any;
        saveSettings: any;
    };
};

type Statistics = {
    mostCorrectWordsOverall: number;
    mostCorrectWordsCurrent: number;
    mostIncorrectWordsOverall: number;
    mostIncorrectWordsCurrent: number;
    mostCorrectLettersOverall: number;
    mostCorrectLettersCurrent: number;
    mostIncorrectLettersOverall: number;
    mostIncorrectLettersCurrent: number;
    correctWordsTotal: number;
    incorrectWordsTotal: number;
    correctLettersTotal: number;
    incorrectLettersTotal: number;
};

type Language = 'DE' | 'EN';

type ColorTheme = 'LightTheme' | 'DarkTheme';

type Settings = {
    language: Language;
    theme: ColorTheme;
};

type FlagsType = {
    statistics: Statistics | null;
    settings: Settings | null;
};

type InitOptionsType = {
    flags: FlagsType;
    node: HTMLElement | null;
};

type MainType = {
    init: (options: InitOptionsType) => MainAppType;
};

type ElmType = {
    Main: MainType;
};

declare module '*.elm' {
    export const Elm: ElmType;
}
