type MainAppType = {
    ports: any;
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

type FlagsType = {
    statistics: Statistics | null;
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
