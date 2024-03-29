interface Subscribe<Func> {
    subscribe: (f: Func) => void;
}
interface Send<Func> {
    send: Func;
}
type MainAppType = {
    ports: {
        receiveWord: Send<(word: string) => void>;
        receiveWordPackInfos: Send<(infos: WordPackInfo[]) => void>;

        requestWord: Subscribe<(ids: number[]) => void>;
        saveStatistics: Subscribe<(stats: Statistics) => void>;
        saveSettings: Subscribe<(settings: Settings) => void>;
        saveGameData: Subscribe<(data: GameData) => void>;
        saveFileWordPack: Subscribe<(wp: FileWordPack) => void>;
        deleteFileWordPack: Subscribe<(id: number) => void>;
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
    activeWordPacks: number[];
};

type GameData = {};

type FlagsType = {
    statistics: Statistics | null;
    settings: Settings | null;
    gameData: GameData | null;
    wordPackInfos: WordPackInfo[];
};

type WordPackInfo = {
    id: number;
    name: string;
    isDefault: boolean;
    wordCount: number;
};

type FileWordPack = {
    name: string;
    words: string[];
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
