import './main.css';
import { Elm } from './Main.elm';
import groupSizes from './wordList';
import { encrypt, decrypt } from './crypto';

const PUBLIC_URL = process.env.PUBLIC_URL;
const NUM_GROUPS_PER_LANGUAGE = 15;
const languages = ['DE', 'EN'];
let app: MainAppType | null = null;

type WordList = {
    localGroups: string[][];
    remoteGroups: number[];
};

/**
 * Saves the games statistics in the browsers local storage.
 * Before saving, the data is encrypted.
 * @param statistics
 */
function saveStatistics(statistics: Statistics) {
    const stringStatistics = JSON.stringify(statistics);
    const encryptedStatistics = encrypt(stringStatistics);
    localStorage.setItem('statistics', encryptedStatistics);
}

/**
 * Loads the games statistics from the browsers local storage.
 * After loading, the data is decrypted.
 */
function loadStatistics(): Statistics | null {
    const storedStatistics = localStorage.getItem('statistics');
    let parsedStatistics = null;
    if (storedStatistics) {
        const decryptedStatistics = decrypt(storedStatistics);
        parsedStatistics = JSON.parse(decryptedStatistics);
    }
    return parsedStatistics;
}

/**
 * Checks that the loaded object has the correct language data schema.
 * @param obj
 */
function schemaIsCorrect(obj: any) {
    const keys = Object.keys(obj);
    if (keys.indexOf('localGroups') === -1) {
        return false;
    }
    if (keys.indexOf('remoteGroups') === -1) {
        return false;
    }
    return true;
}

/**
 * Loads the word list from the browsers local storage.
 * If there is no data yet, an empty word list is generated and returned
 * @param language
 */
function getWordList(language: Language): WordList {
    const wordListStr = localStorage.getItem(language);
    if (wordListStr) {
        const parsedObj = JSON.parse(wordListStr);
        if (
            schemaIsCorrect(parsedObj) &&
            (parsedObj as WordList).localGroups.length ===
                NUM_GROUPS_PER_LANGUAGE &&
            (parsedObj as WordList).remoteGroups.length ===
                NUM_GROUPS_PER_LANGUAGE
        ) {
            return parsedObj;
        }
        localStorage.removeItem(language);
    }

    const localGroups = [];
    for (let i = 0; i < NUM_GROUPS_PER_LANGUAGE; i++) {
        localGroups.push([]);
    }

    const remoteGroups = [];
    while (remoteGroups.length < NUM_GROUPS_PER_LANGUAGE) {
        const group = random(groupSizes[language].length);
        if (remoteGroups.indexOf(group) === -1) {
            remoteGroups.push(group);
        }
    }

    return { localGroups, remoteGroups };
}

/**
 * Saves the given word list in the browsers local storage.
 * @param language
 * @param wordList
 */
function setWordList(language: Language, wordList: WordList) {
    try {
        localStorage.setItem(language, JSON.stringify(wordList));
    } catch (error) {
        console.error('PANIC! Could not save word list for ' + language + '.');
    }
}

/**
 * Generates a random number between 0 (inclusive) and num (exclusive).
 * @param num
 */
function random(num: number): number {
    return Math.floor(Math.random() * num);
}

/**
 * Sends a random word from the given group to the Elm application.
 * @param language
 * @param group
 */
function sendRandomWordToElm(language: Language, group: string[]) {
    const chosenWordIndex = random(group.length);
    if (chosenWordIndex >= group.length) {
        console.error(language + ': PANIC!', { chosenWordIndex, group });
        return;
    }

    app?.ports.receiveWord.send([language, group[chosenWordIndex]]);
}

/**
 * Sends a random word from the specified language to the Elm application.
 * @param langUpper
 */
function getWord(language: Language) {
    const wordList = getWordList(language);
    const localGroupIndex = random(wordList.localGroups.length);
    const localGroup = wordList.localGroups[localGroupIndex];

    if (localGroup.length !== 0) {
        sendRandomWordToElm(language, localGroup);
        return;
    }

    const remoteGroupIndex = wordList.remoteGroups[localGroupIndex];

    downloadGroup(language, localGroupIndex, remoteGroupIndex)
        .then((newGroup: string[]) => {
            sendRandomWordToElm(language, newGroup);
        })
        .catch((error) => {
            console.error(language + ': failed to get next word.', {
                error,
                localGroupIndex,
                remoteGroupIndex,
            });
        });
}

/**
 * Downloads the group given by remoteGroupIndex for the given language and saves it to the give localGroupIndex.
 * @param language
 * @param localGroupIndex
 * @param remoteGroupIndex
 */
async function downloadGroup(
    language: Language,
    localGroupIndex: number,
    remoteGroupIndex: number
) {
    const langLower = language.toLowerCase();
    const url = PUBLIC_URL + '/languages/' + langLower + '/' + remoteGroupIndex;
    const response = await fetch(url);

    if (!response.ok) {
        throw 'Bad response code! (' + response.status + ')';
    }

    const words = await response.text();
    const newGroup = words.split('\n');

    // save to local storage
    const wordList = getWordList(language);
    wordList.localGroups[localGroupIndex] = newGroup;
    setWordList(language, wordList);

    return Promise.resolve(newGroup);
}

/**
 * Downloads all missing groups for the specified language.
 * @param wordList
 * @param language
 */
function downloadMissingGroups(wordList: WordList, language: Language) {
    for (
        let localGroupIndex = 0;
        localGroupIndex < wordList.localGroups.length;
        localGroupIndex++
    ) {
        if (wordList.localGroups[localGroupIndex].length !== 0) {
            continue;
        }

        const remoteGroupIndex = wordList.remoteGroups[localGroupIndex];
        downloadGroup(language, localGroupIndex, remoteGroupIndex)
            .then(() => {
                console.debug(language + ': downloaded group', {
                    localGroupIndex,
                    remoteGroupIndex,
                });
            })
            .catch((error: any) => {
                console.error(language + ': failed to pre-fetch word group.', {
                    error,
                    localGroupIndex,
                    remoteGroupIndex,
                });
            });
    }
}

/**
 * Downloads all missing groups for all languages.
 */
function downloadLanguages() {
    for (let language of languages) {
        const wordList = getWordList(language as Language);
        downloadMissingGroups(wordList, language as Language);
    }
}

/**
 * Loads the settings from local storage.
 */
function loadSettings(): Settings | null {
    const storedSettings = localStorage.getItem('settings');
    let parsedSettings = null;
    if (storedSettings) {
        parsedSettings = JSON.parse(storedSettings);
    }
    return parsedSettings;
}

/**
 * Saves the settings to local storage.
 * @param settings
 */
function saveSettings(settings: Settings) {
    const stringSettings = JSON.stringify(settings);
    localStorage.setItem('settings', stringSettings);
}

/**
 * Saves the games data in the browsers local storage.
 * Before saving, the data is encrypted.
 * @param gameData
 */
function saveGameData(gameData: GameData) {
    const stringGameData = JSON.stringify(gameData);
    const encryptedGameData = encrypt(stringGameData);
    localStorage.setItem('gameData', encryptedGameData);
}

/**
 * Loads the games data from the browsers local storage.
 * After loading, the data is decrypted.
 */
function loadGameData(): GameData | null {
    const storedGameData = localStorage.getItem('gameData');
    let parsedGameData = null;
    if (storedGameData) {
        const decryptedGameData = decrypt(storedGameData);
        try {
            parsedGameData = JSON.parse(decryptedGameData);
        } catch (error) {
            console.warn('Could not load game data.', error);
        }
    }
    return parsedGameData;
}

document.addEventListener('DOMContentLoaded', function () {
    const statistics = loadStatistics();
    const settings = loadSettings();
    const gameData = loadGameData();
    app = Elm.Main.init({
        flags: { statistics, settings, gameData },
        node: document.getElementById('root'),
    });

    app.ports.saveStatistics.subscribe(saveStatistics);
    app.ports.saveSettings.subscribe(saveSettings);
    app.ports.saveGameData.subscribe(saveGameData);
    app.ports.requestWord.subscribe(getWord);

    downloadLanguages();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
import * as serviceWorker from './serviceWorker';
serviceWorker.unregister();
