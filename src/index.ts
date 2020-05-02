import './main.css';
import { Elm } from './Main.elm';
import groupSizes from './wordList';

const key = 'SecretKey';
const PUBLIC_URL = process.env.PUBLIC_URL;
let app: MainAppType | null = null;

type Language = 'de' | 'en';

type WordList = {
    localGroups: string[][];
    remoteGroups: number[];
};

/**
 * Encrypts a string to a hexadecimal number using the XOR cipher
 */
function encrypt(input: string): string {
    let c = '';
    let privateKey = key;
    while (privateKey.length < input.length) {
        privateKey += key;
    }
    for (let i = 0; i < input.length; i++) {
        const value1 = input[i].charCodeAt(0);
        const value2 = privateKey[i].charCodeAt(0);

        const xorValue = value1 ^ value2;

        let xorValueAsHexString = xorValue.toString(16);

        if (xorValueAsHexString.length < 2) {
            xorValueAsHexString = '0' + xorValueAsHexString;
        }

        c += xorValueAsHexString;
    }
    return c;
}

/**
 * Decrypts a string of hexadecimal numbers using the XOR cipher
 */
function decrypt(input: string): string {
    let c = '';
    let privateKey = key;
    while (privateKey.length < input.length / 2) {
        privateKey += key;
    }

    for (let i = 0; i < input.length; i += 2) {
        const hexValueString = input.substring(i, i + 2);
        const value1 = parseInt(hexValueString, 16);
        const value2 = privateKey.charCodeAt(i / 2);

        const xorValue = value1 ^ value2;
        c += String.fromCharCode(xorValue);
    }
    return c;
}

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
        if (schemaIsCorrect(parsedObj)) {
            return parsedObj;
        }
        localStorage.removeItem(language);
    }

    const localGroups = [];
    for (let i = 0; i < 10; i++) {
        localGroups.push([]);
    }

    const remoteGroups = [];
    while (remoteGroups.length < 10) {
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
 * Sends a random word from the specified language to the Elm application.
 * @param langUpper
 */
function getWord(langUpper: 'DE' | 'EN') {
    const language = langUpper.toLowerCase() as Language;
    const wordList = getWordList(language);
    const localGroupIndex = random(wordList.localGroups.length);
    const localGroup = wordList.localGroups[localGroupIndex];

    if (localGroup.length !== 0) {
        const chosenWordIndex = random(localGroup.length);
        if (chosenWordIndex >= localGroup.length) {
            console.error('PANIC!', { chosenWordIndex, localGroup });
            return;
        }
        app?.ports.receiveWord.send([
            language.toUpperCase(),
            localGroup[chosenWordIndex],
        ]);
        return;
    }

    const remoteGroupIndex = wordList.remoteGroups[localGroupIndex];
    const url = PUBLIC_URL + '/languages/' + language + '/' + remoteGroupIndex;
    fetch(url)
        .then((response) => {
            if (!response.ok) {
                throw 'Bad response code! (' + response.status + ')';
            }
            return response.text();
        })
        .then((words) => {
            const newGroup = words.split('\n');

            wordList.localGroups[localGroupIndex] = newGroup;
            setWordList(language, wordList);

            const chosenWordIndex = random(newGroup.length);
            if (chosenWordIndex >= newGroup.length) {
                console.error('PANIC!', { chosenWordIndex, newGroup });
                return;
            }

            app?.ports.receiveWord.send([
                language.toUpperCase(),
                newGroup[chosenWordIndex],
            ]);
        })
        .catch((e) => {
            console.error(e);
        });
}

document.addEventListener('DOMContentLoaded', function () {
    app = Elm.Main.init({
        flags: { statistics: loadStatistics() },
        node: document.getElementById('root'),
    });

    app.ports.saveStatistics.subscribe(saveStatistics);
    app.ports.requestWord.subscribe(getWord);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
import * as serviceWorker from './serviceWorker';
serviceWorker.unregister();
