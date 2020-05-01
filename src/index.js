import './main.css';
import { Elm } from './Main.elm';
import groupSizes from './wordList';
import * as serviceWorker from './serviceWorker';

const key = 'SecretKey';
const PUBLIC_URL = process.env.PUBLIC_URL;

/**
 * Encrypts a string to a hexadecimal number using the XOR cipher
 */
function encrypt(input) {
    let c = '';
    let privateKey = key;
    while (privateKey.length < input.length) {
        privateKey += key;
    }
    for (let i = 0; i < input.length; i++) {
        const value1 = input[i].charCodeAt(0);
        const value2 = privateKey[i].charCodeAt(0);

        const xorValue = value1 ^ value2;

        let xorValueAsHexString = xorValue.toString('16');

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
function decrypt(input) {
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

function saveStatistics(statistics) {
    const stringStatistics = JSON.stringify(statistics);
    const encryptedStatistics = encrypt(stringStatistics);
    localStorage.setItem('statistics', encryptedStatistics);
}

function loadStatistics() {
    const storedStatistics = localStorage.getItem('statistics');
    let parsedStatistics = null;
    if (storedStatistics) {
        const decryptedStatistics = decrypt(storedStatistics);
        parsedStatistics = JSON.parse(decryptedStatistics);
    }
    return parsedStatistics;
}

const app = Elm.Main.init({
    flags: { statistics: loadStatistics() },
    node: document.getElementById('root'),
});

function getWordList(language) {
    const wordListStr = localStorage.getItem(language);
    if (wordListStr) {
        return JSON.parse(wordListStr);
    }

    const wordList = [];
    for (let i in groupSizes[language]) {
        wordList.push([]);
    }

    return wordList;
}

function setWordList(language, wordList) {
    localStorage.setItem(language, JSON.stringify(wordList));
}

function getWord(langUpper) {
    const language = langUpper.toLowerCase();
    const wordList = getWordList(language);
    const chosenGroupIndex = Math.floor(Math.random() * wordList.length);
    if (chosenGroupIndex >= wordList.length) {
        console.error('PANIC!', { chosenGroupIndex, wordList });
        return;
    }

    const chosenGroup = wordList[chosenGroupIndex];
    if (chosenGroup.length !== 0) {
        const chosenWordIndex = Math.floor(Math.random() * chosenGroup.length);
        if (chosenWordIndex >= chosenGroup.length) {
            console.error('PANIC!', { chosenWordIndex, chosenGroup });
            return;
        }
        app.ports.receiveWord.send([
            language.toUpperCase(),
            chosenGroup[chosenWordIndex],
        ]);
        return;
    }

    const url = PUBLIC_URL + '/languages/' + language + '/' + chosenGroupIndex;
    fetch(url)
        .then((response) => {
            if (!response.ok) {
                throw 'Bad response code! ' + response.status;
            }
            return response.text();
        })
        .then((words) => {
            const newGroup = words.split('\n');

            wordList[chosenGroupIndex] = newGroup;
            setWordList(language, wordList);

            const chosenWordIndex = Math.floor(Math.random() * newGroup.length);
            if (chosenWordIndex >= newGroup.length) {
                console.error('PANIC!', { chosenWordIndex, newGroup });
                return;
            }

            app.ports.receiveWord.send([
                language.toUpperCase(),
                newGroup[chosenWordIndex],
            ]);
        })
        .catch((e) => {
            console.error(e);
        });
}

app.ports.saveStatistics.subscribe(saveStatistics);
app.ports.requestWord.subscribe(getWord);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
