import './main.css';
import { Elm } from './Main.elm';
import {
    loadStatistics,
    loadGameData,
    loadSettings,
    saveGameData,
    saveSettings,
    saveStatistics,
} from './storage';
import HangmanDB, { DefaultSource, IWordPack } from './db';

const PUBLIC_URL = process.env.PUBLIC_URL;
const languages: Language[] = ['DE', 'EN'];
let app: MainAppType | null = null;
const db = new HangmanDB();

/**
 * Generates a random number between 0 (inclusive) and num (exclusive).
 * @param num
 */
function random(num: number): number {
    return Math.floor(Math.random() * num);
}

/**
 * Sends a word to Elm
 */
function sendWordToElm(word: string) {
    app?.ports.receiveWord.send(word);
}

/**
 * Sends information about all the available word packs to Elm
 */
async function sendWordPackInfosToElm() {
    const infos = await createWordPackInfos();
    app?.ports.receiveWordPackInfos.send(infos);
}

async function createWordPackInfos() {
    const wordPacks = await db.getWordPacks();
    const infos: WordPackInfo[] = [];
    for (const wordPack of wordPacks) {
        const wordCount = await db.getWordCount(wordPack);
        infos.push({
            id: wordPack.id,
            isDefault: wordPack.sourceType === 'default',
            name: wordPack.name,
            wordCount: wordCount,
        });
    }
    return infos;
}

/**
 * Sends a random word from the specified language to the Elm application.
 * @param langUpper
 */
function getWord(ids: number[]) {
    if (ids.length === 0) {
        return;
    }
    const randomWordPackIdx = random(ids.length);
    db.getWordPack(ids[randomWordPackIdx])
        .then(async (wordPack) => {
            const wordCount = await db.getWordCount(wordPack);
            const wordIndex = random(wordCount);
            const word = await db.getWord(wordPack, wordIndex);
            if (word !== undefined) {
                sendWordToElm(word.word);
                return;
            }

            const source = wordPack.source as DefaultSource;
            const remoteGroup = random(source.remoteGroups.length);
            downloadDefaultWordPackGroup(wordPack, remoteGroup)
                .then((words) => {
                    const index = random(words.length);
                    sendWordToElm(words[index]);
                })
                .catch((error) => {
                    console.error(
                        randomWordPackIdx + ': failed to get next word.',
                        {
                            error,
                        }
                    );
                });
        })
        .catch(() => {
            // TODO retry with a different id?
        });
}

async function downloadDefaultWordPackGroup(
    wordPack: IWordPack,
    groupIndex: number
): Promise<string[]> {
    if (wordPack.sourceType != 'default') {
        throw 'Tried to download a non-default wordpack!';
    }

    const langLower = (wordPack.source as DefaultSource).language.toLowerCase();
    const url = PUBLIC_URL + '/languages/' + langLower + '/' + groupIndex;
    const response = await fetch(url);

    if (!response.ok) {
        throw 'Bad response code! (' + response.status + ')';
    }

    const content = await response.text();
    const words = content.split('\n');
    try {
        await db.addWords(
            words.map((w) => {
                return { word: w, wordPackId: wordPack.id!!, index: 0 };
            }),
            groupIndex
        );
    } catch (err) {
        // ignore, so that we can still return words
    }
    return words;
}

async function downloadDefaultWordPack(language: Language) {
    const MAX_LOCAL_GROUPS = 10;
    const wordPack = await db.getDefaultWordPack(language);
    const source = wordPack.source as DefaultSource;

    const downloadCount = MAX_LOCAL_GROUPS - source.localGroups.length;
    if (downloadCount <= 0) {
        return;
    }

    const randomGroups = new Set<number>();
    while (randomGroups.size < downloadCount) {
        randomGroups.add(random(source.remoteGroups.length));
    }

    for (const groupIndex of randomGroups.values()) {
        downloadDefaultWordPackGroup(wordPack, groupIndex);
    }
}

async function downloadDefaultWordPacks() {
    for (const language of languages) {
        downloadDefaultWordPack(language);
    }
}

async function saveFileWordPack(wp: FileWordPack) {
    try {
        await db.addFileWordPack(wp.name, wp.words);
        sendWordPackInfosToElm();
    } catch (err) {
        // TODO send error to elm, so that it can be displayed
    }
}

async function deleteFileWordPack(id: number) {
    try {
        await db.deleteFileWordPack(id);
        sendWordPackInfosToElm();
    } catch (err) {
        // TODO send error to elm, so that it can be displayed
    }
}

document.addEventListener('DOMContentLoaded', async () => {
    await db.init();

    const quota = await navigator.storage.estimate();
    const totalSpace = quota.quota / 1024 / 1024;
    const usedSpace = quota.usage / 1024 / 1024;
    console.log(
        'Disk usage: ' +
            usedSpace.toFixed(2) +
            'MB/' +
            totalSpace.toFixed(2) +
            'MB'
    );

    const statistics = loadStatistics();
    const settings = loadSettings();
    const gameData = loadGameData();
    const wordPackInfos = await createWordPackInfos();
    app = Elm.Main.init({
        flags: { statistics, settings, gameData, wordPackInfos },
        node: document.getElementById('root'),
    });

    app.ports.saveStatistics.subscribe(saveStatistics);
    app.ports.saveSettings.subscribe(saveSettings);
    app.ports.saveGameData.subscribe(saveGameData);
    app.ports.saveFileWordPack.subscribe(saveFileWordPack);
    app.ports.deleteFileWordPack.subscribe(deleteFileWordPack);
    app.ports.requestWord.subscribe(getWord);

    downloadDefaultWordPacks();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
import * as serviceWorker from './serviceWorker';
serviceWorker.unregister();
