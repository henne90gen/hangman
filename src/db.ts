import Dexie, { Transaction } from 'dexie';

export type PackSourceType = 'default' | 'file';

export type DefaultSource = {
    localGroups: number[];
    remoteGroups: number[];
};

export type FileSource = {};

export interface IWordPack {
    id?: number;
    name: string;
    created: Date;
    language: Language;
    sourceType: PackSourceType;
    source: DefaultSource | FileSource;
}

export interface IWord {
    id?: number;
    wordPackId: number;
    word: string;
    groupIndex?: number;
    index: number; // index inside the word pack
}

async function rejectUndefined<T>(obj: T | undefined): Promise<T> {
    if (obj === undefined) {
        return Promise.reject();
    }
    return obj;
}

export default class HangmanDB extends Dexie {
    wordPacks: Dexie.Table<IWordPack, number>;
    words: Dexie.Table<IWord, number>;

    constructor() {
        super('Hangman');

        this.version(1).stores({
            wordPacks: '++id,name,created,language,sourceType,source',
            words: '++id,wordPackId,word,groupIndex,index',
        });

        this.wordPacks = this.table('wordPacks');
        this.words = this.table('words');
    }

    init(): Promise<void> {
        // NOTE version().upgrade() does not work, that's why we are doing it this way
        return this.transaction('rw', this.wordPacks, async () => {
            const count = await this.wordPacks
                .where('sourceType')
                .equals('default')
                .count();
            if (count === 2) {
                return;
            }

            this.wordPacks.bulkAdd([
                {
                    name: 'DE',
                    created: new Date(),
                    language: 'DE',
                    sourceType: 'default',
                    source: {
                        localGroups: [],
                        remoteGroups: Array.from(Array(56).keys()),
                    },
                },
                {
                    name: 'EN',
                    created: new Date(),
                    language: 'EN',
                    sourceType: 'default',
                    source: {
                        localGroups: [],
                        remoteGroups: Array.from(Array(20).keys()),
                    },
                },
            ]);
        });
    }

    clear(): Promise<void> {
        return this.transaction('rw', this.wordPacks, this.words, async () => {
            await this.words.clear();
            await this.wordPacks.clear();
        });
    }

    async getDefaultWordPack(language: Language): Promise<IWordPack> {
        const obj = await this.wordPacks
            .where('sourceType')
            .equals('default')
            .and((x) => x.language == language)
            .first();
        return rejectUndefined(obj);
    }

    getWordPacks(): Promise<IWordPack[]> {
        return this.wordPacks.toArray();
    }

    async getWordPack(id: number): Promise<IWordPack> {
        const obj = await this.wordPacks.where('id').equals(id).first();
        return rejectUndefined(obj);
    }

    getWords(wordPack: IWordPack): Promise<IWord[]> {
        if (wordPack.id === undefined) {
            return Promise.reject();
        }
        return this.words
            .where('wordPackId')
            .equals(wordPack.id)
            .sortBy('index');
    }

    async getWord(
        wordPack: IWordPack,
        index: number
    ): Promise<IWord | undefined> {
        if (wordPack.id === undefined) {
            throw 'Invalid word pack!';
        }

        return this.words
            .where('index')
            .equals(index)
            .and((x) => x.wordPackId == wordPack.id)
            .first();
    }

    async getWordCount(wordPack: IWordPack): Promise<number> {
        if (wordPack.id === undefined) {
            throw '';
        }
        return this.words.where('wordPackId').equals(wordPack.id).count();
    }

    async addWords(words: IWord[], groupIndex?: number): Promise<void> {
        await this.transaction('rw', this.words, this.wordPacks, async () => {
            if (words.length === 0) {
                return;
            }

            let wordPackId = words[0].wordPackId;
            for (const w of words) {
                if (w.wordPackId != wordPackId) {
                    return Promise.reject();
                }
            }

            let index = await this.words
                .where('wordPackId')
                .equals(wordPackId)
                .count();
            for (const w of words) {
                w.index = index;
                w.groupIndex = groupIndex;
                index++;
            }

            await this.words.bulkAdd(words);

            if (groupIndex !== undefined) {
                const wordPack = await this.wordPacks
                    .get(wordPackId)
                    .then(rejectUndefined);
                const source = wordPack.source as DefaultSource;
                const idx = source.remoteGroups.findIndex(
                    (value) => value == groupIndex
                );
                if (idx === -1) {
                    throw 'Group ' + groupIndex + ' has already been saved';
                }
                source.remoteGroups.splice(idx, 1);
                source.localGroups.push(groupIndex);
                wordPack.source = source;
                await this.wordPacks.update(wordPackId, wordPack);
            }
        });
    }

    async addFileWordPack(
        language: Language,
        name: string,
        words: string[]
    ): Promise<number> {
        return this.transaction('rw', this.wordPacks, this.words, async () => {
            const id = await this.wordPacks.add({
                created: new Date(),
                language,
                name,
                sourceType: 'file',
                source: {},
            });

            await this.words.bulkAdd(
                words.map((word, index) => {
                    return { wordPackId: id, word, index };
                })
            );
            return id;
        });
    }
    async deleteFileWordPack(id: number) {
        return this.transaction('rw', this.wordPacks, this.words, async () => {
            await this.wordPacks.delete(id);
            const words = await this.words
                .where('wordPackId')
                .equals(id)
                .toArray();
            this.words.bulkDelete(words.map((w) => w.id));
        });
    }
}
