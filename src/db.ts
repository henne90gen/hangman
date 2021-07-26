import Dexie, { Transaction } from 'dexie';

export type PackSourceType = 'default' | 'file';

export type DefaultSource = {
    localGroups: number[];
    remoteGroups: number[];
};

export type FileSource = {
    name: string;
    created: Date;
    content: string;
};

export interface IWordPack {
    id?: number;
    name: string;
    description: string;
    language: Language;
    sourceType: PackSourceType;
    source: DefaultSource | FileSource;
}

export interface IWord {
    id?: number;
    wordPackId: number;
    word: string;
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
        super('HangmanDB');

        this.version(1).stores({
            wordPacks: '++id,name,description,language,sourceType,source',
            words: '++id,wordPackId,word,index',
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
                return Promise.resolve();
            }

            this.wordPacks.bulkAdd([
                {
                    name: 'DE',
                    description: '', // TODO create description
                    language: 'DE',
                    sourceType: 'default',
                    source: {
                        localGroups: [],
                        remoteGroups: Array.from(Array(56).keys()),
                    },
                },
                {
                    name: 'EN',
                    description: '', // TODO create description
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
            return Promise.reject();
        }
        return await this.words
            .where('wordPackId')
            .equals(wordPack.id)
            .and((x) => x.index == index)
            .first();
    }

    async getWordCount(wordPack: IWordPack): Promise<number> {
        if (wordPack.id === undefined) {
            throw '';
        }
        return this.words.where('wordPackId').equals(wordPack.id).count();
    }

    async addWords(words: IWord[], groupIndex?: number): Promise<void> {
        await this.transaction(
            'rw',
            this.words,
            this.wordPacks,
            async (trans: Transaction) => {
                if (words.length === 0) {
                    return;
                }

                let wordPackId = words[0].wordPackId;
                for (const w of words) {
                    if (w.wordPackId != wordPackId) {
                        return Promise.reject();
                    }
                }

                let index = await trans
                    .table('words')
                    .where('wordPackId')
                    .equals(wordPackId)
                    .count();
                for (const w_1 of words) {
                    w_1.index = index;
                    index++;
                }

                await this.words.bulkAdd(words);

                if (groupIndex !== undefined) {
                    const wordPack = await trans
                        .table('wordPacks')
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
            }
        );
    }
}
