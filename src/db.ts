import Dexie, { Transaction } from 'dexie';

export type PackSourceType = 'default' | 'file';

export type DefaultSource = {
    localGroups: number[];
    remoteGroups: number[];
};

export type FileSource = {
    name: string;
    content: string;
};

export interface IWordPack {
    id?: number;
    name: string;
    created: Date;
    description: string;
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
        super('HangmanDB');

        this.version(1).stores({
            wordPacks:
                '++id,name,created,description,language,sourceType,source',
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
                console.log('Default word packs already exist');
                return;
            }

            console.log('Creating default word packs');
            this.wordPacks.bulkAdd([
                {
                    name: 'DE',
                    description: '', // TODO create description
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
                    description: '', // TODO create description
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
                for (const w of words) {
                    w.index = index;
                    w.groupIndex = groupIndex;
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
