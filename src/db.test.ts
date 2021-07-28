import HangmanDB, { DefaultSource, IWord } from './db';

async function getNewDB(): Promise<HangmanDB> {
    const db = new HangmanDB();
    await db.clear();
    await db.init();
    return db;
}

describe('database', () => {
    it('can init correctly', async () => {
        const db = await getNewDB();
        const wordPacks = await db.getWordPacks();
        expect(wordPacks).toHaveLength(2);
        for (const wordPack of wordPacks) {
            const words = await db.getWords(wordPack);
            expect(words).toHaveLength(0);
        }
    });

    it('can save single word', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        expect(wordPack).toBeDefined();

        const wordPackId = wordPack!!.id!!;
        const words: IWord[] = [{ index: 0, word: 'test', wordPackId }];
        await db.addWords(words);
        const resultWords = await db.getWords(wordPack!!);
        expect(resultWords).toHaveLength(1);
        expect(resultWords[0].word).toEqual('test');
        expect(resultWords[0].index).toEqual(0);
    });

    it('can save multiple words', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        expect(wordPack).toBeDefined();

        const wordPackId = wordPack!!.id!!;
        await db.addWords([
            { index: 0, word: 'test', wordPackId },
            { index: 0, word: 'hello', wordPackId },
        ]);
        const resultWords = await db.getWords(wordPack!!);
        expect(resultWords).toHaveLength(2);
        expect(resultWords[0].word).toEqual('test');
        expect(resultWords[0].index).toEqual(0);
        expect(resultWords[1].word).toEqual('hello');
        expect(resultWords[1].index).toEqual(1);
    });

    it('can save multiple words with multiple calls to addWords', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        expect(wordPack).toBeDefined();

        const wordPackId = wordPack!!.id!!;
        await db.addWords([
            { index: 0, word: 'test', wordPackId },
            { index: 0, word: 'hello', wordPackId },
        ]);
        await db.addWords([{ index: 0, word: 'world', wordPackId }]);
        const resultWords = await db.getWords(wordPack!!);
        expect(resultWords).toHaveLength(3);
        expect(resultWords[0].word).toEqual('test');
        expect(resultWords[0].index).toEqual(0);
        expect(resultWords[1].word).toEqual('hello');
        expect(resultWords[1].index).toEqual(1);
        expect(resultWords[2].word).toEqual('world');
        expect(resultWords[2].index).toEqual(2);
    });

    it('can get a single word', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        expect(wordPack).toBeDefined();

        const wordPackId = wordPack!!.id!!;
        await db.addWords([
            { index: 0, word: 'test', wordPackId },
            { index: 0, word: 'hello', wordPackId },
            { index: 0, word: 'world', wordPackId },
        ]);
        const word = await db.getWord(wordPack!!, 1);
        expect(word).toBeDefined();
        expect(word!!.index).toEqual(1);
        expect(word!!.word).toEqual('hello');
    });

    it('can returns undefined if there are no words', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        const word = await db.getWord(wordPack!!, 1);
        expect(word).toBeUndefined();
    });

    it('can save words and update group indices', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        expect(wordPack).toBeDefined();

        const wordPackId = wordPack!!.id!!;
        await db.addWords(
            [
                { index: 0, word: 'test', wordPackId },
                { index: 0, word: 'hello', wordPackId },
                { index: 0, word: 'world', wordPackId },
            ],
            1
        );
        const updatedWordPack = await db.getDefaultWordPack('EN');
        expect(updatedWordPack).toBeDefined();
        const source = updatedWordPack.source as DefaultSource;
        expect(source.localGroups).toHaveLength(1);
        expect(source.localGroups[0]).toEqual(1);
        expect(source.remoteGroups).toHaveLength(19);
    });

    it('can handle when a group has already been saved', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        expect(wordPack).toBeDefined();

        const wordPackId = wordPack!!.id!!;
        await db.addWords(
            [
                { index: 0, word: 'test', wordPackId },
                { index: 0, word: 'hello', wordPackId },
                { index: 0, word: 'world', wordPackId },
            ],
            1
        );

        await expect(async () => {
            await db.addWords(
                [
                    { index: 0, word: 'test', wordPackId },
                    { index: 0, word: 'hello', wordPackId },
                    { index: 0, word: 'world', wordPackId },
                ],
                1
            );
        }).rejects.toEqual('Group 1 has already been saved');

        const updatedWordPack = await db.getDefaultWordPack('EN');
        expect(updatedWordPack).toBeDefined();
        const source = updatedWordPack.source as DefaultSource;
        expect(source.localGroups).toHaveLength(1);
        expect(source.localGroups[0]).toEqual(1);
        expect(source.remoteGroups).toHaveLength(19);

        const resultWords = await db.getWords(updatedWordPack);
        expect(resultWords).toHaveLength(3);
        expect(resultWords[0].word).toEqual('test');
        expect(resultWords[1].word).toEqual('hello');
        expect(resultWords[2].word).toEqual('world');
    });

    it('can count words', async () => {
        const db = await getNewDB();
        const wordPack = await db.getDefaultWordPack('EN');
        const wordPackId = wordPack!!.id!!;
        const wordPack2 = await db.getDefaultWordPack('DE');
        const wordPackId2 = wordPack2!!.id!!;

        await db.addWords([
            { index: 0, word: 'test', wordPackId },
            { index: 0, word: 'hello', wordPackId },
            { index: 0, word: 'world', wordPackId },
        ]);

        await db.addWords([
            { index: 0, word: 'test', wordPackId: wordPackId2 },
            { index: 0, word: 'hello', wordPackId: wordPackId2 },
            { index: 0, word: 'world', wordPackId: wordPackId2 },
        ]);
        const count = await db.getWordCount(wordPack);
        expect(count).toEqual(3);
    });

    it('can save file word pack', async () => {
        const db = await getNewDB();
        const id = await db.addFileWordPack('test.txt', ['hello', 'world']);

        const wordPack = await db.getWordPack(id);
        const words = await db.getWords(wordPack);
        expect(words).toHaveLength(2);
        expect(words[0].word).toEqual('hello');
        expect(words[1].word).toEqual('world');
    });

    it('can delete file word pack', async () => {
        const db = await getNewDB();
        const id = await db.addFileWordPack('test.txt', ['hello', 'world']);

        await db.deleteFileWordPack(id);

        await expect(async () => {
            await db.getWordPack(id);
        }).rejects.toBeUndefined();

        const words = await db.words.where('wordPackId').equals(id).toArray();
        expect(words).toHaveLength(0);
    });
});
