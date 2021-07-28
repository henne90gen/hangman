import { decrypt, encrypt } from './crypto';

describe('crypto', () => {
    it('can encrypt content', () => {
        const encrypted = encrypt('test string');
        expect(encrypted).toEqual('2700100645073f17103d02');
    });

    it('can decrypt content', () => {
        const decrypted = decrypt('2700100645073f17103d02');
        expect(decrypted).toEqual('test string');
    });
});
