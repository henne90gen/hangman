const SECRET_KEY = 'SecretKey';

/**
 * Encrypts a string to a hexadecimal number using the XOR cipher
 */
export function encrypt(input: string): string {
    let c = '';
    let privateKey = SECRET_KEY;
    while (privateKey.length < input.length) {
        privateKey += SECRET_KEY;
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
export function decrypt(input: string): string {
    let c = '';
    let privateKey = SECRET_KEY;
    while (privateKey.length < input.length / 2) {
        privateKey += SECRET_KEY;
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
