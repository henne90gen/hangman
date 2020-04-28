import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const key = 'SecretKey';

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

const storedStatistics = localStorage.getItem('statistics');
let parsedStatistics = null;
if (storedStatistics) {
    const decryptedStatistics = decrypt(storedStatistics);
    parsedStatistics = JSON.parse(decryptedStatistics);
}

const app = Elm.Main.init({
    flags: { statistics: parsedStatistics },
    node: document.getElementById('root'),
});

app.ports.saveStatistics.subscribe(function (statistics) {
    const stringStatistics = JSON.stringify(statistics);
    const encryptedStatistics = encrypt(stringStatistics);
    localStorage.setItem('statistics', encryptedStatistics);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
