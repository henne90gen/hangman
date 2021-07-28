import { encrypt, decrypt } from './crypto';

/**
 * Saves the games statistics in the browsers local storage.
 * Before saving, the data is encrypted.
 * @param statistics
 */
export function saveStatistics(statistics: Statistics) {
    const stringStatistics = JSON.stringify(statistics);
    const encryptedStatistics = encrypt(stringStatistics);
    localStorage.setItem('statistics', encryptedStatistics);
}

/**
 * Loads the games statistics from the browsers local storage.
 * After loading, the data is decrypted.
 */
export function loadStatistics(): Statistics | null {
    const storedStatistics = localStorage.getItem('statistics');
    let parsedStatistics = null;
    if (storedStatistics) {
        const decryptedStatistics = decrypt(storedStatistics);
        parsedStatistics = JSON.parse(decryptedStatistics);
    }
    return parsedStatistics;
}

/**
 * Loads the settings from local storage.
 */
export function loadSettings(): Settings | null {
    const storedSettings = localStorage.getItem('settings');
    let parsedSettings = null;
    if (storedSettings) {
        parsedSettings = JSON.parse(storedSettings);
    }
    if (parsedSettings != null && !parsedSettings.hasOwnProperty('activeWordPacks')) {
        return null;
    }
    return parsedSettings;
}

/**
 * Saves the settings to local storage.
 * @param settings
 */
export function saveSettings(settings: Settings) {
    const settingsStr = JSON.stringify(settings);
    localStorage.setItem('settings', settingsStr);
}

/**
 * Saves the games data in the browsers local storage.
 * Before saving, the data is encrypted.
 * @param gameData
 */
export function saveGameData(gameData: GameData) {
    const gameDataStr = JSON.stringify(gameData);
    const gameDataEncrypted = encrypt(gameDataStr);
    localStorage.setItem('gameData', gameDataEncrypted);
}

/**
 * Loads the games data from the browsers local storage.
 * After loading, the data is decrypted.
 */
export function loadGameData(): GameData | null {
    const storedGameData = localStorage.getItem('gameData');
    let parsedGameData = null;
    if (storedGameData) {
        const gameDataDecrypted = decrypt(storedGameData);
        try {
            parsedGameData = JSON.parse(gameDataDecrypted);
        } catch (error) {
            console.warn('Could not load game data.', error);
        }
    }
    return parsedGameData;
}
