# Hangman

A simple hangman game written in Elm.

## Credit

-   German word list: https://sourceforge.net/projects/germandict/files/
-   Short German word list: https://stackabuse.com/levenshtein-distance-and-text-similarity-in-python/
-   English word list: https://github.com/dwyl/english-words

## Local Development

Start the development server with `npm start`

Create a production build with `npm run build`

Run `python generate_wordlist.py` to generate `src/WordList.elm`.
Settings for which languages to generate can be found at the top of `generate_wordlist.py`
