from typing import List

wordlist_file = "german/german.dic"
wordlist_elm_file = "src/WordList.elm"


def write_file(result: List[str]):
    resultStr = "\n    , ".join(map(lambda x: f"\"{x[:-1]}\"", result))

    template = f"""\
module WordList exposing (wordList)

wordList : List String
wordList =
    [ {resultStr}
    ]
"""

    with open(wordlist_elm_file, "w+") as f:
        f.write(template)


def has_double_letter(word: str):
    return len(set(word)) != len(word)


def main():
    with open(wordlist_file, errors="ignore") as f:
        lines = f.readlines()

    result = []
    for line in lines:
        word = line.strip()
        if not word:
            continue

        if word[0].islower():
            continue

        if len(word) < 6:
            continue

        if has_double_letter(word):
            continue

        result.append(line)

    write_file(result)


if __name__ == "__main__":
    main()
