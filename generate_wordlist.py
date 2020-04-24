from typing import List, Dict, Tuple
import numpy as np

languages = {
    "de_short": "german/nouns.dic",
    "de": "german/german.dic",
    "en": "english/words.txt"
}
wordlist_elm_file = "src/WordList.elm"


def levenshtein(seq1, seq2):
    size_x = len(seq1) + 1
    size_y = len(seq2) + 1
    matrix = np.zeros((size_x, size_y))
    for x in range(size_x):
        matrix[x, 0] = x
    for y in range(size_y):
        matrix[0, y] = y

    for x in range(1, size_x):
        for y in range(1, size_y):
            if seq1[x-1] == seq2[y-1]:
                matrix[x, y] = min(
                    matrix[x-1, y] + 1,
                    matrix[x-1, y-1],
                    matrix[x, y-1] + 1
                )
            else:
                matrix[x, y] = min(
                    matrix[x-1, y] + 1,
                    matrix[x-1, y-1] + 1,
                    matrix[x, y-1] + 1
                )
    return (matrix[size_x - 1, size_y - 1])


def write_file(word_lists: Dict[str, List[str]]):
    variables = []
    exposed_variables = []
    for lang in word_lists:
        total_num_characters = 0
        words = []
        for word in word_lists[lang]:
            word = word[:-1]
            words.append(f"\"{word}\"")
            total_num_characters += len(word)
        resultStr = "\n    , ".join(words)

        variable = f"""\
wordList_{lang} : List String
wordList_{lang} =
    [ {resultStr}
    ]
"""
        exposed_variables.append(f"wordList_{lang}")
        variables.append(variable)

        num_words = len(words)
        average_word_length = total_num_characters / num_words
        print(f"{lang}: {num_words} words, {average_word_length} average word length")

    exposed_variables_str = ", ".join(exposed_variables)
    variables_str = "\n".join(variables)
    file_template = f"""\
module WordList exposing ({exposed_variables_str})

{variables_str}\
"""

    with open(wordlist_elm_file, "w+") as f:
        f.write(file_template)


def has_double_letter(word: str):
    return len(set(word)) != len(word)


def generate_word_list(lang: str, wordlist_file: str) -> List[str]:
    with open(wordlist_file) as f:
        lines = f.readlines()

    result = []
    for line in lines:
        word = line.strip()
        if not word:
            continue

        if lang == "de" and word[0].islower():
            continue

        if len(word) < 6:
            continue

        if has_double_letter(word):
            continue

        result.append(line)

    return result


def check_distances(word_list: List[str]) -> List[str]:
    result = []
    for index, word in enumerate(word_list):
        if index == 0:
            result.append(word)
            continue

        prev_word = word_list[index-1]

        dist = levenshtein(word, prev_word)
        if dist >= 3:
            result.append(word)

    return result


def main():
    lists = {}
    for lang in languages:
        word_list = generate_word_list(lang, languages[lang])
        word_list = check_distances(word_list)
        lists[lang] = word_list

    write_file(lists)


if __name__ == "__main__":
    main()
