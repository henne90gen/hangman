from typing import List, Set, Dict, Tuple
import numpy as np

languages = {
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


def write_file(variables: List[str], exposed_variables: List[str]):
    exposed_variables_str = ", ".join(exposed_variables)
    variables_str = "\n".join(variables)
    file_template = f"""\
module WordList exposing ({exposed_variables_str})

{variables_str}\
"""

    with open(wordlist_elm_file, "w+") as f:
        f.write(file_template)


def has_double_letter(word: str):
    for index in range(1, len(word)):
        if word[index] == word[index-1]:
            return True
    return False


def test_double_letter():
    assert has_double_letter("Hello")
    assert not has_double_letter("Helo")


def should_remove_word(lang: str, words: Set[str], word: str):
    if not word:
        return True

    if lang == "de" and word[0].islower():
        return True

    if len(word) < 6 or len(word) > 18:
        return True

    if has_double_letter(word):
        return True

    if word in words:
        return True
    if word[:-1] in words:
        return True
    if word[:-2] in words:
        return True
    if word[:-3] in words:
        return True

    return False


def main():
    lists = {}
    variables = []
    exposed_variables = []
    for lang in languages:
        with open(languages[lang]) as f:
            lines = f.readlines()

        words = set()
        total_num_words = 0
        total_num_characters = 0
        resultStr = ""
        for index, line in enumerate(lines):
            word = line.strip()

            if should_remove_word(lang, words, word):
                continue

            words.add(word)

            resultStr += f"\"{word}\"\n    , "
            total_num_words += 1
            total_num_characters += len(word)

        # remove trailing line break and comma
        resultStr = resultStr[:-7]

        variable = f"""\
wordList_{lang}_count : Int
wordList_{lang}_count =
    {total_num_words}

wordList_{lang} : List String
wordList_{lang} =
    [ {resultStr}
    ]
"""
        exposed_variables.append(f"wordList_{lang}")
        variables.append(variable)

        average_word_length = total_num_characters / total_num_words
        print(
            f"{lang}: {total_num_words} words, {average_word_length} average word length")

    write_file(variables, exposed_variables)


if __name__ == "__main__":
    main()
