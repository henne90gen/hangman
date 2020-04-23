from typing import List, Dict

wordlist_files = {"de": "german/german.dic", "en": "english/words.txt"}
wordlist_elm_file = "src/WordList.elm"


def write_file(word_lists: Dict[str, List[str]]):
    variables = []
    exposed_variables = []
    for lang in word_lists:
        resultStr = "\n    , ".join(
            map(lambda x: f"\"{x[:-1]}\"", word_lists[lang]))

        variable = f"""\
wordList_{lang} : List String
wordList_{lang} =
    [ {resultStr}
    ]
"""
        exposed_variables.append(f"wordList_{lang}")
        variables.append(variable)

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


def main():
    lists = {}
    for lang in wordlist_files:
        word_list = generate_word_list(lang, wordlist_files[lang])
        lists[lang] = word_list
    write_file(lists)


if __name__ == "__main__":
    main()
