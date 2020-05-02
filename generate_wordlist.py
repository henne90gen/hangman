from typing import List, Set, Dict, Tuple
import numpy as np
import os
import shutil

languages = {
    "de": ("german/german.dic", 56),
    "en": ("english/words.txt", 20)
}
base_directory = "public/languages"
wordlist_ts_file = "src/wordList.ts"


def has_double_letter(word: str):
    for index in range(1, len(word)):
        if word[index] == word[index-1]:
            return True
    return False


def test_double_letter():
    assert has_double_letter("Hello")
    assert not has_double_letter("Helo")


def has_uppercase_letters(word: str):
    for index, letter in enumerate(word):
        if index != 0 and letter.isupper():
            return True
    return False


def test_has_uppercase_letters():
    assert has_uppercase_letters("AMOLED")
    assert not has_uppercase_letters("Hello")


def should_remove_word(lang: str, words: Set[str], word: str):
    if not word:
        return True

    if lang == "de" and word[0].islower():
        return True

    if len(word) < 6 or len(word) > 18:
        return True

    if has_double_letter(word):
        return True

    if has_uppercase_letters(word):
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


def format_file_size(file_size: float) -> str:
    if file_size < 1024:
        return f"{file_size:.2f} B"

    if file_size < 1024 * 1024:
        file_size /= 1024
        return f"{file_size:.2f} KB"

    if file_size < 1024 * 1024 * 1024:
        file_size /= 1024 * 1024
        return f"{file_size:.2f} MB"

    return f"{file_size:.2f} B"


def process_language(base_directory: str, languages: Dict[str, Tuple[str, int]], lang: str):
    file_name, num_groups = languages[lang]
    with open(file_name) as f:
        lines = f.readlines()

    words = set()
    total_num_words = 0
    total_num_characters = 0
    resultStr = ""
    for line in lines:
        word = line.strip()

        if should_remove_word(lang, words, word):
            continue

        words.add(word)

        total_num_words += 1
        total_num_characters += len(word)

    average_word_length = total_num_characters / total_num_words
    print(f"{lang}: {total_num_words} words collected")
    print(f"{lang}: {average_word_length:.2f} average word length")

    # create a folder for the language
    language_directory = os.path.join(base_directory, lang)
    if not os.path.exists(language_directory):
        os.mkdir(language_directory)

    # sort words
    words = sorted(list(words))

    # split words into groups
    written_words = 0
    words_per_group = 0
    group_sizes = []
    group_file_sizes = []
    for group_index in range(num_groups):
        group = []
        i = group_index
        while i < len(words):
            group.append(f"{words[i]}\n")
            i += num_groups
            written_words += 1

        group_file = os.path.join(language_directory, str(group_index))
        with open(group_file, "w+") as f:
            f.writelines(group)

        words_per_group = len(group)

        group_sizes.append(len(group))
        group_file_size = os.path.getsize(group_file)
        group_file_sizes.append(group_file_size)

    total_language_size = sum(group_file_sizes)
    average_group_file_size = total_language_size / len(group_file_sizes)
    print(f"{lang}: {written_words} words written in {num_groups} groups")
    print(f"{lang}: ~{words_per_group} words per group")
    print(f"{lang}: {format_file_size(average_group_file_size)} average group file size")
    print(f"{lang}: {format_file_size(total_language_size)} total language size")

    return group_sizes


def write_elm_file(language_group_sizes: Dict[str, int]):
    language_variables = []
    exposed_variables = []
    for lang in language_group_sizes:
        group_sizes = language_group_sizes[lang]
        group_sizes_str = ",\n        ".join(map(str, group_sizes))

        variable = f"""\
    {lang.upper()}: [
        {group_sizes_str},
    ],"""
        language_variables.append(variable)

    language_variables_str = "\n".join(language_variables)
    file_template = f"""\
const groupSizes = {{
{language_variables_str}
}};

export default groupSizes;
"""

    with open(wordlist_ts_file, "w+") as f:
        f.write(file_template)


def main():
    if os.path.exists(base_directory):
        shutil.rmtree(base_directory)
    os.mkdir(base_directory)

    language_group_sizes = {}
    for lang in languages:
        print()
        group_sizes = process_language(base_directory, languages, lang)
        language_group_sizes[lang] = group_sizes

    write_elm_file(language_group_sizes)


if __name__ == "__main__":
    main()
