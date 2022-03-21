import os
import re
import modin.pandas as pd
from functools import reduce
import argparse
from rich import print

ns_threshold = 16

parser = argparse.ArgumentParser()
parser.add_argument("comp_unit", nargs=1)


def make_carpro_of_dataframe(dataframe):
    # prepare lhs
    dataframe1 = dataframe.copy()
    dataframe1 = dataframe1.rename(
        columns={
            "methname": "methname",
            "return_type": "return_type1",
            "class_name": "class_name1",
            "method_name": "method_name1",
            "package_name": "package_name1",
            "is_framework_method": "is_framework_method1",
            "is_java_builtin_method": "is_java_builtin_method1",
            "is_library_code": "is_library_code1",
            "returnval_not_used_in_caller": "returnval_not_used_in_caller1",
            "is_initializer": "is_initializer1",
            "annots": "annots1",
        }
    )
    # prepare rhs
    dataframe2 = dataframe.copy()
    dataframe2 = dataframe2.rename(
        columns={
            "methname": "methname",
            "return_type": "return_type2",
            "class_name": "class_name2",
            "method_name": "method_name2",
            "package_name": "package_name2",
            "is_framework_method": "is_framework_method2",
            "is_java_builtin_method": "is_java_builtin_method2",
            "is_library_code": "is_library_code2",
            "returnval_not_used_in_caller": "returnval_not_used_in_caller2",
            "is_initializer": "is_initializer2",
            "annots": "annots2",
        }
    )
    carpro = pd.merge(dataframe1, dataframe2, how="cross")
    return carpro


# https://stackoverflow.com/questions/29916065/how-to-do-camelcase-split-in-python
def camel_case_split(identifier):
    matches = re.finditer(
        ".+?(?:(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])|$)", identifier
    )
    return [m.group(0) for m in matches]


def get_word_set(identifier):
    return set(map(lambda string: string.lower(), camel_case_split(identifier)))


class PairwiseFeature:
    scores = {
        "is_both_framework_code": 4,
        "belong_to_same_class": 8,
        "belong_to_same_package": 6,
        "returnval_not_used_in_caller": 3,
        "return_type_is_anothers_class": 4,
        "has_same_return_type": 2,
        "is_both_java_builtin": 4,
        "is_both_initializer": 4,
        "method_contains_same_words": 2,
        "method_has_same_prefixes": 5,
        "class_name_has_same_words": 4,
        "class_name_has_same_prefixes": 7,
        "class_name_has_same_suffixes": 6
    }

    @staticmethod
    def is_both_framework_code(row):
        method1_is_framework_method = row.is_framework_method1
        method2_is_framework_method = row.is_framework_method2
        # if method1_is_framework_method and method2_is_framework_method:
        if method1_is_framework_method == method2_is_framework_method:
            return PairwiseFeature.scores["is_both_framework_code"]
        else:
            return 0

    @staticmethod
    def belong_to_same_class(row):
        method1_class_name = row.class_name1
        method2_class_name = row.class_name2
        if method1_class_name == method2_class_name:
            return PairwiseFeature.scores["belong_to_same_class"]
        else:
            return 0

    @staticmethod
    def belong_to_same_package(row):
        method1_package_name = row.package_name1
        method2_package_name = row.package_name2
        if method1_package_name == method2_package_name:
            return PairwiseFeature.scores["belong_to_same_package"]
        else:
            return 0

    @staticmethod
    def returnval_not_used_in_caller(row):
        method1_returnval_not_used_in_caller = row.returnval_not_used_in_caller1
        method2_returnval_not_used_in_caller = row.returnval_not_used_in_caller2
        if (
            method1_returnval_not_used_in_caller,
            method2_returnval_not_used_in_caller,
        ) == (True, True) or (
            method1_returnval_not_used_in_caller,
            method2_returnval_not_used_in_caller,
        ) == (
            False,
            False,
        ):
            return PairwiseFeature.scores["returnval_not_used_in_caller"]
        else:
            return 0

    @staticmethod
    def return_type_is_anothers_class(row):
        method1_return_type = row.return_type1
        method1_class_name = row.class_name1
        method2_return_type = row.return_type2
        method2_class_name = row.class_name2
        if (
            method1_return_type == method2_class_name
            or method2_return_type == method1_class_name
        ):
            return PairwiseFeature.scores["return_type_is_anothers_class"]
        else:
            return 0

    @staticmethod
    def has_same_return_type(row):
        method1_return_type = row.return_type1
        method2_return_type = row.return_type2
        if method1_return_type == method2_return_type:
            return PairwiseFeature.scores["has_same_return_type"]
        else:
            return 0

    @staticmethod
    def is_both_java_builtin(row):
        method1_is_java_builtin_method = row.is_java_builtin_method1
        method2_is_java_builtin_method = row.is_java_builtin_method2
        # if method1_is_java_builtin_method and method2_is_java_builtin_method:
        if method1_is_java_builtin_method == method2_is_java_builtin_method:
            return PairwiseFeature.scores["is_both_java_builtin"]
        else:
            return 0

    @staticmethod
    def is_both_initializer(row):
        method1_is_initializer = row.is_initializer1
        method2_is_initializer = row.is_initializer2
        # if method1_is_initializer and method2_is_initializer:
        if method1_is_initializer == method2_is_initializer:
            return PairwiseFeature.scores["is_both_initializer"]
        else:
            return 0

    @staticmethod
    def has_same_annots(row):
        method1_annotation = row.annots1
        method2_annotation = row.annots2
        if method1_annotation == method2_annotation:
            return PairwiseFeature.scores["has_same_annots"]
        else:
            return 0

    @staticmethod
    def method_contains_same_words(row):
        method1_method_word_set = get_word_set(row.method_name1)
        method2_method_word_set = get_word_set(row.method_name2)
        for word1 in method1_method_word_set:
            if word1 in method2_method_word_set:
                return PairwiseFeature.scores["method_contains_same_words"]
        return 0

    @staticmethod
    def method_has_same_prefixes(row):
        method1_method_words = camel_case_split(row.method_name1)
        method2_method_words = camel_case_split(row.method_name2)
        if method1_method_words[0] == method2_method_words[0]:
            return PairwiseFeature.scores["method_has_same_prefixes"]
        else:
            return 0

    @staticmethod
    def class_name_has_same_words(row):
        method1_class_name = row.class_name1
        method2_class_name = row.class_name2
        method1_class_name_camelcase = (
            method1_class_name[0].lower() + method1_class_name[1:]
        )
        method2_class_name_camelcase = (
            method2_class_name[0].lower() + method2_class_name[2:]
        )
        method1_method_words = camel_case_split(method1_class_name_camelcase)
        method2_method_words = camel_case_split(method2_class_name_camelcase)
        there_is_common_word = len(set(method1_method_words).intersection(method2_method_words))
        if there_is_common_word:
            return PairwiseFeature.scores["class_name_has_same_words"]
        else:
            return 0

    @staticmethod
    def class_name_has_same_prefixes(row):
        method1_class_name = row.class_name1
        method2_class_name = row.class_name2
        method1_class_name_camelcase = (
            method1_class_name[0].lower() + method1_class_name[1:]
        )
        method2_class_name_camelcase = (
            method2_class_name[0].lower() + method2_class_name[1:]
        )
        method1_class_words = camel_case_split(method1_class_name_camelcase)
        method2_class_words = camel_case_split(method2_class_name_camelcase)
        if method1_class_words[0].lower() == method2_class_words[0].lower():
            return PairwiseFeature.scores["class_name_has_same_prefixes"]
        else:
            return 0

    @staticmethod
    def class_name_has_same_suffixes(row):
        method1_class_name = row.class_name1
        method2_class_name = row.class_name2
        method1_class_name_camelcase = (
            method1_class_name[0].lower() + method1_class_name[1:]
        )
        method2_class_name_camelcase = (
            method2_class_name[0].lower() + method2_class_name[1:]
        )
        method1_class_words = camel_case_split(method1_class_name_camelcase)
        method2_class_words = camel_case_split(method2_class_name_camelcase)
        if method1_class_words[-1].lower() == method2_class_words[-1].lower():
            return PairwiseFeature.scores["class_name_has_same_suffixes"]
        else:
            return 0

def run_all_pairwise_feature(row):
    return reduce(
        lambda acc, feature: acc + feature(row),
        [
            # PairwiseFeature.is_both_framework_code,
            PairwiseFeature.belong_to_same_class,
            PairwiseFeature.belong_to_same_package,
            PairwiseFeature.returnval_not_used_in_caller,
            PairwiseFeature.return_type_is_anothers_class,
            PairwiseFeature.has_same_return_type,
            # PairwiseFeature.is_both_java_builtin,
            # PairwiseFeature.is_both_initializer,
            PairwiseFeature.method_contains_same_words,
            # PairwiseFeature.method_has_same_prefixes,
            PairwiseFeature.class_name_has_same_words,
            PairwiseFeature.class_name_has_same_prefixes,
            PairwiseFeature.class_name_has_same_suffixes,
        ],
        0,
    )


def no_reflexive(dataframe):
    cond1 = dataframe["methname_x"] != dataframe["methname_y"]
    cond2 = dataframe["return_type1"] != dataframe["return_type2"]
    cond3 = dataframe["class_name1"] != dataframe["class_name2"]
    cond4 = dataframe["method_name1"] != dataframe["method_name2"]
    cond5 = dataframe["package_name1"] != dataframe["package_name2"]
    cond6 = dataframe["is_framework_method1"] != dataframe["is_framework_method2"]
    cond7 = dataframe["is_java_builtin_method1"] != dataframe["is_java_builtin_method2"]
    cond8 = dataframe["is_library_code1"] != dataframe["is_library_code2"]
    cond9 = (
        dataframe["returnval_not_used_in_caller1"]
        != dataframe["returnval_not_used_in_caller2"]
    )
    cond10 = dataframe["is_initializer1"] != dataframe["is_initializer2"]
    cond11 = dataframe["annots1"] != dataframe["annots2"]
    return dataframe[
        cond1
        | cond2
        | cond3
        | cond4
        | cond5
        | cond6
        | cond7
        | cond8
        | cond9
        | cond10
        | cond11
    ]


def main():
    print(f"Python is spawn on {os.getcwd()}")
    args = parser.parse_args()
    comp_unit = args.comp_unit[0]
    csvfile = f"NodeWiseFeatures_{comp_unit}_apis.csv"
    dataframe = pd.read_csv(csvfile)
    carpro = make_carpro_of_dataframe(dataframe)
    nodewise_sim_column = carpro.apply(run_all_pairwise_feature, axis=1)
    carpro["ns_score"] = nodewise_sim_column
    # filter rows based on ns_score
    filtered_above_threshold = carpro[carpro.ns_score > ns_threshold]
    filtered = no_reflexive(filtered_above_threshold)
    filtered[["methname_x", "methname_y", "ns_score"]].to_csv(f"NodeWiseFeatures_{comp_unit}_apis.csv_filtered.csv")


if __name__ == "__main__":
    main()


def repl_setup():
    os.chdir("/Users/jslee/Dropbox/InferenceEngine/")
    comp_unit = "sagan-renderer"
    csvfile = f"NodeWiseFeatures_{comp_unit}_apis.csv"
    dataframe = pd.read_csv(csvfile)


def comment(dataframe):
    pass
