import modin.pandas as pd
import json
import os
from glob import glob
from functools import reduce

cs_threshold = 1

# Port of Contextual Similarity handler of InferenceEngine.

def read_trunks(json_filename):
    with open(json_filename, "r+") as jsonfile:
        trunks = json.load(jsonfile)
        trunks = list(map(lambda tup: (tup[0], list(map(lambda string: eval(string), tup[1]))),
                          list(trunks.items())))
    return pd.DataFrame(trunks, columns=["index", "trunk"])


def read_redefines():
    with open("Redefines.txt", "r+") as redefines:
        lines_read = list(map(lambda method: method.rstrip(),
                              redefines.readlines()))
        acc = dict()
    for redefine_method in lines_read:
        acc[redefine_method] = True
    return acc


redefine_dict = read_redefines()


class ContextualFeature:
    @staticmethod
    def same_callee_in_trunk_count(row):
        "counts the same callee in the trunks"
        trunk1_methods = list(map(lambda tup: tup[0], row.trunk1))
        trunk2_methods = list(map(lambda tup: tup[0], row.trunk2))
        acc = 0
        for method1 in trunk1_methods:
            if method1 in trunk2_methods:
                acc += 1
        return acc

    @staticmethod
    def trunks_share_same_prefixes_length(row):
        "counts the shared prefixes in the trunks"
        trunk1_methods = list(map(lambda tup: tup[0], row.trunk1))
        trunk2_methods = list(map(lambda tup: tup[0], row.trunk2))
        acc = 0
        if len(trunk1_methods) >= len(trunk2_methods):
            for i in range(len(trunk2_methods)):
                if trunk1_methods[i] == trunk2_methods[i]:
                    acc += 1
        if len(trunk1_methods) < len(trunk2_methods) :
            for i in range(len(trunk1_methods)):
                if trunk1_methods[i] == trunk2_methods[i]:
                    acc += 1
        return acc

    @staticmethod
    def trunks_share_same_suffixes_length(row):
        "counts the shared suffixes in the trunks"
        trunk1_methods = list(map(lambda tup: tup[0], row.trunk1))
        trunk2_methods = list(map(lambda tup: tup[0], row.trunk2))
        acc = 0
        length_offset = len(trunk1_methods) - len(trunk2_methods)
        if length_offset > 0:
            fillers = ["filler" for _ in range(length_offset)]
            trunk2_methods = fillers + trunk2_methods
        if length_offset < 0:
            fillers = ["filler" for _ in range(-length_offset)]
            trunk1_methods = fillers + trunk1_methods
        for i in range(len(trunk1_methods)-1, -1, -1):
            if trunk1_methods[i] == trunk2_methods[i]:
                acc += 1
        return acc


def make_carpro_of_dataframe(dataframe):
    # prepare lhs
    dataframe1 = dataframe.copy()
    dataframe1 = dataframe1.rename(columns={
        "index": "index",
        "trunk": "trunk1",
    })
    # prepare rhs
    dataframe2 = dataframe.copy()
    dataframe2 = dataframe2.rename(columns={
        "index": "index",
        "trunk": "trunk2",
    })
    carpro = pd.merge(dataframe1, dataframe2, how="cross")
    return carpro



def no_reflexive(dataframe):
    cond1 = dataframe["index_x"] != dataframe["index_y"]
    cond2 = dataframe["trunk1"] != dataframe["trunk2"]
    return dataframe[cond1 | cond2]


def get_trunk_similarity(row):
    return reduce(lambda acc, feature: acc + feature(row),
                  [ContextualFeature.same_callee_in_trunk_count,
                   ContextualFeature.trunks_share_same_prefixes_length,
                   ContextualFeature.trunks_share_same_suffixes_length], 0)


def leave_only_most_similar_pairs(carpro):
    lhs_unique_values = carpro.index_x.unique()
    acc = []
    for index in lhs_unique_values:
        rows_with_this_index_as_lhs = carpro[carpro.index_x == index]
        rows_with_max_similarity_with_lhs =\
            rows_with_this_index_as_lhs[rows_with_this_index_as_lhs.cs_score == rows_with_this_index_as_lhs.cs_score.max()]
        acc.append(rows_with_max_similarity_with_lhs)
    return pd.concat(acc)


def is_initializer(method):
    return "<init>" in method


def is_redefine(method):
    try:
        return redefine_dict[method]
    except KeyError:
        return False


def find_methods_to_connect(carpro_row):
    "port of SimilarityHandler.identify_similar_method_from_similar_trunk:\
     we determine which methods to connect"
    trunk1 = carpro_row.trunk1
    trunk2 = carpro_row.trunk2
    cs_score = carpro_row.cs_score
    assert cs_score > cs_threshold
    # computing root_pair_list
    trunk1_root = trunk1[0]
    trunk2_root = trunk2[0]
    if not (trunk1_root == trunk2_root) and\
       not (is_initializer(trunk1_root) or is_initializer(trunk2_root)):
        root_pair_list = [(trunk1_root, trunk2_root),
                          (trunk2_root, trunk1_root)]
    else:
        root_pair_list = []
        # computing leaf_pair_list
    trunk1_leaf = trunk1[-1]
    trunk2_leaf = trunk2[-1]
    if not (trunk1_leaf == trunk2_leaf) and\
       not (is_initializer(trunk1_leaf) or is_initializer(trunk2_leaf)):
        leaf_pair_list = [(trunk1_leaf, trunk2_leaf),
                          (trunk2_leaf, trunk1_leaf)]
    else:
        leaf_pair_list = []
    redefines_carpro = []
    for method1 in trunk1:
        for method2 in trunk2:
            if is_redefine(method1) and is_redefine(method2)\
               and (method1 != method2):
                redefines_carpro.append((method1, method2))
    return root_pair_list + leaf_pair_list + redefines_carpro


def main():
    for jsonfile in glob("*.json"):
        dataframe = read_trunks(jsonfile)
        carpro = make_carpro_of_dataframe(dataframe)

        contextual_sim_column = carpro.apply(get_trunk_similarity, axis=1)
        carpro["cs_score"] = contextual_sim_column
        # filter rows based on cs_score
        filtered_above_threshold = carpro[carpro.cs_score > cs_threshold]
        filtered = leave_only_most_similar_pairs(no_reflexive(filtered_above_threshold))

        # TODO: flatten this thing!!!!!
        methods_to_connect_columns = filtered.apply(find_methods_to_connect, axis=1)
        acc = []
        for row in methods_to_connect_columns.itertuples():
            acc += row[1]
        methods_to_connect_df = pd.DataFrame(acc)

        filename = os.path.split(jsonfile)[-1]
        methods_to_connect_df.to_csv(f"{filename}_filtered.csv")


if __name__ == "__main__":
    main()
