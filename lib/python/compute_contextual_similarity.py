import networkx as nx
import modin.pandas as pd
import json
import os
import argparse
import weakly
from functools import reduce

cs_threshold = 1

parser = argparse.ArgumentParser()
parser.add_argument("comp_unit", nargs=1)

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


def read_apis():
    with open("skip_func.txt", "r+") as f:
        return list(map(lambda string: string.rstrip(),
                        f.readlines()))


def read_udfs():
    with open("Methods.txt", "r+") as f:
        return list(map(lambda string: string.rstrip(),
                        f.readlines()))

# constant params
redefine_dict = read_redefines()
apis = read_apis()
udfs = read_udfs()


def get_only_classname_and_method(method_name):
    """void RelationalDataAccessApplication.printer(Map)
    to RelationalDataAccessApplication.printer"""
    without_rtntype = method_name.split(" ")
    return without_rtntype[-1].split("(")[0]


class ContextualFeature:
    @staticmethod
    def same_callee_in_trunk_count(row):
        "counts the same callee in the trunks"
        trunk1_methods = list(map(lambda tup: get_only_classname_and_method(tup[0]), row.trunk1))
        trunk2_methods = list(map(lambda tup: get_only_classname_and_method(tup[0]), row.trunk2))
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
        if len(trunk1_methods) < len(trunk2_methods):
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

    @staticmethod
    def both_starts_and_ends_with_apis(row):
        trunk1_methods = list(map(lambda tup: tup[0], row.trunk1))
        trunk2_methods = list(map(lambda tup: tup[0], row.trunk2))
        trunk1_head, trunk1_end = trunk1_methods[0], trunk1_methods[-1]
        trunk2_head, trunk2_end = trunk2_methods[0], trunk2_methods[-1]

        trunk1_head_classname_and_method = get_only_classname_and_method(trunk1_head)
        trunk1_end_classname_and_method = get_only_classname_and_method(trunk1_end)
        trunk2_head_classname_and_method = get_only_classname_and_method(trunk2_head)
        trunk2_end_classname_and_method = get_only_classname_and_method(trunk2_end)

        trunk1_head_is_api = any(list(map(lambda line: trunk1_head_classname_and_method in line, apis)))
        trunk1_end_is_api = any(list(map(lambda line: trunk1_end_classname_and_method in line, apis)))
        trunk2_head_is_api = any(list(map(lambda line: trunk2_head_classname_and_method in line, apis)))
        trunk2_end_is_api = any(list(map(lambda line: trunk2_end_classname_and_method in line, apis)))

        if trunk1_head_is_api and trunk1_end_is_api and trunk2_head_is_api and trunk2_end_is_api:
            return 10
        else:
            return 0

    @staticmethod
    def both_starts_and_ends_with_resp_same_method(row):
        trunk1_methods = list(map(lambda tup: tup[0], row.trunk1))
        trunk2_methods = list(map(lambda tup: tup[0], row.trunk2))
        trunk1_head, trunk1_end = trunk1_methods[0], trunk1_methods[-1]
        trunk2_head, trunk2_end = trunk2_methods[0], trunk2_methods[-1]

        trunk1_head_classname_and_method = get_only_classname_and_method(trunk1_head)
        trunk1_end_classname_and_method = get_only_classname_and_method(trunk1_end)
        trunk2_head_classname_and_method = get_only_classname_and_method(trunk2_head)
        trunk2_end_classname_and_method = get_only_classname_and_method(trunk2_end)

        trunk1_head_matches_trunk1_end = trunk1_head_classname_and_method == trunk1_end_classname_and_method
        trunk2_head_matches_trunk2_end = trunk2_head_classname_and_method == trunk2_end_classname_and_method

        if trunk1_head_matches_trunk1_end and trunk2_head_matches_trunk2_end:
            return 10
        else:
            return 0


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
                   ContextualFeature.trunks_share_same_suffixes_length,
                   ContextualFeature.both_starts_and_ends_with_apis,
                   ContextualFeature.both_starts_and_ends_with_resp_same_method], 0)


def leave_only_most_similar_pairs(carpro):
    lhs_unique_values = carpro.index_x.unique()
    acc = []
    for index in lhs_unique_values:
        rows_with_this_index_as_lhs = carpro[carpro.index_x == index]
        rows_with_max_similarity_with_lhs =\
            rows_with_this_index_as_lhs[rows_with_this_index_as_lhs.cs_score ==
                                        rows_with_this_index_as_lhs.cs_score.max()]
        acc.append(rows_with_max_similarity_with_lhs)
    return pd.concat(acc)


def is_initializer(method):
    return "<init>" in method


def is_redefine(method):
    try:
        return redefine_dict[method]
    except KeyError:
        return False


def build_cs_graph(edges):
    acc = nx.DiGraph()
    for tup in edges:
        acc.add_edge(tup[0], tup[1])
    return acc


def find_methods_to_connect(carpro_row):
    """port of SimilarityHandler.identify_similar_method_from_similar_trunk:
     we determine which methods to connect"""
    trunk1 = carpro_row.trunk1
    trunk2 = carpro_row.trunk2
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
    print(f"Python is spawn on {os.getcwd()}")
    args = parser.parse_args()
    comp_unit = args.comp_unit[0]
    jsonfile = f"{comp_unit}_all_longest_trunks.json"
    dataframe = read_trunks(jsonfile)
    carpro = make_carpro_of_dataframe(dataframe)

    contextual_sim_column = carpro.apply(get_trunk_similarity, axis=1)
    carpro["cs_score"] = contextual_sim_column
    # filter rows based on cs_score
    filtered_above_threshold = carpro[carpro.cs_score >= cs_threshold]

    # TEMP
    filtered_above_threshold.to_csv("debug.csv")

    # KLUDGE
    # filtered = leave_only_most_similar_pairs(
    #     no_reflexive(filtered_above_threshold))

    filtered = no_reflexive(filtered_above_threshold)

    methods_to_connect_columns = filtered.apply(
        find_methods_to_connect, axis=1)
    all_edges = []
    for row in methods_to_connect_columns.itertuples():
        all_edges += row[1]

    # HERE
    bidigraph = build_cs_graph(all_edges)
    filename = os.path.split(jsonfile)[-1]
    weakly.main(bidigraph, f"{filename}_filtered" )


if __name__ == "__main__":
    main()
