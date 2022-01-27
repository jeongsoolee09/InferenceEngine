import networkit as nk
import modin.pandas as pd
import json
import os
from glob import glob
from functools import reduce

cs_threshold = 1

# Port of Contextual Similarity handler of InferenceEngine.

# unused for now
def read_graph():
    # return nk.readGraph("jot.dot", nk.Format.GML)
    pass


# unused for now
def read_ns_pairs():
    # all_csvs = list(map(lambda csvfile: pd.read_csv(csvfile), glob(".csv")))
    # return pd.concat(all_csvs)
    pass


def read_trunks():
    with open("sagan-renderer_all_longest_trunks.json", "r+") as jsonfile:
        trunks = json.load(jsonfile)
    return pd.DataFrame(trunks.items(), columns=["index", "trunk"])


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


class ContextualFeature:
    @staticmethod
    def same_callee_in_trunk_count(row):
        "counts the same callee in the trunks"
        trunk1 = row.trunk1
        trunk2 = row.trunk2
        acc = 0
        for method1 in trunk1:
            if method1 in trunk2:
                acc += 1
        return acc

    @staticmethod
    def trunks_share_same_prefixes_length(row):
        "counts the shared prefixes in the trunks"
        trunk1 = row.trunk1
        trunk2 = row.trunk2
        acc = 0
        if len(trunk1) >= len(trunk2):
            for i in range(len(trunk2)):
                if trunk1[i] == trunk2[i]:
                    acc += 1
        if len(trunk1) < len(trunk2) :
            for i in range(len(trunk1)):
                if trunk1[i] == trunk2[i]:
                    acc += 1
        return acc

    @staticmethod
    def trunks_share_same_suffixes_length(row):
        "counts the shared suffixes in the trunks"
        trunk1 = row.trunk1
        trunk2 = row.trunk2
        acc = 0
        length_offset = len(trunk1) - len(trunk2)
        if length_offset > 0:
            fillers = ["filler" for _ in range(length_offset)]
            trunk2 = fillers + trunk2
        if length_offset < 0:
            fillers = ["filler" for _ in range(length_offset)]
            trunk1 = fillers + trunk1
        for i in range(len(trunk1)-1, -1, -1):
            if trunk1[i] == trunk2[i]:
                acc += 1
        return acc


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


def main():
    for jsonfile in glob("*.json"):
        dataframe = read_trunks()
        carpro = make_carpro_of_dataframe(dataframe)
        contextual_sim_column = carpro.apply(get_trunk_similarity, axis=1)
        carpro["cs_score"] = contextual_sim_column
        # filter rows based on cs_score
        filtered_above_threshold = carpro[carpro.cs_score > cs_threshold]
        filtered = leave_only_most_similar_pairs(no_reflexive(filtered_above_threshold))
        filename = os.path.split(jsonfile)[-1]
        filtered.to_csv(f"{filename}_filtered.csv")


if __name__ == "__main__":
    main()
