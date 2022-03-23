import networkx as nx
import csv
import modin.pandas as pd
import random
from networkx.algorithms import weakly_connected_components


class Graphs:
    @staticmethod
    def edge_list_to_digraph(edge_list):
        acc = nx.DiGraph()
        for (v1, v2) in edge_list:
            acc.add_edge(v1, v2)
        return acc

    @staticmethod
    def prepare_api_graph(api_csv_filename):
        with open(api_csv_filename, "r") as apicsv:
            reader = csv.reader(apicsv)
            edge_list = list(map(lambda lst: (lst[1], lst[2]), reader))
        return Graphs.edge_list_to_digraph(edge_list)

    @staticmethod
    def prepare_udf_graph(udf_csv_filename):
        with open(udf_csv_filename, "r") as udfcsv:
            reader = csv.reader(udfcsv)
            edge_list = list(map(lambda lst: (lst[1], lst[2]), reader))
        return Graphs.edge_list_to_digraph(edge_list)


class Pruner:
    @staticmethod
    def collect_method_pairs_for_ns_cluster(ns_cluster, transfer_df):
        "kernel function for `collect_method_pairs_for_ns_clusters`"
        this_cluster_method_pairs = transfer_df[
            transfer_df["methname2"].isin(ns_cluster)
        ]
        return this_cluster_method_pairs

    @staticmethod
    def collect_method_pairs_for_ns_clusters_api(api_ns_clusters, api_transfer_df):
        return list(
            map(
                lambda api_ns_cluster: Pruner.collect_method_pairs_for_ns_cluster(
                    api_ns_cluster, api_transfer_df
                ),
                api_ns_clusters,
            )
        )

    @staticmethod
    def collect_method_pairs_for_ns_clusters_udf(udf_ns_clusters, udf_transfer_df):
        return list(
            map(
                lambda udf_ns_cluster: Pruner.collect_method_pairs_for_ns_cluster(
                    udf_ns_cluster, udf_transfer_df
                ),
                udf_ns_clusters,
            )
        )

    @staticmethod
    def select_one_pair_from_pairs(dataframe):
        random_row_index = random.randint(0, len(dataframe) + 1)
        return dataframe.iloc[random_row_index].to_frame()


def main():
    def flatten(coll_coll):
        acc = []
        for coll in coll_coll:
            for elem in coll:
                acc.append(elem)
        return acc

    api_transfer_csv_filename = "sagan-renderer->sagan-site_api_transferred.csv"
    udf_transfer_csv_filename = "sagan-renderer->sagan-site_udf_transferred.csv"

    api_csv_filename = "NodeWiseFeatures_sagan-site_apis.csv_filtered.csv"
    udf_csv_filename = "NodeWiseFeatures_sagan-site_udfs.csv_filtered.csv"

    api_graph = Graphs.prepare_api_graph(api_csv_filename)
    udf_graph = Graphs.prepare_udf_graph(udf_csv_filename)

    api_ns_clusters = list(weakly_connected_components(api_graph))
    udf_ns_clusters = list(weakly_connected_components(udf_graph))

    api_transfer_df = pd.read_csv(api_transfer_csv_filename, index_col=0)
    udf_transfer_df = pd.read_csv(udf_transfer_csv_filename, index_col=0)

    api_transfer_csv_methods = api_transfer_df.methname2.unique()
    udf_transfer_csv_methods = udf_transfer_df.methname2.unique()

    # pre-collect method pairs not in api_ns_clusters.
    apis_not_forming_clusters = [
        method
        for method in api_transfer_csv_methods
        if method not in flatten(api_ns_clusters)
    ]
    api_pairs_not_forming_clusters = api_transfer_df[
        api_transfer_df["methname2"].isin(apis_not_forming_clusters)
    ]

    # pre-collect method pairs not in udf_ns_clusters.
    udfs_not_forming_clusters = [
        method
        for method in udf_transfer_csv_methods
        if method not in flatten(udf_ns_clusters)
    ]
    udf_pairs_not_forming_clusters = udf_transfer_df[
        udf_transfer_df["methname2"].isin(udfs_not_forming_clusters)
    ]

    # (api) leave only one method pairs (for transfer) per NS clusters.
    api_transfer_df_dieted = pd.concat(
        Pruner.collect_method_pairs_for_ns_clusters_api(
            api_ns_clusters, api_transfer_df
        )
    )

    # (udf) leave only one method pairs (for transfer) per NS clusters.
    udf_transfer_df_dieted = pd.concat(
        Pruner.collect_method_pairs_for_ns_clusters_udf(
            udf_ns_clusters, udf_transfer_df
        )
    )
    finalized = pd.concat(
        [
            api_pairs_not_forming_clusters,
            udf_pairs_not_forming_clusters,
            api_transfer_df_dieted,
            udf_transfer_df_dieted,
        ]
    )
    finalized.to_csv("sagan-renderer->sagan-site.csv")


if __name__ == "__main__":
    main()


def repl():
    import os
    os.chdir("~/Dropbox/InferenceEngine")
