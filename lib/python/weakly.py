import json
import csv
import networkx as nx
import pygraphviz as pgv
from networkx.algorithms.components.weakly_connected import weakly_connected_components
from networkx.algorithms.tree.mst import minimum_spanning_edges


def read_graph():
    return pgv.AGraph("test.dot")


def filter_NS(agraph):
    acc = pgv.AGraph()
    for edge in agraph.edges():
        if agraph.get_edge(*edge).attr["label"] == "NS":
            acc.add_edge(*edge)
        else:
            continue
    return acc


def drain(iterator, f):
    print(f"draining {iterator}...", end="")
    while True:
        try:
            f(next(iterator))
        except StopIteration:
            print("done")
            return


def to_bidigraph(undigraph):
    acc = nx.DiGraph()
    acc.add_nodes_from(undigraph.nodes)
    for (e1, e2) in undigraph.edges:
        acc.add_edge(e1, e2)
        acc.add_edge(e2, e1)
    return acc


def run_prim(graph):
    return list(minimum_spanning_edges(graph, algorithm="prim"))


def node_set_to_undigraph(node_set, graph):
    acc = nx.Graph()
    for (e1, e2) in graph.edges:
        if e1 in node_set and e2 in node_set:
            acc.add_edge(e1, e2)
    return acc


def flatten(ll):
    acc = []
    for l in ll:
        acc += l
    return acc


def check(subgraphs):
    acc = 0
    for subgraph in subgraphs:
        acc += subgraph.number_of_edges()
    return acc


def serialize_to_csv(edges, filename):
    with open(filename, "w+") as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(edges)


def serialize_to_json(edges, filename):
    with open(filename, "w+") as jsonfile:
        json.dump(edges, jsonfile)


def main(bidigraph, filename):
    subgraph_nodesets = weakly_connected_components(bidigraph)
    subgraphs = set(map(lambda nodeset: node_set_to_undigraph(nodeset, bidigraph), subgraph_nodesets))
    mst_edges = list(map(run_prim, subgraphs))
    serialize_to_json(mst_edges, f"{filename}.json")
    all_edges = flatten(mst_edges)
    serialize_to_csv(all_edges, f"{filename}.csv")
