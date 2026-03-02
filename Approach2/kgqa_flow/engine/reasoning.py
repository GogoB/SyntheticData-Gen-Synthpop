from __future__ import annotations

from typing import Dict, List, Optional, Tuple

import networkx as nx

Triple = Tuple[str, str, str]


class GraphReasoner:
    def __init__(self, graph: nx.MultiDiGraph) -> None:
        self.graph = graph

    def constrained_walk(self, start: str, relations: List[str]) -> Optional[Tuple[List[str], List[Triple]]]:
        current_nodes = [start]
        path_triples: List[Triple] = []
        for rel in relations:
            next_nodes: List[str] = []
            chosen_edge: Optional[Triple] = None
            for node in sorted(current_nodes):
                edges = self.graph.out_edges(node, keys=True, data=True)
                for _, target, _, data in sorted(edges, key=lambda e: (e[1], str(e[3].get("relation", "")))):
                    if data.get("relation") == rel:
                        next_nodes.append(target)
                        if chosen_edge is None:
                            chosen_edge = (node, rel, target)
            if not next_nodes:
                return None
            if chosen_edge:
                path_triples.append(chosen_edge)
            current_nodes = next_nodes
        return current_nodes, path_triples

    def shortest_path(self, start: str, end: str) -> Optional[List[str]]:
        try:
            return nx.shortest_path(self.graph, start, end)
        except (nx.NetworkXNoPath, nx.NodeNotFound):
            return None

    def _path_edges_exist(self, path_triples: List[Triple]) -> bool:
        for subject, relation, obj in path_triples:
            if not self.graph.has_edge(subject, obj):
                return False
            edge_data = self.graph.get_edge_data(subject, obj)
            if not edge_data:
                return False
            relations = [data.get("relation") for data in edge_data.values()]
            if relation not in relations:
                return False
        return True

    def answer_from_path(self, path_triples: List[Triple]) -> Optional[str]:
        if not path_triples:
            return None
        if self._path_edges_exist(path_triples):
            return path_triples[-1][2]
        relations = [triple[1] for triple in path_triples]
        start = path_triples[0][0]
        result = self.constrained_walk(start, relations)
        if result is None:
            return None
        endpoints, _ = result
        return sorted(endpoints)[0] if endpoints else None

    def answer_from_disambiguation(
        self,
        subject: str,
        rel1: str,
        rel2: str,
        end: str,
        candidates: List[str],
        path_triples: Optional[List[Triple]] = None,
    ) -> Optional[str]:
        if path_triples and len(path_triples) >= 2:
            candidate = path_triples[0][2]
            if candidate in candidates and self._path_edges_exist(path_triples[:2]):
                return candidate
        for candidate in sorted(candidates):
            if not self.graph.has_edge(subject, candidate):
                continue
            edge_data = self.graph.get_edge_data(subject, candidate) or {}
            rels = [data.get("relation") for data in edge_data.values()]
            if rel1 not in rels:
                continue
            if not self.graph.has_edge(candidate, end):
                continue
            edge_data2 = self.graph.get_edge_data(candidate, end) or {}
            rels2 = [data.get("relation") for data in edge_data2.values()]
            if rel2 in rels2:
                return candidate
        return None

    def answer_from_metadata(self, metadata: Dict) -> Optional[str]:
        if metadata.get("question_type") == "disambiguation":
            return self.answer_from_disambiguation(
                metadata.get("disambiguation_subject"),
                metadata.get("disambiguation_relation"),
                metadata.get("disambiguation_followup_relation"),
                metadata.get("disambiguation_end"),
                metadata.get("candidate_entities", []),
                metadata.get("path_triples"),
            )
        return self.answer_from_path(metadata.get("path_triples", []))
