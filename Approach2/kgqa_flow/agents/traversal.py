from __future__ import annotations

import random
from typing import List, Optional, Tuple

import networkx as nx

from ..models import GenerationState

Triple = Tuple[str, str, str]


def _choose_difficulty(state: GenerationState, rng: random.Random) -> str:
    difficulty = state.metadata.get("difficulty")
    if difficulty:
        return difficulty
    return rng.choices(["easy", "medium", "hard"], weights=[0.5, 0.3, 0.2])[0]


def _random_path(graph: nx.MultiDiGraph, length: int, rng: random.Random) -> Optional[List[Triple]]:
    nodes = list(graph.nodes)
    if not nodes:
        return None
    for _ in range(200):
        current = rng.choice(nodes)
        path: List[Triple] = []
        for _ in range(length):
            edges = list(graph.out_edges(current, keys=True, data=True))
            if not edges:
                path = []
                break
            _, target, _, data = rng.choice(edges)
            rel = data.get("relation")
            if rel is None:
                path = []
                break
            path.append((current, rel, target))
            current = target
        if len(path) == length:
            return path
    return None


def _has_rel_to(graph: nx.MultiDiGraph, source: str, rel: str, target: str) -> bool:
    if not graph.has_edge(source, target):
        return False
    edge_data = graph.get_edge_data(source, target) or {}
    rels = [data.get("relation") for data in edge_data.values()]
    return rel in rels


def _disambiguation_case(graph: nx.MultiDiGraph, rng: random.Random) -> Optional[dict]:
    nodes = list(graph.nodes)
    rng.shuffle(nodes)
    for subject in nodes:
        rel_to_targets = {}
        for _, target, _, data in graph.out_edges(subject, keys=True, data=True):
            rel = data.get("relation")
            if rel is None:
                continue
            rel_to_targets.setdefault(rel, set()).add(target)
        viable = [(rel, targets) for rel, targets in rel_to_targets.items() if len(targets) >= 3]
        if not viable:
            continue
        rel1, targets = rng.choice(viable)
        targets = sorted(targets)
        rng.shuffle(targets)
        for candidate in targets:
            out_edges = list(graph.out_edges(candidate, keys=True, data=True))
            rng.shuffle(out_edges)
            for _, end, _, data in out_edges:
                rel2 = data.get("relation")
                if rel2 is None:
                    continue
                non_matching = [
                    t
                    for t in targets
                    if t != candidate and not _has_rel_to(graph, t, rel2, end)
                ]
                if not non_matching:
                    continue
                others = rng.sample(non_matching, k=min(3, len(non_matching)))
                candidates = [candidate] + others
                rng.shuffle(candidates)
                return {
                    "question_type": "disambiguation",
                    "difficulty": "hard",
                    "disambiguation_subject": subject,
                    "disambiguation_relation": rel1,
                    "disambiguation_followup_relation": rel2,
                    "disambiguation_end": end,
                    "candidate_entities": candidates,
                    "path_triples": [(subject, rel1, candidate), (candidate, rel2, end)],
                }
    return None


def traversal(state: GenerationState, graph: nx.MultiDiGraph, rng: random.Random) -> GenerationState:
    difficulty = _choose_difficulty(state, rng)

    if difficulty == "hard":
        disambig = _disambiguation_case(graph, rng)
        if disambig:
            state.metadata.update(disambig)
            state.subgraph = disambig["path_triples"]
            return state

    length = {"easy": 1, "medium": 2, "hard": 3}.get(difficulty, 1)
    path = _random_path(graph, length, rng)
    if path is None and length > 1:
        path = _random_path(graph, 1, rng)
        difficulty = "easy"

    if path is None:
        state.metadata["difficulty"] = difficulty
        return state

    state.subgraph = path
    state.metadata.update(
        {
            "difficulty": difficulty,
            "question_type": "path",
            "path_triples": path,
        }
    )
    return state
