from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Sequence
import random

Triple = Dict[str, Any]


@dataclass
class SampledSubgraph:
    """Container for sampled triples and metadata."""

    triples: List[Triple]
    pattern_type: str
    hop_length: int


class SubgraphSampler:
    """Sampling strategies for subgraphs."""

    def __init__(self, connector: Any, rng: Optional[random.Random] = None) -> None:
        self.connector = connector
        self.rng = rng or random.Random()

    def _node_matches(self, a: Any, b: Any) -> bool:
        return str(a) == str(b)

    def _next_node(self, triple: Triple, current: Any) -> Any:
        if self._node_matches(triple.get("s"), current):
            return triple.get("o")
        return triple.get("s")

    def _dedupe(self, triples: List[Triple]) -> List[Triple]:
        seen = set()
        out: List[Triple] = []
        for t in triples:
            key = (t.get("s"), t.get("p"), t.get("o"))
            if key in seen:
                continue
            seen.add(key)
            out.append(t)
        return out

    def sample_path(
        self,
        seed: Any,
        depth: int,
        max_paths: int,
        rel_whitelist: Optional[Sequence[str]] = None,
    ) -> SampledSubgraph:
        """Input: seed id, depth, max_paths. Output: SampledSubgraph with labeled triples."""
        triples: List[Triple] = []
        seed_id = self.connector.canonicalize_entity(seed)
        for _ in range(max_paths):
            current = seed_id
            for _ in range(depth):
                neighbors = self.connector.get_neighbors(current, rel_whitelist=rel_whitelist)
                if not neighbors:
                    break
                triple = self.rng.choice(neighbors)
                triples.append(triple)
                current = self._next_node(triple, current)
        return SampledSubgraph(self._dedupe(triples), "path", depth)

    def sample_star(
        self,
        seed: Any,
        max_edges: int,
        rel_whitelist: Optional[Sequence[str]] = None,
    ) -> SampledSubgraph:
        """Input: seed id, max_edges. Output: SampledSubgraph with labeled triples."""
        seed_id = self.connector.canonicalize_entity(seed)
        neighbors = self.connector.get_neighbors(seed_id, rel_whitelist=rel_whitelist)
        if not neighbors:
            return SampledSubgraph([], "star", 1)
        self.rng.shuffle(neighbors)
        triples = neighbors[: max_edges]
        return SampledSubgraph(self._dedupe(triples), "star", 1)

    def sample_join(
        self,
        seed: Any,
        max_depth: int,
        rel_whitelist: Optional[Sequence[str]] = None,
    ) -> SampledSubgraph:
        """Input: seed id, max_depth. Output: SampledSubgraph with labeled triples."""
        depth = max(1, int(max_depth))
        path_a = self.sample_path(seed, depth=depth, max_paths=1, rel_whitelist=rel_whitelist)
        path_b = self.sample_path(seed, depth=depth, max_paths=1, rel_whitelist=rel_whitelist)
        combined = self._dedupe(path_a.triples + path_b.triples)
        return SampledSubgraph(combined, "join", depth)
