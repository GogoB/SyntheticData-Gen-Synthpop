from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple
import hashlib

Difficulty = str


def mix_seed(base: int, *parts: Any) -> int:
    h = hashlib.sha256()
    h.update(str(base).encode("utf-8"))
    for part in parts:
        h.update(b"|")
        h.update(str(part).encode("utf-8"))
    return int.from_bytes(h.digest()[:4], "big")


def node_name(node_id: int) -> str:
    return f"Entity_{node_id:06d}"


@dataclass(frozen=True)
class JobSpec:
    job_id: int
    seed: int
    difficulty: Difficulty
    root_node: int
    max_depth: int


@dataclass(frozen=True)
class Subgraph:
    nodes: List[int]
    edges: List[Tuple[int, int, str]]
    root_node: int
    max_depth: int


@dataclass(frozen=True)
class Question:
    text: str
    qtype: str
    params: Dict[str, Any]


@dataclass(frozen=True)
class Answer:
    value: Any
    text: str


@dataclass(frozen=True)
class QAResult:
    job_id: int
    passed: bool
    failure_reason: Optional[str]
    target_difficulty: Difficulty
    assigned_difficulty: Difficulty
    question: Optional[Question]
    answer: Optional[Answer]
    root_node: int
    seed: int

    def to_record(self) -> Dict[str, Any]:
        if not self.passed or self.question is None or self.answer is None:
            raise ValueError("Cannot build record from failed QAResult")
        return {
            "id": self.job_id,
            "question": self.question.text,
            "answer": self.answer.text,
            "difficulty": self.assigned_difficulty,
            "target_difficulty": self.target_difficulty,
            "qtype": self.question.qtype,
            "root": self.root_node,
            "seed": self.seed,
        }

def write_graph(graph: Any, path: str) -> None:
    try:
        from networkx.readwrite.gpickle import write_gpickle as _write

        _write(graph, path)
        return
    except Exception:
        pass
    try:
        import networkx as nx

        if hasattr(nx, "write_gpickle"):
            nx.write_gpickle(graph, path)
            return
    except Exception:
        pass
    import pickle

    with open(path, "wb") as f:
        pickle.dump(graph, f, protocol=pickle.HIGHEST_PROTOCOL)


def read_graph(path: str) -> Any:
    try:
        from networkx.readwrite.gpickle import read_gpickle as _read

        return _read(path)
    except Exception:
        pass
    try:
        import networkx as nx

        if hasattr(nx, "read_gpickle"):
            return nx.read_gpickle(path)
    except Exception:
        pass
    import pickle

    with open(path, "rb") as f:
        return pickle.load(f)
