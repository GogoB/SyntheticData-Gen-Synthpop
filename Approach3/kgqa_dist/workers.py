from __future__ import annotations

from collections import deque
from typing import Dict, List, Optional, Tuple
import random
import os
import multiprocessing as mp

import networkx as nx

from .models import Answer, JobSpec, QAResult, Question, Subgraph, mix_seed, node_name, read_graph


class MockLlmClient:
    def generate(self, templates: List[str], rng: random.Random, **kwargs: str) -> str:
        template = rng.choice(templates)
        return template.format(**kwargs)


class TraversalWorker:
    def __init__(self, graph: nx.DiGraph) -> None:
        self.graph = graph

    def run(self, spec: JobSpec) -> Subgraph:
        if spec.root_node not in self.graph:
            return Subgraph(nodes=[], edges=[], root_node=spec.root_node, max_depth=spec.max_depth)

        visited: Dict[int, int] = {spec.root_node: 0}
        queue: deque[int] = deque([spec.root_node])

        while queue:
            node = queue.popleft()
            depth = visited[node]
            if depth >= spec.max_depth:
                continue
            neighbors = sorted(self.graph.successors(node))
            for nbr in neighbors:
                if nbr not in visited:
                    visited[nbr] = depth + 1
                    queue.append(nbr)

        nodes = sorted(visited.keys())
        edges: List[Tuple[int, int, str]] = []
        for u in nodes:
            for v in sorted(self.graph.successors(u)):
                if v in visited:
                    rel = self.graph[u][v].get("rel", "related_to")
                    edges.append((u, v, rel))
        edges.sort()
        return Subgraph(nodes=nodes, edges=edges, root_node=spec.root_node, max_depth=spec.max_depth)


class QuestionWorker:
    DIFFICULTY_QTYPES = {
        "easy": ["node_attr", "count_neighbors"],
        "medium": ["neighbors_by_rel"],
        "hard": ["two_hop_count"],
    }

    def __init__(self) -> None:
        self.llm = MockLlmClient()

    def run(self, spec: JobSpec, subgraph: Subgraph) -> Tuple[Optional[Question], Optional[str]]:
        rng = random.Random(mix_seed(spec.seed, "question"))
        qtypes = list(self.DIFFICULTY_QTYPES.get(spec.difficulty, []))
        rng.shuffle(qtypes)
        for qtype in qtypes:
            question = self._build_question(qtype, subgraph, rng)
            if question is not None:
                return question, None
        return None, "no_candidate"

    def _build_question(
        self,
        qtype: str,
        subgraph: Subgraph,
        rng: random.Random,
    ) -> Optional[Question]:
        if not subgraph.nodes:
            return None

        if qtype == "node_attr":
            node_id = rng.choice(subgraph.nodes)
            name = node_name(node_id)
            templates = [
                "What is the category of {name}?",
                "Which category does {name} belong to?",
            ]
            text = self.llm.generate(templates, rng, name=name)
            return Question(text=text, qtype=qtype, params={"node_id": node_id, "attr": "category"})

        if qtype == "count_neighbors":
            node_id = rng.choice(subgraph.nodes)
            name = node_name(node_id)
            templates = [
                "How many outgoing neighbors does {name} have?",
                "What is the number of outgoing neighbors for {name}?",
            ]
            text = self.llm.generate(templates, rng, name=name)
            return Question(text=text, qtype=qtype, params={"node_id": node_id})

        if qtype == "neighbors_by_rel":
            rel_map: Dict[Tuple[int, str], List[int]] = {}
            for u, v, rel in subgraph.edges:
                rel_map.setdefault((u, rel), []).append(v)
            candidates = sorted(rel_map.keys())
            if not candidates:
                return None
            node_id, rel = rng.choice(candidates)
            name = node_name(node_id)
            templates = [
                "Which nodes are connected from {name} via relation '{rel}'?",
                "List nodes that {name} connects to with relation '{rel}'.",
            ]
            text = self.llm.generate(templates, rng, name=name, rel=rel)
            return Question(text=text, qtype=qtype, params={"node_id": node_id, "rel": rel})

        if qtype == "two_hop_count":
            node_id = rng.choice(subgraph.nodes)
            name = node_name(node_id)
            templates = [
                "How many nodes are reachable from {name} in exactly two hops?",
                "Count nodes reachable from {name} by a path of length two.",
            ]
            text = self.llm.generate(templates, rng, name=name)
            return Question(text=text, qtype=qtype, params={"node_id": node_id})

        return None


class AnswerWorker:
    def __init__(self, graph: nx.DiGraph) -> None:
        self.graph = graph

    def run(self, question: Question) -> Answer:
        qtype = question.qtype
        params = question.params
        if qtype == "node_attr":
            node_id = params.get("node_id")
            attr = params.get("attr", "category")
            if node_id not in self.graph:
                return Answer(value=None, text="")
            value = self.graph.nodes[node_id].get(attr)
            if value is None:
                return Answer(value=None, text="")
            return Answer(value=value, text=str(value))

        if qtype == "count_neighbors":
            node_id = params.get("node_id")
            if node_id not in self.graph:
                return Answer(value=None, text="")
            count = len(list(self.graph.successors(node_id)))
            return Answer(value=count, text=str(count))

        if qtype == "neighbors_by_rel":
            node_id = params.get("node_id")
            rel = params.get("rel")
            if node_id not in self.graph or rel is None:
                return Answer(value=None, text="")
            neighbors: List[str] = []
            for v in self.graph.successors(node_id):
                edge_rel = self.graph[node_id][v].get("rel", "related_to")
                if edge_rel == rel:
                    neighbors.append(node_name(v))
            neighbors = sorted(set(neighbors))
            if not neighbors:
                return Answer(value=[], text="")
            return Answer(value=neighbors, text=", ".join(neighbors))

        if qtype == "two_hop_count":
            node_id = params.get("node_id")
            if node_id not in self.graph:
                return Answer(value=None, text="")
            first_hop = list(self.graph.successors(node_id))
            second_hop = set()
            for mid in first_hop:
                for dst in self.graph.successors(mid):
                    if dst != node_id:
                        second_hop.add(dst)
            count = len(second_hop)
            return Answer(value=count, text=str(count))

        return Answer(value=None, text="")


class QAWorker:
    DIFFICULTY_BY_QTYPE = {
        "node_attr": "easy",
        "count_neighbors": "easy",
        "neighbors_by_rel": "medium",
        "two_hop_count": "hard",
    }

    def __init__(self, max_list_len: int = 10) -> None:
        self.max_list_len = max_list_len

    def run(self, spec: JobSpec, question: Question, answer: Answer) -> QAResult:
        assigned = self.DIFFICULTY_BY_QTYPE.get(question.qtype, "unknown")
        if assigned == "unknown":
            return QAResult(
                job_id=spec.job_id,
                passed=False,
                failure_reason="unknown_qtype",
                target_difficulty=spec.difficulty,
                assigned_difficulty=assigned,
                question=question,
                answer=answer,
                root_node=spec.root_node,
                seed=spec.seed,
            )

        if answer.value is None or answer.text == "":
            return QAResult(
                job_id=spec.job_id,
                passed=False,
                failure_reason="empty_answer",
                target_difficulty=spec.difficulty,
                assigned_difficulty=assigned,
                question=question,
                answer=answer,
                root_node=spec.root_node,
                seed=spec.seed,
            )

        if isinstance(answer.value, list) and len(answer.value) > self.max_list_len:
            return QAResult(
                job_id=spec.job_id,
                passed=False,
                failure_reason="answer_too_long",
                target_difficulty=spec.difficulty,
                assigned_difficulty=assigned,
                question=question,
                answer=answer,
                root_node=spec.root_node,
                seed=spec.seed,
            )

        if assigned != spec.difficulty:
            return QAResult(
                job_id=spec.job_id,
                passed=False,
                failure_reason="difficulty_mismatch",
                target_difficulty=spec.difficulty,
                assigned_difficulty=assigned,
                question=question,
                answer=answer,
                root_node=spec.root_node,
                seed=spec.seed,
            )

        return QAResult(
            job_id=spec.job_id,
            passed=True,
            failure_reason=None,
            target_difficulty=spec.difficulty,
            assigned_difficulty=assigned,
            question=question,
            answer=answer,
            root_node=spec.root_node,
            seed=spec.seed,
        )


class PipelineWorker:
    def __init__(self, graph: nx.DiGraph) -> None:
        self.graph = graph
        self.traversal = TraversalWorker(graph)
        self.question = QuestionWorker()
        self.answer = AnswerWorker(graph)
        self.qa = QAWorker()

    def process(self, spec: JobSpec) -> QAResult:
        try:
            subgraph = self.traversal.run(spec)
            question, reason = self.question.run(spec, subgraph)
            if question is None:
                return QAResult(
                    job_id=spec.job_id,
                    passed=False,
                    failure_reason=reason or "no_question",
                    target_difficulty=spec.difficulty,
                    assigned_difficulty="unknown",
                    question=None,
                    answer=None,
                    root_node=spec.root_node,
                    seed=spec.seed,
                )
            answer = self.answer.run(question)
            return self.qa.run(spec, question, answer)
        except Exception as exc:  # pragma: no cover
            return QAResult(
                job_id=spec.job_id,
                passed=False,
                failure_reason=f"exception:{exc.__class__.__name__}",
                target_difficulty=spec.difficulty,
                assigned_difficulty="unknown",
                question=None,
                answer=None,
                root_node=spec.root_node,
                seed=spec.seed,
            )


_PIPELINE_WORKER: Optional[PipelineWorker] = None


def init_worker(graph_path: str, base_seed: int) -> None:
    global _PIPELINE_WORKER
    identity = mp.current_process()._identity
    worker_id = identity[0] if identity else 0
    random.seed(mix_seed(base_seed, "worker", worker_id))
    graph = read_graph(graph_path)
    _PIPELINE_WORKER = PipelineWorker(graph)


def process_job(spec: JobSpec) -> QAResult:
    if _PIPELINE_WORKER is None:
        raise RuntimeError("Worker not initialized")
    return _PIPELINE_WORKER.process(spec)
