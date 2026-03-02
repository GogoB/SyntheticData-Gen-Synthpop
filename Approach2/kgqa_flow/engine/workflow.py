from __future__ import annotations

import random
from typing import Callable, Dict, Optional

import networkx as nx

from ..agents.answer_gen import answer_gen
from ..agents.quality_assurance import quality_assurance
from ..agents.question_synthesis import question_synthesis
from ..agents.revise_answer import revise_answer
from ..agents.revise_question import revise_question
from ..agents.traversal import traversal
from ..llm import ILlmClient
from ..models import GenerationState
from .reasoning import GraphReasoner


class WorkflowEngine:
    def __init__(
        self,
        graph: nx.MultiDiGraph,
        llm_client: ILlmClient,
        max_attempts: int = 3,
        seed: int = 0,
    ) -> None:
        self.graph = graph
        self.llm_client = llm_client
        self.reasoner = GraphReasoner(graph)
        self.max_attempts = max_attempts
        self.rng = random.Random(seed)

        self.workflow = nx.DiGraph()
        self.workflow.add_nodes_from(
            [
                "traversal",
                "question_synthesis",
                "answer_gen",
                "quality_assurance",
                "revise_question",
                "revise_answer",
            ]
        )
        self.workflow.add_edge("traversal", "question_synthesis")
        self.workflow.add_edge("question_synthesis", "answer_gen")
        self.workflow.add_edge("answer_gen", "quality_assurance")
        self.workflow.add_edge("revise_question", "answer_gen")
        self.workflow.add_edge("revise_answer", "quality_assurance")

        self.node_handlers: Dict[str, Callable[[GenerationState], GenerationState]] = {
            "traversal": lambda state: traversal(state, self.graph, self.rng),
            "question_synthesis": lambda state: question_synthesis(state, self.llm_client),
            "answer_gen": lambda state: answer_gen(state, self.graph, self.reasoner),
            "quality_assurance": lambda state: quality_assurance(state, self.graph, self.reasoner),
            "revise_question": lambda state: revise_question(state, self.llm_client),
            "revise_answer": lambda state: revise_answer(state, self.graph, self.reasoner),
        }

    def run(self, state: GenerationState) -> GenerationState:
        current = "traversal"
        state.metadata.setdefault("trace", [])

        while True:
            state.metadata["trace"].append(current)
            handler = self.node_handlers[current]
            state = handler(state)

            if current == "quality_assurance":
                state.attempt_count += 1
                qa_report = state.qa_report or {}
                passed = bool(qa_report.get("passed"))
                if passed or state.attempt_count >= self.max_attempts:
                    break
                reason = qa_report.get("reason")
                if reason == "fluency":
                    current = "revise_question"
                else:
                    current = "revise_answer"
                continue

            successors = list(self.workflow.successors(current))
            if not successors:
                break
            current = successors[0]

        return state
