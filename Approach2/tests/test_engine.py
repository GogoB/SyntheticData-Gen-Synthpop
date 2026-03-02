from __future__ import annotations

import networkx as nx

from kgqa_flow.engine import WorkflowEngine
from kgqa_flow.llm import MockLlmClient
from kgqa_flow.models import GenerationState


class EmptyFirstLlm(MockLlmClient):
    def __init__(self) -> None:
        super().__init__(seed=0)
        self.calls = 0

    def generate_question(self, context):
        self.calls += 1
        if self.calls == 1:
            return ""
        return super().generate_question(context)


class SimpleLlm(MockLlmClient):
    def generate_question(self, context):
        return "What is the relation of A?"

    def revise_question(self, question, context):
        return "What is the relation of A?"


def build_graph():
    g = nx.MultiDiGraph()
    g.add_edge("A", "B", relation="relation")
    return g


def test_retry_on_fluency():
    graph = build_graph()
    llm = EmptyFirstLlm()
    engine = WorkflowEngine(graph=graph, llm_client=llm, max_attempts=2, seed=0)

    state = GenerationState(metadata={"difficulty": "easy"})
    final = engine.run(state)

    assert final.qa_report["passed"] is True
    assert final.attempt_count == 2
    assert "revise_question" in final.metadata.get("trace", [])


def test_retry_on_factual_mismatch():
    graph = build_graph()
    llm = SimpleLlm(seed=0)
    engine = WorkflowEngine(graph=graph, llm_client=llm, max_attempts=2, seed=0)

    def wrong_answer(state):
        state.answer = "C"
        return state

    engine.node_handlers["answer_gen"] = wrong_answer

    state = GenerationState(metadata={"difficulty": "easy"})
    final = engine.run(state)

    assert final.qa_report["passed"] is True
    assert final.attempt_count == 2
    assert "revise_answer" in final.metadata.get("trace", [])
