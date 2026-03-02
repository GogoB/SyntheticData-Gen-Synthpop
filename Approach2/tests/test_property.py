from __future__ import annotations

import networkx as nx
from hypothesis import given, strategies as st

from kgqa_flow.engine import WorkflowEngine
from kgqa_flow.engine.reasoning import GraphReasoner
from kgqa_flow.llm import MockLlmClient
from kgqa_flow.models import GenerationState


def build_graph(triples):
    g = nx.MultiDiGraph()
    for s, r, o in triples:
        g.add_edge(s, o, relation=r)
    return g


def answer_supported(graph, state) -> bool:
    reasoner = GraphReasoner(graph)
    expected = reasoner.answer_from_metadata(state.metadata)
    return expected is not None and expected == state.answer


triple_strategy = st.lists(
    st.tuples(
        st.text(min_size=1, max_size=3),
        st.text(min_size=1, max_size=3),
        st.text(min_size=1, max_size=3),
    ),
    min_size=1,
    max_size=20,
)


@given(triple_strategy)
def test_answers_exist_in_kg(triples):
    graph = build_graph(triples)
    llm = MockLlmClient(seed=0)
    engine = WorkflowEngine(graph=graph, llm_client=llm, max_attempts=2, seed=0)

    for idx in range(3):
        state = GenerationState(metadata={"sample_id": idx})
        final = engine.run(state)
        if final.qa_report and final.qa_report.get("passed"):
            assert answer_supported(graph, final)
