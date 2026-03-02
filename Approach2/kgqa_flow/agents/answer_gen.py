from __future__ import annotations

import networkx as nx

from ..engine.reasoning import GraphReasoner
from ..models import GenerationState


def answer_gen(state: GenerationState, graph: nx.MultiDiGraph, reasoner: GraphReasoner) -> GenerationState:
    answer = reasoner.answer_from_metadata(state.metadata)
    state.answer = answer
    return state
