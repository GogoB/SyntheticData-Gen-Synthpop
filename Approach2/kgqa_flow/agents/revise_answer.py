from __future__ import annotations

import networkx as nx

from ..engine.reasoning import GraphReasoner
from ..models import GenerationState


def revise_answer(state: GenerationState, graph: nx.MultiDiGraph, reasoner: GraphReasoner) -> GenerationState:
    state.answer = reasoner.answer_from_metadata(state.metadata)
    return state
