from __future__ import annotations

import re
from typing import Dict

import networkx as nx

from ..engine.reasoning import GraphReasoner
from ..models import GenerationState


def _is_fluent(question: str) -> bool:
    if not question:
        return False
    if len(question.strip()) < 8:
        return False
    if not question.strip().endswith("?"):
        return False
    alpha = sum(ch.isalpha() for ch in question)
    if alpha < 3:
        return False
    if re.search(r"\?\?\?", question):
        return False
    return True


def quality_assurance(state: GenerationState, graph: nx.MultiDiGraph, reasoner: GraphReasoner) -> GenerationState:
    question = state.question_draft or ""
    report: Dict[str, object] = {}

    if not _is_fluent(question):
        report.update(
            {
                "passed": False,
                "reason": "fluency",
                "issues": ["Question not fluent"],
            }
        )
        state.qa_report = report
        return state

    expected = reasoner.answer_from_metadata(state.metadata)
    if expected is None or state.answer != expected:
        report.update(
            {
                "passed": False,
                "reason": "factual_mismatch",
                "issues": ["Answer does not match KG"],
                "expected": expected,
            }
        )
        state.qa_report = report
        return state

    report.update({"passed": True, "reason": None})
    state.qa_report = report
    return state
