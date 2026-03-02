from __future__ import annotations

from ..llm import ILlmClient
from ..models import GenerationState


def revise_question(state: GenerationState, llm_client: ILlmClient) -> GenerationState:
    current = state.question_draft or ""
    state.question_draft = llm_client.revise_question(current, dict(state.metadata))
    return state
