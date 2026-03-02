from __future__ import annotations

from ..llm import ILlmClient
from ..models import GenerationState


def question_synthesis(state: GenerationState, llm_client: ILlmClient) -> GenerationState:
    context = dict(state.metadata)
    question = llm_client.generate_question(context)
    state.question_draft = question
    return state
