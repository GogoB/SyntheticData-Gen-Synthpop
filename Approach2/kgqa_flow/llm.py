from __future__ import annotations

import random
from typing import Dict, List, Protocol


class ILlmClient(Protocol):
    def generate_question(self, context: Dict) -> str:
        ...

    def revise_question(self, question: str, context: Dict) -> str:
        ...


class MockLlmClient:
    def __init__(self, seed: int = 0) -> None:
        self._rand = random.Random(seed)

    def _pick(self, options: List[str]) -> str:
        return self._rand.choice(options)

    def generate_question(self, context: Dict) -> str:
        question_type = context.get("question_type", "path")
        difficulty = context.get("difficulty", "easy")
        path = context.get("path_triples", [])

        if question_type == "disambiguation":
            subject = context.get("disambiguation_subject")
            rel1 = context.get("disambiguation_relation")
            rel2 = context.get("disambiguation_followup_relation")
            end = context.get("disambiguation_end")
            candidates = context.get("candidate_entities", [])
            intro = self._pick([
                "Which of these",
                "Out of these options, which one",
                "Which option here",
            ])
            middle = self._pick([
                "is connected to",
                "links to",
                "is related to",
            ])
            tail = self._pick([
                "and also",
                "and then",
                "and further",
            ])
            candidate_str = ", ".join(candidates)
            return (
                f"{intro} {middle} {subject} via {rel1} {tail} "
                f"connects to {end} via {rel2}: {candidate_str}?"
            )

        if not path:
            return "What is the missing answer?"

        if len(path) == 1:
            subject, rel, _ = path[0]
            phr = self._pick([
                "What is the",
                "Which entity is the",
                "Name the",
            ])
            return f"{phr} {rel} of {subject}?"

        relations = [trip[1] for trip in path]
        subject = path[0][0]
        chain = " then ".join(relations)
        if difficulty == "hard" and len(relations) >= 3:
            opener = self._pick([
                "Following the chain",
                "Trace the path",
                "Walk the links",
            ])
            return f"{opener} {chain} starting from {subject}. What do you reach?"

        opener = self._pick([
            "Starting from",
            "From",
            "Beginning at",
        ])
        return f"{opener} {subject}, if you follow {chain}, what do you get?"

    def revise_question(self, question: str, context: Dict) -> str:
        prompt = self._pick([
            "Rephrase this clearly:",
            "Rewrite for clarity:",
            "Improve the fluency of:",
        ])
        cleaned = question.strip()
        if cleaned and not cleaned.endswith("?"):
            cleaned += "?"
        if not cleaned:
            cleaned = "What is the correct answer?"
        return f"{prompt} {cleaned}"
