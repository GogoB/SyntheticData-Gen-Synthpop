from __future__ import annotations

import json
import random
import re
import urllib.error
import urllib.request
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


def _normalize_label(value: str) -> str:
    raw = value.strip().strip("<>").strip()
    if not raw:
        return raw
    if "://" in raw or raw.startswith("urn:"):
        raw = re.split(r"[#/]", raw)[-1]
    if ":" in raw and " " not in raw:
        raw = raw.split(":")[-1]
    raw = raw.replace("_", " ").replace("-", " ")
    return " ".join(raw.split())


def _sanitize_context(context: Dict) -> Dict:
    cleaned = dict(context)

    if isinstance(cleaned.get("path_triples"), list):
        sanitized = []
        for item in cleaned["path_triples"]:
            if isinstance(item, (list, tuple)) and len(item) >= 3:
                subject = _normalize_label(str(item[0]))
                relation = _normalize_label(str(item[1]))
                obj = _normalize_label(str(item[2]))
                sanitized.append((subject, relation, obj))
            else:
                sanitized.append(item)
        cleaned["path_triples"] = sanitized

    if isinstance(cleaned.get("candidate_entities"), list):
        cleaned["candidate_entities"] = [
            _normalize_label(str(entity)) for entity in cleaned["candidate_entities"]
        ]

    for key in (
        "disambiguation_subject",
        "disambiguation_relation",
        "disambiguation_followup_relation",
        "disambiguation_end",
    ):
        if key in cleaned and cleaned[key] is not None:
            cleaned[key] = _normalize_label(str(cleaned[key]))

    return cleaned


class LmStudioClient:
    def __init__(
        self,
        base_url: str = "http://localhost:1234/v1",
        model: str = "meta-llama-3.1-8b-instruct",
        temperature: float = 0.2,
        timeout: int = 60,
    ) -> None:
        self.base_url = base_url.rstrip("/")
        self.model = model
        self.temperature = temperature
        self.timeout = timeout

    def _chat(self, messages: List[Dict[str, str]]) -> str:
        payload = {
            "model": self.model,
            "messages": messages,
            "temperature": self.temperature,
        }
        data = json.dumps(payload).encode("utf-8")
        req = urllib.request.Request(
            f"{self.base_url}/chat/completions",
            data=data,
            headers={"Content-Type": "application/json"},
        )
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as resp:
                body = resp.read()
        except urllib.error.URLError as exc:
            raise RuntimeError(f"LM Studio request failed: {exc}") from exc
        response = json.loads(body.decode("utf-8"))
        content = response["choices"][0]["message"]["content"]
        return content.strip()

    def generate_question(self, context: Dict) -> str:
        system = (
            "You generate high-quality questions from knowledge graph triples.\n"
            "The question must require understanding of relationships between entities.\n"
            "Rules:\n"
            "1. Do NOT ask trivial questions such as:\n"
            "   - What is the name of X?\n"
            "   - What is the type of X?\n"
            "   - What is the relationship between X and Y?\n"
            "2. Do NOT ask questions where the answer appears verbatim in the question.\n"
            "3. Prefer questions that use one or more relations; use multi-hop paths when present.\n"
            "4. Convert URIs into readable entity names; avoid http/https or raw identifiers.\n"
            "5. The question must sound natural and be medically meaningful when possible.\n"
            "6. Only ask questions whose answer can be derived from the provided triples.\n"
            "Question styles to prefer: causal, treatment, anatomy, symptom→condition, "
            "condition→treatment, mechanism of action.\n"
            "Examples:\n"
            "Triples: (Asthma, associatedAnatomy, Lungs)\n"
            "Good: Which organ is affected by asthma?\n"
            "Bad: What is the name of the lungs?\n"
            "Triples: (Type2Diabetes, possibleTreatment, Metformin)\n"
            "Good: What medication is commonly used to treat type 2 diabetes?\n"
            "Think briefly before answering. Return only the final question text."
        )
        cleaned_context = _sanitize_context(context)
        messages = [
            {"role": "system", "content": system},
            {"role": "user", "content": json.dumps(cleaned_context, ensure_ascii=False)},
        ]
        question = self._chat(messages)
        if question and not question.endswith("?"):
            question += "?"
        return question or "What is the missing answer?"

    def revise_question(self, question: str, context: Dict) -> str:
        system = (
            "You are improving a knowledge-graph question.\n"
            "Rewrite the question to make it natural, non-trivial, and medically meaningful.\n"
            "Rules:\n"
            "1. Do not produce identity questions like 'What is the name of X?'\n"
            "2. Avoid mentioning URIs or raw identifiers.\n"
            "3. Keep the answer unchanged.\n"
            "4. Ensure the question requires understanding relationships in the context.\n"
            "Return only the revised question text."
        )
        cleaned_context = _sanitize_context(context)
        payload = {"question": question, "context": cleaned_context}
        messages = [
            {"role": "system", "content": system},
            {"role": "user", "content": json.dumps(payload, ensure_ascii=False)},
        ]
        revised = self._chat(messages)
        if revised and not revised.endswith("?"):
            revised += "?"
        return revised or "What is the correct answer?"
