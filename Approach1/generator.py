from __future__ import annotations

from typing import Any, Callable, Dict, List, Optional
import json
import re

Triple = Dict[str, Any]
LLMCallable = Callable[[str], str]

_CAMEL_RE = re.compile(r"(?<!^)([A-Z])")
_SPACE_RE = re.compile(r"\s+")

PREDICATE_PROFILES: Dict[str, Dict[str, Any]] = {
    "code": {
        "noun": "code",
        "object": ["What is the code for {s}?", "What is {s}'s code?"],
        "boolean": ["Does {s} have code {o}?", "Is {o} the code for {s}?"],
        "attribute": True,
    },
    "name": {
        "noun": "name",
        "object": ["What is the name of {s}?", "What is {s}'s name?"],
        "boolean": ["Is {s} named {o}?", "Is {o} the name of {s}?"],
        "attribute": True,
    },
    "birthdate": {
        "noun": "birth date",
        "object": ["When was {s} born?", "What is {s}'s birth date?"],
        "boolean": ["Was {s} born on {o}?", "Is {o} the birth date of {s}?"],
        "attribute": True,
    },
    "gender": {
        "noun": "gender",
        "object": ["What is the gender of {s}?", "What gender is {s}?"],
        "list": ["What gender does {s} have?", "What is the gender of {s}?"],
        "boolean": ["Is {s} {o}?", "Is {o} the gender of {s}?"],
        "attribute": True,
    },
    "possibletreatment": {
        "noun": "possible treatment",
        "object": ["What is a possible treatment for {s}?", "Which treatment is possible for {s}?"],
        "list": ["What are possible treatments for {s}?", "Which treatments are possible for {s}?"],
        "count": [
            "How many possible treatments are listed for {s}?",
            "How many treatments are available for {s}?",
        ],
        "boolean": ["Does {s} have possible treatment {o}?", "Is {o} a possible treatment for {s}?"],
    },
    "associatedanatomy": {
        "noun": "associated body part",
        "object": ["Which body part is associated with {s}?", "What body part is associated with {s}?"],
        "list": ["Which body parts are associated with {s}?", "What body parts are associated with {s}?"],
        "count": ["How many body parts are associated with {s}?", "How many associated body parts are listed for {s}?"],
        "boolean": ["Is {s} associated with {o}?", "Is {o} associated with {s}?"],
    },
    "healthcondition": {
        "noun": "condition",
        "object": ["What condition does {s} have?", "Which condition does {s} have?"],
        "list": ["What conditions does {s} have?", "Which conditions does {s} have?"],
        "count": ["How many conditions does {s} have?", "How many conditions are listed for {s}?"],
        "boolean": ["Does {s} have {o}?", "Is {o} a condition of {s}?"],
    },
    "drug": {
        "noun": "drug",
        "object": ["Which drug does {s} take?", "What drug does {s} take?"],
        "list": ["Which drugs does {s} take?", "What drugs does {s} take?"],
        "count": ["How many drugs does {s} take?", "How many drugs are listed for {s}?"],
        "boolean": ["Does {s} take {o}?", "Is {o} a drug taken by {s}?"],
    },
    "signorsymptom": {
        "noun": "symptom",
        "object": ["What symptom does {s} have?", "Which symptom does {s} have?"],
        "list": ["What symptoms does {s} have?", "Which symptoms does {s} have?"],
        "count": ["How many symptoms does {s} have?", "How many symptoms are listed for {s}?"],
        "boolean": ["Does {s} have symptom {o}?", "Is {o} a symptom of {s}?"],
    },
    "activeingredient": {
        "noun": "active ingredient",
        "object": ["What is the active ingredient of {s}?", "Which active ingredient does {s} contain?"],
        "list": ["What are the active ingredients of {s}?", "Which active ingredients does {s} contain?"],
        "count": [
            "How many active ingredients does {s} have?",
            "How many active ingredients are listed for {s}?",
        ],
        "boolean": ["Does {s} have active ingredient {o}?", "Is {o} an active ingredient of {s}?"],
    },
    "mechanismofaction": {
        "noun": "mechanism of action",
        "object": ["What is the mechanism of action of {s}?", "What mechanism of action does {s} have?"],
        "list": ["What are the mechanisms of action of {s}?", "Which mechanisms of action does {s} have?"],
        "count": [
            "How many mechanisms of action are listed for {s}?",
            "How many mechanisms of action does {s} have?",
        ],
        "boolean": ["Does {s} have mechanism of action {o}?", "Is {o} a mechanism of action of {s}?"],
    },
    "similarto": {
        "noun": "similar entity",
        "object": ["What is {s} similar to?", "Which entity is similar to {s}?"],
        "list": ["What is {s} similar to?", "Which entities are similar to {s}?"],
        "count": ["How many entities are similar to {s}?", "How many similar entities does {s} have?"],
        "boolean": ["Is {s} similar to {o}?", "Is {o} similar to {s}?"],
        "subject": ["Which entity is similar to {o}?", "What is similar to {o}?"],
    },
}


def _label(triple: Triple, label_key: str, raw_key: str) -> str:
    value = triple.get(label_key)
    if value is not None and str(value).strip():
        return str(value)
    return str(triple.get(raw_key, ""))

def _clean_text(value: str) -> str:
    text = str(value).strip()
    if text.startswith('"') and text.endswith('"'):
        text = text[1:-1]
    if "^^" in text:
        text = text.split("^^", 1)[0].strip('"')
    text = _SPACE_RE.sub(" ", text).strip()
    return text


def _local_name(text: str) -> str:
    if "://" in text or text.startswith("urn:"):
        if "#" in text:
            return text.rsplit("#", 1)[-1]
        return text.rsplit("/", 1)[-1]
    return text


def _normalize_key(text: str) -> str:
    return re.sub(r"[^a-z0-9]", "", text.lower())


def _humanize_predicate(text: str) -> str:
    raw = _local_name(text)
    raw = raw.replace("_", " ").replace("-", " ")
    raw = _CAMEL_RE.sub(r" \1", raw)
    raw = _SPACE_RE.sub(" ", raw).strip()
    if not raw:
        return raw
    if raw.isupper():
        return raw
    return raw.lower()


def _pluralize(noun_phrase: str) -> str:
    text = noun_phrase.strip()
    if " of " in text:
        head, tail = text.split(" of ", 1)
        return f"{_pluralize(head)} of {tail}"
    if text.endswith(("s", "x", "z", "ch", "sh")):
        return text + "es"
    if text.endswith("y") and len(text) > 1 and text[-2] not in "aeiou":
        return text[:-1] + "ies"
    return text + "s"


def _predicate_profile(triple: Triple) -> Dict[str, Any]:
    raw = _label(triple, "p_label", "p")
    key = _normalize_key(_local_name(raw))
    profile = dict(PREDICATE_PROFILES.get(key, {}))
    if "noun" not in profile:
        profile["noun"] = _humanize_predicate(raw) or "relation"
    profile["key"] = key
    return profile


def _default_templates(noun: str, attribute_like: bool) -> Dict[str, List[str]]:
    noun_plural = _pluralize(noun)
    if attribute_like:
        return {
            "object": [f"What is the {noun} of {{s}}?", f"What is {{s}}'s {noun}?"],
            "list": [f"What {noun} does {{s}} have?", f"What is the {noun} of {{s}}?"],
            "count": [f"How many {noun_plural} does {{s}} have?", f"What is the number of {noun_plural} for {{s}}?"],
            "subject": [f"Which entity has {noun} {{o}}?", f"Which entity has {{o}} as its {noun}?"],
            "boolean": [f"Does {{s}} have {noun} {{o}}?", f"Is {{o}} the {noun} of {{s}}?"],
        }
    return {
        "object": [f"Which {noun} does {{s}} have?", f"What {noun} does {{s}} have?"],
        "list": [f"Which {noun_plural} does {{s}} have?", f"What {noun_plural} does {{s}} have?"],
        "count": [f"How many {noun_plural} does {{s}} have?", f"What is the number of {noun_plural} for {{s}}?"],
        "subject": [f"Which entity has {noun} {{o}}?", f"Which entity is linked to {{o}} via {noun}?"],
        "boolean": [f"Does {{s}} have {noun} {{o}}?", f"Is {{o}} a {noun} of {{s}}?"],
    }


def _format_question(text: str) -> str:
    text = _SPACE_RE.sub(" ", text).strip()
    if not text.endswith("?"):
        text += "?"
    return text


def _build_question_variants(triples: List[Triple], ask_for: str) -> Dict[str, Any]:
    main = triples[0]
    s = _clean_text(_label(main, "s_label", "s"))
    p_profile = _predicate_profile(main)
    o = _clean_text(_label(main, "o_label", "o"))

    noun = p_profile.get("noun", "relation")
    attribute_like = bool(p_profile.get("attribute"))
    defaults = _default_templates(noun, attribute_like)

    templates = p_profile.get(ask_for) or defaults.get(ask_for) or defaults["object"]
    if isinstance(templates, str):
        templates = [templates]
    templates = list(templates)

    variants = [_format_question(t.format(s=s, o=o)) for t in templates]

    supporting = [main]
    if ask_for in ("list", "count"):
        objects = []
        for t in triples:
            if str(t.get("s")) == str(main.get("s")) and str(t.get("p")) == str(main.get("p")):
                objects.append(_clean_text(_label(t, "o_label", "o")))
        deduped = []
        seen = set()
        for obj in objects:
            if obj in seen:
                continue
            seen.add(obj)
            deduped.append(obj)
        supporting = [
            t for t in triples if str(t.get("s")) == str(main.get("s")) and str(t.get("p")) == str(main.get("p"))
        ]
        answer: Any = deduped if ask_for == "list" else len(deduped)
    elif ask_for == "subject":
        answer = s
    elif ask_for == "boolean":
        answer = True
    else:
        answer = o

    return {
        "variants": variants,
        "draft_answer": answer,
        "supporting_triples": supporting,
    }


def triples_to_text(triples: List[Triple]) -> str:
    """Input: triples list. Output: human-readable text block."""
    lines = []
    for t in triples:
        s = _label(t, "s_label", "s")
        p = _label(t, "p_label", "p")
        o = _label(t, "o_label", "o")
        lines.append(f"- {s} --{p}--> {o}")
    return "\n".join(lines)


def build_prompt(triples: List[Triple], ask_for: str, difficulty: str) -> str:
    """Input: triples, ask_for, difficulty. Output: prompt string."""
    triples_text = triples_to_text(triples)
    return (
        "You are given knowledge graph triples with labels. "
        "Write a single natural language question and its answer. "
        f"ask_for: {ask_for}. difficulty: {difficulty}. "
        "Return JSON with keys 'question' and 'answer'.\n\n"
        f"Triples:\n{triples_text}\n"
    )


def _try_parse_json(text: str) -> Optional[Dict[str, Any]]:
    try:
        data = json.loads(text)
        if isinstance(data, dict) and "question" in data and "answer" in data:
            return data
    except Exception:
        return None
    return None


def _heuristic_generate(triples: List[Triple], ask_for: str) -> Dict[str, Any]:
    """Input: triples, ask_for. Output: dict with question, draft_answer, supporting_triples."""
    payload = _build_question_variants(triples, ask_for)
    question = payload["variants"][0]
    return {
        "question": question,
        "draft_answer": payload["draft_answer"],
        "supporting_triples": payload["supporting_triples"],
    }


def generate_paraphrases(
    triples: List[Triple], ask_for: str, n: int, exclude: Optional[str] = None
) -> List[str]:
    """Input: triples, ask_for, n. Output: list of paraphrased questions."""
    if n <= 0:
        return []
    payload = _build_question_variants(triples, ask_for)
    variants = payload["variants"]
    unique: List[str] = []
    seen = set()
    if exclude:
        seen.add(exclude.strip())
    for q in variants:
        if q in seen:
            continue
        seen.add(q)
        unique.append(q)
        if len(unique) >= n:
            return unique
    base = variants[0] if variants else ""
    while len(unique) < n and base:
        unique.append(base)
    return unique[:n]


def generate_question(
    triples: List[Triple],
    ask_for: str,
    difficulty: str,
    llm: Optional[LLMCallable] = None,
) -> Dict[str, Any]:
    """Input: triples, ask_for, difficulty. Output: dict with question, draft_answer, supporting_triples."""
    if not triples:
        raise ValueError("No triples provided.")

    prompt = build_prompt(triples, ask_for, difficulty)
    if llm is not None:
        raw = llm(prompt)
        parsed = _try_parse_json(raw)
        if parsed:
            return {
                "question": str(parsed["question"]),
                "draft_answer": parsed["answer"],
                "supporting_triples": triples,
            }

    return _heuristic_generate(triples, ask_for)
