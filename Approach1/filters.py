from __future__ import annotations

from typing import Any, Dict, List, Optional
import uuid


def normalize_answer(value: Any) -> Any:
    if isinstance(value, list):
        return sorted(str(v).strip().lower() for v in value)
    return str(value).strip().lower()


def remove_duplicates(pairs: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Input: list of QA pairs. Output: list with duplicates removed."""
    seen = set()
    out = []
    for pair in pairs:
        key = (pair.get("question"), str(normalize_answer(pair.get("answer"))))
        if key in seen:
            continue
        seen.add(key)
        out.append(pair)
    return out


def filter_verified(pairs: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Input: list of QA pairs. Output: list with only verified pairs."""
    return [p for p in pairs if p.get("verification_result", True)]


def classify_difficulty(pair: Dict[str, Any]) -> str:
    """Input: QA pair. Output: difficulty label."""
    hop = int(pair.get("hop_length", 1))
    ask_for = pair.get("ask_for", "")
    pattern = pair.get("pattern_type", "")
    triples = pair.get("supporting_triples", []) or []
    num_triples = len(triples)
    answer = pair.get("answers", pair.get("answer"))
    cardinality = len(answer) if isinstance(answer, list) else (1 if answer not in (None, "") else 0)

    score = 0
    score += max(0, hop - 1)
    if pattern == "join":
        score += 2
    elif pattern == "path" and hop >= 2:
        score += 1
    if num_triples >= 3:
        score += 1
    if ask_for in ("count", "list"):
        score += 1
    if cardinality > 1:
        score += 1
    if cardinality == 0:
        score += 1

    if score <= 1:
        return "easy"
    if score <= 3:
        return "medium"
    return "hard"


def enforce_relation_coverage(
    pairs: List[Dict[str, Any]],
    min_per_relation: int = 1,
) -> List[Dict[str, Any]]:
    """Input: QA pairs. Output: pairs reordered to satisfy relation coverage."""
    coverage: Dict[str, int] = {}
    selected: List[Dict[str, Any]] = []

    for pair in pairs:
        predicates = {t.get("p") for t in pair.get("supporting_triples", []) if t.get("p") is not None}
        if not predicates:
            continue
        needs = False
        for pred in predicates:
            if coverage.get(pred, 0) < min_per_relation:
                needs = True
                break
        if needs:
            selected.append(pair)
            for pred in predicates:
                coverage[pred] = coverage.get(pred, 0) + 1

    remaining = [p for p in pairs if p not in selected]
    return selected + remaining


def apply_filters(
    pairs: List[Dict[str, Any]],
    min_per_relation: Optional[int] = None,
) -> List[Dict[str, Any]]:
    """Input: QA pairs. Output: filtered QA pairs."""
    filtered = filter_verified(pairs)
    for pair in filtered:
        if not pair.get("difficulty"):
            pair["difficulty"] = classify_difficulty(pair)
    filtered = remove_duplicates(filtered)
    if min_per_relation is not None:
        filtered = enforce_relation_coverage(filtered, min_per_relation=min_per_relation)
    return filtered


def _templated_paraphrases(pair: Dict[str, Any], n: int) -> List[str]:
    """Input: QA pair, n. Output: list of paraphrase strings."""
    triples = pair.get("supporting_triples", []) or []
    ask_for = pair.get("ask_for")
    if not triples or not ask_for:
        return []
    try:
        from generator import generate_paraphrases

        return generate_paraphrases(triples, ask_for, n, exclude=pair.get("question"))
    except Exception:
        return []


def paraphrase_questions(
    pairs: List[Dict[str, Any]],
    n: int,
    paraphraser: Optional[Any] = None,
) -> List[Dict[str, Any]]:
    """Input: QA pairs, n. Output: list including n paraphrases per pair."""
    if n <= 0:
        return pairs

    out: List[Dict[str, Any]] = []
    for pair in pairs:
        out.append(pair)
        if paraphraser:
            paras = paraphraser(pair, n)
        else:
            paras = _templated_paraphrases(pair, n)
        for para in paras:
            new_pair = dict(pair)
            new_pair["id"] = str(uuid.uuid4())
            new_pair["question"] = para
            out.append(new_pair)
    return out
