from __future__ import annotations

from typing import Any, Dict, List
import json


def export_jsonl(pairs: List[Dict[str, Any]], path: str) -> None:
    """Input: QA pairs and output path. Output: JSONL file on disk."""
    with open(path, "w", encoding="utf-8") as f:
        for pair in pairs:
            record: Dict[str, Any] = {
                "id": pair.get("id"),
                "question": pair.get("question"),
                "difficulty": pair.get("difficulty"),
                "supporting_triples": pair.get("supporting_triples", []),
                "hop_length": pair.get("hop_length"),
                "pattern_type": pair.get("pattern_type"),
            }
            answer = pair.get("answer")
            if isinstance(answer, list):
                record["answers"] = answer
            else:
                record["answer"] = answer
            f.write(json.dumps(record, ensure_ascii=True, sort_keys=True, separators=(",", ":")) + "\n")
