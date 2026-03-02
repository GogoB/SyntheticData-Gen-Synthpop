from __future__ import annotations

import json
from pathlib import Path


def ttl_to_jsonl(ttl_path: Path, out_path: Path) -> None:
    try:
        from rdflib import Graph  # type: ignore
    except ImportError as exc:  # pragma: no cover
        raise RuntimeError("rdflib is required for Turtle conversion. Install with `pip install rdflib`.") from exc

    graph = Graph()
    graph.parse(str(ttl_path), format="turtle")

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8") as handle:
        for subject, predicate, obj in graph:
            handle.write(
                json.dumps(
                    {
                        "subject": str(subject),
                        "predicate": str(predicate),
                        "object": str(obj),
                    },
                    ensure_ascii=False,
                )
                + "\n"
            )
