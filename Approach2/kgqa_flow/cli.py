from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Iterable, List, Tuple

import networkx as nx

from .engine import WorkflowEngine
from .llm import MockLlmClient
from .models import GenerationState

Triple = Tuple[str, str, str]


def _load_triples(path: Path) -> List[Triple]:
    triples: List[Triple] = []
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line:
                continue
            record = json.loads(line)
            subject = record.get("subject") or record.get("head") or record.get("s")
            relation = record.get("predicate") or record.get("relation") or record.get("r")
            obj = record.get("object") or record.get("tail") or record.get("o")
            if subject is None or relation is None or obj is None:
                continue
            triples.append((str(subject), str(relation), str(obj)))
    return triples


def _build_graph(triples: Iterable[Triple]) -> nx.MultiDiGraph:
    graph = nx.MultiDiGraph()
    for subject, relation, obj in triples:
        graph.add_edge(subject, obj, relation=relation)
    return graph


def _serialize_triples(triples: List[Triple]) -> List[dict]:
    return [
        {"subject": s, "predicate": r, "object": o}
        for s, r, o in triples
    ]


def run(args: argparse.Namespace) -> None:
    triples = _load_triples(Path(args.triples))
    graph = _build_graph(triples)

    llm = MockLlmClient(seed=args.seed)
    engine = WorkflowEngine(graph=graph, llm_client=llm, max_attempts=args.max_attempts, seed=args.seed)

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    with out_path.open("w", encoding="utf-8") as handle:
        for idx in range(args.n):
            state = GenerationState(metadata={"sample_id": idx, "seed": args.seed})
            final = engine.run(state)
            record = {
                "question": final.question_draft,
                "answer": final.answer,
                "difficulty": final.metadata.get("difficulty"),
                "reasoning_path": final.metadata.get("path_triples"),
                "evidence_triples": _serialize_triples(final.metadata.get("path_triples", [])),
                "qa_report": final.qa_report,
                "attempts_used": final.attempt_count,
            }
            handle.write(json.dumps(record, ensure_ascii=False) + "\n")


def convert(args: argparse.Namespace) -> None:
    from .convert import ttl_to_jsonl

    ttl_to_jsonl(Path(args.ttl), Path(args.out))


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="kgqa_flow")
    sub = parser.add_subparsers(dest="command", required=True)

    run_parser = sub.add_parser("run", help="Generate KG QA pairs")
    run_parser.add_argument("--triples", required=True)
    run_parser.add_argument("--out", required=True)
    run_parser.add_argument("--n", type=int, default=100)
    run_parser.add_argument("--max_attempts", type=int, default=3)
    run_parser.add_argument("--seed", type=int, default=0)

    convert_parser = sub.add_parser("convert", help="Convert Turtle (.ttl) to JSONL triples")
    convert_parser.add_argument("--ttl", required=True)
    convert_parser.add_argument("--out", required=True)
    return parser


def main() -> None:
    parser = build_parser()
    args = parser.parse_args()
    if args.command == "run":
        run(args)
    elif args.command == "convert":
        convert(args)


if __name__ == "__main__":
    main()
