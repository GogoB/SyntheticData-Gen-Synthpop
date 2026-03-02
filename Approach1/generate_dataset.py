from __future__ import annotations

import argparse
import random
import uuid
from typing import Any, Dict, List

from kg_connector import Neo4jConnector, SPARQLConnector
from sampler import SubgraphSampler
from generator import generate_question
from verifier import verify_answer
from filters import apply_filters, paraphrase_questions
from exporter import export_jsonl


def _parse_list(value: str) -> List[str]:
    return [v.strip() for v in value.split(",") if v.strip()]


def build_connector(args: argparse.Namespace) -> Any:
    """Input: parsed args. Output: initialized KG connector."""
    if args.backend == "sparql":
        return SPARQLConnector(
            endpoint_url=args.sparql_endpoint,
            rdf_file=args.rdf_file,
            rdf_format=args.rdf_format,
        )
    if args.backend == "neo4j":
        return Neo4jConnector(
            uri=args.neo4j_uri,
            user=args.neo4j_user,
            password=args.neo4j_pass,
            database=args.neo4j_db,
            id_field=args.neo4j_id_field,
            label_field=args.neo4j_label_field,
            use_internal_id=args.neo4j_internal_id,
        )
    raise ValueError(f"Unsupported backend: {args.backend}")


def main() -> None:
    """Input: CLI args. Output: writes qa.jsonl to disk."""
    parser = argparse.ArgumentParser(description="Generate QA pairs from a KG.")
    parser.add_argument("--backend", choices=["sparql", "neo4j"], required=True)

    parser.add_argument("--sparql-endpoint", default=None)
    parser.add_argument("--rdf-file", default=None)
    parser.add_argument("--rdf-format", default=None)

    parser.add_argument("--neo4j-uri", default=None)
    parser.add_argument("--neo4j-user", default=None)
    parser.add_argument("--neo4j-pass", default=None)
    parser.add_argument("--neo4j-db", default=None)
    parser.add_argument("--neo4j-id-field", default="id")
    parser.add_argument("--neo4j-label-field", default="name")
    parser.add_argument("--neo4j-internal-id", action="store_true")

    parser.add_argument("--seeds", required=True, help="Comma-separated seed IDs")
    parser.add_argument("--strategy", default="path", help="path,star,join or comma-separated list")
    parser.add_argument("--depth", type=int, default=2)
    parser.add_argument("--max-paths", type=int, default=1)
    parser.add_argument("--max-edges", type=int, default=3)
    parser.add_argument("--ask-for", default="subject,object,boolean,count,list")
    parser.add_argument("--paraphrases", type=int, default=0)
    parser.add_argument("--output", default="qa.jsonl")
    parser.add_argument("--min-per-relation", type=int, default=None)
    parser.add_argument("--seed", type=int, default=13)

    args = parser.parse_args()

    rng = random.Random(args.seed)
    connector = build_connector(args)
    sampler = SubgraphSampler(connector, rng=rng)

    seeds = _parse_list(args.seeds)
    strategies = _parse_list(args.strategy)
    ask_for_options = _parse_list(args.ask_for)

    pairs: List[Dict[str, Any]] = []
    for idx, seed in enumerate(seeds):
        strategy = strategies[idx % len(strategies)]
        if strategy == "path":
            sample = sampler.sample_path(seed, depth=args.depth, max_paths=args.max_paths)
        elif strategy == "star":
            sample = sampler.sample_star(seed, max_edges=args.max_edges)
        elif strategy == "join":
            sample = sampler.sample_join(seed, max_depth=args.depth)
        else:
            raise ValueError(f"Unsupported strategy: {strategy}")

        if not sample.triples:
            continue

        ask_for = rng.choice(ask_for_options)
        qa = generate_question(sample.triples, ask_for=ask_for, difficulty="medium")
        verification = verify_answer(connector, qa["supporting_triples"], ask_for, qa["draft_answer"])

        answer = verification["canonical_answer"]
        pair = {
            "id": str(uuid.uuid4()),
            "question": qa["question"],
            "answer": answer,
            "difficulty": None,
            "supporting_triples": qa["supporting_triples"],
            "hop_length": sample.hop_length,
            "pattern_type": sample.pattern_type,
            "ask_for": ask_for,
            "verification_result": verification["verification_result"],
        }
        pairs.append(pair)

    filtered = apply_filters(pairs, min_per_relation=args.min_per_relation)
    final_pairs = paraphrase_questions(filtered, n=args.paraphrases)
    export_jsonl(final_pairs, args.output)

    if hasattr(connector, "close"):
        connector.close()


if __name__ == "__main__":
    main()
