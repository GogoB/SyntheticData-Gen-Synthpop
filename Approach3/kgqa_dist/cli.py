from __future__ import annotations

import argparse
import logging

from .orchestrator import run_generation


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Distributed KG->QA generator")
    parser.add_argument("--workers", type=int, default=8, help="Number of parallel workers")
    parser.add_argument("--backend", type=str, default="networkx", help="Graph backend (networkx)")
    parser.add_argument(
        "--exec-backend",
        type=str,
        choices=["auto", "ray", "mp"],
        default="auto",
        help="Execution backend (auto, ray, mp)",
    )
    parser.add_argument("--out", type=str, required=True, help="Output jsonl path")
    parser.add_argument("--n", type=int, required=True, help="Number of QA pairs to emit")
    parser.add_argument("--seed", type=int, default=1234, help="Random seed")
    parser.add_argument("--nodes", type=int, default=5000, help="Number of nodes in the KG")
    parser.add_argument("--avg-degree", type=int, default=6, help="Average node out-degree")
    parser.add_argument(
        "--max-attempts-multiplier",
        type=int,
        default=3,
        help="Max attempts = n * multiplier",
    )
    parser.add_argument("--batch-size", type=int, default=None, help="Batch size for scheduling")
    parser.add_argument("--graph-path", type=str, default=None, help="Optional graph cache path")
    parser.add_argument("--log-every", type=int, default=1000, help="Log progress every N outputs")
    parser.add_argument("--keep-graph", action="store_true", help="Keep serialized graph file")
    return parser


def main() -> None:
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    parser = build_parser()
    args = parser.parse_args()
    run_generation(
        n=args.n,
        out_path=args.out,
        workers=args.workers,
        backend=args.backend,
        exec_backend=args.exec_backend,
        seed=args.seed,
        num_nodes=args.nodes,
        avg_degree=args.avg_degree,
        graph_path=args.graph_path,
        max_attempts_multiplier=args.max_attempts_multiplier,
        batch_size=args.batch_size,
        log_every=args.log_every,
        keep_graph=args.keep_graph,
    )


if __name__ == "__main__":
    main()
