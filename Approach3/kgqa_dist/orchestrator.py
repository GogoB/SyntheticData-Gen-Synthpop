from __future__ import annotations

import json
import logging
import os
import random
import tempfile
import time
from collections import Counter
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

import networkx as nx

from .models import JobSpec, QAResult, mix_seed, node_name, write_graph, read_graph
from .workers import init_worker, process_job, PipelineWorker


DIFFICULTY_DEPTH = {
    "easy": 1,
    "medium": 2,
    "hard": 2,
}


def build_graph(num_nodes: int, avg_degree: int, seed: int) -> nx.DiGraph:
    rng = random.Random(seed)
    graph = nx.DiGraph()
    categories = ["Person", "Place", "Org", "Event", "Concept"]
    rels = ["related_to", "part_of", "located_in", "connected_to", "influences"]

    for node_id in range(num_nodes):
        graph.add_node(
            node_id,
            name=node_name(node_id),
            category=categories[node_id % len(categories)],
        )

    for node_id in range(num_nodes):
        degree = max(1, int(avg_degree + rng.randint(-1, 1)))
        for _ in range(degree):
            dst = rng.randrange(num_nodes)
            if dst == node_id:
                continue
            rel = rng.choice(rels)
            graph.add_edge(node_id, dst, rel=rel)

    return graph


def ensure_graph(graph_path: str, num_nodes: int, avg_degree: int, seed: int) -> None:
    if os.path.exists(graph_path) and os.path.getsize(graph_path) > 0:
        return
    graph = build_graph(num_nodes=num_nodes, avg_degree=avg_degree, seed=seed)
    write_graph(graph, graph_path)


class AdaptiveSampler:
    def __init__(
        self,
        weights: Optional[Dict[str, float]] = None,
        min_weight: float = 0.1,
        max_weight: float = 5.0,
        high_fail_rate: float = 0.6,
        low_fail_rate: float = 0.2,
        down_factor: float = 0.7,
        up_factor: float = 1.1,
        min_attempts: int = 20,
        check_interval: int = 50,
    ) -> None:
        self.weights = weights or {"easy": 1.0, "medium": 1.0, "hard": 1.0}
        self.min_weight = min_weight
        self.max_weight = max_weight
        self.high_fail_rate = high_fail_rate
        self.low_fail_rate = low_fail_rate
        self.down_factor = down_factor
        self.up_factor = up_factor
        self.min_attempts = min_attempts
        self.check_interval = check_interval
        self._last_checked: Dict[str, int] = {k: 0 for k in self.weights}

    def sample(self, rng: random.Random) -> str:
        total = sum(self.weights.values())
        pick = rng.random() * total
        cumulative = 0.0
        for diff, weight in self.weights.items():
            cumulative += weight
            if pick <= cumulative:
                return diff
        return "hard"

    def update(self, per_diff: Dict[str, Dict[str, int]]) -> List[Tuple[str, float, float]]:
        changes: List[Tuple[str, float, float]] = []
        for diff, stats in per_diff.items():
            attempts = stats.get("attempts", 0)
            fails = stats.get("fails", 0)
            if attempts < self.min_attempts:
                continue
            if attempts - self._last_checked.get(diff, 0) < self.check_interval:
                continue
            fail_rate = fails / max(1, attempts)
            old = self.weights.get(diff, 1.0)
            new = old
            if fail_rate > self.high_fail_rate:
                new = max(self.min_weight, old * self.down_factor)
            elif fail_rate < self.low_fail_rate:
                new = min(self.max_weight, old * self.up_factor)
            if new != old:
                self.weights[diff] = new
                changes.append((diff, old, new))
            self._last_checked[diff] = attempts
        return changes


@dataclass
class Metrics:
    attempts: int = 0
    passes: int = 0
    failures: int = 0
    failure_reasons: Counter = field(default_factory=Counter)
    per_diff: Dict[str, Dict[str, int]] = field(
        default_factory=lambda: {
            "easy": {"attempts": 0, "passes": 0, "fails": 0},
            "medium": {"attempts": 0, "passes": 0, "fails": 0},
            "hard": {"attempts": 0, "passes": 0, "fails": 0},
        }
    )

    def record(self, result: QAResult) -> None:
        self.attempts += 1
        bucket = result.target_difficulty
        if bucket not in self.per_diff:
            self.per_diff[bucket] = {"attempts": 0, "passes": 0, "fails": 0}
        self.per_diff[bucket]["attempts"] += 1
        if result.passed:
            self.passes += 1
            self.per_diff[bucket]["passes"] += 1
        else:
            self.failures += 1
            self.per_diff[bucket]["fails"] += 1
            reason = result.failure_reason or "unknown"
            self.failure_reasons[reason] += 1

    def snapshot(self) -> Dict[str, object]:
        pass_rate = self.passes / max(1, self.attempts)
        return {
            "attempts": self.attempts,
            "passes": self.passes,
            "failures": self.failures,
            "pass_rate": pass_rate,
            "failure_reasons": dict(self.failure_reasons),
            "per_diff": self.per_diff,
        }


def _resolve_exec_backend(exec_backend: str) -> str:
    if exec_backend != "auto":
        return exec_backend
    try:
        import ray  # noqa: F401

        return "ray"
    except Exception:
        return "mp"


def _build_job_specs(
    start_id: int,
    count: int,
    sampler: AdaptiveSampler,
    rng: random.Random,
    seed: int,
    num_nodes: int,
) -> List[JobSpec]:
    specs: List[JobSpec] = []
    for offset in range(count):
        job_id = start_id + offset
        difficulty = sampler.sample(rng)
        root_node = rng.randrange(num_nodes)
        job_seed = mix_seed(seed, job_id, difficulty, root_node)
        max_depth = DIFFICULTY_DEPTH.get(difficulty, 1)
        specs.append(
            JobSpec(
                job_id=job_id,
                seed=job_seed,
                difficulty=difficulty,
                root_node=root_node,
                max_depth=max_depth,
            )
        )
    return specs


def run_generation(
    n: int,
    out_path: str,
    workers: int = 8,
    backend: str = "networkx",
    exec_backend: str = "auto",
    seed: int = 1234,
    num_nodes: int = 5000,
    avg_degree: int = 6,
    graph_path: Optional[str] = None,
    max_attempts_multiplier: int = 3,
    batch_size: Optional[int] = None,
    log_every: int = 1000,
    keep_graph: bool = False,
) -> Dict[str, object]:
    if backend != "networkx":
        raise ValueError(f"Unsupported backend: {backend}")

    logger = logging.getLogger(__name__)

    if graph_path is None:
        tmp = tempfile.NamedTemporaryFile(delete=False, suffix=".gpickle")
        graph_path = tmp.name
        tmp.close()

    ensure_graph(graph_path, num_nodes=num_nodes, avg_degree=avg_degree, seed=seed)

    exec_backend = _resolve_exec_backend(exec_backend)
    batch_size = batch_size or max(1, workers * 4)
    max_attempts = max(n, n * max_attempts_multiplier)

    os.makedirs(os.path.dirname(out_path) or ".", exist_ok=True)

    rng = random.Random(seed)
    sampler = AdaptiveSampler()
    metrics = Metrics()
    output_count = 0
    job_id = 0
    start_time = time.monotonic()
    last_log = start_time

    ray_ctx = None
    ray_workers = None
    executor = None

    try:
        if exec_backend == "ray":
            import ray

            ray_ctx = ray.init(ignore_reinit_error=True, include_dashboard=False, num_cpus=workers)

            @ray.remote
            class RayPipelineWorker:
                def __init__(self, graph_path: str, base_seed: int, worker_id: int) -> None:
                    random.seed(mix_seed(base_seed, "worker", worker_id))
                    graph = read_graph(graph_path)
                    self.worker = PipelineWorker(graph)

                def process(self, spec: JobSpec) -> QAResult:
                    return self.worker.process(spec)

            ray_workers = [RayPipelineWorker.remote(graph_path, seed, i) for i in range(workers)]
        else:
            from concurrent.futures import ProcessPoolExecutor

            executor = ProcessPoolExecutor(
                max_workers=workers,
                initializer=init_worker,
                initargs=(graph_path, seed),
            )

        with open(out_path, "w", encoding="utf-8") as out_f:
            while output_count < n and job_id < max_attempts:
                remaining = max_attempts - job_id
                batch = min(batch_size, remaining)
                specs = _build_job_specs(job_id, batch, sampler, rng, seed, num_nodes)

                if exec_backend == "ray":
                    futures = [
                        ray_workers[i % workers].process.remote(spec)
                        for i, spec in enumerate(specs)
                    ]
                    results = list(ray.get(futures))
                else:
                    chunk = max(1, batch // max(1, workers * 2))
                    results = list(executor.map(process_job, specs, chunksize=chunk))

                for result in results:
                    metrics.record(result)
                    if result.passed and output_count < n:
                        record = result.to_record()
                        out_f.write(json.dumps(record, sort_keys=True) + "\n")
                        output_count += 1

                out_f.flush()
                job_id += batch

                changes = sampler.update(metrics.per_diff)
                if changes:
                    for diff, old, new in changes:
                        logger.info("adjusted difficulty weight %s: %.2f -> %.2f", diff, old, new)

                now = time.monotonic()
                if output_count % log_every == 0 or (now - last_log) > 5:
                    elapsed = now - start_time
                    items_per_sec = output_count / max(1e-6, elapsed)
                    snapshot = metrics.snapshot()
                    logger.info(
                        "progress outputs=%d attempts=%d pass_rate=%.3f items_sec=%.2f failures=%s",
                        output_count,
                        snapshot["attempts"],
                        snapshot["pass_rate"],
                        items_per_sec,
                        snapshot["failure_reasons"],
                    )
                    last_log = now

        elapsed = time.monotonic() - start_time
        summary = metrics.snapshot()
        summary["outputs"] = output_count
        summary["items_per_sec"] = output_count / max(1e-6, elapsed)
        logger.info("finished outputs=%d attempts=%d", output_count, metrics.attempts)
        return summary
    finally:
        if executor is not None:
            executor.shutdown(wait=True)
        if ray_ctx is not None:
            import ray

            ray.shutdown()
        if graph_path and (not keep_graph):
            try:
                os.remove(graph_path)
            except OSError:
                pass
