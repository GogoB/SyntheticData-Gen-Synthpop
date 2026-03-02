import json

from kgqa_dist.orchestrator import run_generation


def _load(path):
    lines = path.read_text(encoding="utf-8").splitlines()
    data = [json.loads(line) for line in lines if line.strip()]
    return sorted(data, key=lambda x: x["id"])


def test_determinism(tmp_path):
    out1 = tmp_path / "qa1.jsonl"
    out2 = tmp_path / "qa2.jsonl"

    run_generation(
        n=30,
        out_path=str(out1),
        workers=2,
        exec_backend="mp",
        seed=42,
        num_nodes=300,
        avg_degree=4,
        max_attempts_multiplier=4,
    )

    run_generation(
        n=30,
        out_path=str(out2),
        workers=2,
        exec_backend="mp",
        seed=42,
        num_nodes=300,
        avg_degree=4,
        max_attempts_multiplier=4,
    )

    data1 = _load(out1)
    data2 = _load(out2)
    assert data1 == data2
