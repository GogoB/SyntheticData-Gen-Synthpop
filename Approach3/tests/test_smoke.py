import json

from kgqa_dist.orchestrator import run_generation


def test_smoke(tmp_path):
    out_path = tmp_path / "qa.jsonl"
    run_generation(
        n=20,
        out_path=str(out_path),
        workers=2,
        exec_backend="mp",
        seed=123,
        num_nodes=200,
        avg_degree=4,
        max_attempts_multiplier=4,
    )

    lines = out_path.read_text(encoding="utf-8").splitlines()
    records = [json.loads(line) for line in lines if line.strip()]
    assert len(records) >= 20
