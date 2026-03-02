from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple

try:
    from pydantic import BaseModel, Field, ConfigDict  # type: ignore
    _HAS_CONFIG_DICT = True
except Exception:  # pragma: no cover - pydantic v1 fallback
    from pydantic import BaseModel, Field  # type: ignore
    _HAS_CONFIG_DICT = False


class GenerationState(BaseModel):
    subgraph: List[Tuple[str, str, str]] = Field(default_factory=list)
    question_draft: Optional[str] = None
    answer: Optional[str] = None
    metadata: Dict[str, Any] = Field(default_factory=dict)
    qa_report: Optional[Dict[str, Any]] = None
    attempt_count: int = 0

    if _HAS_CONFIG_DICT:
        model_config = ConfigDict(extra="allow")
    else:  # pragma: no cover - pydantic v1
        class Config:
            extra = "allow"

    def to_dict(self) -> Dict[str, Any]:
        if hasattr(self, "model_dump"):
            return self.model_dump()  # type: ignore[attr-defined]
        return self.dict()
