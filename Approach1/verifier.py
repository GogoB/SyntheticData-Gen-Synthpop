from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple

Triple = Dict[str, Any]


def _normalize_answer(value: Any) -> Any:
    if isinstance(value, list):
        return sorted(str(v).strip().lower() for v in value)
    return str(value).strip().lower()


def _answers_match(draft: Any, canonical: Any) -> bool:
    return _normalize_answer(draft) == _normalize_answer(canonical)


def _sparql_term(value: Any) -> str:
    if value is None:
        return ""
    text = str(value)
    if text.startswith("?"):
        return text
    if text.startswith("<") and text.endswith(">"):
        return text
    if text.startswith('"'):
        return text
    if text.startswith("http://") or text.startswith("https://") or text.startswith("urn:"):
        return f"<{text}>"
    return f"<{text}>"


def build_sparql_query(triples: List[Triple], ask_for: str) -> Tuple[str, Optional[str]]:
    """Input: triples, ask_for. Output: SPARQL query and target variable name."""
    if not triples:
        raise ValueError("No triples supplied.")

    target_id = None
    if ask_for == "subject":
        target_id = triples[0].get("s")
    elif ask_for in ("object", "list", "count"):
        target_id = triples[0].get("o")

    patterns = []
    for t in triples:
        s_val = t.get("s")
        p_val = t.get("p")
        o_val = t.get("o")
        s_term = "?target" if target_id is not None and str(s_val) == str(target_id) else _sparql_term(s_val)
        o_term = "?target" if target_id is not None and str(o_val) == str(target_id) else _sparql_term(o_val)
        p_term = _sparql_term(p_val)
        patterns.append(f"{s_term} {p_term} {o_term} .")

    where_block = "\n".join(patterns)
    if ask_for == "boolean":
        query = f"ASK WHERE {{\n{where_block}\n}}"
    elif ask_for == "count":
        query = f"SELECT (COUNT(DISTINCT ?target) AS ?count) WHERE {{\n{where_block}\n}}"
    else:
        query = f"SELECT DISTINCT ?target WHERE {{\n{where_block}\n}}"
    return query, "?target" if ask_for != "boolean" else None


def _cypher_rel(rel: Any) -> str:
    text = str(rel).replace("`", "")
    return f"`{text}`"


def build_cypher_query(
    connector: Any, triples: List[Triple], ask_for: str
) -> Tuple[str, Dict[str, Any]]:
    """Input: connector, triples, ask_for. Output: Cypher query and parameters."""
    if not triples:
        raise ValueError("No triples supplied.")

    target_id = None
    if ask_for == "subject":
        target_id = triples[0].get("s")
    elif ask_for in ("object", "list", "count"):
        target_id = triples[0].get("o")

    node_vars: Dict[str, str] = {}

    def node_var(node_id: Any) -> str:
        if target_id is not None and str(node_id) == str(target_id):
            return "target"
        key = str(node_id)
        if key not in node_vars:
            node_vars[key] = f"n{len(node_vars)}"
        return node_vars[key]

    patterns = []
    for t in triples:
        s_var = node_var(t.get("s"))
        o_var = node_var(t.get("o"))
        rel = _cypher_rel(t.get("p"))
        patterns.append(f"({s_var})-[:{rel}]->({o_var})")

    params: Dict[str, Any] = {}
    constraints = []
    for node_id, var in node_vars.items():
        if var == "target":
            continue
        param_name = f"node_{len(params)}"
        if connector.use_internal_id:
            constraints.append(f"id({var}) = ${param_name}")
            params[param_name] = int(node_id)
        else:
            constraints.append(f"{var}.{connector.id_field} = ${param_name}")
            params[param_name] = node_id

    match_clause = "MATCH " + ", ".join(patterns)
    where_clause = "WHERE " + " AND ".join(constraints) if constraints else ""

    if ask_for == "boolean":
        query = f"{match_clause} {where_clause} RETURN count(*) > 0 AS exists"
    elif ask_for == "count":
        query = f"{match_clause} {where_clause} RETURN count(DISTINCT target) AS count"
    else:
        query = f"{match_clause} {where_clause} RETURN DISTINCT target AS target"

    return query, params


def verify_answer(
    connector: Any,
    triples: List[Triple],
    ask_for: str,
    draft_answer: Any,
) -> Dict[str, Any]:
    """Input: connector, triples, ask_for, draft_answer. Output: canonical_answer and verification_result."""
    if connector.kind == "sparql":
        query, _ = build_sparql_query(triples, ask_for)
        raw = connector.run_query(query)
        if ask_for == "boolean":
            canonical = bool(raw)
        elif ask_for == "count":
            canonical = 0
            for row in raw:
                canonical = int(row[0])
                break
        else:
            values = []
            for row in raw:
                value = str(row[0])
                if hasattr(connector, "label_for"):
                    value = connector.label_for(value)
                values.append(value)
            canonical = values[0] if ask_for in ("subject", "object") and values else values
    elif connector.kind == "neo4j":
        query, params = build_cypher_query(connector, triples, ask_for)
        raw = connector.run_query(query, params)
        if ask_for == "boolean":
            canonical = False
            for row in raw:
                canonical = bool(row["exists"])
                break
        elif ask_for == "count":
            canonical = 0
            for row in raw:
                canonical = int(row["count"])
                break
        else:
            values = []
            for row in raw:
                node = row["target"]
                if hasattr(connector, "_node_label"):
                    values.append(connector._node_label(node))
                else:
                    values.append(str(node))
            canonical = values[0] if ask_for in ("subject", "object") and values else values
    else:
        raise ValueError(f"Unsupported connector kind: {connector.kind}")

    verification = _answers_match(draft_answer, canonical)
    return {
        "canonical_answer": canonical,
        "verification_result": verification,
        "query": query,
    }
