from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional, Sequence

Triple = Dict[str, Any]

DEFAULT_LABEL_PREDICATES = [
    "http://www.w3.org/2000/01/rdf-schema#label",
    "http://www.w3.org/2004/02/skos/core#prefLabel",
    "http://schema.org/name",
]


def _label_from_iri(value: str) -> str:
    """Derive a readable label from an IRI or identifier."""
    if not value:
        return ""
    raw = value
    if raw.startswith("<") and raw.endswith(">"):
        raw = raw[1:-1]
    if raw.startswith('"') and raw.endswith('"'):
        return raw.strip('"')
    if "#" in raw:
        frag = raw.rsplit("#", 1)[-1]
    else:
        frag = raw.rsplit("/", 1)[-1]
    return frag.replace("_", " ")


class KGConnector(ABC):
    """Abstract KG connector interface."""

    @abstractmethod
    def get_neighbors(
        self,
        node: Any,
        rel_whitelist: Optional[Sequence[str]] = None,
        limit: Optional[int] = None,
    ) -> List[Triple]:
        """Input: node id/IRI, optional rel whitelist, optional limit. Output: list of triple dicts with labels."""

    @abstractmethod
    def run_query(self, query: str) -> Any:
        """Input: query string. Output: backend-specific result object."""

    @abstractmethod
    def canonicalize_entity(self, entity_id: Any) -> str:
        """Input: entity identifier. Output: canonical string for comparisons."""


class SPARQLConnector(KGConnector):
    """SPARQL connector backed by rdflib."""

    def __init__(
        self,
        endpoint_url: Optional[str] = None,
        rdf_file: Optional[str] = None,
        rdf_format: Optional[str] = None,
        graph: Any = None,
        label_predicates: Optional[Sequence[str]] = None,
    ) -> None:
        self.kind = "sparql"
        if graph is not None:
            self.graph = graph
        elif endpoint_url:
            from rdflib import Graph
            from rdflib.plugins.stores.sparqlstore import SPARQLStore

            store = SPARQLStore(endpoint_url)
            self.graph = Graph(store=store)
        elif rdf_file:
            from rdflib import Graph

            self.graph = Graph()
            self.graph.parse(rdf_file, format=rdf_format)
        else:
            raise ValueError("Provide endpoint_url, rdf_file, or graph.")

        self.label_predicates = list(label_predicates or DEFAULT_LABEL_PREDICATES)
        self._label_cache: Dict[str, str] = {}

    def canonicalize_entity(self, entity_id: Any) -> str:
        """Input: entity id/IRI. Output: canonical string representation."""
        if entity_id is None:
            return ""
        return str(entity_id)

    def _format_iri(self, value: str) -> str:
        """Format a value as a SPARQL term."""
        if not value:
            return value
        if value.startswith("?"):
            return value
        if value.startswith("<") and value.endswith(">"):
            return value
        if value.startswith('"'):
            return value
        if value.startswith("http://") or value.startswith("https://") or value.startswith("urn:"):
            return f"<{value}>"
        return f"<{value}>"

    def label_for(self, entity_iri: str) -> str:
        """Input: entity IRI. Output: human-readable label string."""
        if not entity_iri:
            return ""
        cached = self._label_cache.get(entity_iri)
        if cached:
            return cached
        if entity_iri.startswith('"') and entity_iri.endswith('"'):
            label = _label_from_iri(entity_iri)
            self._label_cache[entity_iri] = label
            return label

        iri = self._format_iri(entity_iri)
        values = " ".join(f"<{p}>" for p in self.label_predicates)
        query = (
            "SELECT ?label WHERE { "
            f"{iri} ?lp ?label . VALUES ?lp {{ {values} }} "
            "} LIMIT 1"
        )
        label = None
        try:
            rows = self.graph.query(query)
            for row in rows:
                label = str(row[0])
                break
        except Exception:
            label = None

        if not label:
            label = _label_from_iri(entity_iri)

        self._label_cache[entity_iri] = label
        return label

    def run_query(self, query: str) -> Any:
        """Input: SPARQL string. Output: rdflib Result."""
        return self.graph.query(query)

    def get_neighbors(
        self,
        node: Any,
        rel_whitelist: Optional[Sequence[str]] = None,
        limit: Optional[int] = None,
    ) -> List[Triple]:
        """Input: node id/IRI. Output: labeled triples for incoming/outgoing edges."""
        node_id = self.canonicalize_entity(node)
        node_iri = self._format_iri(node_id)
        rel_filter = ""
        if rel_whitelist:
            rel_values = " ".join(self._format_iri(self.canonicalize_entity(r)) for r in rel_whitelist)
            rel_filter = f"VALUES ?p {{ {rel_values} }}"
        limit_clause = f"LIMIT {int(limit)}" if limit else ""

        query = (
            "SELECT ?s ?p ?o WHERE { "
            f"{{ {node_iri} ?p ?o . BIND({node_iri} AS ?s) }} "
            "UNION "
            f"{{ ?s ?p {node_iri} . BIND({node_iri} AS ?o) }} "
            f"{rel_filter} "
            "} "
            f"{limit_clause}"
        )

        from rdflib.term import Literal as RdflibLiteral

        def term_value(term: Any) -> str:
            if isinstance(term, RdflibLiteral):
                return term.n3()
            return str(term)

        def term_label(term: Any) -> str:
            if isinstance(term, RdflibLiteral):
                return str(term)
            return self.label_for(str(term))

        triples: List[Triple] = []
        seen = set()
        for row in self.run_query(query):
            s_term = row[0]
            p_term = row[1]
            o_term = row[2]
            s = str(s_term)
            p = str(p_term)
            o = term_value(o_term)
            key = (s, p, o)
            if key in seen:
                continue
            seen.add(key)
            triples.append(
                {
                    "s": s,
                    "p": p,
                    "o": o,
                    "s_label": term_label(s_term),
                    "p_label": term_label(p_term),
                    "o_label": term_label(o_term),
                }
            )
        return triples


class Neo4jConnector(KGConnector):
    """Neo4j connector backed by the official neo4j driver."""

    def __init__(
        self,
        uri: str,
        user: str,
        password: str,
        database: Optional[str] = None,
        id_field: str = "id",
        label_field: str = "name",
        use_internal_id: bool = False,
    ) -> None:
        from neo4j import GraphDatabase

        self.kind = "neo4j"
        self.driver = GraphDatabase.driver(uri, auth=(user, password))
        self.database = database
        self.id_field = id_field
        self.label_field = label_field
        self.use_internal_id = use_internal_id

    def close(self) -> None:
        """Input: none. Output: closes driver connection."""
        self.driver.close()

    def canonicalize_entity(self, entity_id: Any) -> str:
        """Input: entity id. Output: canonical string representation."""
        if entity_id is None:
            return ""
        return str(entity_id)

    def _node_id_value(self, node: Any) -> Any:
        """Input: Neo4j node. Output: stable identifier value."""
        if self.use_internal_id:
            return getattr(node, "id", None)
        if self.id_field in node:
            return node[self.id_field]
        return getattr(node, "id", None)

    def _node_label(self, node: Any) -> str:
        """Input: Neo4j node. Output: display label string."""
        if node is None:
            return ""
        if self.label_field in node:
            return str(node[self.label_field])
        for key in ("name", "label", "title"):
            if key in node:
                return str(node[key])
        fallback = self._node_id_value(node)
        return "" if fallback is None else str(fallback)

    def run_query(self, query: str, params: Optional[Dict[str, Any]] = None) -> Any:
        """Input: Cypher string and params. Output: neo4j.Result."""
        with self.driver.session(database=self.database) as session:
            return session.run(query, params or {})

    def get_neighbors(
        self,
        node: Any,
        rel_whitelist: Optional[Sequence[str]] = None,
        limit: Optional[int] = None,
    ) -> List[Triple]:
        """Input: node id. Output: labeled triples for incoming/outgoing edges."""
        params: Dict[str, Any] = {}
        if self.use_internal_id:
            match_clause = "MATCH (n) WHERE id(n) = $node_id"
            params["node_id"] = int(node)
        else:
            match_clause = f"MATCH (n) WHERE n.{self.id_field} = $node_id"
            params["node_id"] = node

        rel_filter = ""
        if rel_whitelist:
            params["rels"] = list(rel_whitelist)
            rel_filter = "AND type(r) IN $rels"

        limit_clause = ""
        if limit:
            params["limit"] = int(limit)
            limit_clause = "LIMIT $limit"

        query = (
            f"{match_clause} "
            "MATCH (n)-[r]-(m) "
            f"WHERE 1 = 1 {rel_filter} "
            "RETURN n, r, m "
            f"{limit_clause}"
        )

        triples: List[Triple] = []
        seen = set()
        for record in self.run_query(query, params):
            n = record["n"]
            r = record["r"]
            m = record["m"]
            s_id = self._node_id_value(n)
            o_id = self._node_id_value(m)
            p_id = r.type
            key = (s_id, p_id, o_id)
            if key in seen:
                continue
            seen.add(key)
            triples.append(
                {
                    "s": s_id,
                    "p": p_id,
                    "o": o_id,
                    "s_label": self._node_label(n),
                    "p_label": str(p_id),
                    "o_label": self._node_label(m),
                }
            )
        return triples
