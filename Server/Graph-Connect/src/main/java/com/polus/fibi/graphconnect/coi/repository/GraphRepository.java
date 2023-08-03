package com.polus.fibi.graphconnect.coi.repository;

import java.util.List;

import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.neo4j.repository.query.Query;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.coi.entity.GraphNode;

@Repository
public interface GraphRepository extends Neo4jRepository<GraphNode, String> {

    @Query("MATCH (n:Entity) RETURN n")
    List<GraphNode> getAllNodes();

    @Query("MATCH ()-[r]->() RETURN r")
    List<Relationship> getAllRelationships();
}