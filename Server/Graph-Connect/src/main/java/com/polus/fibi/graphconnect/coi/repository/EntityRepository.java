package com.polus.fibi.graphconnect.coi.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.coi.entity.COIEntity;

@Repository
public interface EntityRepository extends Neo4jRepository<COIEntity, String>{
	
}
