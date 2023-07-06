package com.polus.appcoigraph.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.EntityTest;

@Repository
public interface EntityRepositoryTest extends Neo4jRepository<EntityTest, String>{
	
}
