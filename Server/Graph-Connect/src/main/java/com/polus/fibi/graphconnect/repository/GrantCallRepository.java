package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.GrantCall;

@Repository
public interface GrantCallRepository extends Neo4jRepository<GrantCall, String>{
	
}
