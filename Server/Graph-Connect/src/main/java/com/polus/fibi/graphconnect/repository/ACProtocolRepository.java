package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.ACProtocol;

@Repository
public interface ACProtocolRepository extends Neo4jRepository<ACProtocol, String>{
	
}
