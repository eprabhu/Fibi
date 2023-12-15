package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.IRBProtocol;

@Repository
public interface IRBProtocolRepository extends Neo4jRepository<IRBProtocol, String>{
	
}
