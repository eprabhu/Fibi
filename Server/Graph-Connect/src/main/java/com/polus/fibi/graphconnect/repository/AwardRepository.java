package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.Award;
import com.polus.fibi.graphconnect.entity.Proposal;

@Repository
public interface AwardRepository extends Neo4jRepository<Award, String>{
	
		 
}
