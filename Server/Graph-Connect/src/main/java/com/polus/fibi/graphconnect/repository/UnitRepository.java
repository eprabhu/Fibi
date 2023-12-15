package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.Award;
import com.polus.fibi.graphconnect.entity.Proposal;
import com.polus.fibi.graphconnect.entity.Unit;

@Repository
public interface UnitRepository extends Neo4jRepository<Unit, String>{
	
		 
}
