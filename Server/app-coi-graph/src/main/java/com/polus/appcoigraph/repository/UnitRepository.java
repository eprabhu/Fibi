package com.polus.appcoigraph.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Award;
import com.polus.appcoigraph.entity.Proposal;
import com.polus.appcoigraph.entity.Unit;

@Repository
public interface UnitRepository extends Neo4jRepository<Unit, String>{
	
		 
}
