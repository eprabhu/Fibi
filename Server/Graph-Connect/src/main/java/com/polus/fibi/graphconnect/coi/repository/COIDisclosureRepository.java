package com.polus.fibi.graphconnect.coi.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.coi.entity.Award;
import com.polus.fibi.graphconnect.coi.entity.Disclosure;
import com.polus.fibi.graphconnect.coi.entity.Proposal;

@Repository
public interface COIDisclosureRepository extends Neo4jRepository<Disclosure, String>{
	
		 
}
