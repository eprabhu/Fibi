package com.polus.fibi.graphconnect.coi.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.coi.entity.Award;
import com.polus.fibi.graphconnect.coi.entity.Proposal;
import com.polus.fibi.graphconnect.coi.entity.Sponsor;

@Repository
public interface SponsorRepository extends Neo4jRepository<Sponsor, String>{
	
		 
}
