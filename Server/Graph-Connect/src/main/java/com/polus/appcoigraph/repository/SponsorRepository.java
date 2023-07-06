package com.polus.appcoigraph.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Award;
import com.polus.appcoigraph.entity.Proposal;
import com.polus.appcoigraph.entity.Sponsor;

@Repository
public interface SponsorRepository extends Neo4jRepository<Sponsor, String>{
	
		 
}
