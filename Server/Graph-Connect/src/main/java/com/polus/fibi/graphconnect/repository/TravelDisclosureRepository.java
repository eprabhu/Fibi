package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.Award;
import com.polus.fibi.graphconnect.entity.Proposal;
import com.polus.fibi.graphconnect.entity.TravelDisclosure;

@Repository
public interface TravelDisclosureRepository extends Neo4jRepository<TravelDisclosure, String>{
	
		 
}
