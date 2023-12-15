package com.polus.fibi.graphconnect.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.ServiceRequest;

@Repository
public interface ServiceRequestRepository extends Neo4jRepository<ServiceRequest, String>{
	
}
