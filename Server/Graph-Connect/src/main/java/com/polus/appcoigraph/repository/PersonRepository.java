package com.polus.appcoigraph.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Person;

@Repository
public interface PersonRepository extends Neo4jRepository<Person, String>{
	
		 
}
