package com.polus.appcoigraph.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.neo4j.repository.query.Query;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Country;
import com.polus.appcoigraph.entity.Person;
import com.polus.appcoigraph.model.Link;

@Repository
public interface PersonRepository extends Neo4jRepository<Person, String>{
	
		 
}
