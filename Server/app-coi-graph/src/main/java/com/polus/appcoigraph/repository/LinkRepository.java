package com.polus.appcoigraph.repository;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.neo4j.repository.query.Query;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Country;
import com.polus.appcoigraph.model.Link;

@Repository
public interface LinkRepository extends Neo4jRepository<Link, Long>{
	
		 
	 @Query("MATCH (c:Country)<-[r]->(e:Entity) WHERE c.Country_code = $countryCode"
		 		+ " RETURN r;")
		  Iterable<Link> getLink(String countryCode);
		 
}
