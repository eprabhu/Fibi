package com.polus.appcoigraph.repository;

import java.util.List;
import java.util.Map;

import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.neo4j.repository.query.Query;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Country;

@Repository
public interface CountryRepository extends Neo4jRepository<Country, String>{
	 @Query("MATCH (c:Country)<-[r:BELONGS_TO]->(e:Entity) WHERE c.country_code = $countryCode"
	 		+ " RETURN c,r,e;")
	 List<Map<String, Object>>  findAllCustom(String countryCode);
	 
	 
		 
}
