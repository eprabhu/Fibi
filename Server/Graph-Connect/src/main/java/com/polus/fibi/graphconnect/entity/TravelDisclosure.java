package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.data.neo4j.core.schema.Relationship.Direction;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("TravelDisclosure")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TravelDisclosure {
	
	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="travel_number")
	private String travelNumber;
	
	@Property(name="person_id")
	private String personId;
	
	@Property(name="entity_number")
	private String entityNumber;
	
		
}
