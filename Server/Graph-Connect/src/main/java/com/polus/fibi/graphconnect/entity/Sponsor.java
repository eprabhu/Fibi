package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.data.neo4j.core.schema.Relationship.Direction;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Sponsor")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Sponsor {
	
	@Id
	@Property(name="id")
	private String id;
	
	@Property(name="sponsor_code")
	private String sponsorCode;
	
	@Property(name="sponsor_name")
	private String sponsorName;
	
		
	//@Relationship(type = "COUNTRY_OWNED", direction = Direction.INCOMING)
	//private List<Entity> OwnedEntity;
	
//	private LinkProperty linkProperties;
}
