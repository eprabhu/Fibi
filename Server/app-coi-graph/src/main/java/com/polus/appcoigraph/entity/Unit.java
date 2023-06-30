package com.polus.appcoigraph.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Unit")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Unit {
	
	@Id
	@Property(name="id")
	private String id;
	
	@Property(name="unit_number")
	private String unitNumber;
	
	@Property(name="unit_name")
	private String unitName;
	
		
	//@Relationship(type = "COUNTRY_OWNED", direction = Direction.INCOMING)
	//private List<Entity> OwnedEntity;
	
//	private LinkProperty linkProperties;
}
