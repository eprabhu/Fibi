package com.polus.appcoigraph.entity;

import org.springframework.data.neo4j.core.schema.Id;

import java.util.List;

import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.data.neo4j.core.schema.Relationship.Direction;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("EntityTest")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EntityTest {

	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="entity_number")
	private String entityNumber;
	
	@Property(name="name")
	private String entityName;
	
	@Property(name="country_name")
	private String countryName;
	
	@Property(name="country_code")
	private String countryCode;
		
	@Property(name="status")
	private String status;
	
	@Property(name="type")
	private String type;
}
