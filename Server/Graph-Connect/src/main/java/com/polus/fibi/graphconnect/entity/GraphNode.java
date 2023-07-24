package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;

import java.util.List;

import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Node")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GraphNode {
	@Id	
	@Property(name="entity_number")
	private String entityNumber;
	
	@Property(name="name")
	private String entityName;
	
	@Property(name="Country_name")
	private String countryName;
	
	private String status;
	
	private String type;
    
}
