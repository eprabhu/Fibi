package com.polus.fibi.graphconnect.coi.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.data.neo4j.core.schema.Relationship.Direction;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("COIDisclosure")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Disclosure {
	
	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="disclosure_number")
	private String disclosureNumber;
	
	@Property(name="person_id")
	private String personId;

		
}
