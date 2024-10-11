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

@Node("Entity")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class COIEntity {

	@Id	
	@Property(name="id")
	private String id;

	@Property(name="entity_id")
	private String entityId;

	@Property(name="entity_number")
	private String entityNumber;

	@Property(name="name")
	private String entityName;

	@Property(name="country_name")
	private String countryName;

	@Property(name="country_code")
	private String countryCode;
	
	@Property(name="status_type")
	private String statusType;

	@Property(name="document_status_type")
	private String documentStatusType;

	@Property(name="ownership_type")
	private String ownershipType;

	@Property(name="website_address")
	private String websiteAddress;

	@Relationship(type = "COUNTRY_OWNED", direction = Direction.OUTGOING)
	private Country countryOwned;

}
