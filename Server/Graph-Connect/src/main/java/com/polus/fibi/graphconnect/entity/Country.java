package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Country")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Country {
	
	@Id
	@Property(name="id")
	private String id;
	
	@Property(name="country_code")
	private String countryCode;
	
	@Property(name="country_name")
	private String countryName;
	
	@Property(name="currency_code")
	private String currency;
	
	//@Relationship(type = "COUNTRY_OWNED", direction = Direction.INCOMING)
	//private List<Entity> OwnedEntity;
	
//	private LinkProperty linkProperties;
}
