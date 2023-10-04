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

@Node("Person")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Person {
	@Id	
	@Property(name="id")
	private String id;

	@Property(name="person_id")
	private String personId;

	@Property(name="full_name")
	private String fullName;

	@Property(name="home_unit")
	private String homeUnit;

	@Property(name="country_code")
	private String countryCode;

	@Property(name="country_name")
	private String countryName;

	@Property(name="status")
	private String status;

	@Property(name="user_name")
	private String userName;

	@Property(name="email_address")
	private String emailAddress;

	@Property(name="designation")
	private String designation;

}
