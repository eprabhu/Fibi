package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("ACProtocol")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ACProtocol {
	@Id	
	@Property(name="id")
	private String id;

	@Property(name="protocol_id")
	private String protocolId;

	@Property(name="protocol_number")
	private String protocolNumber;

	@Property(name="title")
	private String title;

	@Property(name="purpose_of_study")
	private String purposeOfStudy;

	@Property(name="fda_application_number")
	private String fdaApplicationNumber;

}
