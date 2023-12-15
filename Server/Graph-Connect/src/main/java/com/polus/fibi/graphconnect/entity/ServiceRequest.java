package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("ServiceRequest")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ServiceRequest {
	
	@Id	
	@Property(name="id")
	private String id;

	@Property(name="sr_header_id")
	private String srHeaderId;

	@Property(name="subject")
	private String subject;

	@Property(name="status")
	private String status;

	@Property(name="type")
	private String type;

	@Property(name="unit_number")
	private String unitNumber;

	@Property(name="unit_name")
	private String unitName;

	@Property(name="reporter_person_id")
	private String reporterPersonId;

	@Property(name="reported_by")
	private String reportedBy;

	@Property(name="create_timestamp")
	private String createTimestamp;

	@Property(name="module_code")
	private String moduleCode;

	@Property(name="module_item_key")
	private String moduleItemKey;

}
