package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Agreement")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Agreement {
	
	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="agreement_request_id")
	private String agreementRequestId;

	@Property(name="title")
	private String title;
	
	@Property(name="remarks")
	private String remarks;
	
	@Property(name="requestor_name")
	private String requestorName;
		
	@Property(name="submit_user")
	private String submitUser;
	
	@Property(name="agreement_sequence_status")
	private String agreementSequenceStatus;

	@Property(name="unit_number")
	private String unitNumber;

	@Property(name="unit_name")
	private String unitName;

	@Property(name="sponsor_name")
	private String sponsorName;

	@Property(name="principal_person_full_name")
	private String principalPersonFullName;

	@Property(name="agreement_status")
	private String agreementStatus;

}
