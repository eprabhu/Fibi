package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Proposal")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Proposal {
	
	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="proposal_id")
	private String proposalId;

	@Property(name="lead_unit_number")
	private String leadUnitNumber;

	@Property(name="proposal_status")
	private String proposalStatus;
	
	@Property(name="title")
	private String title;
	
	@Property(name="start_date")
	private String startDate;
	
	@Property(name="end_date")
	private String endDate;
		
	@Property(name="sponsor_code")
	private String sponsorCode;
	
	@Property(name="sponsor_name")
	private String sponsorName;
	
	@Property(name="prime_sponsor_code")
	private String primeSponsorCode;
	
	@Property(name="prime_sponsor_name")
	private String primeSponsorName;
	
	@Property(name="unit_number")
	private String unitNumber;
		
	@Property(name="lead_unit_name")
	private String leadUnitName;
	
	@Property(name="type_of_funding_agency")
	private String typeOfFunding;	
		
	@Property(name="pi_name")
	private String piName;

	@Property(name="grant_header_id")
	private String grantHeaderId;

	@Property(name="ip_number")
	private String ipNumber;

}
