package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("InstituteProposal")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InstituteProposal {

	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="proposal_id")
	private String proposalId;
	
	@Property(name="proposal_number")
	private String proposalNumber;
	
	@Property(name="title")
	private String title;
	
	@Property(name="home_unit_number")
	private String homeUnitNumber;

	@Property(name="start_date")
	private String startDate;

	@Property(name="end_date")
	private String endDate;

	@Property(name="unit_name")
	private String unitName;

	@Property(name="sponsor_proposal_number")
	private String sponsorProposalNumber;

	@Property(name="activity_type")
	private String activityType;

	@Property(name="prime_sponsor_code")
	private String primeSponsorCode;

	@Property(name="prime_sponsor_acronym")
	private String primeSponsorAcronym;

	@Property(name="prime_sponsor")
	private String primeSponsor;

	@Property(name="investigator")
	private String investigator;

	@Property(name="status")
	private String status;

	@Property(name="proposal_type")
	private String proposalType;

	@Property(name="sponsor_code")
	private String sponsorCode;

	@Property(name="acronym")
	private String acronym;

	@Property(name="sponsor")
	private String sponsor;
		
}
