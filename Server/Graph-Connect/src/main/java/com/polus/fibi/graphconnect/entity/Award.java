package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("Award")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Award {
	
	@Id	
	@Property(name="id")
	private String id;

	@Property(name="award_id")
	private String awardId;

	@Property(name="award_number")
	private String awardNumber;

	@Property(name="account_number")
	private String accountNumber;

	@Property(name="anticipated_total_amount")
	private String anticipatedTotalAmount;

	@Property(name="award_status")
	private String awardStatus;
	
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
		
	@Property(name="lead_unit")
	private String leadUnitName;
	
	@Property(name="award_type")
	private String awardType;	
		
	@Property(name="pi_name")
	private String piName;	

	@Property(name="grant_header_id")
	private String grantHeaderId;	

}
