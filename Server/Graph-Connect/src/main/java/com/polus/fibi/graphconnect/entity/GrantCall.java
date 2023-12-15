package com.polus.fibi.graphconnect.entity;

import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Node("GrantCall")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GrantCall {
	
	@Id	
	@Property(name="id")
	private String id;
	
	@Property(name="grant_call_id")
	private String grantCallId;

	@Property(name="grant_call_name")
	private String grantCallName;
	
	@Property(name="description")
	private String description;
	
	@Property(name="grant_theme")
	private String grantTheme;

	@Property(name="max_budget")
	private String maxBudget;

	@Property(name="status")
	private String status;

	@Property(name="grant_type")
	private String grantType;

	@Property(name="funding_source")
	private String fundingSource;

	@Property(name="sponsor")
	private String sponsor;

	@Property(name="home_unit_name")
	private String homeUnitName;

	@Property(name="home_unit_number")
	private String homeUnitNumber;

	@Property(name="opening_date")
	private String openingDate;

	@Property(name="closing_date")
	private String closingDate;

}
