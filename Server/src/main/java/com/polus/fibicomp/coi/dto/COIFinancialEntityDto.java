package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;
import java.util.Date;

public class COIFinancialEntityDto {

	private Integer coiFinancialEntityId;

	private Integer entityVersionNumber;

	private String coiEntityName;

	private Date involvementStartDate;

	private Timestamp createTimestamp;

	private Timestamp lastUpdatedOn;

	private Integer noOfDisclosures;

	private String isActive;

	private Integer noOfProposals;

	private Integer noOfAwards;

	private Date involvementEndDate;

	private String coiEntityType;

	private String coiEntityCountry;

	private String coiEntityEmail;

	public Integer getCoiFinancialEntityId() {
		return coiFinancialEntityId;
	}

	public void setCoiFinancialEntityId(Integer coiFinancialEntityId) {
		this.coiFinancialEntityId = coiFinancialEntityId;
	}

	public Integer getEntityVersionNumber() {
		return entityVersionNumber;
	}

	public void setEntityVersionNumber(Integer entityVersionNumber) {
		this.entityVersionNumber = entityVersionNumber;
	}

	public String getCoiEntityName() {
		return coiEntityName;
	}

	public void setCoiEntityName(String coiEntityName) {
		this.coiEntityName = coiEntityName;
	}

	public Date getInvolvementStartDate() {
		return involvementStartDate;
	}

	public void setInvolvementStartDate(Date involvementStartDate) {
		this.involvementStartDate = involvementStartDate;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Timestamp getLastUpdatedOn() {
		return lastUpdatedOn;
	}

	public void setLastUpdatedOn(Timestamp lastUpdatedOn) {
		this.lastUpdatedOn = lastUpdatedOn;
	}

	public Integer getNoOfDisclosures() {
		return noOfDisclosures;
	}

	public void setNoOfDisclosures(Integer noOfDisclosures) {
		this.noOfDisclosures = noOfDisclosures;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public Integer getNoOfProposals() {
		return noOfProposals;
	}

	public void setNoOfProposals(Integer noOfProposals) {
		this.noOfProposals = noOfProposals;
	}

	public Integer getNoOfAwards() {
		return noOfAwards;
	}

	public void setNoOfAwards(Integer noOfAwards) {
		this.noOfAwards = noOfAwards;
	}

	public Date getInvolvementEndDate() {
		return involvementEndDate;
	}

	public void setInvolvementEndDate(Date involvementEndDate) {
		this.involvementEndDate = involvementEndDate;
	}

	public String getCoiEntityType() {
		return coiEntityType;
	}

	public void setCoiEntityType(String coiEntityType) {
		this.coiEntityType = coiEntityType;
	}

	public String getCoiEntityCountry() {
		return coiEntityCountry;
	}

	public void setCoiEntityCountry(String coiEntityCountry) {
		this.coiEntityCountry = coiEntityCountry;
	}

	public String getCoiEntityEmail() {
		return coiEntityEmail;
	}

	public void setCoiEntityEmail(String coiEntityEmail) {
		this.coiEntityEmail = coiEntityEmail;
	}

}
