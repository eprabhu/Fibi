package com.polus.fibicomp.manpowerintegration.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "RISE_ERROR_ALLOCATIONS")
public class RiseErrorAllocations implements Serializable  {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "RESOURCE_UNIQUE_ID")
	private String resourceUniqueId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "POSITION_ID")
	private String positionId;

	@Column(name = "ALLOCATION_START_DATE")
	private Timestamp allocationStartDate;

	@Column(name = "ALLOCATION_END_DATE")
	private Timestamp allocationEndDate;

	@Column(name = "COST_ALLOCATION")
	private BigDecimal costAllocation;

	@Column(name = "BUDGET_REFERENCE_NUMBER")
	private String budgetReferenceNumber;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public String getResourceUniqueId() {
		return resourceUniqueId;
	}

	public void setResourceUniqueId(String resourceUniqueId) {
		this.resourceUniqueId = resourceUniqueId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public Timestamp getAllocationStartDate() {
		return allocationStartDate;
	}

	public void setAllocationStartDate(Timestamp allocationStartDate) {
		this.allocationStartDate = allocationStartDate;
	}

	public Timestamp getAllocationEndDate() {
		return allocationEndDate;
	}

	public void setAllocationEndDate(Timestamp allocationEndDate) {
		this.allocationEndDate = allocationEndDate;
	}

	public BigDecimal getCostAllocation() {
		return costAllocation;
	}

	public void setCostAllocation(BigDecimal costAllocation) {
		this.costAllocation = costAllocation;
	}

	public String getBudgetReferenceNumber() {
		return budgetReferenceNumber;
	}

	public void setBudgetReferenceNumber(String budgetReferenceNumber) {
		this.budgetReferenceNumber = budgetReferenceNumber;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
