package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;

public class AwardBudgetHeaderDetail {

	private Integer budgetId;

	private Integer versionNumber;

	private Integer awardId;

	private String awardNumber;

	private Integer sequenceNumber;

	private Timestamp startDate;

	private Timestamp endDate;

	private String budgetType;

	private String budgetStatus;

	private String onOffCampusFlag;

	private String fAndARateType;

	private BigDecimal totalDirectCost;

	private BigDecimal totalIndirectCost;

	private BigDecimal totalCost;

	private Timestamp updateTimeStamp;

	private String updateUserName;

	private String budgetStatusCode;

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public String getBudgetType() {
		return budgetType;
	}

	public void setBudgetType(String budgetType) {
		this.budgetType = budgetType;
	}

	public String getBudgetStatus() {
		return budgetStatus;
	}

	public void setBudgetStatus(String budgetStatus) {
		this.budgetStatus = budgetStatus;
	}

	public String getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(String onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
	}

	public String getfAndARateType() {
		return fAndARateType;
	}

	public void setfAndARateType(String fAndARateType) {
		this.fAndARateType = fAndARateType;
	}

	public BigDecimal getTotalDirectCost() {
		return totalDirectCost;
	}

	public void setTotalDirectCost(BigDecimal totalDirectCost) {
		this.totalDirectCost = totalDirectCost;
	}

	public BigDecimal getTotalIndirectCost() {
		return totalIndirectCost;
	}

	public void setTotalIndirectCost(BigDecimal totalIndirectCost) {
		this.totalIndirectCost = totalIndirectCost;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public String getBudgetStatusCode() {
		return budgetStatusCode;
	}

	public void setBudgetStatusCode(String budgetStatusCode) {
		this.budgetStatusCode = budgetStatusCode;
	}
}
