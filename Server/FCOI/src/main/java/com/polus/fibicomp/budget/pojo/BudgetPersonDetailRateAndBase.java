package com.polus.fibicomp.budget.pojo;

import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_PER_DET_RATE_AND_BASE")
public class BudgetPersonDetailRateAndBase {

	@Id
	@Column(name = "BUD_PER_RATE_BASE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUD_PER_RATE_BASE_ID_GENERATOR")
	@SequenceGenerator(name="BUD_PER_RATE_BASE_ID_GENERATOR", sequenceName = "BUD_PER_RATE_BASE_ID_GENERATOR", allocationSize=1)
	private Integer budgetPersonRateAndBaseId;

	@Column(name = "BUDGET_PERIOD")
	private Integer budgetPeriod;

	@Column(name = "LINE_ITEM_NUMBER")
	private Integer lineItemNumber;

	@Column(name = "PERSON_NUMBER")
	private Integer personNumber;

	@Column(name = "RATE_NUMBER")
	private Integer rateNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "RATE_CLASS_CODE")
	private String rateClassCode;

	@Column(name = "RATE_TYPE_CODE")
	private String rateTypeCode;

	@Column(name = "ON_OFF_CAMPUS_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean onOffCampusFlag = false;

	@Column(name = "APPLIED_RATE", precision = 5, scale = 2)
	private BigDecimal appliedRate;

	@Column(name = "SALARY_REQUESTED", precision = 12, scale = 2)
	private BigDecimal salaryRequested;

	@Column(name = "BASE_COST_SHARING", precision = 12, scale = 2)
	private BigDecimal baseCostSharing;

	@Column(name = "CALCULATED_COST", precision = 12, scale = 2)
	private BigDecimal calculatedCost;

	@Column(name = "CALCULATED_COST_SHARING", precision = 12, scale = 2)
	private BigDecimal calculatedCostSharing;

	@Column(name = "JOB_CODE")
	private String jobCode;

	@Column(name = "BASE_COST", precision = 12, scale = 2)
	private BigDecimal baseCost;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Integer getBudgetPersonRateAndBaseId() {
		return budgetPersonRateAndBaseId;
	}

	public void setBudgetPersonRateAndBaseId(Integer budgetPersonRateAndBaseId) {
		this.budgetPersonRateAndBaseId = budgetPersonRateAndBaseId;
	}

	public Integer getBudgetPeriod() {
		return budgetPeriod;
	}

	public void setBudgetPeriod(Integer budgetPeriod) {
		this.budgetPeriod = budgetPeriod;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public Integer getPersonNumber() {
		return personNumber;
	}

	public void setPersonNumber(Integer personNumber) {
		this.personNumber = personNumber;
	}

	public Integer getRateNumber() {
		return rateNumber;
	}

	public void setRateNumber(Integer rateNumber) {
		this.rateNumber = rateNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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

	public String getRateClassCode() {
		return rateClassCode;
	}

	public void setRateClassCode(String rateClassCode) {
		this.rateClassCode = rateClassCode;
	}

	public String getRateTypeCode() {
		return rateTypeCode;
	}

	public void setRateTypeCode(String rateTypeCode) {
		this.rateTypeCode = rateTypeCode;
	}

	public Boolean getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(Boolean onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
	}

	public BigDecimal getAppliedRate() {
		return appliedRate;
	}

	public void setAppliedRate(BigDecimal appliedRate) {
		this.appliedRate = appliedRate;
	}

	public BigDecimal getSalaryRequested() {
		return salaryRequested;
	}

	public void setSalaryRequested(BigDecimal salaryRequested) {
		this.salaryRequested = salaryRequested;
	}

	public BigDecimal getBaseCostSharing() {
		return baseCostSharing;
	}

	public void setBaseCostSharing(BigDecimal baseCostSharing) {
		this.baseCostSharing = baseCostSharing;
	}

	public BigDecimal getCalculatedCost() {
		return calculatedCost;
	}

	public void setCalculatedCost(BigDecimal calculatedCost) {
		this.calculatedCost = calculatedCost;
	}

	public BigDecimal getCalculatedCostSharing() {
		return calculatedCostSharing;
	}

	public void setCalculatedCostSharing(BigDecimal calculatedCostSharing) {
		this.calculatedCostSharing = calculatedCostSharing;
	}

	public String getJobCode() {
		return jobCode;
	}

	public void setJobCode(String jobCode) {
		this.jobCode = jobCode;
	}

	public BigDecimal getBaseCost() {
		return baseCost;
	}

	public void setBaseCost(BigDecimal baseCost) {
		this.baseCost = baseCost;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}
}
