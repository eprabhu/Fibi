package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_RATE_AND_BASE")
public class BudgetRateAndBase implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_RATE_AND_BASE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_RATE_AND_BASE_ID_GENERATOR")
	@SequenceGenerator(name = "BUDGET_RATE_AND_BASE_ID_GENERATOR", sequenceName = "BUDGET_RATE_AND_BASE_ID_GENERATOR", allocationSize = 1)
	private Long budgetRateAndBaseId;

	@Column(name = "BASE_COST", precision = 12, scale = 2)
	private BigDecimal baseCost;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_RATE_AND_BASE_FK1"), name = "BUDGET_DETAILS_ID", referencedColumnName = "BUDGET_DETAILS_ID")
	private BudgetDetail budgetDetail;

	@Column(name = "BUDGET_ID")
	private Integer budgetId;

	@Column(name = "BUDGET_PERIOD")
	private Integer budgetPeriod;

	@Column(name = "BUDGET_PERIOD_NUMBER")
	private Integer budgetPeriodId;

	@Column(name = "LINE_ITEM_NUMBER")
	private Integer lineItemNumber;

	@Column(name = "RATE_CLASS_CODE")
	private String rateClassCode;

	@Column(name = "RATE_TYPE_CODE")
	private String rateTypeCode;

	@Column(name = "RATE_NUMBER")
	private Integer rateNumber;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "ON_OFF_CAMPUS_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean onOffCampusFlag = false;

	@Column(name = "APPLIED_RATE", precision = 5, scale = 2)
	private BigDecimal appliedRate;

	@Column(name = "BASE_COST_SHARING", precision = 14, scale = 2)
	private BigDecimal baseCostSharing;

	@Column(name = "CALCULATED_COST", precision = 14, scale = 2)
	private BigDecimal calculatedCost;

	@Column(name = "CALCULATED_COST_SHARING", precision = 14, scale = 2)
	private BigDecimal calculatedCostSharing;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Long getBudgetRateAndBaseId() {
		return budgetRateAndBaseId;
	}

	public void setBudgetRateAndBaseId(Long budgetRateAndBaseId) {
		this.budgetRateAndBaseId = budgetRateAndBaseId;
	}

	public BudgetDetail getBudgetDetail() {
		return budgetDetail;
	}

	public void setBudgetDetail(BudgetDetail budgetDetail) {
		this.budgetDetail = budgetDetail;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Integer getBudgetPeriod() {
		return budgetPeriod;
	}

	public void setBudgetPeriod(Integer budgetPeriod) {
		this.budgetPeriod = budgetPeriod;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getRateNumber() {
		return rateNumber;
	}

	public void setRateNumber(Integer rateNumber) {
		this.rateNumber = rateNumber;
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

	public BigDecimal getBaseCost() {
		return baseCost;
	}

	public void setBaseCost(BigDecimal baseCost) {
		this.baseCost = baseCost;
	}

}
