package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.adminportal.pojo.RateClass;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_DET_CAL_AMT")
public class BudgetDetailCalcAmount implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_DETAILS_CAL_AMTS_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_DETAIL_CAL_ID_GENERATOR")
	@SequenceGenerator(name = "BUDGET_DETAIL_CAL_ID_GENERATOR", sequenceName = "BUDGET_DETAIL_CAL_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@JsonBackReference
	@ManyToOne(optional = false, cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_DET_CAL_AMT_FK1"), name = "BUDGET_DETAILS_ID", referencedColumnName = "BUDGET_DETAILS_ID", insertable = false, updatable = false)
	private BudgetDetail budgetDetail;

	@Column(name = "BUDGET_DETAILS_ID")
	private Integer budgetDetailId;

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

	@Column(name = "APPLY_RATE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean applyRateFlag;

	@Column(name = "CALCULATED_COST", precision = 12, scale = 2)
	private BigDecimal calculatedCost;

	@Column(name = "CALCULATED_COST_SHARING", precision = 12, scale = 2)
	private BigDecimal calculatedCostSharing;

	@Column(name = "CALCULATED_FUND_REQUESTED", precision = 12, scale = 2)
	private BigDecimal calculatedFundRequested;

	@Column(name = "RATE_TYPE_DESCRIPTION")
	private String rateTypeDescription;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_DET_CAL_AMT_FK2"), name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false)
	private RateClass rateClass;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumns(foreignKey = @ForeignKey(name = "BUDGET_DET_CAL_AMT_FK3"), value = {
			@JoinColumn(name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false),
			@JoinColumn(name = "RATE_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
	private RateType rateType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "APPLICABLE_RATE", precision = 10, scale = 2)
	private BigDecimal applicableRate;
	
	@Column(name = "APPLICABLE_COST_SHARING_RATE", precision = 10, scale = 2)
	private BigDecimal applicableCostSharingRate;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public BudgetDetail getBudgetDetail() {
		return budgetDetail;
	}

	public void setBudgetDetail(BudgetDetail budgetDetail) {
		this.budgetDetail = budgetDetail;
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

	public Boolean getApplyRateFlag() {
		return applyRateFlag;
	}

	public void setApplyRateFlag(Boolean applyRateFlag) {
		this.applyRateFlag = applyRateFlag;
	}

	public String getRateTypeDescription() {
		return rateTypeDescription;
	}

	public void setRateTypeDescription(String rateTypeDescription) {
		this.rateTypeDescription = rateTypeDescription;
	}

	public RateClass getRateClass() {
		return rateClass;
	}

	public void setRateClass(RateClass rateClass) {
		this.rateClass = rateClass;
	}

	public RateType getRateType() {
		return rateType;
	}

	public void setRateType(RateType rateType) {
		this.rateType = rateType;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
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

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
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

	public BigDecimal getCalculatedFundRequested() {
		return calculatedFundRequested;
	}

	public void setCalculatedFundRequested(BigDecimal calculatedFundRequested) {
		this.calculatedFundRequested = calculatedFundRequested;
	}

	public BigDecimal getApplicableRate() {
		return applicableRate;
	}

	public void setApplicableRate(BigDecimal applicableRate) {
		this.applicableRate = applicableRate;
	}

	public BigDecimal getApplicableCostSharingRate() {
		return applicableCostSharingRate;
	}

	public void setApplicableCostSharingRate(BigDecimal applicableCostSharingRate) {
		this.applicableCostSharingRate = applicableCostSharingRate;
	}
}
