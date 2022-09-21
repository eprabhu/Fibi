package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.award.comparator.AwardBudgetDetailComparatorBySortOrder;
import com.polus.fibicomp.proposal.comparator.AwardBudgetDetailComparatorBySystemGenerated;

@Entity
@Table(name = "AWARD_BUDGET_PERIOD")
public class AwardBudgetPeriod implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERIOD_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer budgetPeriodId;

	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetId;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "BUDGET_PERIOD")
	private Integer budgetPeriod;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "TOTAL_COST", precision = 12, scale = 2)
	private BigDecimal totalCost;

	@Column(name = "TOTAL_DIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalDirectCost;

	@Column(name = "TOTAL_INDIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalIndirectCost;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PERIOD_LABEL")
	private String periodLabel;

	@Column(name = "SUBCONTRACT_COST", precision = 12, scale = 2)
	private BigDecimal subcontractCost;

	@Column(name = "DEV_PROPOSAL_ID")
	private Integer devProposalId;

	@Column(name = "DEV_PROP_BUDGET_ID")
	private Integer devProposalBudgetId;

	@Column(name = "DEV_PROP_BUDGET_PERIOD")
	private Integer devProposalBudgetPeriod;

	@Transient
	private List<AwardBudgetDetail> budgetDetails;

	public AwardBudgetPeriod() {
		budgetDetails = new ArrayList<>();
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Integer getBudgetPeriod() {
		return budgetPeriod;
	}

	public void setBudgetPeriod(Integer budgetPeriod) {
		this.budgetPeriod = budgetPeriod;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
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

	public String getPeriodLabel() {
		return periodLabel;
	}

	public void setPeriodLabel(String periodLabel) {
		this.periodLabel = periodLabel;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public List<AwardBudgetDetail> getBudgetDetails() {
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			Collections.sort(budgetDetails, new AwardBudgetDetailComparatorBySortOrder());
			Collections.sort(budgetDetails, new AwardBudgetDetailComparatorBySystemGenerated());
		}
		return budgetDetails;
	}

	public void setBudgetDetails(List<AwardBudgetDetail> budgetDetails) {
		this.budgetDetails = budgetDetails;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getDevProposalId() {
		return devProposalId;
	}

	public void setDevProposalId(Integer devProposalId) {
		this.devProposalId = devProposalId;
	}

	public Integer getDevProposalBudgetId() {
		return devProposalBudgetId;
	}

	public void setDevProposalBudgetId(Integer devProposalBudgetId) {
		this.devProposalBudgetId = devProposalBudgetId;
	}

	public Integer getDevProposalBudgetPeriod() {
		return devProposalBudgetPeriod;
	}

	public void setDevProposalBudgetPeriod(Integer devProposalBudgetPeriod) {
		this.devProposalBudgetPeriod = devProposalBudgetPeriod;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
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

	public BigDecimal getSubcontractCost() {
		return subcontractCost;
	}

	public void setSubcontractCost(BigDecimal subcontractCost) {
		this.subcontractCost = subcontractCost;
	}
}
