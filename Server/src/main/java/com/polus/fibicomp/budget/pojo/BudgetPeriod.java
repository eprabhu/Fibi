package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.proposal.comparator.BudgetDetailComparatorByBudgetCategoryCode;
import com.polus.fibicomp.proposal.comparator.BudgetDetailComparatorBySystemGenerated;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_PERIOD")
public class BudgetPeriod implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERIOD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_PERIOD_ID_GENERATOR")
	@SequenceGenerator(name = "BUDGET_PERIOD_ID_GENERATOR", sequenceName = "BUDGET_PERIOD_ID_GENERATOR", allocationSize = 1)
	private Integer budgetPeriodId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERIOD_FK1"), name = "BUDGET_HEADER_ID", referencedColumnName = "BUDGET_HEADER_ID")
	private BudgetHeader budget;

	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "BUDGET_PERIOD")
	private Integer budgetPeriod;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "TOTAL_COST", precision = 12, scale = 2)
	private BigDecimal totalCost = BigDecimal.ZERO;

	@Column(name = "TOTAL_DIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalDirectCost = BigDecimal.ZERO;

	@Column(name = "TOTAL_INDIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalIndirectCost = BigDecimal.ZERO;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PERIOD_LABEL")
	private String periodLabel;

	@Column(name = "IS_OBLIGATED_PERIOD")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isObligatedPeriod;

	@JsonManagedReference
	@OneToMany(mappedBy = "period", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<BudgetDetail> budgetDetails;

	@Column(name = "SUBCONTRACT_COST", precision = 12, scale = 2)
	private BigDecimal subcontractCost = BigDecimal.ZERO;

	@Column(name = "COST_SHARING_AMOUNT", precision = 12, scale = 2)
	private BigDecimal costSharingAmount = BigDecimal.ZERO;

	@Column(name = "UNDERRECOVERY_AMOUNT", precision = 12, scale = 2)
	private BigDecimal underrecoveryAmount = BigDecimal.ZERO;

	@Column(name = "TOTAL_DIRECT_COST_LIMIT", precision = 12, scale = 2)
	private BigDecimal totalDirectCostLimit = BigDecimal.ZERO;

	@Column(name = "TOTAL_COST_LIMIT", precision = 12, scale = 2)
	private BigDecimal totalCostLimit = BigDecimal.ZERO;

	@Column(name = "COMMENTS")
	private String comments;

	@Transient
	private Boolean generatePeriod = false;

	@Transient
	private BigDecimal totalFundRequested = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalFundRequestedIncludBenifit = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalInKind = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalOfTotalCost = BigDecimal.ZERO;

	public BudgetPeriod() {
		budgetDetails = new ArrayList<>();
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
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

	public void setIsObligatedPeriod(Boolean isObligatedPeriod) {
		this.isObligatedPeriod = isObligatedPeriod;
	}

	public Boolean getIsObligatedPeriod() {
		return isObligatedPeriod;
	}

	public List<BudgetDetail> getBudgetDetails() {
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			Collections.sort(budgetDetails, new BudgetDetailComparatorByBudgetCategoryCode());
			Collections.sort(budgetDetails, new BudgetDetailComparatorBySystemGenerated());
		}
		return budgetDetails;
	}

	public void setBudgetDetails(List<BudgetDetail> budgetDetails) {
		this.budgetDetails = budgetDetails;
	}

	public BigDecimal getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(BigDecimal costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public BigDecimal getUnderrecoveryAmount() {
		return underrecoveryAmount;
	}

	public void setUnderrecoveryAmount(BigDecimal underrecoveryAmount) {
		this.underrecoveryAmount = underrecoveryAmount;
	}

	public BigDecimal getTotalDirectCostLimit() {
		return totalDirectCostLimit;
	}

	public void setTotalDirectCostLimit(BigDecimal totalDirectCostLimit) {
		this.totalDirectCostLimit = totalDirectCostLimit;
	}

	public BigDecimal getTotalCostLimit() {
		return totalCostLimit;
	}

	public void setTotalCostLimit(BigDecimal totalCostLimit) {
		this.totalCostLimit = totalCostLimit;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public BudgetHeader getBudget() {
		return budget;
	}

	public void setBudget(BudgetHeader budget) {
		this.budget = budget;
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

	public Boolean getGeneratePeriod() {
		return generatePeriod;
	}

	public void setGeneratePeriod(Boolean generatePeriod) {
		this.generatePeriod = generatePeriod;
	}

	public BigDecimal getTotalFundRequested() {
		totalFundRequested = BigDecimal.ZERO;
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			for (BudgetDetail budgetDetail : budgetDetails) {
				if (budgetDetail.getSystemGeneratedCEType() != null && budgetDetail.getSystemGeneratedCEType().equals("BUDGET_OH_ON")) {
					totalFundRequested = totalFundRequested.add(BigDecimal.ZERO);
				} else {
					if (budgetDetail.getSponsorRequestedAmount()!=null) {
					totalFundRequested = totalFundRequested.add(budgetDetail.getSponsorRequestedAmount());
					}
				}
			}
		}
		return totalFundRequested;
	}

	public void setTotalFundRequested(BigDecimal totalFundRequested) {
		this.totalFundRequested = totalFundRequested;
	}

	public BigDecimal getTotalFundRequestedIncludBenifit() {
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			BigDecimal totalFundRequestedIncludBenifits = BigDecimal.ZERO;
			for (BudgetDetail budgetDetail : budgetDetails) {
				if (budgetDetail.getSponsorRequestedAmount() != null)
					totalFundRequestedIncludBenifits = budgetDetail.getSponsorRequestedAmount();
				totalFundRequestedIncludBenifit = totalFundRequestedIncludBenifit.add(totalFundRequestedIncludBenifits);
			}
		}
		return totalFundRequestedIncludBenifit;
	}

	public void setTotalFundRequestedIncludBenifit(BigDecimal totalFundRequestedIncludBenifit) {
		this.totalFundRequestedIncludBenifit = totalFundRequestedIncludBenifit;
	}

	public BigDecimal getTotalModifiedDirectCost() {
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			totalModifiedDirectCost = BigDecimal.ZERO;
			BigDecimal subtrahend = budgetDetails.stream().filter(budgetDetail -> (budgetDetail.getBudgetCategory().getBudgetCategoryTypeCode()
					.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_EQUIPMENT)
					|| budgetDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SUBCONTRACT)) && (budgetDetail.getSponsorRequestedAmount() != null))
			.map(BudgetDetail::getSponsorRequestedAmount).reduce(BigDecimal.ZERO,BigDecimal::add);
			if (totalDirectCost != null) {
				totalModifiedDirectCost = totalDirectCost.subtract(subtrahend);
			}
		}
		return totalModifiedDirectCost;
	}

	public void setTotalModifiedDirectCost(BigDecimal totalModifiedDirectCost) {
		this.totalModifiedDirectCost = totalModifiedDirectCost;
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

	public BigDecimal getTotalInKind() {
//		if (!budgetDetails.isEmpty()) {
			totalInKind = BigDecimal.ZERO;
//			totalInKind = totalInKind.add(underrecoveryAmount).add(costSharingAmount);
			totalInKind = totalInKind.add(costSharingAmount);
//		}
		return totalInKind;
	}

	public void setTotalInKind(BigDecimal totalInKind) {
		this.totalInKind = totalInKind;
	}

	public BigDecimal getTotalOfTotalCost() {
		totalOfTotalCost = BigDecimal.ZERO;
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			for (BudgetDetail budgetDetail : budgetDetails) {
				if (budgetDetail.getLineItemCost() != null) {
					totalOfTotalCost = totalOfTotalCost.add(budgetDetail.getLineItemCost());
				}
			}
		} else {
			if (totalInKind != null && totalCost != null) {
				totalOfTotalCost = totalOfTotalCost.add(totalInKind).add(totalCost);
			}
		}
		return totalOfTotalCost;
	}

	public void setTotalOfTotalCost(BigDecimal totalOfTotalCost) {
		this.totalOfTotalCost = totalOfTotalCost;
	}
}
