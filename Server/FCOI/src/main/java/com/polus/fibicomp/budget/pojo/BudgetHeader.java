package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_HEADER")
@EntityListeners(AuditingEntityListener.class)
public class BudgetHeader implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_HEADER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_HEADER_ID_GENERATOR")
	@SequenceGenerator(name = "BUDGET_HEADER_ID_GENERATOR", sequenceName = "BUDGET_HEADER_ID_GENERATOR", allocationSize = 1)
	private Integer budgetId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "MODULE_SEQUENCE_NUMBER")
	private Integer moduleSequenceNumber;

	@Column(name = "OBLIGATED_TOTAL", precision = 12, scale = 2)
	private BigDecimal obligatedTotal;

	@Column(name = "OBLIGATED_CHANGE", precision = 12, scale = 2)
	private BigDecimal obligatedChange;

	@Column(name = "BUDGET_STATUS_CODE")
	private String budgetStatusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_HEADER_FK1"), name = "BUDGET_STATUS_CODE", referencedColumnName = "BUDGET_STATUS_CODE", insertable = false, updatable = false)
	private BudgetStatus budgetStatus;

	@Column(name = "IS_AUTO_CALC")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isAutoCalc = false;

	@Column(name = "BUDGET_TYPE_CODE")
	private String budgetTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_HEADER_FK2"), name = "BUDGET_TYPE_CODE", referencedColumnName = "BUDGET_TYPE_CODE", insertable = false, updatable = false)
	private BudgetType budgetType;

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

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "ON_OFF_CAMPUS_FLAG") // N - ON, F - OFF, D - Both
	private String campusFlag;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_USER_NAME")
	private String createUserName;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_USER_NAME")
	private String updateUserName;

	@Column(name = "RATE_CLASS_CODE")
	private String rateClassCode;

	@Column(name = "RATE_TYPE_CODE")
	private String rateTypeCode;

	@Column(name = "ANTICIPATED_TOTAL", precision = 12, scale = 2)
	private BigDecimal anticipatedTotal;

	@Column(name = "COST_SHARE_TYPE_CODE")
	private Integer costSharingTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_HEADER_FK6"), name = "COST_SHARE_TYPE_CODE", referencedColumnName = "COST_SHARE_TYPE_CODE", insertable = false, updatable = false)
	private CostSharingType costSharingType;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumns(foreignKey = @ForeignKey(name = "BUDGET_HEADER_FK3"), value = {
			@JoinColumn(name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false),
			@JoinColumn(name = "RATE_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
	private RateType rateType;

	@OrderBy("budgetPeriod asc")
	@JsonManagedReference
	@OneToMany(mappedBy = "budget", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<BudgetPeriod> budgetPeriods;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetHeader", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<FibiProposalRate> proposalRates;

	@Column(name = "TOTAL_SUBCONTRACT_COST", precision = 12, scale = 2)
	private BigDecimal totalSubcontractCost;

	@Column(name = "IS_FINAL_BUDGET")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isFinalBudget = false;

	@Column(name = "COST_SHARING_AMOUNT", precision = 12, scale = 2)
	private BigDecimal costSharingAmount = BigDecimal.ZERO;

	@Column(name = "UNDERRECOVERY_AMOUNT", precision = 12, scale = 2)
	private BigDecimal underrecoveryAmount = BigDecimal.ZERO;

	@Column(name = "RESIDUAL_FUNDS", precision = 12, scale = 2)
	private BigDecimal residualFunds;

	@Column(name = "TOTAL_COST_LIMIT", precision = 12, scale = 2)
	private BigDecimal totalCostLimit;

	@Column(name = "MODULAR_BUDGET_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean modularBudgetFlag = false;

	@Column(name = "SUBMIT_COST_SHARING_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean submitCostSharingFlag = false;

	@Column(name = "UNDERRECOVERY_CLASS_CODE")
	private String underrecoveryRateClassCode;

	@Column(name = "UNDERRECOVERY_TYPE_CODE")
	private String underrecoveryRateTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumns(foreignKey = @ForeignKey(name = "BUDGET_HEADER_FK5"), value = {
			@JoinColumn(name = "UNDERRECOVERY_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false),
			@JoinColumn(name = "UNDERRECOVERY_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
	private RateType underrecoveryRateType;

	@Column(name = "IS_LATEST_VERSION")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean isLatestVersion = false;

	@Column(name = "IS_APPROVED_BUDGET")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isApprovedBudget = false;

	@Column(name = "BUDGET_TEMPLATE_TYPE_ID")
	private Integer budgetTemplateTypeId;

	@Column(name = "ON_CAMPUS_RATES")
	private String onCampusRates;

	@Column(name = "OFF_CAMPUS_RATES")
	private String offCampusRates;

	@Transient
	private Set<String> rateClassTypes;

	@Transient
	private Boolean isSelected = false;

	@Transient
	private BigDecimal totalFundRequested = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalInKind = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalOfTotalCost = BigDecimal.ZERO;

	public BudgetHeader() {
		budgetPeriods = new ArrayList<>();
		proposalRates = new ArrayList<>();
		rateClassTypes = new HashSet<>();
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

	public Integer getModuleSequenceNumber() {
		return moduleSequenceNumber;
	}

	public void setModuleSequenceNumber(Integer moduleSequenceNumber) {
		this.moduleSequenceNumber = moduleSequenceNumber;
	}

	public String getBudgetStatusCode() {
		return budgetStatusCode;
	}

	public void setBudgetStatusCode(String budgetStatusCode) {
		this.budgetStatusCode = budgetStatusCode;
	}

	public BudgetStatus getBudgetStatus() {
		return budgetStatus;
	}

	public void setBudgetStatus(BudgetStatus budgetStatus) {
		this.budgetStatus = budgetStatus;
	}

	public Boolean getIsAutoCalc() {
		return isAutoCalc;
	}

	public void setIsAutoCalc(Boolean isAutoCalc) {
		this.isAutoCalc = isAutoCalc;
	}

	public String getBudgetTypeCode() {
		return budgetTypeCode;
	}

	public void setBudgetTypeCode(String budgetTypeCode) {
		this.budgetTypeCode = budgetTypeCode;
	}

	public BudgetType getBudgetType() {
		return budgetType;
	}

	public void setBudgetType(BudgetType budgetType) {
		this.budgetType = budgetType;
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

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public Timestamp getCreateTimeStamp() {
		return createTimeStamp;
	}

	public void setCreateTimeStamp(Timestamp createTimeStamp) {
		this.createTimeStamp = createTimeStamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getCreateUserName() {
		return createUserName;
	}

	public void setCreateUserName(String createUserName) {
		this.createUserName = createUserName;
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

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
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

	public RateType getRateType() {
		return rateType;
	}

	public void setRateType(RateType rateType) {
		this.rateType = rateType;
	}

	public List<FibiProposalRate> getProposalRates() {
		return proposalRates;
	}

	public void setProposalRates(List<FibiProposalRate> proposalRates) {
		this.proposalRates = proposalRates;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Boolean getIsFinalBudget() {
		return isFinalBudget;
	}

	public void setIsFinalBudget(Boolean isFinalBudget) {
		this.isFinalBudget = isFinalBudget;
	}

	public Set<String> getRateClassTypes() {
		return rateClassTypes;
	}

	public void setRateClassTypes(Set<String> rateClassTypes) {
		this.rateClassTypes = rateClassTypes;
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

	public BigDecimal getResidualFunds() {
		return residualFunds;
	}

	public void setResidualFunds(BigDecimal residualFunds) {
		this.residualFunds = residualFunds;
	}

	public BigDecimal getTotalCostLimit() {
		return totalCostLimit;
	}

	public void setTotalCostLimit(BigDecimal totalCostLimit) {
		this.totalCostLimit = totalCostLimit;
	}

	public Boolean getModularBudgetFlag() {
		return modularBudgetFlag;
	}

	public void setModularBudgetFlag(Boolean modularBudgetFlag) {
		this.modularBudgetFlag = modularBudgetFlag;
	}

	public Boolean getSubmitCostSharingFlag() {
		return submitCostSharingFlag;
	}

	public void setSubmitCostSharingFlag(Boolean submitCostSharingFlag) {
		this.submitCostSharingFlag = submitCostSharingFlag;
	}

	public String getUnderrecoveryRateClassCode() {
		return underrecoveryRateClassCode;
	}

	public void setUnderrecoveryRateClassCode(String underrecoveryRateClassCode) {
		this.underrecoveryRateClassCode = underrecoveryRateClassCode;
	}

	public String getUnderrecoveryRateTypeCode() {
		return underrecoveryRateTypeCode;
	}

	public void setUnderrecoveryRateTypeCode(String underrecoveryRateTypeCode) {
		this.underrecoveryRateTypeCode = underrecoveryRateTypeCode;
	}

	public RateType getUnderrecoveryRateType() {
		return underrecoveryRateType;
	}

	public void setUnderrecoveryRateType(RateType underrecoveryRateType) {
		this.underrecoveryRateType = underrecoveryRateType;
	}

	public String getCampusFlag() {
		return campusFlag;
	}

	public void setCampusFlag(String campusFlag) {
		this.campusFlag = campusFlag;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Boolean getIsSelected() {
		return isSelected;
	}

	public void setIsSelected(Boolean isSelected) {
		this.isSelected = isSelected;
	}

	public List<BudgetPeriod> getBudgetPeriods() {
		return budgetPeriods;
	}

	public void setBudgetPeriods(List<BudgetPeriod> budgetPeriods) {
		this.budgetPeriods = budgetPeriods;
	}

	public BigDecimal getTotalFundRequested() {
		totalFundRequested = BigDecimal.ZERO;
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				for (BudgetDetail detail : budgetPeriod.getBudgetDetails()) {
					if (detail.getSponsorRequestedAmount() != null) {
						if (detail.getSystemGeneratedCEType() != null && detail.getSystemGeneratedCEType().equals("BUDGET_OH_ON")) {
							totalFundRequested = totalFundRequested.add(BigDecimal.ZERO);
						} else {
							totalFundRequested = totalFundRequested.add(detail.getSponsorRequestedAmount());
						}
					}
				}
			}
		}
		return totalFundRequested;
	}

	public void setTotalFundRequested(BigDecimal totalFundRequested) {
		this.totalFundRequested = totalFundRequested;
	}

	public BigDecimal getTotalModifiedDirectCost() {
		totalModifiedDirectCost = BigDecimal.ZERO;
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				if (budgetPeriod.getTotalModifiedDirectCost() != null) {
					totalModifiedDirectCost = totalModifiedDirectCost.add(budgetPeriod.getTotalModifiedDirectCost());
				}
			}
		}
		return totalModifiedDirectCost;
	}

	public void setTotalModifiedDirectCost(BigDecimal totalModifiedDirectCost) {
		this.totalModifiedDirectCost = totalModifiedDirectCost;
	}

	public Boolean getIsApprovedBudget() {
		return isApprovedBudget;
	}

	public void setIsApprovedBudget(Boolean isApprovedBudget) {
		this.isApprovedBudget = isApprovedBudget;
	}

	public boolean getIsLatestVersion() {
		return isLatestVersion;
	}

	public void setIsLatestVersion(boolean isLatestVersion) {
		this.isLatestVersion = isLatestVersion;
	}

	public BigDecimal getObligatedTotal() {
		return obligatedTotal;
	}

	public void setObligatedTotal(BigDecimal obligatedTotal) {
		this.obligatedTotal = obligatedTotal;
	}

	public BigDecimal getObligatedChange() {
		return obligatedChange;
	}

	public void setObligatedChange(BigDecimal obligatedChange) {
		this.obligatedChange = obligatedChange;
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

	public BigDecimal getAnticipatedTotal() {
		return anticipatedTotal;
	}

	public void setAnticipatedTotal(BigDecimal anticipatedTotal) {
		this.anticipatedTotal = anticipatedTotal;
	}

	public BigDecimal getTotalSubcontractCost() {
		return totalSubcontractCost;
	}

	public void setTotalSubcontractCost(BigDecimal totalSubcontractCost) {
		this.totalSubcontractCost = totalSubcontractCost;
	}

	public void setLatestVersion(boolean isLatestVersion) {
		this.isLatestVersion = isLatestVersion;
	}

	public BigDecimal getTotalInKind() {
		totalInKind = BigDecimal.ZERO;
//		totalInKind = totalInKind.add(underrecoveryAmount).add(costSharingAmount);
		totalInKind = totalInKind.add(costSharingAmount);
		return totalInKind;
	}

	public void setTotalInKind(BigDecimal totalInKind) {
		this.totalInKind = totalInKind;
	}

	public BigDecimal getTotalOfTotalCost() {
		totalOfTotalCost = BigDecimal.ZERO;
		if (budgetPeriods != null) {
			budgetPeriods.stream().forEach(
					budgetPeriod -> totalOfTotalCost = totalOfTotalCost.add(budgetPeriod.getTotalOfTotalCost()));
		}
		return totalOfTotalCost;
	}

	public void setTotalOfTotalCost(BigDecimal totalOfTotalCost) {
		this.totalOfTotalCost = totalOfTotalCost;
	}

	public Integer getBudgetTemplateTypeId() {
		return budgetTemplateTypeId;
	}

	public void setBudgetTemplateTypeId(Integer budgetTemplateTypeId) {
		this.budgetTemplateTypeId = budgetTemplateTypeId;
	}

	public String getOnCampusRates() {
		return onCampusRates;
	}

	public void setOnCampusRates(String onCampusRates) {
		this.onCampusRates = onCampusRates;
	}

	public String getOffCampusRates() {
		return offCampusRates;
	}

	public void setOffCampusRates(String offCampusRates) {
		this.offCampusRates = offCampusRates;
	}
	
	public Integer getCostSharingTypeCode() {
		return costSharingTypeCode;
	}
	
	public void setCostSharingTypeCode(Integer costSharingTypeCode) {
		this.costSharingTypeCode = costSharingTypeCode;
	}
	
	public CostSharingType getCostSharingType() {
		return costSharingType;
	}

	public void setCostSharingType(CostSharingType costSharingType) {
		this.costSharingType = costSharingType;
	}
}
