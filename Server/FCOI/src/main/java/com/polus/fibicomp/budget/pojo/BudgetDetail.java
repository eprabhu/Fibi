package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
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
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_DETAIL")
public class BudgetDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_DETAILS_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_DETAIL_ID_GENERATOR")
	@SequenceGenerator(name = "BUDGET_DETAIL_ID_GENERATOR", sequenceName = "BUDGET_DETAIL_ID_GENERATOR", allocationSize = 1)
	private Integer budgetDetailId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_DETAIL_FK1"), name = "BUDGET_PERIOD_ID", referencedColumnName = "BUDGET_PERIOD_ID")
	private BudgetPeriod period;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "BUDGET_PERIOD")
	private Integer budgetPeriod;

	@Column(name = "LINE_ITEM_NUMBER")
	private Integer lineItemNumber;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "BUDGET_CATEGORY_CODE")
	private String budgetCategoryCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_DETAIL_FK2"), name = "BUDGET_CATEGORY_CODE", referencedColumnName = "BUDGET_CATEGORY_CODE", insertable = false, updatable = false)
	private BudgetCategory budgetCategory;

	@Column(name = "COST_ELEMENT")
	private String costElementCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_DETAIL_FK3"), name = "COST_ELEMENT", referencedColumnName = "COST_ELEMENT", insertable = false, updatable = false)
	private CostElement costElement;

	@Column(name = "LINE_ITEM_DESCRIPTION")
	private String lineItemDescription;

	@Column(name = "LINE_ITEM_COST", precision = 12, scale = 2)
	private BigDecimal lineItemCost = BigDecimal.ZERO;

	@Column(name = "COST_ELEMENT_BASE", precision = 12, scale = 2)
	private BigDecimal ceBaseRate;

	@Column(name = "PREVIOUS_LINE_ITEM_COST", precision = 12, scale = 2)
	private BigDecimal prevLineItemCost;

	@Column(name = "BUDGET_JUSTIFICATION")
	private String budgetJustification;

	@Column(name = "IS_SYSTEM_GENRTED_COST_ELEMENT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSystemGeneratedCostElement = false;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ON_OFF_CAMPUS_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean onOffCampusFlag;

	@Column(name = "COST_SHARING_AMOUNT", precision = 12, scale = 2)
	private BigDecimal costSharingAmount = BigDecimal.ZERO;

	@Column(name = "COST_SHARING_PERCENT", precision = 5, scale = 2)
	private BigDecimal costSharingPercentage;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetDetail", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<BudgetDetailCalcAmount> budgetDetailCalcAmounts;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetDetail", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<BudgetRateAndBase> budgetRateAndBases;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetDetail", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<BudgetPersonalDetails> personsDetails;

	@Column(name = "SYSTEM_GEN_COST_ELEMENT_TYPE")
	private String systemGeneratedCEType;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PERSON_TYPE") // E - Employee, N - Non Employee, T - To Be Named
	private String personType;

	@Column(name = "TBN_ID")
	private String tbnId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_DETAIL_FK5"), name = "TBN_ID", referencedColumnName = "TBN_ID", insertable = false, updatable = false)
	private TbnPerson tbnPerson;

	@Column(name = "IS_APPLY_INFLATION_RATE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isApplyInflationRate = true;

	@Column(name = "UNDERRECOVERY_AMOUNT", precision = 12, scale = 2)
	private BigDecimal underrecoveryAmount;

	@Column(name = "QUANTITY", precision = 6, scale = 2)
	private BigDecimal quantity;

	@Column(name = "APPLY_IN_RATE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean applyInRateFlag = false;

	@Column(name = "SUBMIT_COST_SHARING_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean submitCostSharingFlag = false;

	@Column(name = "SPONSOR_REQUESTED_AMOUNT", precision = 12, scale = 2)
	private BigDecimal sponsorRequestedAmount = BigDecimal.ZERO;

	@Transient
	private Boolean isDeleted = false;

	@Transient
	private BigDecimal totalFundRequested = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;

	@Transient
	private BigDecimal fundRequestedIncludingBenifit = BigDecimal.ZERO;

	@Transient
	private BigDecimal calculateCostSharingWithBenifit = BigDecimal.ZERO;

	@Transient
	private BigDecimal calculateFundRequestedIncludingBenifit = BigDecimal.ZERO;

	@Transient
	private BigDecimal calculateLineItemCostIncludingBenifit = BigDecimal.ZERO;

	public BudgetDetail() {
		budgetDetailCalcAmounts = new ArrayList<>();
		budgetRateAndBases = new ArrayList<>();
		personsDetails = new ArrayList<>();
	}

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
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

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public BudgetCategory getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(BudgetCategory budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public String getCostElementCode() {
		return costElementCode;
	}

	public void setCostElementCode(String costElementCode) {
		this.costElementCode = costElementCode;
	}

	public CostElement getCostElement() {
		return costElement;
	}

	public void setCostElement(CostElement costElement) {
		this.costElement = costElement;
	}

	public String getLineItemDescription() {
		return lineItemDescription;
	}

	public void setLineItemDescription(String lineItemDescription) {
		this.lineItemDescription = lineItemDescription;
	}

	public String getBudgetJustification() {
		return budgetJustification;
	}

	public void setBudgetJustification(String budgetJustification) {
		this.budgetJustification = budgetJustification;
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

	public Boolean getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(Boolean onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getIsSystemGeneratedCostElement() {
		return isSystemGeneratedCostElement;
	}

	public void setIsSystemGeneratedCostElement(Boolean isSystemGeneratedCostElement) {
		this.isSystemGeneratedCostElement = isSystemGeneratedCostElement;
	}

	public List<BudgetDetailCalcAmount> getBudgetDetailCalcAmounts() {
		return budgetDetailCalcAmounts;
	}

	public void setBudgetDetailCalcAmounts(List<BudgetDetailCalcAmount> budgetDetailCalcAmounts) {
		this.budgetDetailCalcAmounts = budgetDetailCalcAmounts;
	}

	public List<BudgetRateAndBase> getBudgetRateAndBases() {
		return budgetRateAndBases;
	}

	public void setBudgetRateAndBases(List<BudgetRateAndBase> budgetRateAndBases) {
		this.budgetRateAndBases = budgetRateAndBases;
	}

	public String getSystemGeneratedCEType() {
		return systemGeneratedCEType;
	}

	public void setSystemGeneratedCEType(String systemGeneratedCEType) {
		this.systemGeneratedCEType = systemGeneratedCEType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		this.personType = personType;
	}

	public String getTbnId() {
		return tbnId;
	}

	public void setTbnId(String tbnId) {
		this.tbnId = tbnId;
	}

	public TbnPerson getTbnPerson() {
		return tbnPerson;
	}

	public void setTbnPerson(TbnPerson tbnPerson) {
		this.tbnPerson = tbnPerson;
	}

	public Boolean getIsApplyInflationRate() {
		return isApplyInflationRate;
	}

	public void setIsApplyInflationRate(Boolean isApplyInflationRate) {
		this.isApplyInflationRate = isApplyInflationRate;
	}

	public List<BudgetPersonalDetails> getPersonsDetails() {
		return personsDetails;
	}

	public void setPersonsDetails(List<BudgetPersonalDetails> personsDetails) {
		this.personsDetails = personsDetails;
	}

	public BigDecimal getCostSharingAmount() {
		if (!(personsDetails.isEmpty())) {
			costSharingAmount = BigDecimal.ZERO;
			for (BudgetPersonalDetails personsDetail : personsDetails) {
				costSharingAmount = costSharingAmount.add(personsDetail.getCostSharingAmount());
			}
		}
		return costSharingAmount;
	}

	public void setCostSharingAmount(BigDecimal costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public BigDecimal getCostSharingPercentage() {
		return costSharingPercentage;
	}

	public void setCostSharingPercentage(BigDecimal costSharingPercentage) {
		this.costSharingPercentage = costSharingPercentage;
	}

	public Boolean getIsDeleted() {
		return isDeleted;
	}

	public void setIsDeleted(Boolean isDeleted) {
		this.isDeleted = isDeleted;
	}

	public BigDecimal getUnderrecoveryAmount() {
		return underrecoveryAmount;
	}

	public void setUnderrecoveryAmount(BigDecimal underrecoveryAmount) {
		this.underrecoveryAmount = underrecoveryAmount;
	}

	public BigDecimal getQuantity() {
		return quantity;
	}

	public void setQuantity(BigDecimal quantity) {
		this.quantity = quantity;
	}

	public Boolean getApplyInRateFlag() {
		return applyInRateFlag;
	}

	public void setApplyInRateFlag(Boolean applyInRateFlag) {
		this.applyInRateFlag = applyInRateFlag;
	}

	public Boolean getSubmitCostSharingFlag() {
		return submitCostSharingFlag;
	}

	public void setSubmitCostSharingFlag(Boolean submitCostSharingFlag) {
		this.submitCostSharingFlag = submitCostSharingFlag;
	}

	public BudgetPeriod getPeriod() {
		return period;
	}

	public void setPeriod(BudgetPeriod period) {
		this.period = period;
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

	public BigDecimal getTotalFundRequested() {
		if (costSharingAmount != null && lineItemCost != null) {
			if (!isSystemGeneratedCostElement && (lineItemCost.compareTo(BigDecimal.ZERO) > 0
					|| costSharingAmount.compareTo(BigDecimal.ZERO) > 0)) {
				totalFundRequested = lineItemCost.subtract(costSharingAmount);
			}
		}
		return totalFundRequested;
	}

	public void setTotalFundRequested(BigDecimal totalFundRequested) {
		this.totalFundRequested = totalFundRequested;
	}

	public BigDecimal getTotalModifiedDirectCost() {
		BigDecimal subtrahend = BigDecimal.ZERO;
		BigDecimal fundRequested = BigDecimal.ZERO;
		BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;
		if ((budgetCategory.getBudgetCategoryTypeCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_EQUIPMENT)
				|| budgetCategory.getBudgetCategoryTypeCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SUBCONTRACT))) {
			fundRequested = sponsorRequestedAmount;
			if (fundRequested != null) {
				subtrahend = subtrahend.add(fundRequested);
			}
		}
		if (sponsorRequestedAmount != null) {
			totalModifiedDirectCost = sponsorRequestedAmount.subtract(subtrahend);
		}
		return totalModifiedDirectCost;
	}

	public void setTotalModifiedDirectCost(BigDecimal totalModifiedDirectCost) {
		this.totalModifiedDirectCost = totalModifiedDirectCost;
	}
//need to be removed
	public BigDecimal getFundRequestedIncludingBenifit() {
		BigDecimal fundRequestedIncludingBenifit = BigDecimal.ZERO;
		for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetailCalcAmounts) {
			if (!isSystemGeneratedCostElement
					&& (Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS.equals(budgetDetailCalcAmount.getRateClassCode()))) {				
				if (budgetDetailCalcAmount.getCalculatedFundRequested()!=null) {
				fundRequestedIncludingBenifit = budgetDetailCalcAmount.getCalculatedFundRequested().add(totalFundRequested);
				}
			}
		}
		return fundRequestedIncludingBenifit;
	}

	public void setFundRequestedIncludingBenifit(BigDecimal fundRequestedIncludingBenifit) {
		this.fundRequestedIncludingBenifit = fundRequestedIncludingBenifit;
	}

	public BigDecimal getSponsorRequestedAmount() {
		return sponsorRequestedAmount;
	}

	public void setSponsorRequestedAmount(BigDecimal sponsorRequestedAmount) {
		this.sponsorRequestedAmount = sponsorRequestedAmount;
	}

	public BigDecimal getCalculateCostSharingWithBenifit() {
		calculateCostSharingWithBenifit = BigDecimal.ZERO;
		for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetailCalcAmounts) {
			if (!isSystemGeneratedCostElement && (Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS.equals(budgetDetailCalcAmount.getRateClassCode()))) {
				if (budgetDetailCalcAmount.getCalculatedCostSharing() != null) {
					calculateCostSharingWithBenifit = budgetDetailCalcAmount.getCalculatedCostSharing().add(costSharingAmount);
				}
			}
		}
		if (calculateCostSharingWithBenifit.equals(BigDecimal.ZERO)) {
			if (costSharingAmount != null) {
				calculateCostSharingWithBenifit = costSharingAmount;
			} else {
				calculateCostSharingWithBenifit = BigDecimal.ZERO;
			}
		}
		return calculateCostSharingWithBenifit;
	}

	public void setCalculateCostSharingWithBenifit(BigDecimal calculateCostSharingWithBenifit) {
		this.calculateCostSharingWithBenifit = calculateCostSharingWithBenifit;
	}

	public BigDecimal getCalculateFundRequestedIncludingBenifit() {
		BigDecimal calculateFundRequestedIncludingBenifit = BigDecimal.ZERO;
		for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetailCalcAmounts) {
			if (!isSystemGeneratedCostElement
					&& (Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS.equals(budgetDetailCalcAmount.getRateClassCode()) && sponsorRequestedAmount != null)) {
				if (budgetDetailCalcAmount.getCalculatedFundRequested()!=null) {
					calculateFundRequestedIncludingBenifit = budgetDetailCalcAmount.getCalculatedFundRequested().add(sponsorRequestedAmount);
				}
			}
		}
		if ((calculateFundRequestedIncludingBenifit.equals(BigDecimal.ZERO) && sponsorRequestedAmount != null)) {
			calculateFundRequestedIncludingBenifit = sponsorRequestedAmount;
		}
		return calculateFundRequestedIncludingBenifit;
	}

	public void setCalculateFundRequestedIncludingBenifit(BigDecimal calculateFundRequestedIncludingBenifit) {
		this.calculateFundRequestedIncludingBenifit = calculateFundRequestedIncludingBenifit;
	}

	public BigDecimal getCalculateLineItemCostIncludingBenifit() {
		BigDecimal calculateLineItemCostIncludingBenifit = BigDecimal.ZERO;
		for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetailCalcAmounts) {
			if (!isSystemGeneratedCostElement && (Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS.equals(budgetDetailCalcAmount.getRateClassCode())
							&& lineItemCost != null)) {
				if (budgetDetailCalcAmount.getCalculatedCost()!=null) {
					calculateLineItemCostIncludingBenifit = budgetDetailCalcAmount.getCalculatedCost().add(lineItemCost);
				}
			}
		}
		if ((calculateLineItemCostIncludingBenifit.equals(BigDecimal.ZERO) && lineItemCost != null)) {
			calculateLineItemCostIncludingBenifit = lineItemCost;
		}
		return calculateLineItemCostIncludingBenifit;
	}

	public void setCalculateLineItemCostIncludingBenifit(BigDecimal calculateLineItemCostIncludingBenifit) {
		this.calculateLineItemCostIncludingBenifit = calculateLineItemCostIncludingBenifit;
	}

	public BigDecimal getLineItemCost() {
		return lineItemCost;
	}

	public void setLineItemCost(BigDecimal lineItemCost) {
		this.lineItemCost = lineItemCost;
	}

	public BigDecimal getCeBaseRate() {
		return ceBaseRate;
	}

	public void setCeBaseRate(BigDecimal ceBaseRate) {
		this.ceBaseRate = ceBaseRate;
	}

	public BigDecimal getPrevLineItemCost() {
		return prevLineItemCost;
	}

	public void setPrevLineItemCost(BigDecimal prevLineItemCost) {
		this.prevLineItemCost = prevLineItemCost;
	}

}
