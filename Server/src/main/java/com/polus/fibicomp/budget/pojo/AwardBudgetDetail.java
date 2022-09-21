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
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_BUDGET_DETAIL")
public class AwardBudgetDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_DETAILS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer budgetDetailId;

	@Column(name = "BUDGET_PERIOD_ID")
	private Integer budgetPeriodId;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

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
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_DETAIL_FK2"), name = "BUDGET_CATEGORY_CODE", referencedColumnName = "BUDGET_CATEGORY_CODE", insertable = false, updatable = false)
	private BudgetCategory budgetCategory;

	@Column(name = "COST_ELEMENT")
	private String costElementCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_DETAIL_FK3"), name = "COST_ELEMENT", referencedColumnName = "COST_ELEMENT", insertable = false, updatable = false)
	private CostElement costElement;

	@Column(name = "LINE_ITEM_DESCRIPTION")
	private String lineItemDescription;

	@Column(name = "LINE_ITEM_COST", precision = 12, scale = 2)
	private BigDecimal lineItemCost;

	/*
	 * @Column(name = "COST_ELEMENT_BASE", precision = 12, scale = 2) private
	 * BigDecimal ceBaseRate;
	 */

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
	private BigDecimal costSharingAmount;

	@Column(name = "COST_SHARING_PERCENT", precision = 12, scale = 2)
	private BigDecimal costSharingPercentage;

	@Transient
	private List<AwardBudgetDetailCalcAmount> budgetDetailCalcAmounts;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetDetail", orphanRemoval = true, cascade = {
			CascadeType.REMOVE }, fetch = FetchType.LAZY)
	private List<AwardBudgetRateAndBase> budgetRateAndBases;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetDetail", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<AwardBudgetPersonalDetail> personsDetails;

	@Column(name = "SYSTEM_GEN_COST_ELEMENT_TYPE")
	private String systemGeneratedCEType;

	/*
	 * @Column(name = "PERSON_ID") private String personId;
	 * 
	 * @Column(name = "ROLODEX_ID") private Integer rolodexId;
	 * 
	 * @Column(name = "FULL_NAME") private String fullName;
	 * 
	 * @Column(name = "PERSON_TYPE") // E - Employee, N - Non Employee, T - To Be
	 * Named private String personType;
	 */

	@Column(name = "TBN_ID")
	private String tbnId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_DETAIL_FK5"), name = "TBN_ID", referencedColumnName = "TBN_ID", insertable = false, updatable = false)
	private TbnPerson tbnPerson;

	@Column(name = "IS_APPLY_INFLATION_RATE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isApplyInflationRate = true;

	@Column(name = "QUANTITY", precision = 6, scale = 2)
	private BigDecimal quantity;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetId;

	@Transient
	private BigDecimal balanceToDate;

	@JsonIgnore
	@OneToMany(mappedBy = "awardBudgetDetail", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmount;

	@JsonManagedReference
	@OneToMany(mappedBy = "awardnonPersonBudgetDetail", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<AwardBudgetNonPersonDetail> nonPersonsDetails;

	@Transient
	private Boolean isIOCodeExistInManpower = false;

	@Transient
	private BigDecimal awardManpowerCommittedCost = BigDecimal.ZERO;

	@Transient
	private Boolean isManpowerResourceExistInManpower = false;

	@Transient
	private Integer previousAwardBudgetDetailId;

	public AwardBudgetDetail() {
		budgetDetailCalcAmounts = new ArrayList<>();
		budgetRateAndBases = new ArrayList<>();
		personsDetails = new ArrayList<>();
		nonPersonsDetails = new ArrayList<>();
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

	public List<AwardBudgetDetailCalcAmount> getBudgetDetailCalcAmounts() {
		return budgetDetailCalcAmounts;
	}

	public void setBudgetDetailCalcAmounts(List<AwardBudgetDetailCalcAmount> budgetDetailCalcAmounts) {
		this.budgetDetailCalcAmounts = budgetDetailCalcAmounts;
	}

	public List<AwardBudgetRateAndBase> getBudgetRateAndBases() {
		return budgetRateAndBases;
	}

	public void setBudgetRateAndBases(List<AwardBudgetRateAndBase> budgetRateAndBases) {
		this.budgetRateAndBases = budgetRateAndBases;
	}

	public String getSystemGeneratedCEType() {
		return systemGeneratedCEType;
	}

	public void setSystemGeneratedCEType(String systemGeneratedCEType) {
		this.systemGeneratedCEType = systemGeneratedCEType;
	}

	/*
	 * public String getPersonId() { return personId; }
	 * 
	 * public void setPersonId(String personId) { this.personId = personId; }
	 * 
	 * public Integer getRolodexId() { return rolodexId; }
	 * 
	 * public void setRolodexId(Integer rolodexId) { this.rolodexId = rolodexId; }
	 * 
	 * public String getFullName() { return fullName; }
	 * 
	 * public void setFullName(String fullName) { this.fullName = fullName; }
	 * 
	 * public String getPersonType() { return personType; }
	 * 
	 * public void setPersonType(String personType) { this.personType = personType;
	 * }
	 */

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

	public BigDecimal getCostSharingAmount() {
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

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public BigDecimal getQuantity() {
		return quantity;
	}

	public void setQuantity(BigDecimal quantity) {
		this.quantity = quantity;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public BigDecimal getBalanceToDate() {
		return balanceToDate;
	}

	public void setBalanceToDate(BigDecimal balanceToDate) {
		this.balanceToDate = balanceToDate;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public List<AwardBudgetPersonalDetail> getPersonsDetails() {
		return personsDetails;
	}

	public void setPersonsDetails(List<AwardBudgetPersonalDetail> personsDetails) {
		this.personsDetails = personsDetails;
	}

	/*
	 * public BigDecimal getCeBaseRate() { return ceBaseRate; }
	 * 
	 * public void setCeBaseRate(BigDecimal ceBaseRate) { this.ceBaseRate =
	 * ceBaseRate; }
	 */

	public BigDecimal getLineItemCost() {
		return lineItemCost;
	}

	public void setLineItemCost(BigDecimal lineItemCost) {
		this.lineItemCost = lineItemCost;
	}

	public BigDecimal getPrevLineItemCost() {
		return prevLineItemCost;
	}

	public void setPrevLineItemCost(BigDecimal prevLineItemCost) {
		this.prevLineItemCost = prevLineItemCost;
	}

	public List<AwardBudgetDetailCalcAmount> getAwardBudgetDetailCalcAmount() {
		return awardBudgetDetailCalcAmount;
	}

	public void setAwardBudgetDetailCalcAmount(List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmount) {
		this.awardBudgetDetailCalcAmount = awardBudgetDetailCalcAmount;
	}

	public List<AwardBudgetNonPersonDetail> getNonPersonsDetails() {
		return nonPersonsDetails;
	}

	public void setNonPersonsDetails(List<AwardBudgetNonPersonDetail> nonPersonsDetails) {
		this.nonPersonsDetails = nonPersonsDetails;
	}

	public Boolean getIsIOCodeExistInManpower() {
		return isIOCodeExistInManpower;
	}

	public void setIsIOCodeExistInManpower(Boolean isIOCodeExistInManpower) {
		this.isIOCodeExistInManpower = isIOCodeExistInManpower;
	}

	public BigDecimal getAwardManpowerCommittedCost() {
		return awardManpowerCommittedCost;
	}

	public void setAwardManpowerCommittedCost(BigDecimal awardManpowerCommittedCost) {
		this.awardManpowerCommittedCost = awardManpowerCommittedCost;
	}

	public Boolean getIsManpowerResourceExistInManpower() {
		return isManpowerResourceExistInManpower;
	}

	public void setIsManpowerResourceExistInManpower(Boolean isManpowerResourceExistInManpower) {
		this.isManpowerResourceExistInManpower = isManpowerResourceExistInManpower;
	}

	public Integer getPreviousAwardBudgetDetailId() {
		return previousAwardBudgetDetailId;
	}

	public void setPreviousAwardBudgetDetailId(Integer previousAwardBudgetDetailId) {
		this.previousAwardBudgetDetailId = previousAwardBudgetDetailId;
	}


}
