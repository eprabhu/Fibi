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
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_BUDGET_HEADER")
public class AwardBudgetHeader implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_HEADER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer budgetId;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "OBLIGATED_TOTAL", precision = 12, scale = 2)
	private BigDecimal obligatedTotal;

	@Column(name = "OBLIGATED_CHANGE", precision = 12, scale = 2)
	private BigDecimal obligatedChange;

	@Column(name = "AWARD_BUDGET_STATUS_CODE")
	private String budgetStatusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_HEADER_FK1"), name = "AWARD_BUDGET_STATUS_CODE", referencedColumnName = "AWARD_BUDGET_STATUS_CODE", insertable = false, updatable = false)
	private AwardBudgetStatus budgetStatus;

	@Column(name = "IS_AUTO_CALC")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isAutoCalc = false;

	@Column(name = "IS_LATEST_VERSION")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isLatestVersion = false;

	@Column(name = "BUDGET_TYPE_CODE")
	private String budgetTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_HEADER_FK2"), name = "BUDGET_TYPE_CODE", referencedColumnName = "BUDGET_TYPE_CODE", insertable = false, updatable = false)
	private BudgetType budgetType;

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
	
	@Column(name = "TOTAL_COST_SHARE", precision = 12, scale = 2)
	private BigDecimal totalCostShare;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "ON_OFF_CAMPUS_FLAG")
	// @Convert(converter = JpaCharBooleanConversion.class)
	private String onOffCampusFlag;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@Column(name = "CREATE_USER")
	private String createUser;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "RATE_CLASS_CODE")
	private String rateClassCode;

	@Column(name = "RATE_TYPE_CODE")
	private String rateTypeCode;

	@Column(name = "ANTICIPATED_TOTAL", precision = 10, scale = 2)
	private BigDecimal anticipatedTotal;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumns(foreignKey = @ForeignKey(name = "AWARD_BUDGET_HEADER_FK3"), value = {
			@JoinColumn(name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false),
			@JoinColumn(name = "RATE_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
	private RateType rateType;

	@Column(name = "AVAILABLE_FUND_TYPE")
	private String availableFundType;
	
	@Column(name = "VIREMENT", precision = 5, scale = 2)
	private BigDecimal virement = BigDecimal.ZERO;

	@Column(name = "CUMULATIVE_VIREMENT", precision = 5, scale = 2)
	private BigDecimal cumulativeVirement = BigDecimal.ZERO;

	@Column(name = "BUDGET_TEMPLATE_TYPE_ID")
	private Integer budgetTemplateTypeId;

	@Column(name = "ON_CAMPUS_RATES")
	private String onCampusRates;

	@Column(name = "OFF_CAMPUS_RATES")
	private String offCampusRates;

	@Column(name = "COST_SHARE_TYPE_CODE")
	private Integer costSharingTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_HEADER_FK4"), name = "COST_SHARE_TYPE_CODE", referencedColumnName = "COST_SHARE_TYPE_CODE", insertable = false, updatable = false)
	private CostSharingType costSharingType;

	@Column(name = "FUND_DISBURSEMENT_BASIS_TYPE_CODE" )
	private String fundDisbursementBasisTypeCode;

	@ManyToOne()
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_HEADER_FK5"), name = "FUND_DISBURSEMENT_BASIS_TYPE_CODE", referencedColumnName = "FUND_DISBURSEMENT_BASIS_TYPE_CODE", insertable = false, updatable = false)
	private FundDisbursementBasisType fundDisbursementBasisType;

	@Transient
	private List<AwardBudgetPeriod> budgetPeriods;

	@Transient
	private String fundCode;

	@Transient
	private String fundCenter;

	@Transient
	private String awardBudgetType;

	@Transient
	private String awardBudgetStatus;

	@Transient
	private String awardRateType;

	@Transient
	private String createUserName;

	@Transient
	private String updateUserName;

	@Transient
	private List<AwardRates> awardRates;
	
	@Transient
	private BigDecimal availableFund;

	@Transient
	private Boolean enableAwardBudgetVirementCalculation;

	public Boolean getEnableAwardBudgetVirementCalculation() {
		return enableAwardBudgetVirementCalculation;
	}

	public void setEnableAwardBudgetVirementCalculation(Boolean enableAwardBudgetVirementCalculation) {
		this.enableAwardBudgetVirementCalculation = enableAwardBudgetVirementCalculation;
	}

	@Transient
	private BigDecimal initialAvailableFund;
	
	@Transient
	private Boolean manpowerEnabled;
	
	@Transient
	private Boolean budgetAssociatedWithManpower;

	public AwardBudgetHeader() {
		budgetPeriods = new ArrayList<>();
		awardRates = new ArrayList<>();
	}

	public AwardBudgetHeader(Integer budgetId, Integer versionNumber) {
		super();
		this.budgetId = budgetId;
		this.versionNumber = versionNumber;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getBudgetStatusCode() {
		return budgetStatusCode;
	}

	public void setBudgetStatusCode(String budgetStatusCode) {
		this.budgetStatusCode = budgetStatusCode;
	}

	public AwardBudgetStatus getBudgetStatus() {
		return budgetStatus;
	}

	public void setBudgetStatus(AwardBudgetStatus budgetStatus) {
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

	public String getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(String onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
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

	public BigDecimal getAnticipatedTotal() {
		return anticipatedTotal;
	}

	public void setAnticipatedTotal(BigDecimal anticipatedTotal) {
		this.anticipatedTotal = anticipatedTotal;
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

	public List<AwardBudgetPeriod> getBudgetPeriods() {
		return budgetPeriods;
	}

	public void setBudgetPeriods(List<AwardBudgetPeriod> budgetPeriods) {
		this.budgetPeriods = budgetPeriods;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
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

	public String getAwardBudgetType() {
		return awardBudgetType;
	}

	public void setAwardBudgetType(String awardBudgetType) {
		this.awardBudgetType = awardBudgetType;
	}

	public String getAwardBudgetStatus() {
		return awardBudgetStatus;
	}

	public void setAwardBudgetStatus(String awardBudgetStatus) {
		this.awardBudgetStatus = awardBudgetStatus;
	}

	public String getAwardRateType() {
		return awardRateType;
	}

	public void setAwardRateType(String awardRateType) {
		this.awardRateType = awardRateType;
	}

	public String getFundCode() {
		return fundCode;
	}

	public void setFundCode(String fundCode) {
		this.fundCode = fundCode;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public Boolean getIsLatestVersion() {
		return isLatestVersion;
	}

	public void setIsLatestVersion(Boolean isLatestVersion) {
		this.isLatestVersion = isLatestVersion;
	}

	public List<AwardRates> getAwardRates() {
		return awardRates;
	}

	public void setAwardRates(List<AwardRates> awardRates) {
		this.awardRates = awardRates;
	}

	public String getAvailableFundType() {
		return availableFundType;
	}

	public void setAvailableFundType(String availableFundType) {
		this.availableFundType = availableFundType;
	}

	public BigDecimal getAvailableFund() {
		return availableFund;
	}

	public void setAvailableFund(BigDecimal availableFund) {
		this.availableFund = availableFund;
	}

	public BigDecimal getVirement() {
		return virement;
	}

	public void setVirement(BigDecimal virement) {
		this.virement = virement;
	}

	public BigDecimal getCumulativeVirement() {
		return cumulativeVirement;
	}

	public void setCumulativeVirement(BigDecimal cumulativeVirement) {
		this.cumulativeVirement = cumulativeVirement;
	}

	public BigDecimal getInitialAvailableFund() {
		return initialAvailableFund;
	}

	public void setInitialAvailableFund(BigDecimal initialAvailableFund) {
		this.initialAvailableFund = initialAvailableFund;
	}

	public boolean manpowerEnabled() {
		return manpowerEnabled;
	}

	public void setManpowerEnabled(boolean manpowerEnabled) {
		this.manpowerEnabled = manpowerEnabled;
	}

	public Boolean getBudgetAssociatedWithManpower() {
		return budgetAssociatedWithManpower;
	}

	public void setBudgetAssociatedWithManpower(Boolean budgetAssociatedWithManpower) {
		this.budgetAssociatedWithManpower = budgetAssociatedWithManpower;
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

	public BigDecimal getTotalCostShare() {
		return totalCostShare;
	}

	public void setTotalCostShare(BigDecimal totalCostShare) {
		this.totalCostShare = totalCostShare;
	}

	public Boolean getManpowerEnabled() {
		return manpowerEnabled;
	}

	public void setManpowerEnabled(Boolean manpowerEnabled) {
		this.manpowerEnabled = manpowerEnabled;
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

	public String getFundDisbursementBasisTypeCode() {
		return fundDisbursementBasisTypeCode;
	}

	public void setFundDisbursementBasisTypeCode(String fundDisbursementBasisTypeCode) {
		this.fundDisbursementBasisTypeCode = fundDisbursementBasisTypeCode;
	}

	public FundDisbursementBasisType getFundDisbursementBasisType() {
		return fundDisbursementBasisType;
	}

	public void setFundDisbursementBasisType(FundDisbursementBasisType fundDisbursementBasisType) {
		this.fundDisbursementBasisType = fundDisbursementBasisType;
	}
}
