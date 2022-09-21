package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_SUMMARY_DETAILS")
@EntityListeners(AuditingEntityListener.class)
public class ClaimSummaryDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "CLAIM_SUMARY_DTLS_ID_GENERATOR")
	@SequenceGenerator(name = "CLAIM_SUMARY_DTLS_ID_GENERATOR", sequenceName = "CLAIM_SUMARY_DTLS_ID_GENERATOR", allocationSize = 1)
	@Column(name = "CLAIM_DETAILS_ID")
	private Integer claimDetailsId;
	
	@Column(name = "CLAIM_SUMMARY_ID")
	private Integer claimSummaryId;

	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;

	@Column(name = "BUDGET_CATEGORY_CODE")
	private String budgetCategoryCode;

	@Column(name = "AWARD_EXPENSE_TRANS_ID")
	private Integer awardExpenseTransId;

	@Column(name = "LEVEL_OF_SUPPORT_PERCENT")
	private BigDecimal levelOfSupportPercentage = BigDecimal.ZERO; 

	@Column(name = "TOTAL_AMOUNT", precision = 12, scale = 2)
	private BigDecimal totalAmount = BigDecimal.ZERO; 

	@Column(name = "ADJUSTED_TOTAL", precision = 12, scale = 2)
	private BigDecimal adjustedTotal = BigDecimal.ZERO; 

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_EXCLUDED_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isExcludedFlag;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "INVOICE_DATE")
	private Date invoiceDate;
	
	@Column(name = "BUDGET_APPROVAL_DATE")
	private Date budgetApprovalDate;
	
	@Column(name = "DOCUMENT_NUMBER")
	private String documentNumber;
	
	@Column(name = "FM_POSTING_DATE")
	private Date postingDate;
	
	@Column(name = "PAYMENT_DATE")
	private Date paymentDate;

	@Column(name = "DELIVERY_DATE")
	private Date deliveryDate;

	@Column(name = "VENDOR_NAME")
	private String vendorName;

	@Column(name = "EXPENSE_ITEM_DESCRIPTION")
	private String expenseItemDescription;

	@Column(name = "ORIGINAL_LINE_ITEM_DESCRIPTION")
	private String orginalLineItemDescription;

	@Column(name = "COUNTRY")
	private String country;

	@Column(name = "TRAVEL_TYPE")
	private String travelType;

	@Column(name = "PERIOD_OF_VISIT")
	private String periodOfVisit;

	@Column(name = "NUMBER_OF_DAYS_OF_OFFICIAL_VISIT")
	private Integer numberOfDaysOfOfficialVisit;

	@Column(name = "TRIP_TOTAL_COST", precision = 12, scale = 2)
	private BigDecimal tripTotalCost = BigDecimal.ZERO;

	@Column(name = "EXPENSE_CAPPED_AMOUNT", precision = 12, scale = 2)
	private BigDecimal expenseCaped = BigDecimal.ZERO;

	@Column(name = "QUALIFYING_COST", precision = 12, scale = 2)
	private BigDecimal qualifyingCost = BigDecimal.ZERO;

	@Column(name = "PREV_EXLUDED_SUMMARY_DTL_ID")
	private Integer prevExcludedSummaryDetId;

	@Column(name = "PREV_ADJUSTED_SUMMARY_DTL_ID")
	private Integer prevAdjustedSummaryDetId;

	@Column(name = "MANPOWER_PAYROLL_ID")
	private Integer manpowerPayrollId;

	@Column(name = "ENCRYPTED_AMOUNT")
	private String encryptedAmount;

	@Column(name = "UNIT_PRICE", precision = 12, scale = 2)
	private BigDecimal unitPrice = BigDecimal.ZERO;

	@Column(name = "DESCRIPTION_OF_EQUIPMENT")
	private String descriptionOfEquipment;
		   
	@Column(name = "CITY")
	private String city;

	@Column(name = "ORGANIZATION_OR_CONFERENCE_NAME")
	private String organizationOrConferenceName;
	
	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCodes;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_SUMMARY_DETAILS_FK2"), name = "CLAIM_SUMMARY_ID", referencedColumnName = "CLAIM_SUMMARY_ID", insertable = false, updatable = false)
	private ClaimSummary claimSummary;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_SUMMARY_DETAILS_FK3"), name = "AWARD_EXPENSE_TRANS_ID", referencedColumnName = "AWARD_EXPENSE_TRANS_ID", insertable = false, updatable = false)
	private AwardExpenseTransaction awardExpenseTransaction;
	
	@Column(name = "PERIOD_OF_OFFICIAL_VISIT")
	private String periodOfOfficialVisit;
	
	@Column(name = "NUMBER_OF_DAYS_OF_VISIT")
	private Integer numberOfDaysOfVisit;
	
	@Column(name = "COLA_RATE_PER_DAY", precision = 12, scale = 2)
	private BigDecimal colaRatePerDay = BigDecimal.ZERO;

	private transient String internalOrderCode;
	
	private transient String fullNameEOM = "";
	
	private transient String jobDescription = "";
	
	private transient Integer approvedHeadCount;
	
	private transient Integer actualHeadCount;
	
	private transient String personId;
	
	private transient String glAccountCode;
	
	private transient String descriptionOfExpenditure;
		
	private transient BigDecimal approvedBudget = BigDecimal.ZERO;

	private transient String involvementPeriod;
	
	private transient String involvementPeriodForTemplate;

	private transient String expenseCode;

	public String getInvolvementPeriodForTemplate() {
		return involvementPeriodForTemplate;
	}

	public void setInvolvementPeriodForTemplate(String involvementPeriodForTemplate) {
		this.involvementPeriodForTemplate = involvementPeriodForTemplate;
	}

	public String getJobDescription() {
		return jobDescription;
	}

	public void setJobDescription(String jobDescription) {
		this.jobDescription = jobDescription;
	}

	public Integer getClaimDetailsId() {
		return claimDetailsId;
	}

	public void setClaimDetailsId(Integer claimDetailsId) {
		this.claimDetailsId = claimDetailsId;
	}

	public Integer getClaimSummaryId() {
		return claimSummaryId;
	}

	public void setClaimSummaryId(Integer claimSummaryId) {
		this.claimSummaryId = claimSummaryId;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public Integer getAwardExpenseTransId() {
		return awardExpenseTransId;
	}

	public void setAwardExpenseTransId(Integer awardExpenseTransId) {
		this.awardExpenseTransId = awardExpenseTransId;
	}

	public BigDecimal getLevelOfSupportPercentage() {
		return levelOfSupportPercentage;
	}

	public void setLevelOfSupportPercentage(BigDecimal levelOfSupportPercentage) {
		this.levelOfSupportPercentage = levelOfSupportPercentage;
	}

	public BigDecimal getTotalAmount() {
		return totalAmount;
	}

	public void setTotalAmount(BigDecimal totalAmount) {
		this.totalAmount = totalAmount;
	}

	public BigDecimal getAdjustedTotal() {
		return adjustedTotal;
	}

	public void setAdjustedTotal(BigDecimal adjustedTotal) {
		this.adjustedTotal = adjustedTotal;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getIsExcludedFlag() {
		return isExcludedFlag;
	}

	public void setIsExcludedFlag(Boolean isExcludedFlag) {
		this.isExcludedFlag = isExcludedFlag;
	}
	
	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public ClaimSummary getClaimSummary() {
		return claimSummary;
	}

	public void setClaimSummary(ClaimSummary claimSummary) {
		this.claimSummary = claimSummary;
	}

	public AwardExpenseTransaction getAwardExpenseTransaction() {
		return awardExpenseTransaction;
	}

	public void setAwardExpenseTransaction(AwardExpenseTransaction awardExpenseTransaction) {
		this.awardExpenseTransaction = awardExpenseTransaction;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public Date getInvoiceDate() {
		return invoiceDate;
	}

	public void setInvoiceDate(Date invoiceDate) {
		this.invoiceDate = invoiceDate;
	}

	public Date getBudgetApprovalDate() {
		return budgetApprovalDate;
	}

	public void setBudgetApprovalDate(Date budgetApprovalDate) {
		this.budgetApprovalDate = budgetApprovalDate;
	}

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public Date getPostingDate() {
		return postingDate;
	}

	public void setPostingDate(Date postingDate) {
		this.postingDate = postingDate;
	}

	public Date getPaymentDate() {
		return paymentDate;
	}

	public void setPaymentDate(Date paymentDate) {
		this.paymentDate = paymentDate;
	}

	public Date getDeliveryDate() {
		return deliveryDate;
	}

	public void setDeliveryDate(Date deliveryDate) {
		this.deliveryDate = deliveryDate;
	}

	public String getVendorName() {
		return vendorName;
	}

	public void setVendorName(String vendorName) {
		this.vendorName = vendorName;
	}

	public String getExpenseItemDescription() {
		return expenseItemDescription;
	}

	public void setExpenseItemDescription(String expenseItemDescription) {
		this.expenseItemDescription = expenseItemDescription;
	}

	public String getOrginalLineItemDescription() {
		return orginalLineItemDescription;
	}

	public void setOrginalLineItemDescription(String orginalLineItemDescription) {
		this.orginalLineItemDescription = orginalLineItemDescription;
	}

	public String getTravelType() {
		return travelType;
	}

	public void setTravelType(String travelType) {
		this.travelType = travelType;
	}

	public String getPeriodOfVisit() {
		return periodOfVisit;
	}

	public void setPeriodOfVisit(String periodOfVisit) {
		this.periodOfVisit = periodOfVisit;
	}

	public Integer getNumberOfDaysOfVisit() {
		return numberOfDaysOfVisit;
	}

	public void setNumberOfDaysOfVisit(Integer numberOfDaysOfVisit) {
		this.numberOfDaysOfVisit = numberOfDaysOfVisit;
	}

	public BigDecimal getTripTotalCost() {
		return tripTotalCost;
	}

	public void setTripTotalCost(BigDecimal tripTotalCost) {
		this.tripTotalCost = tripTotalCost;
	}

	public BigDecimal getExpenseCaped() {
		return expenseCaped;
	}

	public void setExpenseCaped(BigDecimal expenseCaped) {
		this.expenseCaped = expenseCaped;
	}

	public Integer getPrevExcludedSummaryDetId() {
		return prevExcludedSummaryDetId;
	}

	public void setPrevExcludedSummaryDetId(Integer prevExcludedSummaryDetId) {
		this.prevExcludedSummaryDetId = prevExcludedSummaryDetId;
	}

	public Integer getPrevAdjustedSummaryDetId() {
		return prevAdjustedSummaryDetId;
	}

	public void setPrevAdjustedSummaryDetId(Integer prevAdjustedSummaryDetId) {
		this.prevAdjustedSummaryDetId = prevAdjustedSummaryDetId;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public BigDecimal getQualifyingCost() {
		return qualifyingCost;
	}

	public void setQualifyingCost(BigDecimal qualifyingCost) {
		this.qualifyingCost = qualifyingCost;
	}

	public String getFullNameEOM() {
		return fullNameEOM;
	}

	public void setFullNameEOM(String fullNameEOM) {
		this.fullNameEOM = fullNameEOM;
	}

	public Integer getApprovedHeadCount() {
		return approvedHeadCount;
	}

	public void setApprovedHeadCount(Integer approvedHeadCount) {
		this.approvedHeadCount = approvedHeadCount;
	}

	public Integer getActualHeadCount() {
		return actualHeadCount;
	}

	public void setActualHeadCount(Integer actualHeadCount) {
		this.actualHeadCount = actualHeadCount;
	}

	public String getGlAccountCode() {
		return glAccountCode;
	}

	public void setGlAccountCode(String glAccountCode) {
		this.glAccountCode = glAccountCode;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getManpowerPayrollId() {
		return manpowerPayrollId;
	}

	public void setManpowerPayrollId(Integer manpowerPayrollId) {
		this.manpowerPayrollId = manpowerPayrollId;
	}

	public String getEncryptedAmount() {
		return encryptedAmount;
	}

	public void setEncryptedAmount(String encryptedAmount) {
		this.encryptedAmount = encryptedAmount;
	}

	public String getDescriptionOfExpenditure() {
		return descriptionOfExpenditure;
	}

	public void setDescriptionOfExpenditure(String descriptionOfExpenditure) {
		this.descriptionOfExpenditure = descriptionOfExpenditure;
	}

	public BigDecimal getApprovedBudget() {
		return approvedBudget;
	}

	public void setApprovedBudget(BigDecimal approvedBudget) {
		this.approvedBudget = approvedBudget;
	}

	public String getInvolvementPeriod() {
		return involvementPeriod;
	}

	public void setInvolvementPeriod(String involvementPeriod) {
		this.involvementPeriod = involvementPeriod;
	}

	public BigDecimal getUnitPrice() {
		return unitPrice;
	}

	public void setUnitPrice(BigDecimal unitPrice) {
		this.unitPrice = unitPrice;
	}

	public String getDescriptionOfEquipment() {
		return descriptionOfEquipment;
	}

	public void setDescriptionOfEquipment(String descriptionOfEquipment) {
		this.descriptionOfEquipment = descriptionOfEquipment;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getOrganizationOrConferenceName() {
		return organizationOrConferenceName;
	}

	public void setOrganizationOrConferenceName(String organizationOrConferenceName) {
		this.organizationOrConferenceName = organizationOrConferenceName;
	}

	public String getExpenseCode() {
		return expenseCode;
	}

	public void setExpenseCode(String expenseCode) {
		this.expenseCode = expenseCode;
	}

	public Integer getNumberOfDaysOfOfficialVisit() {
		return numberOfDaysOfOfficialVisit;
	}

	public void setNumberOfDaysOfOfficialVisit(Integer numberOfDaysOfOfficialVisit) {
		this.numberOfDaysOfOfficialVisit = numberOfDaysOfOfficialVisit;
	}

	public String getPeriodOfOfficialVisit() {
		return periodOfOfficialVisit;
	}

	public void setPeriodOfOfficialVisit(String periodOfOfficialVisit) {
		this.periodOfOfficialVisit = periodOfOfficialVisit;
	}

	public BigDecimal getColaRatePerDay() {
		return colaRatePerDay;
	}

	public void setColaRatePerDay(BigDecimal colaRatePerDay) {
		this.colaRatePerDay = colaRatePerDay;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	public String getInternalOrderCodes() {
		return internalOrderCodes;
	}

	public void setInternalOrderCodes(String internalOrderCodes) {
		this.internalOrderCodes = internalOrderCodes;
	}

}

