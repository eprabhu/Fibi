package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.award.pojo.Award;

@Entity
@Table(name = "CLAIM")
@EntityListeners(AuditingEntityListener.class)
public class Claim implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;
	
	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;
	
	@Column(name = "TITLE")
	private String title;
	
	@Column(name = "START_DATE")
	private Date startDate;
	
	@Column(name = "END_DATE")
	private Date endDate;
	
	@Column(name ="DURATION")
	private String duration;

	@Column(name = "HOST_UNIT_NUMBER")
	private String hostUnitNumber;
	
	@Column(name = "CLAIM_STATUS_CODE")
	private String claimStatusCode;

	@Column(name = "RECIPIENT_ORGANIZATION")
	private String recipientOrganization;

	@Column(name = "RECIPIENT_STREET_NAME")
	private String recipientStreetName;

	@Column(name = "RECIPIENT_STATE")
	private String recipientState;

	@Column(name = "RECIPIENT_POSTAL_CODE")
	private String recipientPostalCode;

	@Column(name = "PAYEE_ACCOUNT_NAME")
	private String payeeAccountName;
	
	@Column(name = "PAYEE_ACCOUNT_NUMBER")
	private String payeeAccountNumber;

	@Column(name = "PAYEE_BANK")
	private String payeeBank;

	@Column(name = "FUNDING_SCHEME_CERTIFICATION1")
	private String fundingSchemeCertification1;

	@Column(name = "FUNDING_SCHEME_CERTIFICATION2")
	private String fundingSchemeCertification2;

	@Column(name = "FUNDING_SCHEME_ENDORSEMENT")
	private String fundingSchemeEndorsement;

	@Column(name = "RSO_NAME")
	private String rsoName;

	@Column(name = "RSO_DESIGNATION")
	private String rsoDesignation;

	@Column(name = "RSO_EMAIL")
	private String rsoEmail;

	@Column(name = "RSO_APPROVAL_DATE")
	private Date rsoApprovalDate;

	@Column(name = "FO_NAME")
	private String foName;

	@Column(name = "FO_DESIGNATION")
	private String foDesignation;

	@Column(name = "FO_EMAIL")
	private String foEmail;

	@Column(name = "FO_APPROVAL_DATE")
	private Date foApprovalDate;

	@Column(name = "RDO_NAME")
	private String rdoName;

	@Column(name = "RDO_DESIGNATION")
	private String rdoDesignation;

	@Column(name = "RDO_EMAIL")
	private String rdoEmail;

	@Column(name = "RDO_APPROVAL_DATE")
	private Date rdoApprovalDate;

	@Column(name = "CLAIM_SUBMISSION_DATE")
	private Date claimSubmissionDate;

	@Column(name = "AUDITOR_NAME")
	private String auditorName;

	@Column(name = "EXTERNAL_AUDITOR_NAME")
	private String externalAuditorName;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Date createTimeStamp;

	@Column(name = "CLAIM_SUBMIT_USER")
	private String claimSubmitUser;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;
	
	@Column(name = "RSO_PHONE_NUMBER")
	private String rsoPhoneNumber;
	
	@Column(name = "FO_PHONE_NUMBER")
	private String foPhoneNumber;
	
	@Column(name = "RDO_PHONE_NUMBER")
	private String rdoPhoneNumber;
	
	@Column(name = "OVERHEAD_PERCENTAGE")
	private BigDecimal overHeadPercentage = BigDecimal.ZERO; 
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_FK1"), name = "CLAIM_STATUS_CODE", referencedColumnName = "CLAIM_STATUS_CODE", insertable = false, updatable = false)
	private ClaimStatus claimStatus;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_FK2"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;
	
	@Column(name = "TOTAL_AMOUNT")
	private BigDecimal totalAmount = BigDecimal.ZERO;

	@Column(name = "ADJUSTED_INDIRECT_COST")
	private BigDecimal adjustedIndirectCost = BigDecimal.ZERO;
	
	@Column(name = "IDC_CUM_CLAIM_AMT_UPTO_LATEST")
	private BigDecimal idcCumClaimAmtUptoLatest = BigDecimal.ZERO;
	
	@Column(name = "MIGRATED_REIM_PREV_IDC_AMT")
	private BigDecimal migratedReimPrevIdcAmt = BigDecimal.ZERO;
	
	@Column(name = "IDC_ORGINAL_AMOUNT")
	private BigDecimal idcOrginalAmount = BigDecimal.ZERO;
	
	@Column(name = "IDC_LATEST_AMOUNT")
	private BigDecimal idcLatestAmount = BigDecimal.ZERO;
	
	@Column(name = "IDC_CUM_CLAIM_AMT_UPTO_CLAIM")
	private BigDecimal idcCumClaimAmtUptoClaim = BigDecimal.ZERO;

	@Column(name = "IDC_CUM_EXP_UPTO_PREV_CLAIM")
	private BigDecimal idcCumExpUptoPrevClaim = BigDecimal.ZERO;

	@Column(name = "IDC_COMMITMENT_UPTO_PREV_CLAIM")
	private BigDecimal idcCommitmentUptoPrevClaim = BigDecimal.ZERO;

	@Column(name = "IDC_AMOUNT_REQUESTED")
	private BigDecimal idcAmountRequested = BigDecimal.ZERO;

	@Column(name = "IDC_AMOUNT_FORCASTED")
	private BigDecimal idcAmountForcasted = BigDecimal.ZERO;

	@Column(name = "FUNDER_APPROVAL_DATE")
	private Date funderApprovalDate;
	
	@Column(name = "PAYMENT_DATE")
	private Date paymentDate;
	
	@Column(name = "DOCUMENT_DATE")
	private Date documentDate;
	
	@Transient
	private String submitUserFullName;
	
	@Transient
	private String hostUnitDescription;
	
	@Transient
	private String claimStartAndEndMonth;

	@Transient
	private String invoiceNumber;
	
	@Transient
	private Date invoiceDate;
	
	@Transient
	private Date fundReceivedDate;
	
	@Transient
	private Date awardStartDate;
	
	@Transient
	private Date awardEndDate;
	
	@Transient
	private String updateUserName;

	@Transient
	private String createUserName;
	
	@Transient
	private String outputDocNumber;
	
	@Transient
	private String claimStartAndEndYear;
	
	@Transient
	private BigDecimal subTotalDirectCost;
	
	@Transient
	private BigDecimal indirectCostOverHead;
	
	@Transient
	private BigDecimal totalDirectAndIndirectClaimed;
	
	@Transient
	private List<ClaimSummary> claimSummaryList;

	@Transient
	private Integer claimIndex;
	
	@Transient
	private String durationInMonths;

	@Transient
	private String claimHalfYearlyPeriod;

	@Transient
	private String financialYear;

	@Transient
	private List<LinkedHashMap<Object, Object>>  claimSummaryDetails;
	
	@Transient
	private ClaimSummary summaryOOEMACSumValues;

	@Transient
	private String quarter;
	
	@Transient
	private Map<String,Object> summaryCalculations;

	public Map<String, Object> getSummaryCalculations() {
		return summaryCalculations;
	}

	public void setSummaryCalculations(Map<String, Object> summaryCalculations) {
		this.summaryCalculations = summaryCalculations;
	}

	public ClaimSummary getSummaryOOEMACSumValues() {
		return summaryOOEMACSumValues;
	}

	public void setSummaryOOEMACSumValues(ClaimSummary summaryOOEMACSumValues) {
		this.summaryOOEMACSumValues = summaryOOEMACSumValues;
	}

	public String getDurationInMonths() {
		return durationInMonths;
	}

	public void setDurationInMonths(String durationInMonths) {
		this.durationInMonths = durationInMonths;
	}

	public BigDecimal getTotalDirectAndIndirectClaimed() {
		return totalDirectAndIndirectClaimed;
	}

	public void setTotalDirectAndIndirectClaimed(BigDecimal totalDirectAndIndirectClaimed) {
		this.totalDirectAndIndirectClaimed = totalDirectAndIndirectClaimed;
	}

	public BigDecimal getIndirectCostOverHead() {
		return indirectCostOverHead;
	}

	public void setIndirectCostOverHead(BigDecimal indirectCostOverHead) {
		this.indirectCostOverHead = indirectCostOverHead;
	}

	public BigDecimal getSubTotalDirectCost() {
		return subTotalDirectCost;
	}

	public void setSubTotalDirectCost(BigDecimal subTotalDirectCost) {
		this.subTotalDirectCost = subTotalDirectCost;
	}

	public List<ClaimSummary> getClaimSummaryList() {
		return claimSummaryList;
	}

	public void setClaimSummaryList(List<ClaimSummary> claimSummaryList) {
		this.claimSummaryList = claimSummaryList;
	}

	public String getClaimStartAndEndYear() {
		return claimStartAndEndYear;
	}

	public void setClaimStartAndEndYear(String claimStartAndEndYear) {
		this.claimStartAndEndYear = claimStartAndEndYear;
	}

	public BigDecimal getAdjustedIndirectCost() {
		return adjustedIndirectCost;
	}

	public void setAdjustedIndirectCost(BigDecimal adjustedIndirectCost) {
		this.adjustedIndirectCost = adjustedIndirectCost;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
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

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}

	public String getHostUnitNumber() {
		return hostUnitNumber;
	}

	public void setHostUnitNumber(String hostUnitNumber) {
		this.hostUnitNumber = hostUnitNumber;
	}

	public String getClaimStatusCode() {
		return claimStatusCode;
	}

	public void setClaimStatusCode(String claimStatusCode) {
		this.claimStatusCode = claimStatusCode;
	}

	public String getRecipientOrganization() {
		return recipientOrganization;
	}

	public void setRecipientOrganization(String recipientOrganization) {
		this.recipientOrganization = recipientOrganization;
	}

	public String getRecipientStreetName() {
		return recipientStreetName;
	}

	public void setRecipientStreetName(String recipientStreetName) {
		this.recipientStreetName = recipientStreetName;
	}

	public String getRecipientState() {
		return recipientState;
	}

	public void setRecipientState(String recipientState) {
		this.recipientState = recipientState;
	}

	public String getRecipientPostalCode() {
		return recipientPostalCode;
	}

	public void setRecipientPostalCode(String recipientPostalCode) {
		this.recipientPostalCode = recipientPostalCode;
	}

	public String getPayeeAccountName() {
		return payeeAccountName;
	}

	public void setPayeeAccountName(String payeeAccountName) {
		this.payeeAccountName = payeeAccountName;
	}

	public String getPayeeAccountNumber() {
		return payeeAccountNumber;
	}

	public void setPayeeAccountNumber(String payeeAccountNumber) {
		this.payeeAccountNumber = payeeAccountNumber;
	}

	public String getPayeeBank() {
		return payeeBank;
	}

	public void setPayeeBank(String payeeBank) {
		this.payeeBank = payeeBank;
	}

	public String getFundingSchemeCertification1() {
		return fundingSchemeCertification1;
	}

	public void setFundingSchemeCertification1(String fundingSchemeCertification1) {
		this.fundingSchemeCertification1 = fundingSchemeCertification1;
	}

	public String getFundingSchemeCertification2() {
		return fundingSchemeCertification2;
	}

	public void setFundingSchemeCertification2(String fundingSchemeCertification2) {
		this.fundingSchemeCertification2 = fundingSchemeCertification2;
	}

	public String getFundingSchemeEndorsement() {
		return fundingSchemeEndorsement;
	}

	public void setFundingSchemeEndorsement(String fundingSchemeEndorsement) {
		this.fundingSchemeEndorsement = fundingSchemeEndorsement;
	}

	public String getRsoName() {
		return rsoName;
	}

	public void setRsoName(String rsoName) {
		this.rsoName = rsoName;
	}

	public String getRsoDesignation() {
		return rsoDesignation;
	}

	public void setRsoDesignation(String rsoDesignation) {
		this.rsoDesignation = rsoDesignation;
	}

	public String getRsoEmail() {
		return rsoEmail;
	}

	public void setRsoEmail(String rsoEmail) {
		this.rsoEmail = rsoEmail;
	}

	public Date getRsoApprovalDate() {
		return rsoApprovalDate;
	}

	public void setRsoApprovalDate(Date rsoApprovalDate) {
		this.rsoApprovalDate = rsoApprovalDate;
	}

	public String getFoName() {
		return foName;
	}

	public void setFoName(String foName) {
		this.foName = foName;
	}

	public String getFoDesignation() {
		return foDesignation;
	}

	public void setFoDesignation(String foDesignation) {
		this.foDesignation = foDesignation;
	}

	public String getFoEmail() {
		return foEmail;
	}

	public void setFoEmail(String foEmail) {
		this.foEmail = foEmail;
	}

	public Date getFoApprovalDate() {
		return foApprovalDate;
	}

	public void setFoApprovalDate(Date foApprovalDate) {
		this.foApprovalDate = foApprovalDate;
	}

	public String getRdoName() {
		return rdoName;
	}

	public void setRdoName(String rdoName) {
		this.rdoName = rdoName;
	}

	public String getRdoDesignation() {
		return rdoDesignation;
	}

	public void setRdoDesignation(String rdoDesignation) {
		this.rdoDesignation = rdoDesignation;
	}

	public String getRdoEmail() {
		return rdoEmail;
	}

	public void setRdoEmail(String rdoEmail) {
		this.rdoEmail = rdoEmail;
	}

	public Date getRdoApprovalDate() {
		return rdoApprovalDate;
	}

	public void setRdoApprovalDate(Date rdoApprovalDate) {
		this.rdoApprovalDate = rdoApprovalDate;
	}

	public Date getClaimSubmissionDate() {
		return claimSubmissionDate;
	}

	public void setClaimSubmissionDate(Date claimSubmissionDate) {
		this.claimSubmissionDate = claimSubmissionDate;
	}

	public String getAuditorName() {
		return auditorName;
	}

	public void setAuditorName(String auditorName) {
		this.auditorName = auditorName;
	}

	public String getExternalAuditorName() {
		return externalAuditorName;
	}

	public void setExternalAuditorName(String externalAuditorName) {
		this.externalAuditorName = externalAuditorName;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Date getCreateTimeStamp() {
		return createTimeStamp;
	}

	public void setCreateTimeStamp(Date createTimeStamp) {
		this.createTimeStamp = createTimeStamp;
	}

	public String getClaimSubmitUser() {
		return claimSubmitUser;
	}

	public void setClaimSubmitUser(String claimSubmitUser) {
		this.claimSubmitUser = claimSubmitUser;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public ClaimStatus getClaimStatus() {
		return claimStatus;
	}

	public void setClaimStatus(ClaimStatus claimStatus) {
		this.claimStatus = claimStatus;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public String getRsoPhoneNumber() {
		return rsoPhoneNumber;
	}

	public void setRsoPhoneNumber(String rsoPhoneNumber) {
		this.rsoPhoneNumber = rsoPhoneNumber;
	}

	public String getFoPhoneNumber() {
		return foPhoneNumber;
	}

	public void setFoPhoneNumber(String foPhoneNumber) {
		this.foPhoneNumber = foPhoneNumber;
	}

	public String getRdoPhoneNumber() {
		return rdoPhoneNumber;
	}

	public void setRdoPhoneNumber(String rdoPhoneNumber) {
		this.rdoPhoneNumber = rdoPhoneNumber;
	}

	public BigDecimal getOverHeadPercentage() {
		return overHeadPercentage;
	}

	public void setOverHeadPercentage(BigDecimal overHeadPercentage) {
		this.overHeadPercentage = overHeadPercentage;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public String getHostUnitDescription() {
		return hostUnitDescription;
	}

	public void setHostUnitDescription(String hostUnitDescription) {
		this.hostUnitDescription = hostUnitDescription;
	}

	public String getClaimStartAndEndMonth() {
		return claimStartAndEndMonth;
	}

	public void setClaimStartAndEndMonth(String claimStartAndEndMonth) {
		this.claimStartAndEndMonth = claimStartAndEndMonth;
	}

	public String getInvoiceNumber() {
		return invoiceNumber;
	}

	public void setInvoiceNumber(String invoiceNumber) {
		this.invoiceNumber = invoiceNumber;
	}

	public Date getInvoiceDate() {
		return invoiceDate;
	}

	public void setInvoiceDate(Date invoiceDate) {
		this.invoiceDate = invoiceDate;
	}

	public Date getFundReceivedDate() {
		return fundReceivedDate;
	}

	public void setFundReceivedDate(Date fundReceivedDate) {
		this.fundReceivedDate = fundReceivedDate;
	}

	public BigDecimal getTotalAmount() {
		return totalAmount;
	}

	public void setTotalAmount(BigDecimal totalAmount) {
		this.totalAmount = totalAmount;
	}
	
	public Claim(Integer claimId, String claimNumber, Integer awardId, String awardNumber, Date startDate, Date endDate,
			Date claimSubmissionDate, ClaimStatus claimStatus, BigDecimal totalAmount) {
		super();
		this.claimId = claimId;
		this.claimNumber = claimNumber;
		this.awardId = awardId;
		this.awardNumber = awardNumber;
		this.startDate = startDate;
		this.endDate = endDate;
		this.claimSubmissionDate = claimSubmissionDate;
		this.claimStatus = claimStatus;
		this.totalAmount = totalAmount;
	}

	public Claim() {
		super();
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public Date getAwardStartDate() {
		return awardStartDate;
	}

	public void setAwardStartDate(Date awardStartDate) {
		this.awardStartDate = awardStartDate;
	}

	public Date getAwardEndDate() {
		return awardEndDate;
	}

	public void setAwardEndDate(Date awardEndDate) {
		this.awardEndDate = awardEndDate;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public String getCreateUserName() {
		return createUserName;
	}

	public void setCreateUserName(String createUserName) {
		this.createUserName = createUserName;
	}

	public BigDecimal getIdcCumClaimAmtUptoLatest() {
		return idcCumClaimAmtUptoLatest;
	}

	public void setIdcCumClaimAmtUptoLatest(BigDecimal idcCumClaimAmtUptoLatest) {
		this.idcCumClaimAmtUptoLatest = idcCumClaimAmtUptoLatest;
	}

	public BigDecimal getMigratedReimPrevIdcAmt() {
		return migratedReimPrevIdcAmt;
	}

	public void setMigratedReimPrevIdcAmt(BigDecimal migratedReimPrevIdcAmt) {
		this.migratedReimPrevIdcAmt = migratedReimPrevIdcAmt;
	}

	public BigDecimal getIdcOrginalAmount() {
		return idcOrginalAmount;
	}

	public void setIdcOrginalAmount(BigDecimal idcOrginalAmount) {
		this.idcOrginalAmount = idcOrginalAmount;
	}

	public BigDecimal getIdcLatestAmount() {
		return idcLatestAmount;
	}

	public void setIdcLatestAmount(BigDecimal idcLatestAmount) {
		this.idcLatestAmount = idcLatestAmount;
	}

	public BigDecimal getIdcCumClaimAmtUptoClaim() {
		return idcCumClaimAmtUptoClaim;
	}

	public void setIdcCumClaimAmtUptoClaim(BigDecimal idcCumClaimAmtUptoClaim) {
		this.idcCumClaimAmtUptoClaim = idcCumClaimAmtUptoClaim;
	}

	public BigDecimal getIdcCumExpUptoPrevClaim() {
		return idcCumExpUptoPrevClaim;
	}

	public void setIdcCumExpUptoPrevClaim(BigDecimal idcCumExpUptoPrevClaim) {
		this.idcCumExpUptoPrevClaim = idcCumExpUptoPrevClaim;
	}

	public BigDecimal getIdcCommitmentUptoPrevClaim() {
		return idcCommitmentUptoPrevClaim;
	}

	public void setIdcCommitmentUptoPrevClaim(BigDecimal idcCommitmentUptoPrevClaim) {
		this.idcCommitmentUptoPrevClaim = idcCommitmentUptoPrevClaim;
	}

	public BigDecimal getIdcAmountRequested() {
		return idcAmountRequested;
	}

	public void setIdcAmountRequested(BigDecimal idcAmountRequested) {
		this.idcAmountRequested = idcAmountRequested;
	}

	public BigDecimal getIdcAmountForcasted() {
		return idcAmountForcasted;
	}

	public void setIdcAmountForcasted(BigDecimal idcAmountForcasted) {
		this.idcAmountForcasted = idcAmountForcasted;
	}

	public Date getFunderApprovalDate() {
		return funderApprovalDate;
	}

	public void setFunderApprovalDate(Date funderApprovalDate) {
		this.funderApprovalDate = funderApprovalDate;
	}

	public Date getPaymentDate() {
		return paymentDate;
	}

	public void setPaymentDate(Date paymentDate) {
		this.paymentDate = paymentDate;
	}

	public Date getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(Date documentDate) {
		this.documentDate = documentDate;
	}

	public String getOutputDocNumber() {
		return outputDocNumber;
	}

	public void setOutputDocNumber(String outputDocNumber) {
		this.outputDocNumber = outputDocNumber;
	}

	public Integer getClaimIndex() {
		return claimIndex;
	}

	public void setClaimIndex(Integer claimIndex) {
		this.claimIndex = claimIndex;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getClaimHalfYearlyPeriod() {
		return claimHalfYearlyPeriod;
	}

	public void setClaimHalfYearlyPeriod(String claimHalfYearlyPeriod) {
		this.claimHalfYearlyPeriod = claimHalfYearlyPeriod;
	}

	public String getFinancialYear() {
		return financialYear;
	}

	public void setFinancialYear(String financialYear) {
		this.financialYear = financialYear;
	}

	public List<LinkedHashMap<Object, Object>> getClaimSummaryDetails() {
		return claimSummaryDetails;
	}

	public void setClaimSummaryDetails(List<LinkedHashMap<Object, Object>> claimSummaryDetails) {
		this.claimSummaryDetails = claimSummaryDetails;
	}

	public String getQuarter() {
		return quarter;
	}

	public void setQuarter(String quarter) {
		this.quarter = quarter;
	}
}
