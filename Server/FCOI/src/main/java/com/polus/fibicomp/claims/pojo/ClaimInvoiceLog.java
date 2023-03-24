package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

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
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.Email;

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_INVOICE_LOG")
@EntityListeners(AuditingEntityListener.class)
public class ClaimInvoiceLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CLAIM_INVOICE_LOG")
	@SequenceGenerator(name = "SEQ_CLAIM_INVOICE_LOG", sequenceName = "SEQ_CLAIM_INVOICE_LOG", allocationSize = 1)
	@Column(name = "CLAIM_INVOICE_LOG_ID")
	private Integer claimInvoiceLogId;

	@Column(name = "INVOICE_ID")
	private Integer invoiceId;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;
	
	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;

	@Column(name = "DOCUMENT_TYPE_CODE")
	private String documentTypeCode;
	
	@Column(name = "CURRENCY_CODE")
	private String currencyCode;
	
	@Column(name = "DOCUMENT_HEADER_TEXT")
	private String documentHeaderText;
	
	@Email
	@Column(name = "CUSTOMER_EMAIL_ADDRESS")
	private String customerEmailAddress;
	
	@Email
	@Column(name = "REQUESTER_EMAIL_ADDRESS")
	private String requesterEmailAddress;
	
	@Column(name = "HEADER_POSTING_KEY")
	private String headerPostingKey;
	
	@Column(name = "DOCUMENT_DATE")
	private Timestamp documentDate;
	
	@Column(name = "BASE_DATE")
	private Timestamp baseDate;
	
	@Column(name = "COMPANY_CODE")
	private String companyCode;
	
	@Column(name = "PARTICULARS1")
	private String particulars1;
	
	@Column(name = "PARTICULARS2")
	private String particulars2;
	
	@Column(name = "PARTICULARS3")
	private String particulars3;
	
	@Column(name = "PARTICULARS4")
	private String particulars4;
	
	@Column(name = "CONTACT_TELEPHONE_NO")
	private String contactTelephoneNo; 
	
	@Column(name = "INPUT_DOCUMENT_NUMBER")
	private String inputDocumentNumber;
	
	@Column(name = "FISCAL_YEAR")
	private String fiscalYear;
	
	@Column(name = "ACTION_INDICATOR")
	private String actionIndicator;
	
	@Column(name = "BATCH_ID")
	private Integer batchId;
	
	@Column(name = "TYPE_CODE")
	private String typeCode;
	
	@Column(name = "ASSIGNMENT_FIELD")
	private String assignmentField;

	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "GL_ACCOUNT_CODE")
	private String glAccountCode;
	
	@Column(name = "OUTPUT_DOCUMENT_NUMBER")
	private String outputDocumentNumber;	

	@Column(name = "GRANT_CODE")
	private String grantCode;

	@Column(name = "PROFIT_CENTRE")
	private String profitCentre;
	
	@Column(name = "STATUS")
	private String status;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "claimInvoiceLog", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<ClaimInvoiceDetailsLog> claimInvoiceDetails;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_INVOICE_LOG_FK1"), name = "GL_ACCOUNT_CODE", referencedColumnName = "GL_ACCOUNT_CODE", insertable = false, updatable = false)
	private ClaimGlAccount claimGlAccount;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_INVOICE_LOG_FK2"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ClaimInvoiceFeedType claimInvoiceFeedType;

	@Column(name = "CUSTOMER_NUMBER")
	private String customerNumber;

	@Transient
	private String baCode;
	
	@Transient
	private BigDecimal claimAmount;
	
	@Transient
	private ClaimInvoiceMetadata claimInvoiceMetadata;
	
	@Transient
	private String message;
	
	public ClaimInvoiceLog() {
		super();
	}

  public ClaimInvoiceLog(Integer claimInvoiceLogId, String claimNumber, String fiscalYear, Integer batchId,
			String outputDocumentNumber, String status, String message) {
		super();
		this.claimInvoiceLogId = claimInvoiceLogId;
		this.claimNumber = claimNumber;
		this.fiscalYear = fiscalYear;
		this.batchId = batchId;
		this.outputDocumentNumber = outputDocumentNumber;
		this.status = status;
		this.message = message;
	}

	public Integer getInvoiceId() {
		return invoiceId;
	}

	public void setInvoiceId(Integer invoiceId) {
		this.invoiceId = invoiceId;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public String getDocumentTypeCode() {
		return documentTypeCode;
	}

	public void setDocumentTypeCode(String documentTypeCode) {
		this.documentTypeCode = documentTypeCode;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public String getDocumentHeaderText() {
		return documentHeaderText;
	}

	public void setDocumentHeaderText(String documentHeaderText) {
		this.documentHeaderText = documentHeaderText;
	}

	public String getCustomerEmailAddress() {
		return customerEmailAddress;
	}

	public void setCustomerEmailAddress(String customerEmailAddress) {
		this.customerEmailAddress = customerEmailAddress;
	}

	public String getRequesterEmailAddress() {
		return requesterEmailAddress;
	}

	public void setRequesterEmailAddress(String requesterEmailAddress) {
		this.requesterEmailAddress = requesterEmailAddress;
	}

	public Timestamp getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(Timestamp documentDate) {
		this.documentDate = documentDate;
	}

	public Timestamp getBaseDate() {
		return baseDate;
	}

	public void setBaseDate(Timestamp baseDate) {
		this.baseDate = baseDate;
	}
	
	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getParticulars1() {
		return particulars1;
	}

	public void setParticulars1(String particulars1) {
		this.particulars1 = particulars1;
	}

	public String getParticulars2() {
		return particulars2;
	}

	public void setParticulars2(String particulars2) {
		this.particulars2 = particulars2;
	}

	public String getParticulars3() {
		return particulars3;
	}

	public void setParticulars3(String particulars3) {
		this.particulars3 = particulars3;
	}

	public String getParticulars4() {
		return particulars4;
	}

	public void setParticulars4(String particulars4) {
		this.particulars4 = particulars4;
	}

	public String getContactTelephoneNo() {
		return contactTelephoneNo;
	}

	public void setContactTelephoneNo(String contactTelephoneNo) {
		this.contactTelephoneNo = contactTelephoneNo;
	}

	public String getActionIndicator() {
		return actionIndicator;
	}

	public void setActionIndicator(String actionIndicator) {
		this.actionIndicator = actionIndicator;
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

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getHeaderPostingKey() {
		return headerPostingKey;
	}

	public void setHeaderPostingKey(String headerPostingKey) {
		this.headerPostingKey = headerPostingKey;
	}

	public String getBaCode() {
		return baCode;
	}

	public void setBaCode(String baCode) {
		this.baCode = baCode;
	}

	public ClaimInvoiceMetadata getClaimInvoiceMetadata() {
		return claimInvoiceMetadata;
	}

	public void setClaimInvoiceMetadata(ClaimInvoiceMetadata claimInvoiceMetadata) {
		this.claimInvoiceMetadata = claimInvoiceMetadata;
	}

	public BigDecimal getClaimAmount() {
		return claimAmount;
	}

	public void setClaimAmount(BigDecimal claimAmount) {
		this.claimAmount = claimAmount;
	}

	public String getFiscalYear() {
		return fiscalYear;
	}

	public void setFiscalYear(String fiscalYear) {
		this.fiscalYear = fiscalYear;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public List<ClaimInvoiceDetailsLog> getClaimInvoiceDetails() {
		return claimInvoiceDetails;
	}

	public void setClaimInvoiceDetails(List<ClaimInvoiceDetailsLog> claimInvoiceDetails) {
		this.claimInvoiceDetails = claimInvoiceDetails;
	}

	public String getAssignmentField() {
		return assignmentField;
	}

	public void setAssignmentField(String assignmentField) {
		this.assignmentField = assignmentField;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getInputDocumentNumber() {
		return inputDocumentNumber;
	}

	public void setInputDocumentNumber(String inputDocumentNumber) {
		this.inputDocumentNumber = inputDocumentNumber;
	}

	public String getGlAccountCode() {
		return glAccountCode;
	}

	public void setGlAccountCode(String glAccountCode) {
		this.glAccountCode = glAccountCode;
	}

	public String getOutputDocumentNumber() {
		return outputDocumentNumber;
	}

	public void setOutputDocumentNumber(String outputDocumentNumber) {
		this.outputDocumentNumber = outputDocumentNumber;
	}

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getProfitCentre() {
		return profitCentre;
	}

	public void setProfitCentre(String profitCentre) {
		this.profitCentre = profitCentre;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public ClaimGlAccount getClaimGlAccount() {
		return claimGlAccount;
	}

	public void setClaimGlAccount(ClaimGlAccount claimGlAccount) {
		this.claimGlAccount = claimGlAccount;
	}

	public Integer getClaimInvoiceLogId() {
		return claimInvoiceLogId;
	}

	public void setClaimInvoiceLogId(Integer claimInvoiceLogId) {
		this.claimInvoiceLogId = claimInvoiceLogId;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public ClaimInvoiceFeedType getClaimInvoiceFeedType() {
		return claimInvoiceFeedType;
	}

	public void setClaimInvoiceFeedType(ClaimInvoiceFeedType claimInvoiceFeedType) {
		this.claimInvoiceFeedType = claimInvoiceFeedType;
	}

	public String getCustomerNumber() {
		return customerNumber;
	}

	public void setCustomerNumber(String customerNumber) {
		this.customerNumber = customerNumber;
	}
}
