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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_INVOICE")
@EntityListeners(AuditingEntityListener.class)
public class ClaimInvoice implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CLAIM_INVOICE")
	@SequenceGenerator(name = "SEQ_CLAIM_INVOICE", sequenceName = "SEQ_CLAIM_INVOICE", allocationSize = 1)
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
	
	@Column(name = "ACTION_INDICATOR")
	private String actionIndicator;
	
	@Column(name = "ASSIGNMENT_FIELD")
	private String assignmentField;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "GL_ACCOUNT_CODE")
	private String glAccountCode;
	
	@Column(name = "GRANT_CODE")
	private String grantCode;
	
	@Column(name = "PROFIT_CENTRE")
	private String profitCentre;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "claimInvoice", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<ClaimInvoiceDetails> claimInvoiceDetails;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_INVOICE_FK1"), name = "GL_ACCOUNT_CODE", referencedColumnName = "GL_ACCOUNT_CODE", insertable = false, updatable = false)
	private ClaimGlAccount claimGlAccount;


	@Column(name = "CUSTOMER_NUMBER")
	private String customerNumber;
	
	@Transient
	private String campus;

	@Transient
	private BigDecimal claimAmount;
	
	@Transient
	private ClaimInvoiceMetadata claimInvoiceMetadata;

	@Transient
	private String baCode;

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

	public List<ClaimInvoiceDetails> getClaimInvoiceDetails() {
		return claimInvoiceDetails;
	}

	public void setClaimInvoiceDetails(List<ClaimInvoiceDetails> claimInvoiceDetails) {
		this.claimInvoiceDetails = claimInvoiceDetails;
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

	public String getGlAccountCode() {
		return glAccountCode;
	}

	public void setGlAccountCode(String glAccountCode) {
		this.glAccountCode = glAccountCode;
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

	public ClaimGlAccount getClaimGlAccount() {
		return claimGlAccount;
	}

	public void setClaimGlAccount(ClaimGlAccount claimGlAccount) {
		this.claimGlAccount = claimGlAccount;
	}

	public String getCampus() {
		return campus;
	}

	public void setCampus(String campus) {
		this.campus = campus;
	}

	public String getBaCode() {
		return baCode;
	}

	public void setBaCode(String baCode) {
		this.baCode = baCode;
	}

	public String getCustomerNumber() {
		return customerNumber;
	}

	public void setCustomerNumber(String customerNumber) {
		this.customerNumber = customerNumber;
	}
}
