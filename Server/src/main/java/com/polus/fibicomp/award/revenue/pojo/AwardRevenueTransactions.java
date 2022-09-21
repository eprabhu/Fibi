package com.polus.fibicomp.award.revenue.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "AWARD_REVENUE_TRANSACTIONS")
public class AwardRevenueTransactions implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REVENUE_TRACKER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_REVENUE_TRACKER_ID_GEN")
	@SequenceGenerator(name = "AWARD_REVENUE_TRACKER_ID_GEN", sequenceName = "AWARD_REVENUE_TRACKER_ID_GEN", allocationSize = 1)
	private Integer revenueTrackerId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "REMARKS")
	private String remarks;

	@Column(name = "AMOUNT_IN_FMA_CURRENCY", precision = 12, scale = 2)
	private BigDecimal amountInFmaCurrency;

	@Column(name = "ACTUAL_OR_COMMITTED_FLAG")
	private String actualOrCommittedFlag;

	@Column(name = "ENTRY_DATE")
	private String entryDate;

	@Column(name = "FM_POSTING_DATE")
	private String fmPostingDate;

	@Column(name = "FI_POSTING_DATE")
	private Timestamp fiPostingDate;

	@Column(name = "BP_CODE")
	private String bpCode;

	@Column(name = "BP_NAME")
	private String bpName;

	@Column(name = "DOCUMENT_DATE")
	private Timestamp documentDate;

	@Column(name = "FI_GL_ACCOUNT")
	private String fiGlAccount;

	@Column(name = "FI_GL_DESCRIPTION")
	private String fiGlDescription;

	@Column(name = "TRANSACTION_REFERENCE_NUMBER")
	private String transactionReferenceNumber;

	@Column(name = "REFERENCE_DOCUMENT_NUMBER")
	private String referenceDocumentNumber;

	@Column(name = "REFERENCE_POSTING_LINE")
	private String referencePostingLine;

	@Column(name = "GUID")
	private String guid;

	@Column(name = "GMIA_DOCNR")
	private String gmiaDocnr;

	@Column(name = "DOCLN")
	private String docln;

	@Column(name = "RBUKRS")
	private String rbukrs;

	@Column(name = "RVALUETYPE_9")
	private String rvalueType9;

	@Column(name = "RYEAR")
	private String rYear;

	@Column(name = "GL_SIRID")
	private String glSirid;

	@Column(name = "BATCH_ID")
	private String batchId;

	@Column(name = "GRANT_NBR")
	private String grantNbr;

	@Column(name = "SPONSOR_PROGRAM")
	private String sponsorProgram;

	@Column(name = "SPONSOR_CLASS")
	private String sponsorClass;

	@Column(name = "FUND")
	private String fund;

	@Column(name = "FUND_CENTER")
	private String fundCenter;

	@Column(name = "FILE_ID")
	private Integer fileId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String budgetCategory;

	@Transient
	private String lineItem;

	@Transient
	private BigDecimal totalRevenueAmount = BigDecimal.ZERO;

	@Transient
	private String budgetCategoryTypeCode;

	@Transient
	private BigDecimal quantity = BigDecimal.ZERO;

	@Column(name = "PAYMENT_DOC_NUMBER")
	private String paymentDocNumber;

	@Column(name = "PAYMENT_DATE")
	private Timestamp paymentDate;

	@Column(name = "PAYMENT_FISCAL_YEAR")
	private String paymentFiscalYear;

	public Integer getRevenueTrackerId() {
		return revenueTrackerId;
	}

	public void setRevenueTrackerId(Integer revenueTrackerId) {
		this.revenueTrackerId = revenueTrackerId;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public BigDecimal getAmountInFmaCurrency() {
		return amountInFmaCurrency;
	}

	public void setAmountInFmaCurrency(BigDecimal amountInFmaCurrency) {
		this.amountInFmaCurrency = amountInFmaCurrency;
	}

	public String getActualOrCommittedFlag() {
		return actualOrCommittedFlag;
	}

	public void setActualOrCommittedFlag(String actualOrCommittedFlag) {
		this.actualOrCommittedFlag = actualOrCommittedFlag;
	}

	public String getFiGlAccount() {
		return fiGlAccount;
	}

	public void setFiGlAccount(String fiGlAccount) {
		this.fiGlAccount = fiGlAccount;
	}

	public String getFiGlDescription() {
		return fiGlDescription;
	}

	public void setFiGlDescription(String fiGlDescription) {
		this.fiGlDescription = fiGlDescription;
	}

	public String getTransactionReferenceNumber() {
		return transactionReferenceNumber;
	}

	public void setTransactionReferenceNumber(String transactionReferenceNumber) {
		this.transactionReferenceNumber = transactionReferenceNumber;
	}

	public String getReferenceDocumentNumber() {
		return referenceDocumentNumber;
	}

	public void setReferenceDocumentNumber(String referenceDocumentNumber) {
		this.referenceDocumentNumber = referenceDocumentNumber;
	}

	public String getReferencePostingLine() {
		return referencePostingLine;
	}

	public void setReferencePostingLine(String referencePostingLine) {
		this.referencePostingLine = referencePostingLine;
	}

	public String getBatchId() {
		return batchId;
	}

	public void setBatchId(String batchId) {
		this.batchId = batchId;
	}

	public String getGrantNbr() {
		return grantNbr;
	}

	public void setGrantNbr(String grantNbr) {
		this.grantNbr = grantNbr;
	}

	public String getSponsorProgram() {
		return sponsorProgram;
	}

	public void setSponsorProgram(String sponsorProgram) {
		this.sponsorProgram = sponsorProgram;
	}

	public String getSponsorClass() {
		return sponsorClass;
	}

	public void setSponsorClass(String sponsorClass) {
		this.sponsorClass = sponsorClass;
	}

	public String getFund() {
		return fund;
	}

	public void setFund(String fund) {
		this.fund = fund;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
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

	public String getEntryDate() {
		return entryDate;
	}

	public void setEntryDate(String entryDate) {
		this.entryDate = entryDate;
	}

	public String getBpCode() {
		return bpCode;
	}

	public void setBpCode(String bpCode) {
		this.bpCode = bpCode;
	}

	public String getBpName() {
		return bpName;
	}

	public void setBpName(String bpName) {
		this.bpName = bpName;
	}

	public String getGuid() {
		return guid;
	}

	public void setGuid(String guid) {
		this.guid = guid;
	}

	public String getGmiaDocnr() {
		return gmiaDocnr;
	}

	public void setGmiaDocnr(String gmiaDocnr) {
		this.gmiaDocnr = gmiaDocnr;
	}

	public String getDocln() {
		return docln;
	}

	public void setDocln(String docln) {
		this.docln = docln;
	}

	public String getRbukrs() {
		return rbukrs;
	}

	public void setRbukrs(String rbukrs) {
		this.rbukrs = rbukrs;
	}

	public String getRvalueType9() {
		return rvalueType9;
	}

	public void setRvalueType9(String rvalueType9) {
		this.rvalueType9 = rvalueType9;
	}

	public String getrYear() {
		return rYear;
	}

	public void setrYear(String rYear) {
		this.rYear = rYear;
	}

	public String getGlSirid() {
		return glSirid;
	}

	public void setGlSirid(String glSirid) {
		this.glSirid = glSirid;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getFmPostingDate() {
		return fmPostingDate;
	}

	public void setFmPostingDate(String fmPostingDate) {
		this.fmPostingDate = fmPostingDate;
	}

	public String getLineItem() {
		return lineItem;
	}

	public void setLineItem(String lineItem) {
		this.lineItem = lineItem;
	}

	public String getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(String budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public String getBudgetCategoryTypeCode() {
		return budgetCategoryTypeCode;
	}

	public void setBudgetCategoryTypeCode(String budgetCategoryTypeCode) {
		this.budgetCategoryTypeCode = budgetCategoryTypeCode;
	}

	public BigDecimal getTotalRevenueAmount() {
		return totalRevenueAmount;
	}

	public void setTotalRevenueAmount(BigDecimal totalRevenueAmount) {
		this.totalRevenueAmount = totalRevenueAmount;
	}

	public BigDecimal getQuantity() {
		return quantity;
	}

	public void setQuantity(BigDecimal quantity) {
		this.quantity = quantity;
	}

	public Timestamp getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(Timestamp fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
	}

	public Timestamp getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(Timestamp documentDate) {
		this.documentDate = documentDate;
	}

	public String getPaymentDocNumber() {
		return paymentDocNumber;
	}

	public void setPaymentDocNumber(String paymentDocNumber) {
		this.paymentDocNumber = paymentDocNumber;
	}

	public Timestamp getPaymentDate() {
		return paymentDate;
	}

	public void setPaymentDate(Timestamp paymentDate) {
		this.paymentDate = paymentDate;
	}

	public String getPaymentFiscalYear() {
		return paymentFiscalYear;
	}

	public void setPaymentFiscalYear(String paymentFiscalYear) {
		this.paymentFiscalYear = paymentFiscalYear;
	}
}
