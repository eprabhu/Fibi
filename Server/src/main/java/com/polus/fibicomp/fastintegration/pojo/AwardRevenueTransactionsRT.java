package com.polus.fibicomp.fastintegration.pojo;

import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_REVENUE_TRANSACTIONS_RT")
public class AwardRevenueTransactionsRT {

	@Id
	@Column(name = "REVENUE_TRACKER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "REVENUE_TRACKER_ID_GEN")
	@SequenceGenerator(name = "REVENUE_TRACKER_ID_GEN", sequenceName = "REVENUE_TRACKER_ID_GEN", allocationSize = 1)
	private Integer revenueTrackerId;

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

	@Column(name = "FI_POSTING_DATE")
	private String fiPostingDate;

	@Column(name = "BP_CODE")
	private String bpCode;

	@Column(name = "BP_NAME")
	private String bpName;

	@Column(name = "DOCUMENT_DATE")
	private String documentDate;

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

	@Column(name = "PAYMENT_DOC_NUMBER")
	private String paymentDocNumber;

	@Column(name = "PAYMENT_DATE")
	private Timestamp paymentDate;

	@Column(name = "PAYMENT_FISCAL_YEAR")
	private String paymentFiscalYear;

	public AwardRevenueTransactionsRT() {
	}

	public AwardRevenueTransactionsRT(String accountNumber, String internalOrderCode, String remarks,
			BigDecimal amountInFmaCurrency, String actualOrCommittedFlag, String entryDate, String fiPostingDate,
			String bpCode, String bpName, String documentDate, String fiGlAccount, String fiGlDescription,
			String transactionReferenceNumber, String referenceDocumentNumber, String referencePostingLine,
			String guid, String gmiaDocnr, String docln, String rbukrs,
			String rvalueType9, String rYear, String glSirid, String batchId, String grantNbr,
			String sponsorProgram, String sponsorClass, String fund, String fundCenter) {
		this.accountNumber = accountNumber;
		this.internalOrderCode = internalOrderCode;
		this.remarks = remarks;
		this.amountInFmaCurrency = amountInFmaCurrency;
		this.actualOrCommittedFlag = actualOrCommittedFlag;
		this.entryDate = entryDate;
		this.fiPostingDate = fiPostingDate;
		this.bpCode = bpCode;
		this.bpName = bpName;
		this.documentDate = documentDate;
		this.fiGlAccount = fiGlAccount;
		this.fiGlDescription = fiGlDescription;
		this.transactionReferenceNumber = transactionReferenceNumber;
		this.referenceDocumentNumber = referenceDocumentNumber;
		this.referencePostingLine = referencePostingLine;
		this.guid = guid;
		this.gmiaDocnr = gmiaDocnr;
		this.docln = docln;
		this.rbukrs = rbukrs;
		this.rvalueType9 = rvalueType9;
		this.rYear = rYear;
		this.glSirid = glSirid;
		this.batchId = batchId;
		this.grantNbr = grantNbr;
		this.sponsorProgram = sponsorProgram;
		this.sponsorClass = sponsorClass;
		this.fund = fund;
		this.fundCenter = fundCenter;
	}

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

	public String getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(String fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
	}

	public String getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(String documentDate) {
		this.documentDate = documentDate;
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
