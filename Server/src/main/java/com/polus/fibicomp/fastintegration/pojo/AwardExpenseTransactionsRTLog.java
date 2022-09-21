package com.polus.fibicomp.fastintegration.pojo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "AWARD_EXP_TRANSACTIONS_RT_LOG")
public class AwardExpenseTransactionsRTLog {

	@Id
	@Column(name = "EXPENSE_TRACKER_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_EXP_TRAN_RT_LOG_ID_GEN")
	@SequenceGenerator(name = "AWARD_EXP_TRAN_RT_LOG_ID_GEN", sequenceName = "AWARD_EXP_TRAN_RT_LOG_ID_GEN", allocationSize = 1)
	private Integer expenseTrackerLogId;

	@Column(name = "CREATE_STATUS")
	private String createStatus;

	@Column(name = "ERROR_STATUS")
	private String errorStatus;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

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

	@Column(name = "PO_PR_FLAG")
	private String PoPrFlag;

	@Column(name = "BANK_CLEARING_DATE")
	private String bankClearingDate;

	@Column(name = "FM_POSTING_DATE")
	private String fmPostingDate;

	@Column(name = "FI_POSTING_DATE")
	private String fiPostingDate;

	@Column(name = "VENDOR_CODE")
	private String vendorCode;

	@Column(name = "VENDOR_NAME")
	private String vendorName;

	@Column(name = "DOCUMENT_DATE")
	private String documentDate;

	@Column(name = "FI_GL_ACCOUNT")
	private String fiGlAccount;

	@Column(name = "FI_GL_DESCRIPTION")
	private String fiGlDescription;

	@Column(name = "GR_DATE")
	private String grDate;

	@Column(name = "INVOICE_DATE")
	private String invoiceDate;

	@Column(name = "PR_DATE")
	private String prDate;

	@Column(name = "REQUESTER_NAME")
	private String requesterName;

	@Column(name = "TRANSACTION_REFERENCE_NUMBER")
	private String transactionReferenceNumber;

	@Column(name = "DOCUMENT_NUMBER")
	private String documentNumber;

	@Column(name = "PO_DATE")
	private String poDate;

	@Column(name = "FILE_ID")
	private Integer fileId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "FILE_ID"), name = "FILE_ID", referencedColumnName = "FILE_ID", insertable = false, updatable = false)
	private AwardExpenseFile awardExpenseFiles;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public AwardExpenseTransactionsRTLog() {
	}

	public AwardExpenseTransactionsRTLog(String accountNumber, String internalOrderCode, String remarks,
			BigDecimal amountInFmaCurrency, String actualOrCommittedFlag, String poPrFlag, String bankClearingDate,
			String fmPostingDate, String fiPostingDate, String vendorCode, String vendorName, String documentDate,
			String fiGlAccount, String fiGlDescription, String grDate, String invoiceDate, String prDate,
			String requesterName, String transactionReferenceNumber, String documentNumber, String poDate) {
		this.accountNumber = accountNumber;
		this.internalOrderCode = internalOrderCode;
		this.remarks = remarks;
		this.amountInFmaCurrency = amountInFmaCurrency;
		this.actualOrCommittedFlag = actualOrCommittedFlag;
		this.PoPrFlag = poPrFlag;
		this.bankClearingDate = bankClearingDate;
		this.fmPostingDate = fmPostingDate;
		this.fiPostingDate = fiPostingDate;
		this.vendorCode = vendorCode;
		this.vendorName = vendorName;
		this.documentDate = documentDate;
		this.fiGlAccount = fiGlAccount;
		this.fiGlDescription = fiGlDescription;
		this.grDate = grDate;
		this.invoiceDate = invoiceDate;
		this.prDate = prDate;
		this.requesterName = requesterName;
		this.transactionReferenceNumber = transactionReferenceNumber;
		this.documentNumber = documentNumber;
		this.poDate = poDate;
	}

	public Integer getExpenseTrackerLogId() {
		return expenseTrackerLogId;
	}

	public void setExpenseTrackerLogId(Integer expenseTrackerLogId) {
		this.expenseTrackerLogId = expenseTrackerLogId;
	}

	public String getCreateStatus() {
		return createStatus;
	}

	public void setCreateStatus(String createStatus) {
		this.createStatus = createStatus;
	}

	public String getErrorStatus() {
		return errorStatus;
	}

	public void setErrorStatus(String errorStatus) {
		this.errorStatus = errorStatus;
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

	public String getPoPrFlag() {
		return PoPrFlag;
	}

	public void setPoPrFlag(String poPrFlag) {
		PoPrFlag = poPrFlag;
	}

	public String getBankClearingDate() {
		return bankClearingDate;
	}

	public void setBankClearingDate(String bankClearingDate) {
		this.bankClearingDate = bankClearingDate;
	}

	public String getFmPostingDate() {
		return fmPostingDate;
	}

	public void setFmPostingDate(String fmPostingDate) {
		this.fmPostingDate = fmPostingDate;
	}

	public String getVendorCode() {
		return vendorCode;
	}

	public void setVendorCode(String vendorCode) {
		this.vendorCode = vendorCode;
	}

	public String getVendorName() {
		return vendorName;
	}

	public void setVendorName(String vendorName) {
		this.vendorName = vendorName;
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

	public String getGrDate() {
		return grDate;
	}

	public void setGrDate(String grDate) {
		this.grDate = grDate;
	}

	public String getInvoiceDate() {
		return invoiceDate;
	}

	public void setInvoiceDate(String invoiceDate) {
		this.invoiceDate = invoiceDate;
	}

	public String getPrDate() {
		return prDate;
	}

	public void setPrDate(String prDate) {
		this.prDate = prDate;
	}

	public String getRequesterName() {
		return requesterName;
	}

	public void setRequesterName(String requesterName) {
		this.requesterName = requesterName;
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

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public AwardExpenseFile getAwardExpenseFiles() {
		return awardExpenseFiles;
	}

	public void setAwardExpenseFiles(AwardExpenseFile awardExpenseFiles) {
		this.awardExpenseFiles = awardExpenseFiles;
	}

	public String getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(String fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
	}

	public String getTransactionReferenceNumber() {
		return transactionReferenceNumber;
	}

	public void setTransactionReferenceNumber(String transactionReferenceNumber) {
		this.transactionReferenceNumber = transactionReferenceNumber;
	}

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public String getPoDate() {
		return poDate;
	}

	public void setPoDate(String poDate) {
		this.poDate = poDate;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

}
