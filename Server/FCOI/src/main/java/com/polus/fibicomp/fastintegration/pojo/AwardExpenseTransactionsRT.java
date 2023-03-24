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
@Table(name = "AWARD_EXPENSE_TRANSACTIONS_RT")
public class AwardExpenseTransactionsRT {

	@Id
	@Column(name = "EXPENSE_TRACKER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_EXPENSE_TRAN_RT_ID_GEN")
	@SequenceGenerator(name = "AWARD_EXPENSE_TRAN_RT_ID_GEN", sequenceName = "AWARD_EXPENSE_TRAN_RT_ID_GEN", allocationSize = 1)
	private Integer expenseTrackerId;

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

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ITEM_NUMBER")
	private String itemNumber;

	@Column(name = "REFERENCE_DOCUMENT_CATEGORY")
	private String referenceDocumentCategory;

	@Column(name = "REFERENCE_ORG_UNIT")
	private String referenceOrgUnit;

	@Column(name = "ACCT_ASSIGNMENT_NUMBER")
	private String acctAssignmentNumber;

	@Column(name = "SCHEDULE_LINE_NUMBER")
	private String scheduleLineNumber;

	@Column(name = "CONDITION_COUNTER")
	private String conditionCounter;

	@Column(name = "REFERENCE_PROCEDURE")
	private String referenceProcedure;

	@Column(name = "DOCUMENT_NUMBER_FM_LINE_ITEM")
	private String documentNumberFmLineItem;

	@Column(name = "NUMBER_OF_POST_FM_LINE_ITEM")
	private String numberOfPostFmLineItem;

	@Column(name = "COMPANY_CODE")
	private String companyCode;

	@Column(name = "VALUE_TYPE")
	private String valueType;

	@Column(name = "AMOUNT_TYPE")
	private String amountType;

	@Column(name = "FISCAL_YEAR")
	private String fiscalYear;

	@Column(name = "FM_AREA")
	private String fmArea;

	@Column(name = "FUND")
	private String fund;

	@Column(name = "FUND_CENTER")
	private String fundCenter;

	@Column(name = "FUNCTIONAL_AREA")
	private String functionalArea;

	@Column(name = "BATCH_ID")
	private String batchId;

	@Column(name = "TRANSACTION_NUMBER")
	private String transactionNumber;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "PREDECESSOR_DOC_NUMBER")
	private String predecessorDocNumber;

	public AwardExpenseTransactionsRT() {
	}

	public AwardExpenseTransactionsRT(String accountNumber, String internalOrderCode, String remarks,
			BigDecimal amountInFmaCurrency, String actualOrCommittedFlag, String poPrFlag, String bankClearingDate,
			String fmPostingDate, String fiPostingDate, String vendorCode, String vendorName, String documentDate,
			String fiGlAccount, String fiGlDescription, String grDate, String invoiceDate, String prDate,
			String requesterName, String transactionReferenceNumber, String documentNumber, 
			String itemNumber, String referenceDocumentCategory, String referenceOrgUnit, String acctAssignmentNumber,
			String scheduleLineNumber, String conditionCounter, String referenceProcedure, String documentNumberFmLineItem,
			String numberOfPostFmLineItem, String companyCode, String valueType, String amountType, String fiscalYear, String transactionNumber,
			String poDate, String batchId,	String fmArea, String fund, String fundCenter, String functionalArea, String predecessorDocNumber) {
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
		this.itemNumber = itemNumber;
		this.referenceDocumentCategory = referenceDocumentCategory;
		this.referenceOrgUnit = referenceOrgUnit;
		this.acctAssignmentNumber = acctAssignmentNumber;
		this.scheduleLineNumber = scheduleLineNumber;
		this.conditionCounter = conditionCounter;
		this.referenceProcedure = referenceProcedure;
		this.documentNumberFmLineItem = documentNumberFmLineItem;
		this.numberOfPostFmLineItem = numberOfPostFmLineItem;
		this.companyCode = companyCode;
		this.valueType = valueType;
		this.amountType = amountType;
		this.fiscalYear = fiscalYear;
		this.transactionNumber = transactionNumber;
		this.poDate = poDate;
		this.batchId = batchId;	
		this.fmArea = fmArea;
		this.fund = fund;
		this.fundCenter = fundCenter; 
		this.functionalArea = functionalArea;
		this.predecessorDocNumber = predecessorDocNumber;
	}

	public Integer getExpenseTrackerId() {
		return expenseTrackerId;
	}

	public void setExpenseTrackerId(Integer expenseTrackerId) {
		this.expenseTrackerId = expenseTrackerId;
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

	public BigDecimal getAmountInFmaCurrency() {
		return amountInFmaCurrency;
	}

	public void setAmountInFmaCurrency(BigDecimal amountInFmaCurrency) {
		this.amountInFmaCurrency = amountInFmaCurrency;
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

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public String getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(String fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getItemNumber() {
		return itemNumber;
	}

	public void setItemNumber(String itemNumber) {
		this.itemNumber = itemNumber;
	}

	public String getReferenceDocumentCategory() {
		return referenceDocumentCategory;
	}

	public void setReferenceDocumentCategory(String referenceDocumentCategory) {
		this.referenceDocumentCategory = referenceDocumentCategory;
	}

	public String getReferenceOrgUnit() {
		return referenceOrgUnit;
	}

	public void setReferenceOrgUnit(String referenceOrgUnit) {
		this.referenceOrgUnit = referenceOrgUnit;
	}

	public String getAcctAssignmentNumber() {
		return acctAssignmentNumber;
	}

	public void setAcctAssignmentNumber(String acctAssignmentNumber) {
		this.acctAssignmentNumber = acctAssignmentNumber;
	}

	public String getScheduleLineNumber() {
		return scheduleLineNumber;
	}

	public void setScheduleLineNumber(String scheduleLineNumber) {
		this.scheduleLineNumber = scheduleLineNumber;
	}

	public String getConditionCounter() {
		return conditionCounter;
	}

	public void setConditionCounter(String conditionCounter) {
		this.conditionCounter = conditionCounter;
	}

	public String getReferenceProcedure() {
		return referenceProcedure;
	}

	public void setReferenceProcedure(String referenceProcedure) {
		this.referenceProcedure = referenceProcedure;
	}

	public String getDocumentNumberFmLineItem() {
		return documentNumberFmLineItem;
	}

	public void setDocumentNumberFmLineItem(String documentNumberFmLineItem) {
		this.documentNumberFmLineItem = documentNumberFmLineItem;
	}

	public String getNumberOfPostFmLineItem() {
		return numberOfPostFmLineItem;
	}

	public void setNumberOfPostFmLineItem(String numberOfPostFmLineItem) {
		this.numberOfPostFmLineItem = numberOfPostFmLineItem;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getValueType() {
		return valueType;
	}

	public void setValueType(String valueType) {
		this.valueType = valueType;
	}

	public String getAmountType() {
		return amountType;
	}

	public void setAmountType(String amountType) {
		this.amountType = amountType;
	}

	public String getFiscalYear() {
		return fiscalYear;
	}

	public void setFiscalYear(String fiscalYear) {
		this.fiscalYear = fiscalYear;
	}

	public String getFmArea() {
		return fmArea;
	}

	public void setFmArea(String fmArea) {
		this.fmArea = fmArea;
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

	public String getFunctionalArea() {
		return functionalArea;
	}

	public void setFunctionalArea(String functionalArea) {
		this.functionalArea = functionalArea;
	}

	public String getBatchId() {
		return batchId;
	}

	public void setBatchId(String batchId) {
		this.batchId = batchId;
	}

	public String getTransactionNumber() {
		return transactionNumber;
	}

	public void setTransactionNumber(String transactionNumber) {
		this.transactionNumber = transactionNumber;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getPredecessorDocNumber() {
		return predecessorDocNumber;
	}

	public void setPredecessorDocNumber(String predecessorDocNumber) {
		this.predecessorDocNumber = predecessorDocNumber;
	}

}
