package com.polus.fibicomp.award.expense.vo;

import java.math.BigDecimal;
import java.util.List;

import javax.validation.constraints.Pattern;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;

public class AwardExpenseTransactionVO {

	@Pattern(regexp="^[0-9 -]*$", message="awardNumber must not include special characters.")
	private String awardNumber;

	@Pattern(regexp="^[0-9a-zA-Z]*$", message="Account Number must not include special characters.")
	private String accountNumber;

	@Pattern(regexp="^[0-9a-zA-Z]*$", message="internalOrderCode must not include special characters.")
	private String internalOrderCode;

	private String fiPostingDate;

	private String bankClearingDate;

	private String documentDate;

	private String fiGlAccount;

	private String fiGlDescription;

	private String amountInFmaCurrency;

	private String vendorCode;

	private String vendorName;

	private String assetLocation;

	private String remarks;

	@Pattern(regexp="^[AC]*$", message="actualOrCommittedFlag must include A and C.")
	private String actualOrCommittedFlag;

	private List<AwardExpenseTransaction> expenseTransactions;

	@Pattern(regexp="^[0-9 -]*$", message="fmPostingStartDate must not include special characters.")
	private String fmPostingStartDate;

	@Pattern(regexp="^[0-9 -]*$", message="fmPostingEndDate must not include special characters.")
	private String fmPostingEndDate;

	private List<String> internalOrderCodes;

	private String invoiceDate;

	private String documentNumber;

	private String poNumber;

	private String poDate;

	private String fmPostingDate;

	private BigDecimal amount = BigDecimal.ZERO;

	private String invoiceNumber;

	private String budgetCategory;

	private String budgetCategoryAcronym;

	private String travelFrom;

	private String travelTo;
	
	private String isShowAllTransactions;

	private String commitment;

	private String sponsorAwardNumber;

	private String transactionReferenceNumber;

	private String predecessorDocNumber;

	public String getTravelFrom() {
		return travelFrom;
	}

	public void setTravelFrom(String travelFrom) {
		this.travelFrom = travelFrom;
	}

	public String getTravelTo() {
		return travelTo;
	}

	public void setTravelTo(String travelTo) {
		this.travelTo = travelTo;
	}

	public AwardExpenseTransactionVO() {
	}

	public AwardExpenseTransactionVO(String awardNumber, String accountNumber, String internalOrderCode,
			String fmPostingDate, String bankClearingDate, String documentDate, String fiGlAccount,
			String fiGlDescription, String amountInFmaCurrency, String vendorCode, String vendorName,
			String assetLocation, String remarks, String actualOrCommittedFlag) {
		super();
		this.awardNumber = awardNumber;
		this.accountNumber = accountNumber;
		this.internalOrderCode = internalOrderCode;
		this.fiPostingDate = fmPostingDate;
		this.bankClearingDate = bankClearingDate;
		this.documentDate = documentDate;
		this.fiGlAccount = fiGlAccount;
		this.fiGlDescription = fiGlDescription;
		this.amountInFmaCurrency = amountInFmaCurrency;
		this.vendorCode = vendorCode;
		this.vendorName = vendorName;
		this.assetLocation = assetLocation;
		this.remarks = remarks;
		this.actualOrCommittedFlag = actualOrCommittedFlag;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
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

	public String getFiGlDescription() {
		return fiGlDescription;
	}

	public void setFiGlDescription(String fiGlDescription) {
		this.fiGlDescription = fiGlDescription;
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

	public String getAssetLocation() {
		return assetLocation;
	}

	public void setAssetLocation(String assetLocation) {
		this.assetLocation = assetLocation;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public String getFiGlAccount() {
		return fiGlAccount;
	}

	public void setFiGlAccount(String fiGlAccount) {
		this.fiGlAccount = fiGlAccount;
	}

	public String getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(String fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
	}

	public String getBankClearingDate() {
		return bankClearingDate;
	}

	public void setBankClearingDate(String bankClearingDate) {
		this.bankClearingDate = bankClearingDate;
	}

	public String getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(String documentDate) {
		this.documentDate = documentDate;
	}

	public String getAmountInFmaCurrency() {
		return amountInFmaCurrency;
	}

	public void setAmountInFmaCurrency(String amountInFmaCurrency) {
		this.amountInFmaCurrency = amountInFmaCurrency;
	}

	public String getActualOrCommittedFlag() {
		return actualOrCommittedFlag;
	}

	public void setActualOrCommittedFlag(String actualOrCommittedFlag) {
		this.actualOrCommittedFlag = actualOrCommittedFlag;
	}

	public List<AwardExpenseTransaction> getExpenseTransactions() {
		return expenseTransactions;
	}

	public void setExpenseTransactions(List<AwardExpenseTransaction> expenseTransactions) {
		this.expenseTransactions = expenseTransactions;
	}

	public String getFmPostingStartDate() {
		return fmPostingStartDate;
	}

	public void setFmPostingStartDate(String fmPostingStartDate) {
		this.fmPostingStartDate = fmPostingStartDate;
	}

	public String getFmPostingEndDate() {
		return fmPostingEndDate;
	}

	public void setFmPostingEndDate(String fmPostingEndDate) {
		this.fmPostingEndDate = fmPostingEndDate;
	}

	public List<String> getInternalOrderCodes() {
		return internalOrderCodes;
	}

	public void setInternalOrderCodes(List<String> internalOrderCodes) {
		this.internalOrderCodes = internalOrderCodes;
	}

	public String getInvoiceDate() {
		return invoiceDate;
	}

	public void setInvoiceDate(String invoiceDate) {
		this.invoiceDate = invoiceDate;
	}

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public String getPoNumber() {
		return poNumber;
	}

	public void setPoNumber(String poNumber) {
		this.poNumber = poNumber;
	}

	public String getPoDate() {
		return poDate;
	}

	public void setPoDate(String poDate) {
		this.poDate = poDate;
	}

	public String getFmPostingDate() {
		return fmPostingDate;
	}

	public void setFmPostingDate(String fmPostingDate) {
		this.fmPostingDate = fmPostingDate;
	}

	public BigDecimal getAmount() {
		return amount;
	}

	public void setAmount(BigDecimal amount) {
		this.amount = amount;
	}

	public String getInvoiceNumber() {
		return invoiceNumber;
	}

	public void setInvoiceNumber(String invoiceNumber) {
		this.invoiceNumber = invoiceNumber;
	}

	public String getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(String budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public String getBudgetCategoryAcronym() {
		return budgetCategoryAcronym;
	}

	public void setBudgetCategoryAcronym(String budgetCategoryAcronym) {
		this.budgetCategoryAcronym = budgetCategoryAcronym;
	}

	public String getIsShowAllTransactions() {
		return isShowAllTransactions;
	}

	public void setIsShowAllTransactions(String isShowAllTransactions) {
		this.isShowAllTransactions = isShowAllTransactions;
	}

	public String getCommitment() {
		return commitment;
	}

	public void setCommitment(String commitment) {
		this.commitment = commitment;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public String getTransactionReferenceNumber() {
		return transactionReferenceNumber;
	}

	public void setTransactionReferenceNumber(String transactionReferenceNumber) {
		this.transactionReferenceNumber = transactionReferenceNumber;
	}

	public String getPredecessorDocNumber() {
		return predecessorDocNumber;
	}

	public void setPredecessorDocNumber(String predecessorDocNumber) {
		this.predecessorDocNumber = predecessorDocNumber;
	}

}
