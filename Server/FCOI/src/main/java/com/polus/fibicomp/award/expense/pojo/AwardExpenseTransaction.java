package com.polus.fibicomp.award.expense.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

@Entity
@Table(name = "AWARD_EXPENSE_TRANSACTIONS")
public class AwardExpenseTransaction implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "awardExpenseTransactionIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "awardExpenseTransactionIdGenerator")
	@Column(name = "AWARD_EXPENSE_TRANS_ID")
	private Integer awardExpenseTransactionId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "REMARKS")
	private String remarks;

	@Column(name = "AMOUNT_IN_FMA_CURRENCY", precision = 12, scale = 2)
	private BigDecimal amountInFmacurrency;

	@Column(name = "BANK_CLEARING_DATE")
	private Timestamp bankClearingDate;

	@Column(name = "FM_POSTING_DATE")
	private Timestamp fmPostingDate;

	@Column(name = "FI_POSTING_DATE")
	private Timestamp fiPostingDate;

	@Column(name = "GL_DESCRIPTION")
	private String glDescription;

	@Column(name = "ASSET")
	private String asset;

	@Column(name = "ASSET_INTERNAL_ORDER")
	private String assetInternalOrder;

	@Column(name = "COMMITMENT_ITEM")
	private String commitmentItem;

	@Column(name = "VENDOR_NAME")
	private String vendorName;

	@Column(name = "TC_AMOUNT", precision = 12, scale = 2)
	private BigDecimal tcAmount;

	@Column(name = "TRANSACTION_CURRENCY")
	private String transactionCurrency;

	@Column(name = "ASSET_LOCATION")
	private String assetLocation;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PREDECESSOR_DOC_NUMBER")
	private String predecessorDocNumber;

	@Column(name = "PREDECESSOR_DOC_ITEM")
	private String predecessorDocItem;

	@Column(name = "PREDECESSOR_DOC_ITEM_ACCOUNT")
	private String predecessorDocItemAccount;

	@Column(name = "PREDECESSOR_DOC_ITEM_ORG_UNIT")
	private String predecessorDocItemOrgUnit;

	@Column(name = "PREDECESSOR_DOC_CAT")
	private String predecessorDocCat;

	@Column(name = "PREDECESSOR_DOC_CAT_TEXT")
	private String predecessorDocCatText;

	@Column(name = "FM_DOC_NUMBER")
	private String FmDocNumber;

	@Column(name = "INVOICE_NUMBER")
	private String InvoiceNumber;

	@Column(name = "DOCUMENT_DATE")
	private Timestamp documentDate;

	@Column(name = "REFERENCE_DOC_NUMBER")
	private String referenceDocNumber;

	@Column(name = "PO_NUMBER")
	private String poNumber;

	@Column(name = "PO_DATE")
	private Timestamp poDate;

	@Column(name = "ASSET_COST_CENTER")
	private String assetCostCenter;

	@Column(name = "ASSET_LOCATION_CODE")
	private String assetLocationCode;

	@Column(name = "VENDOR_CODE")
	private String vendorCode;

	@Column(name = "FI_GL_ACCOUNT")
	private String fiGlAccount;

	@Column(name = "FI_GL_DESCRIPTION")
	private String fiGlDescription;

	@Column(name = "PO_GL_ACCOUNT")
	private String poGlAccount;

	@Column(name = "PO_GL_DESCRIPTION")
	private String poGlDescription;

	@Column(name = "ACTUAL_OR_COMMITTED_FLAG")
	private String actualOrCommittedFlag;

	@Column(name = "FI_PO_COST_CENTRE")
	private String flPoCostCentre;

	@Column(name = "VENDOR_CLEARING_DATE")
	private Timestamp vendorClearingDate;

	@Column(name = "GR_DATE")
	private Timestamp grDate;

	@Column(name = "PR_DATE")
	private Timestamp prDate;

	@Column(name = "INVOICE_DATE")
	private Timestamp invoiceDate;

	@Column(name = "REQUESTER_NAME")
	private String requesterName;

	@Column(name = "PO_PR_FLAG")
	private String poPrFlag;

	@Column(name = "TRANSACTION_REFERENCE_NUMBER")
	private String transactionReferenceNumber;

	@Column(name = "DOCUMENT_NUMBER")
	private String documentNumber;
	
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
	private String documentNumberFMLineItem;
	
	@Column(name = "TRANSACTION_NUMBER")
	private String transactionNumber;
	
	@Transient
	private String isFromSap = "Y";

	public AwardExpenseTransaction() {}

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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public BigDecimal getAmountInFmacurrency() {
		return amountInFmacurrency;
	}

	public void setAmountInFmacurrency(BigDecimal amountInFmacurrency) {
		this.amountInFmacurrency = amountInFmacurrency;
	}

	public Timestamp getBankClearingDate() {
		return bankClearingDate;
	}

	public void setBankClearingDate(Timestamp bankClearingDate) {
		this.bankClearingDate = bankClearingDate;
	}

	public Timestamp getFmPostingDate() {
		return fmPostingDate;
	}

	public void setFmPostingDate(Timestamp fmPostingDate) {
		this.fmPostingDate = fmPostingDate;
	}

	public Timestamp getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(Timestamp fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
	}

	public String getGlDescription() {
		return glDescription;
	}

	public void setGlDescription(String glDescription) {
		this.glDescription = glDescription;
	}

	public String getAsset() {
		return asset;
	}

	public void setAsset(String asset) {
		this.asset = asset;
	}

	public String getAssetInternalOrder() {
		return assetInternalOrder;
	}

	public void setAssetInternalOrder(String assetInternalOrder) {
		this.assetInternalOrder = assetInternalOrder;
	}

	public String getCommitmentItem() {
		return commitmentItem;
	}

	public void setCommitmentItem(String commitmentItem) {
		this.commitmentItem = commitmentItem;
	}

	public String getVendorName() {
		return vendorName;
	}

	public void setVendorName(String vendorName) {
		this.vendorName = vendorName;
	}

	public BigDecimal getTcAmount() {
		return tcAmount;
	}

	public void setTcAmount(BigDecimal tcAmount) {
		this.tcAmount = tcAmount;
	}

	public String getTransactionCurrency() {
		return transactionCurrency;
	}

	public void setTransactionCurrency(String transactionCurrency) {
		this.transactionCurrency = transactionCurrency;
	}

	public String getAssetLocation() {
		return assetLocation;
	}

	public void setAssetLocation(String assetLocation) {
		this.assetLocation = assetLocation;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getPredecessorDocNumber() {
		return predecessorDocNumber;
	}

	public void setPredecessorDocNumber(String predecessorDocNumber) {
		this.predecessorDocNumber = predecessorDocNumber;
	}

	public String getPredecessorDocItem() {
		return predecessorDocItem;
	}

	public void setPredecessorDocItem(String predecessorDocItem) {
		this.predecessorDocItem = predecessorDocItem;
	}

	public String getPredecessorDocItemAccount() {
		return predecessorDocItemAccount;
	}

	public void setPredecessorDocItemAccount(String predecessorDocItemAccount) {
		this.predecessorDocItemAccount = predecessorDocItemAccount;
	}

	public String getPredecessorDocItemOrgUnit() {
		return predecessorDocItemOrgUnit;
	}

	public void setPredecessorDocItemOrgUnit(String predecessorDocItemOrgUnit) {
		this.predecessorDocItemOrgUnit = predecessorDocItemOrgUnit;
	}

	public String getPredecessorDocCat() {
		return predecessorDocCat;
	}

	public void setPredecessorDocCat(String predecessorDocCat) {
		this.predecessorDocCat = predecessorDocCat;
	}

	public String getPredecessorDocCatText() {
		return predecessorDocCatText;
	}

	public void setPredecessorDocCatText(String predecessorDocCatText) {
		this.predecessorDocCatText = predecessorDocCatText;
	}

	public String getFmDocNumber() {
		return FmDocNumber;
	}

	public void setFmDocNumber(String fmDocNumber) {
		FmDocNumber = fmDocNumber;
	}

	public String getInvoiceNumber() {
		return InvoiceNumber;
	}

	public void setInvoiceNumber(String invoiceNumber) {
		InvoiceNumber = invoiceNumber;
	}

	public Timestamp getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(Timestamp documentDate) {
		this.documentDate = documentDate;
	}

	public String getReferenceDocNumber() {
		return referenceDocNumber;
	}

	public void setReferenceDocNumber(String referenceDocNumber) {
		this.referenceDocNumber = referenceDocNumber;
	}

	public String getPoNumber() {
		return poNumber;
	}

	public void setPoNumber(String poNumber) {
		this.poNumber = poNumber;
	}

	public Timestamp getPoDate() {
		return poDate;
	}

	public void setPoDate(Timestamp poDate) {
		this.poDate = poDate;
	}

	public String getAssetCostCenter() {
		return assetCostCenter;
	}

	public void setAssetCostCenter(String assetCostCenter) {
		this.assetCostCenter = assetCostCenter;
	}

	public String getAssetLocationCode() {
		return assetLocationCode;
	}

	public void setAssetLocationCode(String assetLocationCode) {
		this.assetLocationCode = assetLocationCode;
	}

	public String getVendorCode() {
		return vendorCode;
	}

	public void setVendorCode(String vendorCode) {
		this.vendorCode = vendorCode;
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

	public String getPoGlAccount() {
		return poGlAccount;
	}

	public void setPoGlAccount(String poGlAccount) {
		this.poGlAccount = poGlAccount;
	}

	public String getPoGlDescription() {
		return poGlDescription;
	}

	public void setPoGlDescription(String poGlDescription) {
		this.poGlDescription = poGlDescription;
	}

	public String getActualOrCommittedFlag() {
		return actualOrCommittedFlag;
	}

	public void setActualOrCommittedFlag(String actualOrCommittedFlag) {
		this.actualOrCommittedFlag = actualOrCommittedFlag;
	}

	public String getFlPoCostCentre() {
		return flPoCostCentre;
	}

	public void setFlPoCostCentre(String flPoCostCentre) {
		this.flPoCostCentre = flPoCostCentre;
	}

	public Timestamp getVendorClearingDate() {
		return vendorClearingDate;
	}

	public void setVendorClearingDate(Timestamp vendorClearingDate) {
		this.vendorClearingDate = vendorClearingDate;
	}

	public Integer getAwardExpenseTransactionId() {
		return awardExpenseTransactionId;
	}

	public void setAwardExpenseTransactionId(Integer awardExpenseTransactionId) {
		this.awardExpenseTransactionId = awardExpenseTransactionId;
	}

	public String getIsFromSap() {
		return isFromSap;
	}

	public void setIsFromSap(String isFromSap) {
		this.isFromSap = isFromSap;
	}

	public Timestamp getGrDate() {
		return grDate;
	}

	public void setGrDate(Timestamp grDate) {
		this.grDate = grDate;
	}

	public Timestamp getPrDate() {
		return prDate;
	}

	public void setPrDate(Timestamp prDate) {
		this.prDate = prDate;
	}

	public Timestamp getInvoiceDate() {
		return invoiceDate;
	}

	public void setInvoiceDate(Timestamp invoiceDate) {
		this.invoiceDate = invoiceDate;
	}

	public String getRequesterName() {
		return requesterName;
	}

	public void setRequesterName(String requesterName) {
		this.requesterName = requesterName;
	}

	public String getPoPrFlag() {
		return poPrFlag;
	}

	public void setPoPrFlag(String poPrFlag) {
		this.poPrFlag = poPrFlag;
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

	public String getDocumentNumberFMLineItem() {
		return documentNumberFMLineItem;
	}

	public void setDocumentNumberFMLineItem(String documentNumberFMLineItem) {
		this.documentNumberFMLineItem = documentNumberFMLineItem;
	}

	public String getTransactionNumber() {
		return transactionNumber;
	}

	public void setTransactionNumber(String transactionNumber) {
		this.transactionNumber = transactionNumber;
	}

}
