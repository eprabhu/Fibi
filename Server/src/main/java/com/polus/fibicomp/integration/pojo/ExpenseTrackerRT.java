package com.polus.fibicomp.integration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "EXPENSE_TRACKER_RT")
public class ExpenseTrackerRT implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EXPENSE_TRACKER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EXPENSE_TRACKER_ID_GENERATOR")
	@SequenceGenerator(name="EXPENSE_TRACKER_ID_GENERATOR", sequenceName = "EXPENSE_TRACKER_ID_GENERATOR", allocationSize=1)
	private Integer expenseTrackerId;

	@Column(name = "AC")
	private String ac;

	@Column(name = "FI_PO_COST_CENTRE")
	private String fiPoCostCentre;

	@Column(name = "FUND_CENTRE")
	private String fundCentre;

	@Column(name = "FUND")  
	private String fund;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "AMOUNT_IN_FMA_CURRENCY")
	private String amountInFmaCurrency;

	@Column(name = "REMARKS")
	private String remarks;

	@Column(name = "VENDOR_CLEARING_DATE")
	private String vendorClearingDate;

	@Column(name = "BANK_CLEARING_DATE")
	private String bankClearingDate;

	@Column(name = "FM_POSTING_DATE")
	private String fmPostingDate;

	@Column(name = "FI_POSTING_DATE")
	private String fiPostingDate;

	@Column(name = "FI_GL_ACCOUNT")  
	private String fiGlAccount;

	@Column(name = "FI_GL_DESCRIPTION")
	private String fiGlDescription;

	@Column(name = "PO_GL_ACCOUNT")
	private String poGlAccount;

	@Column(name = "PO_GL_DESCRIPTION")
	private String poGlDescription;

	@Column(name = "ASSET")
	private String asset;

	@Column(name = "SUBNUMBER")
	private String subnumber;

	@Column(name = "ASSET_INTERNAL_ORDER")
	private String assetInternalCode;

	@Column(name = "ASSET_COST_CENTER")
	private String assetCostCenter;

	@Column(name = "ASSET_LOCATION_CODE")  
	private String assetLocationCode;

	@Column(name = "ASSET_LOCATION_DESC")
	private String assetLocationDescription;

	@Column(name = "COMMITMENT_ITEM")
	private String commitmentItem;

	@Column(name = "VENDOR_CODE")
	private String vendorCode;

	@Column(name = "VENDOR_NAME_1")
	private String vendorNameOne;

	@Column(name = "VENDOR_NAME_2")
	private String vendorNameTwo;

	@Column(name = "TC_AMOUNT")
	private String tcAmount;

	@Column(name = "TRANSACTION_CURRENCY")
	private String transactionCurrency;

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
	private String fmDocNumber;

	@Column(name = "FI_REFERENCE")
	private String fiReference;

	@Column(name = "DOCUMENT_DATE")
	private String documentDate;

	@Column(name = "PO_NUMBER")
	private String poNumber;

	@Column(name = "PO_DATE")
	private String poDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getExpenseTrackerId() {
		return expenseTrackerId;
	}

	public void setExpenseTrackerId(Integer expenseTrackerId) {
		this.expenseTrackerId = expenseTrackerId;
	}

	public String getAc() {
		return ac;
	}

	public void setAc(String ac) {
		this.ac = ac;
	}

	public String getFiPoCostCentre() {
		return fiPoCostCentre;
	}

	public void setFiPoCostCentre(String fiPoCostCentre) {
		this.fiPoCostCentre = fiPoCostCentre;
	}

	public String getFundCentre() {
		return fundCentre;
	}

	public void setFundCentre(String fundCentre) {
		this.fundCentre = fundCentre;
	}

	public String getFund() {
		return fund;
	}

	public void setFund(String fund) {
		this.fund = fund;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public String getAmountInFmaCurrency() {
		return amountInFmaCurrency;
	}

	public void setAmountInFmaCurrency(String amountInFmaCurrency) {
		this.amountInFmaCurrency = amountInFmaCurrency;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public String getVendorClearingDate() {
		return vendorClearingDate;
	}

	public void setVendorClearingDate(String vendorClearingDate) {
		this.vendorClearingDate = vendorClearingDate;
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

	public String getFiPostingDate() {
		return fiPostingDate;
	}

	public void setFiPostingDate(String fiPostingDate) {
		this.fiPostingDate = fiPostingDate;
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

	public String getAsset() {
		return asset;
	}

	public void setAsset(String asset) {
		this.asset = asset;
	}

	public String getSubnumber() {
		return subnumber;
	}

	public void setSubnumber(String subnumber) {
		this.subnumber = subnumber;
	}

	public String getAssetInternalCode() {
		return assetInternalCode;
	}

	public void setAssetInternalCode(String assetInternalCode) {
		this.assetInternalCode = assetInternalCode;
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

	public String getAssetLocationDescription() {
		return assetLocationDescription;
	}

	public void setAssetLocationDescription(String assetLocationDescription) {
		this.assetLocationDescription = assetLocationDescription;
	}

	public String getCommitmentItem() {
		return commitmentItem;
	}

	public void setCommitmentItem(String commitmentItem) {
		this.commitmentItem = commitmentItem;
	}

	public String getVendorCode() {
		return vendorCode;
	}

	public void setVendorCode(String vendorCode) {
		this.vendorCode = vendorCode;
	}

	public String getVendorNameOne() {
		return vendorNameOne;
	}

	public void setVendorNameOne(String vendorNameOne) {
		this.vendorNameOne = vendorNameOne;
	}

	public String getVendorNameTwo() {
		return vendorNameTwo;
	}

	public void setVendorNameTwo(String vendorNameTwo) {
		this.vendorNameTwo = vendorNameTwo;
	}

	public String getTcAmount() {
		return tcAmount;
	}

	public void setTcAmount(String tcAmount) {
		this.tcAmount = tcAmount;
	}

	public String getTransactionCurrency() {
		return transactionCurrency;
	}

	public void setTransactionCurrency(String transactionCurrency) {
		this.transactionCurrency = transactionCurrency;
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
		return fmDocNumber;
	}

	public void setFmDocNumber(String fmDocNumber) {
		this.fmDocNumber = fmDocNumber;
	}

	public String getFiReference() {
		return fiReference;
	}

	public void setFiReference(String fiReference) {
		this.fiReference = fiReference;
	}

	public String getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(String documentDate) {
		this.documentDate = documentDate;
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

}
