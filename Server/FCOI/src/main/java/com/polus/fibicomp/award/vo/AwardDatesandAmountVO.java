package com.polus.fibicomp.award.vo;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAmountTransaction;
import com.polus.fibicomp.award.pojo.AwardTransactionType;

public class AwardDatesandAmountVO {

	private Integer awardId;

	private String personId;

	private String awardNumber;

	private AwardAmountInfo awardAmountInfo;

	private AwardAmountTransaction awardAmountTransaction;

	private List<AwardTransactionType> awardTransactionTypes;

	private List<AwardAmountInfo> awardAmountInfos;

	private List<AwardAmountTransaction> awardAmountTransactions;

	private List<String> awardNumbers;

	private List<String> sourceAccount;

	private List<String> destinationAccount;

	private boolean isDestinationAccount = false;

	private boolean isInternalTransaction = false;

	private boolean isExternalTransaction = false;

	private boolean isSourceAccount = false;

	private boolean canAddTotal = false;

	private boolean isUnRelatedTransaction = false;

	private boolean isAwardNumberFoundInSource = false;

	private boolean isAwardNumberFoundInDestination = false;

	private boolean isAwardNumberFoundInBoth = false;

	private String message;

	private BigDecimal totalCostInCurrency = BigDecimal.ZERO;

	private String currencyCode;

	private Integer awardSequenceNumber;

	private BigDecimal transactionId;

	private Boolean isDatesAndAmountsEditable = false;

	public AwardDatesandAmountVO() {
		this.awardTransactionTypes = new ArrayList<>();
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<AwardTransactionType> getAwardTransactionTypes() {
		return awardTransactionTypes;
	}

	public void setAwardTransactionTypes(List<AwardTransactionType> awardTransactionTypes) {
		this.awardTransactionTypes = awardTransactionTypes;
	}

	public AwardAmountInfo getAwardAmountInfo() {
		return awardAmountInfo;
	}

	public void setAwardAmountInfo(AwardAmountInfo awardAmountInfo) {
		this.awardAmountInfo = awardAmountInfo;
	}

	public AwardAmountTransaction getAwardAmountTransaction() {
		return awardAmountTransaction;
	}

	public void setAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction) {
		this.awardAmountTransaction = awardAmountTransaction;
	}

	public List<AwardAmountInfo> getAwardAmountInfos() {
		return awardAmountInfos;
	}

	public void setAwardAmountInfos(List<AwardAmountInfo> awardAmountInfos) {
		this.awardAmountInfos = awardAmountInfos;
	}

	public List<AwardAmountTransaction> getAwardAmountTransactions() {
		return awardAmountTransactions;
	}

	public void setAwardAmountTransactions(List<AwardAmountTransaction> awardAmountTransactions) {
		this.awardAmountTransactions = awardAmountTransactions;
	}

	public List<String> getAwardNumbers() {
		return awardNumbers;
	}

	public void setAwardNumbers(List<String> awardNumbers) {
		this.awardNumbers = awardNumbers;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public List<String> getSourceAccount() {
		return sourceAccount;
	}

	public void setSourceAccount(List<String> sourceAccount) {
		this.sourceAccount = sourceAccount;
	}

	public List<String> getDestinationAccount() {
		return destinationAccount;
	}

	public void setDestinationAccount(List<String> destinationAccount) {
		this.destinationAccount = destinationAccount;
	}

	public boolean isDestinationAccount() {
		return isDestinationAccount;
	}

	public void setDestinationAccount(boolean isDestinationAccount) {
		this.isDestinationAccount = isDestinationAccount;
	}

	public boolean isInternalTransaction() {
		return isInternalTransaction;
	}

	public void setInternalTransaction(boolean isInternalTransaction) {
		this.isInternalTransaction = isInternalTransaction;
	}

	public boolean isExternalTransaction() {
		return isExternalTransaction;
	}

	public void setExternalTransaction(boolean isExternalTransaction) {
		this.isExternalTransaction = isExternalTransaction;
	}

	public boolean isSourceAccount() {
		return isSourceAccount;
	}

	public void setSourceAccount(boolean isSourceAccount) {
		this.isSourceAccount = isSourceAccount;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public boolean isCanAddTotal() {
		return canAddTotal;
	}

	public void setCanAddTotal(boolean canAddTotal) {
		this.canAddTotal = canAddTotal;
	}

	public BigDecimal getTotalCostInCurrency() {
		return totalCostInCurrency;
	}

	public void setTotalCostInCurrency(BigDecimal totalCostInCurrency) {
		this.totalCostInCurrency = totalCostInCurrency;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public boolean isUnRelatedTransaction() {
		return isUnRelatedTransaction;
	}

	public void setUnRelatedTransaction(boolean isUnRelatedTransaction) {
		this.isUnRelatedTransaction = isUnRelatedTransaction;
	}

	public boolean isAwardNumberFoundInSource() {
		return isAwardNumberFoundInSource;
	}

	public void setAwardNumberFoundInSource(boolean isAwardNumberFoundInSource) {
		this.isAwardNumberFoundInSource = isAwardNumberFoundInSource;
	}

	public boolean isAwardNumberFoundInDestination() {
		return isAwardNumberFoundInDestination;
	}

	public void setAwardNumberFoundInDestination(boolean isAwardNumberFoundInDestination) {
		this.isAwardNumberFoundInDestination = isAwardNumberFoundInDestination;
	}

	public boolean isAwardNumberFoundInBoth() {
		return isAwardNumberFoundInBoth;
	}

	public void setAwardNumberFoundInBoth(boolean isAwardNumberFoundInBoth) {
		this.isAwardNumberFoundInBoth = isAwardNumberFoundInBoth;
	}

	public Integer getAwardSequenceNumber() {
		return awardSequenceNumber;
	}

	public void setAwardSequenceNumber(Integer awardSequenceNumber) {
		this.awardSequenceNumber = awardSequenceNumber;
	}

	public BigDecimal getTransactionId() {
		return transactionId;
	}

	public void setTransactionId(BigDecimal transactionId) {
		this.transactionId = transactionId;
	}

	public Boolean getIsDatesAndAmountsEditable() {
		return isDatesAndAmountsEditable;
	}

	public void setIsDatesAndAmountsEditable(Boolean isDatesAndAmountsEditable) {
		this.isDatesAndAmountsEditable = isDatesAndAmountsEditable;
	}

}
