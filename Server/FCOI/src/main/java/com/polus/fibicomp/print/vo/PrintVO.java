package com.polus.fibicomp.print.vo;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.sql.Timestamp;
import java.util.List;

public class PrintVO {

	Integer proposalId;

	Integer budgetId;

	String isBudgetSummaryPrint;

	String isSimpleBudgetPrint;

	String isDetailedBudgetPrint;
	
	String isPersonnelBudgetPrint;

	Integer awardId;

	private String fmPostingStartDate;

	private String fmPostingEndDate;

	private String isExpenseOrPurchase;

	private String isRevenue;

	private String documentHeading;

	private String actualOrCommittedFlag;

	private String awardNumber;

	private String accountNumber;

	private String internalOrderCode;

	private String tabHeading;

	private Timestamp fiPostingStartDate;

	private Timestamp fiPostingEndDate;

	private List<String> internalOrderCodes;
	
	private String isShowAllTransactions;

	private String personId;

	private String userName;

	private Integer instituteProposalId;

	@NotNull
	@NotEmpty
	private List<String> letterTemplateTypeCodes;

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public String getIsBudgetSummaryPrint() {
		return isBudgetSummaryPrint;
	}

	public void setIsBudgetSummaryPrint(String isBudgetSummaryPrint) {
		this.isBudgetSummaryPrint = isBudgetSummaryPrint;
	}

	public String getIsSimpleBudgetPrint() {
		return isSimpleBudgetPrint;
	}

	public void setIsSimpleBudgetPrint(String isSimpleBudgetPrint) {
		this.isSimpleBudgetPrint = isSimpleBudgetPrint;
	}

	public String getIsDetailedBudgetPrint() {
		return isDetailedBudgetPrint;
	}

	public void setIsDetailedBudgetPrint(String isDetailedBudgetPrint) {
		this.isDetailedBudgetPrint = isDetailedBudgetPrint;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Timestamp getFiPostingStartDate() {
		return fiPostingStartDate;
	}

	public void setFiPostingStartDate(Timestamp fiPostingStartDate) {
		this.fiPostingStartDate = fiPostingStartDate;
	}

	public Timestamp getFiPostingEndDate() {
		return fiPostingEndDate;
	}

	public void setFiPostingEndDate(Timestamp fiPostingEndDate) {
		this.fiPostingEndDate = fiPostingEndDate;
	}

	public String getIsRevenue() {
		return isRevenue;
	}

	public void setIsRevenue(String isRevenue) {
		this.isRevenue = isRevenue;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public String getActualOrCommittedFlag() {
		return actualOrCommittedFlag;
	}

	public void setActualOrCommittedFlag(String actualOrCommittedFlag) {
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

	public String getIsExpenseOrPurchase() {
		return isExpenseOrPurchase;
	}

	public void setIsExpenseOrPurchase(String isExpenseOrPurchase) {
		this.isExpenseOrPurchase = isExpenseOrPurchase;
	}

	public String getTabHeading() {
		return tabHeading;
	}

	public void setTabHeading(String tabHeading) {
		this.tabHeading = tabHeading;
	}

	public String getIsPersonnelBudgetPrint() {
		return isPersonnelBudgetPrint;
	}

	public void setIsPersonnelBudgetPrint(String isPersonnelBudgetPrint) {
		this.isPersonnelBudgetPrint = isPersonnelBudgetPrint;
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

	public String getIsShowAllTransactions() {
		return isShowAllTransactions;
	}

	public void setIsShowAllTransactions(String isShowAllTransactions) {
		this.isShowAllTransactions = isShowAllTransactions;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public List<String> getLetterTemplateTypeCodes() {
		return letterTemplateTypeCodes;
	}

	public void setLetterTemplateTypeCodes(List<String> letterTemplateTypeCodes) {
		this.letterTemplateTypeCodes = letterTemplateTypeCodes;
	}

	public Integer getInstituteProposalId() {
		return instituteProposalId;
	}

	public void setInstituteProposalId(Integer instituteProposalId) {
		this.instituteProposalId = instituteProposalId;
	}

}
