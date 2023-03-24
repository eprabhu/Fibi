package com.polus.fibicomp.award.expense.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.validation.constraints.Pattern;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.award.expense.comparator.AwardExpenseDetailsExtComparatorByUpdateTimestamp;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetailsExt;

public class AwardExpenseDetailVO implements Comparable<AwardExpenseDetailVO> {

	@Pattern(regexp="^[0-9 -]*$", message="awardNumber must not include special characters.")
	private String awardNumber;

	@Pattern(regexp="^[0-9a-zA-Z]*$", message="Account Number must not include special characters.")
	private String accountNumber;

	private String internalOrderCode;

	private BigDecimal committedAmount = BigDecimal.ZERO;

	private String isFromSap;

	private String description;

	private String budgetCategory;

	private String lineItem;

	private BigDecimal quantity = BigDecimal.ZERO;

	private BigDecimal originalApprovedBudget = BigDecimal.ZERO;

	private BigDecimal latestApprovedBudget = BigDecimal.ZERO;

	private BigDecimal expenditureToDate = BigDecimal.ZERO;

	private BigDecimal balance = BigDecimal.ZERO;

	private BigDecimal utilizationRate = BigDecimal.ZERO;

	private BigDecimal balanceCommittedBudget = BigDecimal.ZERO;

	private String budgetCategoryTypeCode;

	private Timestamp updateTimeStamp;

	private String updateUser;

	private List<AwardExpenseDetailsExt> awardExpenseDetailsExts;

	private BigDecimal updatedCommittedAmount = BigDecimal.ZERO;

	private Integer awardExpenseDetailsId;

	private boolean studentHoursFlag = false;

	private Integer sortOrder;

	private List<AwardExpensePersonVO> awardExpensePersonVOs;

	private Integer budgetDetailId;

	private String personName;

	private boolean unAssignedTransaction = false;

	@Pattern(regexp="^[EP]*$", message="type must include E and P.")
	private String type;

	public AwardExpenseDetailVO() {
		awardExpenseDetailsExts = new ArrayList<>();
		awardExpensePersonVOs = new ArrayList<>();
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

	public BigDecimal getCommittedAmount() {
		if (awardExpenseDetailsExts != null && !awardExpenseDetailsExts.isEmpty()) {
			for (AwardExpenseDetailsExt expenseDetailsExt : awardExpenseDetailsExts) {
				if (expenseDetailsExt.getIsFromSap().equals("Y")) {
					committedAmount = committedAmount.add(expenseDetailsExt.getCommittedAmount());
				}
			}
		}
		return committedAmount;
	}

	public void setCommittedAmount(BigDecimal committedAmount) {
		this.committedAmount = committedAmount;
	}

	public String getIsFromSap() {
		return isFromSap;
	}

	public void setIsFromSap(String isFromSap) {
		this.isFromSap = isFromSap;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(String budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public String getLineItem() {
		return lineItem;
	}

	public void setLineItem(String lineItem) {
		this.lineItem = lineItem;
	}

	public BigDecimal getQuantity() {
		return quantity;
	}

	public void setQuantity(BigDecimal quantity) {
		this.quantity = quantity;
	}

	public BigDecimal getOriginalApprovedBudget() {
		return originalApprovedBudget;
	}

	public void setOriginalApprovedBudget(BigDecimal originalApprovedBudget) {
		this.originalApprovedBudget = originalApprovedBudget;
	}

	public BigDecimal getLatestApprovedBudget() {
		return latestApprovedBudget;
	}

	public void setLatestApprovedBudget(BigDecimal latestApprovedBudget) {
		this.latestApprovedBudget = latestApprovedBudget;
	}

	public BigDecimal getExpenditureToDate() {
		return expenditureToDate;
	}

	public void setExpenditureToDate(BigDecimal expenditureToDate) {
		this.expenditureToDate = expenditureToDate;
	}

	public BigDecimal getBalance() {
		return balance;
	}

	public void setBalance(BigDecimal balance) {
		this.balance = balance;
	}

	public BigDecimal getUtilizationRate() {
		return utilizationRate;
	}

	public void setUtilizationRate(BigDecimal utilizationRate) {
		this.utilizationRate = utilizationRate;
	}

	public BigDecimal getBalanceCommittedBudget() {
		balanceCommittedBudget = balanceCommittedBudget
				.add((latestApprovedBudget.subtract(expenditureToDate)).subtract(this.getUpdatedCommittedAmount()));
		return balanceCommittedBudget;
	}

	public void setBalanceCommittedBudget(BigDecimal balanceCommittedBudget) {
		this.balanceCommittedBudget = balanceCommittedBudget;
	}

	public String getBudgetCategoryTypeCode() {
		return budgetCategoryTypeCode;
	}

	public void setBudgetCategoryTypeCode(String budgetCategoryTypeCode) {
		this.budgetCategoryTypeCode = budgetCategoryTypeCode;
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

	public List<AwardExpenseDetailsExt> getAwardExpenseDetailsExts() {
		if (awardExpenseDetailsExts != null && !awardExpenseDetailsExts.isEmpty()) {
			Collections.sort(awardExpenseDetailsExts, new AwardExpenseDetailsExtComparatorByUpdateTimestamp());
		}
		return awardExpenseDetailsExts;
	}

	public void setAwardExpenseDetailsExts(List<AwardExpenseDetailsExt> awardExpenseDetailsExts) {
		this.awardExpenseDetailsExts = awardExpenseDetailsExts;
	}

	public BigDecimal getUpdatedCommittedAmount() {
		updatedCommittedAmount = BigDecimal.ZERO;
		if (awardExpenseDetailsExts != null && !awardExpenseDetailsExts.isEmpty()) {
			for (AwardExpenseDetailsExt expenseDetailsExt : awardExpenseDetailsExts) {
				updatedCommittedAmount = updatedCommittedAmount.add(expenseDetailsExt.getCommittedAmount());
//				if (expenseDetailsExt.getIsFromSap().equals("Y")) {
//					committedAmount = committedAmount.add(expenseDetailsExt.getCommittedAmount());
//				}
			}
		}
		return updatedCommittedAmount;
	}

	public void setUpdatedCommittedAmount(BigDecimal updatedCommittedAmount) {
		this.updatedCommittedAmount = updatedCommittedAmount;
	}

	public Integer getAwardExpenseDetailsId() {
		return awardExpenseDetailsId;
	}

	public void setAwardExpenseDetailsId(Integer awardExpenseDetailsId) {
		this.awardExpenseDetailsId = awardExpenseDetailsId;
	}

	public boolean isStudentHoursFlag() {
		return studentHoursFlag;
	}

	public void setStudentHoursFlag(boolean studentHoursFlag) {
		this.studentHoursFlag = studentHoursFlag;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

	@Override
	public int compareTo(AwardExpenseDetailVO o) {
		if (this.sortOrder == null || o.getSortOrder() == null) {
			return 0;
		}
		return new CompareToBuilder().append(this.sortOrder, o.getSortOrder()).toComparison();
	}

	public List<AwardExpensePersonVO> getAwardExpensePersonVOs() {
		return awardExpensePersonVOs;
	}

	public void setAwardExpensePersonVOs(List<AwardExpensePersonVO> awardExpensePersonVOs) {
		this.awardExpensePersonVOs = awardExpensePersonVOs;
	}

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public boolean isUnAssignedTransaction() {
		return unAssignedTransaction;
	}

	public void setUnAssignedTransaction(boolean unAssignedTransaction) {
		this.unAssignedTransaction = unAssignedTransaction;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

}
