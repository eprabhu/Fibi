package com.polus.fibicomp.award.datesandamounts.dto;

import java.util.List;

public class DatesAndAmountsDTO {

	private List<String> awardNumbers;

	private boolean canAddTotal = false;

	private boolean isUnRelatedTransaction = false;

	public List<String> getAwardNumbers() {
		return awardNumbers;
	}

	public void setAwardNumbers(List<String> awardNumbers) {
		this.awardNumbers = awardNumbers;
	}

	public boolean isCanAddTotal() {
		return canAddTotal;
	}

	public void setCanAddTotal(boolean canAddTotal) {
		this.canAddTotal = canAddTotal;
	}

	public boolean isUnRelatedTransaction() {
		return isUnRelatedTransaction;
	}

	public void setUnRelatedTransaction(boolean isUnRelatedTransaction) {
		this.isUnRelatedTransaction = isUnRelatedTransaction;
	}
}
