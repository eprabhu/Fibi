package com.polus.fibicomp.award.vo;

import java.util.List;

public class AwardBudgetImportInfo {

	private String  awardNumber;

	private String  ipNumber;

	private Integer pdNumber;

	private Integer pdBudgetId;

	private Integer pdBudgetVersion;

	private String  pdTitle;

	private Integer isAllPeriods;

	private List<BudgetImportPeriodsInfo> budgetImportPeriods;

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getIpNumber() {
		return ipNumber;
	}

	public void setIpNumber(String ipNumber) {
		this.ipNumber = ipNumber;
	}

	public Integer getPdNumber() {
		return pdNumber;
	}

	public void setPdNumber(Integer pdNumber) {
		this.pdNumber = pdNumber;
	}

	public Integer getPdBudgetId() {
		return pdBudgetId;
	}

	public void setPdBudgetId(Integer pdBudgetId) {
		this.pdBudgetId = pdBudgetId;
	}

	public Integer getPdBudgetVersion() {
		return pdBudgetVersion;
	}

	public void setPdBudgetVersion(Integer pdBudgetVersion) {
		this.pdBudgetVersion = pdBudgetVersion;
	}

	public String getPdTitle() {
		return pdTitle;
	}

	public void setPdTitle(String pdTitle) {
		this.pdTitle = pdTitle;
	}

	public Integer getIsAllPeriods() {
		return isAllPeriods;
	}

	public void setIsAllPeriods(Integer isAllPeriods) {
		this.isAllPeriods = isAllPeriods;
	}

	public List<BudgetImportPeriodsInfo> getBudgetImportPeriods() {
		return budgetImportPeriods;
	}

	public void setBudgetImportPeriods(List<BudgetImportPeriodsInfo> budgetImportPeriods) {
		this.budgetImportPeriods = budgetImportPeriods;
	}

}
