package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class BudgetPeriodPrintParameter {

	private String period;
	private List<LineItemData> lineItemList;
	private String startDate;
	private String endDate;
	private String directCost;
	private String modifiedDirectCost;
	private String indirectCost;
	private String costSharingAmount;
	private String underRecoveryAmount;
	private String totalCost;

	public String getPeriod() {
		return period;
	}

	public void setPeriod(String period) {
		this.period = period;
	}

	public List<LineItemData> getLineItemList() {
		return lineItemList;
	}

	public void setLineItemList(List<LineItemData> lineItemList) {
		this.lineItemList = lineItemList;
	}

	public String getStartDate() {
		return startDate;
	}

	public void setStartDate(String startDate) {
		this.startDate = startDate;
	}

	public String getEndDate() {
		return endDate;
	}

	public void setEndDate(String endDate) {
		this.endDate = endDate;
	}

	public String getDirectCost() {
		return directCost;
	}

	public void setDirectCost(String directCost) {
		this.directCost = directCost;
	}

	public String getModifiedDirectCost() {
		return modifiedDirectCost;
	}

	public void setModifiedDirectCost(String modifiedDirectCost) {
		this.modifiedDirectCost = modifiedDirectCost;
	}

	public String getIndirectCost() {
		return indirectCost;
	}

	public void setIndirectCost(String indirectCost) {
		this.indirectCost = indirectCost;
	}

	public String getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(String costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public String getUnderRecoveryAmount() {
		return underRecoveryAmount;
	}

	public void setUnderRecoveryAmount(String underRecoveryAmount) {
		this.underRecoveryAmount = underRecoveryAmount;
	}

	public String getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(String totalCost) {
		this.totalCost = totalCost;
	}

	public BudgetPeriodPrintParameter(String period, List<LineItemData> lineItemList) {
		this.period = period;
		if (lineItemList != null) {
			this.lineItemList = new ArrayList<LineItemData>(lineItemList);
			Collections.copy(this.lineItemList, lineItemList);
		} else {
			this.lineItemList = new ArrayList<LineItemData>();
		}
	}

	public BudgetPeriodPrintParameter() {
		super();
		lineItemList = new ArrayList<>();
	}

	public BudgetPeriodPrintParameter(String period, String startDate, String endDate, String directCost,
			String indirectCost, String totalCost) {
		super();
		this.period = period;
		this.startDate = startDate;
		this.endDate = endDate;
		this.directCost = directCost;
		this.indirectCost = indirectCost;
		this.totalCost = totalCost;
	}

}
