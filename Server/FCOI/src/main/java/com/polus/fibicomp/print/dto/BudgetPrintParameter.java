package com.polus.fibicomp.print.dto;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.budget.vo.AwardBudgetPeriodSummary;
import com.polus.fibicomp.budget.vo.AwardBudgetSummaryVO;
import com.polus.fibicomp.budget.vo.BudgetPeriodSummary;
import com.polus.fibicomp.budget.vo.BudgetSummaryVO;
import com.polus.fibicomp.constants.Constants;

public class BudgetPrintParameter {

	private String periodStartDate;

	private String periodEndDate;

	private String directCost;

	private String indirectCost;

	private String subContractCost;

	private String costSharing;

	private String underRecoveryAmount;

	private String totalCost;

	private String personType;

	private String name;

	private String jobType;

	private String appointmentType;

	private String effectiveDate;

	private String costElemnt;

	private String lineItemDescription;

	private String quantity;

	private String itemLineCost;

	private String percentage;

	private String amount;

	private String period;

	private String baseSalary;

	private String totalModifiedDirectCost;

	private String personName;

	private String anniversaryDate;

	private List<BudgetSummaryVO> budgetSummaryVOs;

	private List<BudgetPeriodSummary> budgetPeriodSummaries;

	private String periodCostsTotalSum;

	private List<AwardBudgetSummaryVO> awardBudgetSummaryVOs;

	private List<AwardBudgetPeriodSummary> awardBudgetPeriodSummaries;
	
	private String totalInKind;
	
	private String totalOfTotal;
	
	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
	
	private String periodTotalFundRequestedSum ;
	
	private String periodTotalCostShareSum ;

	public String getPeriod() {
		return period;
	}

	public void setPeriod(String period) {
		this.period = period;
	}

	public String getPeriodStartDate() {
		return periodStartDate;
	}

	public void setPeriodStartDate(String periodStartDate) {
		this.periodStartDate = periodStartDate;
	}

	public String getPeriodEndDate() {
		return periodEndDate;
	}

	public void setPeriodEndDate(String periodEndDate) {
		this.periodEndDate = periodEndDate;
	}

	public String getDirectCost() {
		return directCost;
	}

	public void setDirectCost(String directCost) {
		this.directCost = directCost;
	}

	public String getIndirectCost() {
		return indirectCost;
	}

	public void setIndirectCost(String indirectCost) {
		this.indirectCost = indirectCost;
	}

	public String getSubContractCost() {
		return subContractCost;
	}

	public void setSubContractCost(String subContractCost) {
		this.subContractCost = subContractCost;
	}

	public String getCostSharing() {
		return costSharing;
	}

	public void setCostSharing(String costSharing) {
		this.costSharing = costSharing;
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

	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		this.personType = personType;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getJobType() {
		return jobType;
	}

	public void setJobType(String jobType) {
		this.jobType = jobType;
	}

	public String getAppointmentType() {
		return appointmentType;
	}

	public void setAppointmentType(String appointmentType) {
		this.appointmentType = appointmentType;
	}

	public String getEffectiveDate() {
		return effectiveDate;
	}

	public void setEffectiveDate(String effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

	public String getCostElemnt() {
		return costElemnt;
	}

	public void setCostElemnt(String costElemnt) {
		this.costElemnt = costElemnt;
	}

	public String getLineItemDescription() {
		return lineItemDescription;
	}

	public void setLineItemDescription(String lineItemDescription) {
		this.lineItemDescription = lineItemDescription;
	}

	public String getQuantity() {
		return quantity;
	}

	public void setQuantity(String quantity) {
		this.quantity = quantity;
	}

	public String getItemLineCost() {
		return itemLineCost;
	}

	public void setItemLineCost(String itemLineCost) {
		this.itemLineCost = itemLineCost;
	}

	public String getPercentage() {
		return percentage;
	}

	public void setPercentage(String percentage) {
		this.percentage = percentage;
	}

	public String getAmount() {
		return amount;
	}

	public void setAmount(String amount) {
		this.amount = amount;
	}

	public String getBaseSalary() {
		return baseSalary;
	}

	public void setBaseSalary(String baseSalary) {
		this.baseSalary = baseSalary;
	}

	public String getTotalModifiedDirectCost() {
		return totalModifiedDirectCost;
	}

	public void setTotalModifiedDirectCost(String totalModifiedDirectCost) {
		this.totalModifiedDirectCost = totalModifiedDirectCost;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public String getAnniversaryDate() {
		return anniversaryDate;
	}

	public void setAnniversaryDate(String anniversaryDate) {
		this.anniversaryDate = anniversaryDate;
	}

	public List<BudgetSummaryVO> getBudgetSummaryVOs() {
		return budgetSummaryVOs;
	}

	public void setBudgetSummaryVOs(List<BudgetSummaryVO> budgetSummaryVOs) {
		this.budgetSummaryVOs = budgetSummaryVOs;
	}

	public List<BudgetPeriodSummary> getBudgetPeriodSummaries() {
		return budgetPeriodSummaries;
	}

	public void setBudgetPeriodSummaries(List<BudgetPeriodSummary> budgetPeriodSummaries) {
		this.budgetPeriodSummaries = budgetPeriodSummaries;
	}

	public String getPeriodCostsTotalSum() {
		return periodCostsTotalSum;
	}

	public void setPeriodCostsTotalSum(String periodCostsTotalSum) {
		this.periodCostsTotalSum = periodCostsTotalSum;
	}

	public BudgetPrintParameter(String personType, String name, String jobType, String appointmentType,
			String effectiveDate, String baseSalary) {
		this.personType = personType;
		this.name = name;
		this.jobType = jobType;
		this.appointmentType = appointmentType;
		this.effectiveDate = effectiveDate;
		this.baseSalary = baseSalary;
	}

	public BudgetPrintParameter() {
		budgetSummaryVOs = new ArrayList<>();
	}

	public List<AwardBudgetSummaryVO> getAwardBudgetSummaryVOs() {
		return awardBudgetSummaryVOs;
	}

	public void setAwardBudgetSummaryVOs(List<AwardBudgetSummaryVO> awardBudgetSummaryVOs) {
		this.awardBudgetSummaryVOs = awardBudgetSummaryVOs;
	}

	public List<AwardBudgetPeriodSummary> getAwardBudgetPeriodSummaries() {
		return awardBudgetPeriodSummaries;
	}

	public void setAwardBudgetPeriodSummaries(List<AwardBudgetPeriodSummary> awardBudgetPeriodSummaries) {
		this.awardBudgetPeriodSummaries = awardBudgetPeriodSummaries;
	}

	public String getTotalInKind() {
		return totalInKind;
	}

	public void setTotalInKind(String totalInKind) {
		this.totalInKind = totalInKind;
	}

	public String getTotalOfTotal() {
		return totalOfTotal;
	}

	public void setTotalOfTotal(String totalOfTotal) {
		this.totalOfTotal = totalOfTotal;
	}

	public String getPeriodTotalFundRequestedSum() {
		return periodTotalFundRequestedSum;
	}

	public void setPeriodTotalFundRequestedSum(String periodTotalFundRequestedSum) {
		this.periodTotalFundRequestedSum = periodTotalFundRequestedSum;
	}

	public String getPeriodTotalCostShareSum() {
		return periodTotalCostShareSum;
	}

	public void setPeriodTotalCostShareSum(String periodTotalCostShareSum) {
		this.periodTotalCostShareSum = periodTotalCostShareSum;
	}

}
