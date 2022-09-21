package com.polus.fibicomp.award.datesandamounts.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.pojo.Currency;

public class AwardFundsDTO {

	private AwardFunds pendingAmountInfo;

	private AwardFunds activeAmountInfo;

	private BigDecimal instituteTotal = BigDecimal.ZERO;

	private BigDecimal sponsorTotal = BigDecimal.ZERO;

	private BigDecimal costShareTotal = BigDecimal.ZERO;

	private BigDecimal costShareDistributable = BigDecimal.ZERO;

	private Timestamp projectStartDate;

	private Timestamp projectEndDate;

	private List<Currency> currencyDetails;

	public AwardFunds getPendingAmountInfo() {
		return pendingAmountInfo;
	}

	public void setPendingAmountInfo(AwardFunds pendingAmountInfo) {
		this.pendingAmountInfo = pendingAmountInfo;
	}

	public AwardFunds getActiveAmountInfo() {
		return activeAmountInfo;
	}

	public void setActiveAmountInfo(AwardFunds activeAmountInfo) {
		this.activeAmountInfo = activeAmountInfo;
	}

	public BigDecimal getInstituteTotal() {
		return instituteTotal;
	}

	public void setInstituteTotal(BigDecimal instituteTotal) {
		this.instituteTotal = instituteTotal;
	}

	public BigDecimal getSponsorTotal() {
		return sponsorTotal;
	}

	public void setSponsorTotal(BigDecimal sponsorTotal) {
		this.sponsorTotal = sponsorTotal;
	}

	public BigDecimal getCostShareTotal() {
		return costShareTotal;
	}

	public void setCostShareTotal(BigDecimal costShareTotal) {
		this.costShareTotal = costShareTotal;
	}

	public BigDecimal getCostShareDistributable() {
		return costShareDistributable;
	}

	public void setCostShareDistributable(BigDecimal costShareDistributable) {
		this.costShareDistributable = costShareDistributable;
	}

	public Timestamp getProjectStartDate() {
		return projectStartDate;
	}

	public void setProjectStartDate(Timestamp projectStartDate) {
		this.projectStartDate = projectStartDate;
	}

	public Timestamp getProjectEndDate() {
		return projectEndDate;
	}

	public void setProjectEndDate(Timestamp projectEndDate) {
		this.projectEndDate = projectEndDate;
	}

	public List<Currency> getCurrencyDetails() {
		return currencyDetails;
	}

	public void setCurrencyDetails(List<Currency> currencyDetails) {
		this.currencyDetails = currencyDetails;
	}

}
