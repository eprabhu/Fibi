package com.polus.fibicomp.award.datesandamounts.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.pojo.Currency;

public class AwardFunds {

	private Timestamp startDate;

	private Timestamp endDate;

	private BigDecimal obligatedTotal = BigDecimal.ZERO;

	private BigDecimal anticipatedTotal = BigDecimal.ZERO;

	private BigDecimal obligatedDistributableTotal = BigDecimal.ZERO;

	private BigDecimal anticipatedDistributableTotal = BigDecimal.ZERO;

	private BigDecimal totalCost = BigDecimal.ZERO;

	private BigDecimal totalCostInCurrency = BigDecimal.ZERO;

	private String currencyCode;

	private Currency currency;

	private List<Currency> currencyDetails;

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public BigDecimal getObligatedTotal() {
		return obligatedTotal;
	}

	public void setObligatedTotal(BigDecimal obligatedTotal) {
		this.obligatedTotal = obligatedTotal;
	}

	public BigDecimal getAnticipatedTotal() {
		return anticipatedTotal;
	}

	public void setAnticipatedTotal(BigDecimal anticipatedTotal) {
		this.anticipatedTotal = anticipatedTotal;
	}

	public BigDecimal getObligatedDistributableTotal() {
		return obligatedDistributableTotal;
	}

	public void setObligatedDistributableTotal(BigDecimal obligatedDistributableTotal) {
		this.obligatedDistributableTotal = obligatedDistributableTotal;
	}

	public BigDecimal getAnticipatedDistributableTotal() {
		return anticipatedDistributableTotal;
	}

	public void setAnticipatedDistributableTotal(BigDecimal anticipatedDistributableTotal) {
		this.anticipatedDistributableTotal = anticipatedDistributableTotal;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
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

	public Currency getCurrency() {
		return currency;
	}

	public void setCurrency(Currency currency) {
		this.currency = currency;
	}

	/**
	 * @return the currencyDetails
	 */
	public List<Currency> getCurrencyDetails() {
		return currencyDetails;
	}

	/**
	 * @param currencyDetails the currencyDetails to set
	 */
	public void setCurrencyDetails(List<Currency> currencyDetails) {
		this.currencyDetails = currencyDetails;
	}

}
