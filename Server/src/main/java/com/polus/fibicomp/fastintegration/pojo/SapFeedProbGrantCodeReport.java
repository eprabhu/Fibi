package com.polus.fibicomp.fastintegration.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "SAP_FEED_PROB_GRANTCODE_REPORT")
public class SapFeedProbGrantCodeReport implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_PROB_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_PROB_ID_GENERATOR", sequenceName = "SAP_FEED_PROB_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;

	@Column(name = "GRANT_CODE")
	private String grantCode;

	@Column(name = "AWARD_ID")
	private BigDecimal awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "BUSINESS_AREA")
	private String businessArea;

	@Column(name = "VARIATION_TYPE_CODE")
	private String variationTypeCode;

	@Column(name = "VARIATION_TYPE")
	private String variationType;

	@Column(name = "ACCOUNT_TYPE")
	private String accountType;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "IO_CODE")
	private String ioCode;

	@Column(name = "PROCESS")
	private String process;

	@Column(name = "FUND_CODE")
	private String fundCode;

	@Column(name = "BUDGET_AMOUNT", precision = 12, scale = 2)
	private BigDecimal budgetAmount;

	@Column(name = "CUR_LINE_ITEM_COST", precision = 12, scale = 2)
	private BigDecimal curLineItemCost;

	@Column(name = "PREV_LINE_ITEM_COST", precision = 12, scale = 2)
	private BigDecimal prevLineItemCost;

	@Column(name = "CUR_PI_NAME")
	private String curPiName;

	@Column(name = "COST_ELEMENT_DESCRIPTION")
	private String costElementDescription;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PROFIT_CENTER")
	private String profitCenter;

	@Column(name = "COST_CENTER")
	private String costCenter;

	@Column(name = "FUND_CENTER")
	private String fundCenter;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public Integer getFeedId() {
		return feedId;
	}

	public void setFeedId(Integer feedId) {
		this.feedId = feedId;
	}

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public BigDecimal getAwardId() {
		return awardId;
	}

	public void setAwardId(BigDecimal awardId) {
		this.awardId = awardId;
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

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getVariationTypeCode() {
		return variationTypeCode;
	}

	public void setVariationTypeCode(String variationTypeCode) {
		this.variationTypeCode = variationTypeCode;
	}

	public String getVariationType() {
		return variationType;
	}

	public void setVariationType(String variationType) {
		this.variationType = variationType;
	}

	public String getAccountType() {
		return accountType;
	}

	public void setAccountType(String accountType) {
		this.accountType = accountType;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getIoCode() {
		return ioCode;
	}

	public void setIoCode(String ioCode) {
		this.ioCode = ioCode;
	}

	public String getProcess() {
		return process;
	}

	public void setProcess(String process) {
		this.process = process;
	}

	public String getFundCode() {
		return fundCode;
	}

	public void setFundCode(String fundCode) {
		this.fundCode = fundCode;
	}

	public BigDecimal getBudgetAmount() {
		return budgetAmount;
	}

	public void setBudgetAmount(BigDecimal budgetAmount) {
		this.budgetAmount = budgetAmount;
	}

	public BigDecimal getCurLineItemCost() {
		return curLineItemCost;
	}

	public void setCurLineItemCost(BigDecimal curLineItemCost) {
		this.curLineItemCost = curLineItemCost;
	}

	public BigDecimal getPrevLineItemCost() {
		return prevLineItemCost;
	}

	public void setPrevLineItemCost(BigDecimal prevLineItemCost) {
		this.prevLineItemCost = prevLineItemCost;
	}

	public String getCurPiName() {
		return curPiName;
	}

	public void setCurPiName(String curPiName) {
		this.curPiName = curPiName;
	}

	public String getCostElementDescription() {
		return costElementDescription;
	}

	public void setCostElementDescription(String costElementDescription) {
		this.costElementDescription = costElementDescription;
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

	public String getBusinessArea() {
		return businessArea;
	}

	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

	public String getProfitCenter() {
		return profitCenter;
	}

	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	public String getCostCenter() {
		return costCenter;
	}

	public void setCostCenter(String costCenter) {
		this.costCenter = costCenter;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

}
