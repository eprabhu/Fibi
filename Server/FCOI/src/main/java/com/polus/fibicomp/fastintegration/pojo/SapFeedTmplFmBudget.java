package com.polus.fibicomp.fastintegration.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SAP_FEED_TMPL_FM_BUDGET")
@EntityListeners(AuditingEntityListener.class)
public class SapFeedTmplFmBudget implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_TMPL_FM_BUDGET_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_TMPL_FM_BUDGET_ID_GENERATOR", sequenceName = "SAP_FEED_TMPL_FM_BUDGET_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;

	@Column(name = "LINE_ITEM")
	private String lineItem;

	@Column(name = "BCS_VALUE_TYPE")
	private String bcsValueType;

	@Column(name = "PROCESS")
	private String process;

	@Column(name = "DOC_TYPE")
	private String docType;

	@Column(name = "BUDGET_VERSION")
	private String budgetVersion;

	@Column(name = "DOCUMENT_DATE")
	private Timestamp documentDate;

	@Column(name= "YEAR")
	private Integer year;
	
	@Column(name = "BUDGET_TYPE")
	private String budgetType;

	@Column(name = "FM_GRANT")
	private String fmGrant;

	@Column(name = "FUND_CODE")
	private String fundCode;

	@Column(name = "FUND_CENTER")
	private String fundCenter;

	@Column(name = "COMMITMENT_ITEM")
	private String commitmentItem;

	@Column(name = "FUNCTIONAL_AREA")
	private String functionalArea;

	@Column(name = "FUNDED_PROGRAM")
	private String fundedProgram;

	@Column(name = "BUDGET_AMOUNT", precision = 12, scale = 2)
	private BigDecimal budgetAmount;

	@Column(name = "LOCAL_CURRENCY")
	private String localCurrency;

	public SapFeedTmplFmBudget() {}

	public SapFeedTmplFmBudget(Integer batchId, String feedStatus, String errorMessage, String fundedProgram) {
		super();
		this.batchId = batchId;
		this.feedStatus = feedStatus;
		this.errorMessage = errorMessage;
		this.fundedProgram = fundedProgram;
	}

	@Column(name = "DISTRIBUTION_KEY")
	private String distributionKey;

	@Column(name = "LINE_ITEM_TEXT")
	private String lineItemText;

	@Column(name = "CREATE_STATUS")
	private String createStatus;

	@Column(name = "FEED_STATUS")
	private String feedStatus;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "USER_COMMENT")
	private String userComment;

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

	public String getLineItem() {
		return lineItem;
	}

	public void setLineItem(String lineItem) {
		this.lineItem = lineItem;
	}

	public String getBcsValueType() {
		return bcsValueType;
	}

	public void setBcsValueType(String bcsValueType) {
		this.bcsValueType = bcsValueType;
	}

	public String getProcess() {
		return process;
	}

	public void setProcess(String process) {
		this.process = process;
	}

	public String getDocType() {
		return docType;
	}

	public void setDocType(String docType) {
		this.docType = docType;
	}

	public String getBudgetVersion() {
		return budgetVersion;
	}

	public void setBudgetVersion(String budgetVersion) {
		this.budgetVersion = budgetVersion;
	}

	public Timestamp getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(Timestamp documentDate) {
		this.documentDate = documentDate;
	}

	public Integer getYear() {
		return year;
	}

	public void setYear(Integer year) {
		this.year = year;
	}

	public String getBudgetType() {
		return budgetType;
	}

	public void setBudgetType(String budgetType) {
		this.budgetType = budgetType;
	}

	public String getFundCode() {
		return fundCode;
	}

	public void setFundCode(String fundCode) {
		this.fundCode = fundCode;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public String getCommitmentItem() {
		return commitmentItem;
	}

	public void setCommitmentItem(String commitmentItem) {
		this.commitmentItem = commitmentItem;
	}

	public String getFunctionalArea() {
		return functionalArea;
	}

	public void setFunctionalArea(String functionalArea) {
		this.functionalArea = functionalArea;
	}

	public String getFundedProgram() {
		return fundedProgram;
	}

	public void setFundedProgram(String fundedProgram) {
		this.fundedProgram = fundedProgram;
	}

	public BigDecimal getBudgetAmount() {
		return budgetAmount;
	}

	public void setBudgetAmount(BigDecimal budgetAmount) {
		this.budgetAmount = budgetAmount;
	}

	public String getLocalCurrency() {
		return localCurrency;
	}

	public void setLocalCurrency(String localCurrency) {
		this.localCurrency = localCurrency;
	}

	public String getDistributionKey() {
		return distributionKey;
	}

	public void setDistributionKey(String distributionKey) {
		this.distributionKey = distributionKey;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getCreateStatus() {
		return createStatus;
	}

	public void setCreateStatus(String createStatus) {
		this.createStatus = createStatus;
	}

	public String getFeedStatus() {
		return feedStatus;
	}

	public void setFeedStatus(String feedStatus) {
		this.feedStatus = feedStatus;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getLineItemText() {
		return lineItemText;
	}

	public void setLineItemText(String lineItemText) {
		this.lineItemText = lineItemText;
	}

	public String getFmGrant() {
		return fmGrant;
	}

	public void setFmGrant(String fmGrant) {
		this.fmGrant = fmGrant;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}

}
