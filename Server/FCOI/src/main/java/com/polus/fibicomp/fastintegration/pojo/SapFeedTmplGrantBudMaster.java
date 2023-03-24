package com.polus.fibicomp.fastintegration.pojo;

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
@Table(name = "SAP_FEED_TMPL_GRANT_BUD_MASTER")
@EntityListeners(AuditingEntityListener.class)
public class SapFeedTmplGrantBudMaster {
	
	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_TMPL_GRANT_BUD_MASTER_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_TMPL_GRANT_BUD_MASTER_ID_GENERATOR", sequenceName = "SAP_FEED_TMPL_GRANT_BUD_MASTER_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;
	
	@Column(name = "PROCESS")
	private String process;
	
	@Column(name = "GRANT_CODE")
	private String grantCode;
	
	@Column(name = "HEADER_DESCRIPTION")
	private String headerDescription;
	
	@Column(name = "FUND_CODE")
	private String fundCode;
	
	@Column(name = "SPONSOR_PROGRAM")
	private String sponsorProgram;
	
	@Column(name = "SPONSOR_CLASS")
	private String sponsorClass;

	@Column(name = "BUDGET_AMOUNT", precision = 12, scale = 2)
	private BigDecimal budgetAmount;
	
	@Column(name = "POSTING_DATE")
	private Timestamp postingDate;
	
	@Column(name = "DOCUMENT_DATE")
	private Timestamp documentDate;
	
	@Column(name = "BUDGET_VERSION")
	private String budgetVersion;
	
	@Column(name = "LINE_ITEM_TEXT")
	private String lineItemText;
	
	@Column(name = "FUNCTIONAL_AREA")
	private String functionalArea;
	
	@Column(name = "COMMITMENT_ITEM")
	private String commitmentItem;
	
	@Column(name = "YEAR")
	private Integer year;
	
	@Column(name = "CREATE_STATUS")
	private String createStatus;

	@Column(name = "FEED_STATUS")
	private String feedStatus;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "GM_DOC_TYPE")
	private String gmDocType;

	@Column(name = "GRANT_CUR")
	private String grantCur;

	@Column(name = "FUNDED_PROGRAM")
	private String fundedProgram;

	@Column(name = "FUND_CENTER")
	private String fundCenter;

	@Column(name = "DISTR_KEY")
	private String distrKey;

	@Column(name = "FM_AREA")
	private String fmArea;

	@Column(name = "USER_COMMENT")
	private String userComment;

	public SapFeedTmplGrantBudMaster() {}
	
	public SapFeedTmplGrantBudMaster(String sponsorProgram, Integer batchId, String feedStatus, String message, String grantCode, String fundCode,  String sponsorClass, String process) {
		this.sponsorProgram = sponsorProgram;
		this.batchId = batchId;
		this.feedStatus = feedStatus;
		this.errorMessage = message;
		this.grantCode = grantCode;
		this.fundCode = fundCode;
		this.sponsorClass = sponsorClass;
		this.process = process;
	}

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

	public String getProcess() {
		return process;
	}

	public void setProcess(String process) {
		this.process = process;
	}

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getHeaderDescription() {
		return headerDescription;
	}

	public void setHeaderDescription(String headerDescription) {
		this.headerDescription = headerDescription;
	}
	
	public String getSponsorProgram() {
		return sponsorProgram;
	}

	public void setSponsorProgram(String sponsorProgram) {
		this.sponsorProgram = sponsorProgram;
	}

	public String getSponsorClass() {
		return sponsorClass;
	}

	public void setSponsorClass(String sponsorClass) {
		this.sponsorClass = sponsorClass;
	}
	
	public Timestamp getPostingDate() {
		return postingDate;
	}

	public void setPostingDate(Timestamp postingDate) {
		this.postingDate = postingDate;
	}

	public Timestamp getDocumentDate() {
		return documentDate;
	}

	public void setDocumentDate(Timestamp documentDate) {
		this.documentDate = documentDate;
	}

	public String getBudgetVersion() {
		return budgetVersion;
	}

	public void setBudgetVersion(String budgetVersion) {
		this.budgetVersion = budgetVersion;
	}

	public String getLineItemText() {
		return lineItemText;
	}

	public void setLineItemText(String lineItemText) {
		this.lineItemText = lineItemText;
	}

	public String getFunctionalArea() {
		return functionalArea;
	}

	public void setFunctionalArea(String functionalArea) {
		this.functionalArea = functionalArea;
	}

	public String getCommitmentItem() {
		return commitmentItem;
	}

	public void setCommitmentItem(String commitmentItem) {
		this.commitmentItem = commitmentItem;
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

	public String getFundCode() {
		return fundCode;
	}

	public void setFundCode(String fundCode) {
		this.fundCode = fundCode;
	}
	
	public Integer getYear() {
		return year;
	}

	public void setYear(Integer year) {
		this.year = year;
	}

	public BigDecimal getBudgetAmount() {
		return budgetAmount;
	}

	public void setBudgetAmount(BigDecimal budgetAmount) {
		this.budgetAmount = budgetAmount;
	}

	public String getGmDocType() {
		return gmDocType;
	}

	public void setGmDocType(String gmDocType) {
		this.gmDocType = gmDocType;
	}

	public String getGrantCur() {
		return grantCur;
	}

	public void setGrantCur(String grantCur) {
		this.grantCur = grantCur;
	}

	public String getFundedProgram() {
		return fundedProgram;
	}

	public void setFundedProgram(String fundedProgram) {
		this.fundedProgram = fundedProgram;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public String getDistrKey() {
		return distrKey;
	}

	public void setDistrKey(String distrKey) {
		this.distrKey = distrKey;
	}

	public String getFmArea() {
		return fmArea;
	}

	public void setFmArea(String fmArea) {
		this.fmArea = fmArea;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}

}
