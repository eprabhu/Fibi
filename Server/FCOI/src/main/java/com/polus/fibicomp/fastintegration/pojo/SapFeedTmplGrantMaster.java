package com.polus.fibicomp.fastintegration.pojo;

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
@Table(name = "SAP_FEED_TMPL_GRANT_MASTER")
@EntityListeners(AuditingEntityListener.class)
public class SapFeedTmplGrantMaster {

	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_TMPL_SPONSOR_CLASS_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_TMPL_SPONSOR_CLASS_ID_GENERATOR", sequenceName = "SAP_FEED_TMPL_SPONSOR_CLASS_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;

	@Column(name = "GRANT_CODE")
	private String grantCode;

	@Column(name = "COMPANY_CODE")
	private String companyCode;
	
	@Column(name = "GRANT_CURRENCY")
	private String grantCurrency;
	
	@Column(name = "FUND_CODE")
	private String fundCode;
	
	@Column(name = "SPONSOR_PROGRAM")
	private String sponsorProgram;
	
	@Column(name = "SPONSOR_CLASS")
	private String sponsorClass;

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

	@Column(name = "USER_COMMENT")
	private String userComment;

	public SapFeedTmplGrantMaster() {}
	
	public SapFeedTmplGrantMaster(Integer batchId, String feedStatus, String message, String grantCode, String companyCode) {
//		this.sponsorClass = sponsorClass;
		this.batchId = batchId;
		this.feedStatus = feedStatus;
		this.errorMessage = message;
		this.grantCode = grantCode;
		this.companyCode = companyCode;
//		this.sponsorProgram = sponsorProgram;
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

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getGrantCurrency() {
		return grantCurrency;
	}

	public void setGrantCurrency(String grantCurrency) {
		this.grantCurrency = grantCurrency;
	}

	public String getFundCode() {
		return fundCode;
	}

	public void setFundCode(String fundCode) {
		this.fundCode = fundCode;
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

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}
	
}
