package com.polus.fibicomp.fastintegration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "SAP_AWARD_FEED_BATCH_ERROR_LOG")
public class SapAwardFeedBatchErrorLog {


	@Id
	@Column(name = "BATCH_ERROR_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BATCH_ERROR_ID_GENERATOR")
	@SequenceGenerator(name = "BATCH_ERROR_ID_GENERATOR", sequenceName = "BATCH_ERROR_ID_GENERATOR", allocationSize = 1)
	private Integer batchErrorId;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FEED_BATCH_ERROR_LOG_FK1"), name = "BATCH_ID", referencedColumnName = "BATCH_ID", insertable = false, updatable = false)
	private SapAwardFeedBatch sapAwardFeedBatch;

	@Column(name = "FEED_ID")
	private Integer feedId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FEED_BATCH_ERROR_LOG_FK2"), name = "FEED_ID", referencedColumnName = "FEED_ID", insertable = false, updatable = false)
	private SapAwardFeed sapAwardFeed;

	@Column(name = "ERROR_MESSAGE")
	private String errorLog;

	@Column(name = "ERROR_TYPE")
	private String errorType;

	@Column(name = "GRANT_CODE")
	private String grantCode;

	@Column(name = "BUSINESS_AREA")
	private String businessArea;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getBatchErrorId() {
		return batchErrorId;
	}

	public void setBatchErrorId(Integer batchErrorId) {
		this.batchErrorId = batchErrorId;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public SapAwardFeedBatch getSapAwardFeedBatch() {
		return sapAwardFeedBatch;
	}

	public void setSapAwardFeedBatch(SapAwardFeedBatch sapAwardFeedBatch) {
		this.sapAwardFeedBatch = sapAwardFeedBatch;
	}

	public Integer getFeedId() {
		return feedId;
	}

	public void setFeedId(Integer feedId) {
		this.feedId = feedId;
	}

	public SapAwardFeed getSapAwardFeed() {
		return sapAwardFeed;
	}

	public void setSapAwardFeed(SapAwardFeed sapAwardFeed) {
		this.sapAwardFeed = sapAwardFeed;
	}

	public String getErrorLog() {
		return errorLog;
	}

	public void setErrorLog(String errorLog) {
		this.errorLog = errorLog;
	}

	public String getErrorType() {
		return errorType;
	}

	public void setErrorType(String errorType) {
		this.errorType = errorType;
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

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getBusinessArea() {
		return businessArea;
	}

	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

}
