package com.polus.fibicomp.fastintegration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "SAP_AWARD_FEED_BATCH_FILES")
public class SapAwardFeedBatchFiles {

	@Id
	@Column(name = "BATCH_FILE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_AWARD_FEED_BATCH_FILES_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_AWARD_FEED_BATCH_FILES_ID_GENERATOR", sequenceName = "SAP_AWARD_FEED_BATCH_FILES_ID_GENERATOR", allocationSize = 1)
	private Integer batchFileId;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "BATCH_FILE_NAME")
	private String batchFileName;

	@Column(name = "NO_OF_RECORDS")
	private Integer noOfRecords;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getBatchFileId() {
		return batchFileId;
	}

	public void setBatchFileId(Integer batchFileId) {
		this.batchFileId = batchFileId;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public String getBatchFileName() {
		return batchFileName;
	}

	public void setBatchFileName(String batchFileName) {
		this.batchFileName = batchFileName;
	}

	public Integer getNoOfRecords() {
		return noOfRecords;
	}

	public void setNoOfRecords(Integer noOfRecords) {
		this.noOfRecords = noOfRecords;
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

}
