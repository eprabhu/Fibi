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

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SAP_AWARD_FEED_BATCH")
@EntityListeners(AuditingEntityListener.class)
public class SapAwardFeedBatch {

	@Id
	@Column(name = "BATCH_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BATCH_ID_GENERATOR")
	@SequenceGenerator(name = "BATCH_ID_GENERATOR", sequenceName = "BATCH_ID_GENERATOR", allocationSize = 1)
	private Integer batchId;

	@Column(name = "NO_OF_RECORDS")
	private Integer noOfRecords;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@LastModifiedDate
	@Column(name = "RESPONSE_TIMESTAMP")
	private Timestamp responseTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public Integer getNoOfRecords() {
		return noOfRecords;
	}

	public void setNoOfRecords(Integer noOfRecords) {
		this.noOfRecords = noOfRecords;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Timestamp getResponseTimestamp() {
		return responseTimestamp;
	}

	public void setResponseTimestamp(Timestamp responseTimestamp) {
		this.responseTimestamp = responseTimestamp;
	}

}
