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
@Table(name = "AWARD_REVENUE_FILES")
public class AwardRevenueFile {

	@Id
	@Column(name = "FILE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_REVE_FILE_ID_GEN")
	@SequenceGenerator(name = "AWARD_REVE_FILE_ID_GEN", sequenceName = "AWARD_REVE_FILE_ID_GEN", allocationSize = 1)
	private Integer fileId;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "NO_OF_RECORDS")
	private Integer noOfRecords;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "INSERTED_IN_RT")
	private String insertedInRT;

	@Column(name = "SYSTEM_ARCHIVED")
	private String systemArchived;

	@Column(name = "REMOTE_ARCHIVED")
	private String remoteArchived;

	@Column(name = "INSERTED_ROWS")
	private Integer insertedRows;

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
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

	public String getInsertedInRT() {
		return insertedInRT;
	}

	public void setInsertedInRT(String insertedInRT) {
		this.insertedInRT = insertedInRT;
	}

	public String getSystemArchived() {
		return systemArchived;
	}

	public void setSystemArchived(String systemArchived) {
		this.systemArchived = systemArchived;
	}

	public String getRemoteArchived() {
		return remoteArchived;
	}

	public void setRemoteArchived(String remoteArchived) {
		this.remoteArchived = remoteArchived;
	}

	public Integer getInsertedRows() {
		return insertedRows;
	}

	public void setInsertedRows(Integer insertedRows) {
		this.insertedRows = insertedRows;
	}

}
