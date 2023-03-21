package com.polus.fibicomp.claims.claimsIntegration.excelity.pojo;

import java.sql.Timestamp;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "CLAIM_FILES")
public class ClaimFiles {

	@Id
	@Column(name = "FILE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "FILE_ID_GENERATOR")
	@SequenceGenerator(name = "FILE_ID_GENERATOR", sequenceName = "FILE_ID_GENERATOR", allocationSize = 1)
	private Integer fileId;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "NO_OF_RECORDS")
	private Integer noOfRecords;	

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "FILE_INTERFACE")
	private String fileInterface;

	public String getFileInterface() {
		return fileInterface;
	}

	public void setFileInterface(String fileInterface) {
		this.fileInterface = fileInterface;
	}

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

}

