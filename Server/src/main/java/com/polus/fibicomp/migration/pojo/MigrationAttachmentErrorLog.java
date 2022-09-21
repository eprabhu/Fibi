package com.polus.fibicomp.migration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "MIGRATION_ATTACHMENT_ERROR_LOG")
public class MigrationAttachmentErrorLog {

	@Id
	@Column(name = "MIGRATION_ATTCHMNT_ERROR_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "MIGRATION_ATTCHMNT_ERROR_LOG_ID_GENERATOR")
	@SequenceGenerator(name = "MIGRATION_ATTCHMNT_ERROR_LOG_ID_GENERATOR", sequenceName = "MIGRATION_ATTCHMNT_ERROR_LOG_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "LEGACY_PROJECT_ID")
	private String projectId;

	@Column(name = "LEGACY_WBS_NUMBER")
	private String legacyWbsNumber;

	@Column(name = "GRANT_HEADER_ID")
	private String grantHeaderId;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "ERROR_TYPE")
	private String errorType;
	
	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@Column(name = "VALIDATION_TYPE")
	private String validationType;

	@Column(name = "FILE_TYPE")
	private String fileType;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getProjectId() {
		return projectId;
	}

	public void setProjectId(String projectId) {
		this.projectId = projectId;
	}

	public String getLegacyWbsNumber() {
		return legacyWbsNumber;
	}

	public void setLegacyWbsNumber(String legacyWbsNumber) {
		this.legacyWbsNumber = legacyWbsNumber;
	}

	public String getGrantHeaderId() {
		return grantHeaderId;
	}

	public void setGrantHeaderId(String grantHeaderId) {
		this.grantHeaderId = grantHeaderId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getErrorType() {
		return errorType;
	}

	public void setErrorType(String errorType) {
		this.errorType = errorType;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getValidationType() {
		return validationType;
	}

	public void setValidationType(String validationType) {
		this.validationType = validationType;
	}

	public String getFileType() {
		return fileType;
	}

	public void setFileType(String fileType) {
		this.fileType = fileType;
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
	
}
