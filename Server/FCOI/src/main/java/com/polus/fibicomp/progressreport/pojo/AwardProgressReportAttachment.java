package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "AWARD_PROGRESS_REPORT_ATTACHMENT")
@EntityListeners(AuditingEntityListener.class)
public class AwardProgressReportAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROGRESS_REPORT_ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer progressReportAttachmentId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;

	@Column(name = "ATTACHMENT_TYPE_CODE")
	private String attachmentTypeCode;
	
	@Column(name = "DESCRIPTION")
	private String description;	

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@Column(name = "DOCUMENT_STATUS_CODE")
	private String documentStatusCode;		
	
	@Column(name = "FILE_DATA_ID")
	private String fileDataId;
	
	@Column(name = "FILE_NAME")
	private String fileName;
	
	@Column(name = "MIME_TYPE")
	private String mimeType;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
		
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROGRESS_REPORT_ATTACHMENT_FK1"), name = "ATTACHMENT_TYPE_CODE", referencedColumnName = "ATTACHMENT_TYPE_CODE", insertable = false, updatable = false)
	private ProgressReportAttachmentType progressReportAttachmentType;
	
	private transient String updateUserName;

	public Integer getProgressReportAttachmentId() {
		return progressReportAttachmentId;
	}

	public void setProgressReportAttachmentId(Integer progressReportAttachmentId) {
		this.progressReportAttachmentId = progressReportAttachmentId;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

	public String getAttachmentTypeCode() {
		return attachmentTypeCode;
	}

	public void setAttachmentTypeCode(String attachmentTypeCode) {
		this.attachmentTypeCode = attachmentTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String mimeType) {
		this.mimeType = mimeType;
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

	public ProgressReportAttachmentType getProgressReportAttachmentType() {
		return progressReportAttachmentType;
	}

	public void setProgressReportAttachmentType(ProgressReportAttachmentType progressReportAttachmentType) {
		this.progressReportAttachmentType = progressReportAttachmentType;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}
}
