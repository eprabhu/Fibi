package com.polus.fibicomp.servicerequest.pojo;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.DocumentStatus;

@Entity
@Table(name = "SR_ATTACHMENT")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer attachmentId;
	
	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "CONTENT_TYPE")
	private String contentType;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private Integer documentStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_ATTACHMENT_FK2"), name = "DOCUMENT_STATUS_CODE", referencedColumnName = "DOCUMENT_STATUS_CODE", insertable = false, updatable = false)
	private DocumentStatus documentStatus;

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;

	@Transient
	private String updateUserFullName;

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Integer getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(Integer documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public DocumentStatus getDocumentStatus() {
		return documentStatus;
	}

	public void setDocumentStatus(DocumentStatus documentStatus) {
		this.documentStatus = documentStatus;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}
