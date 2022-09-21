package com.polus.fibicomp.claims.pojo;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "CLAIM_ATTACHMENT")
@EntityListeners(AuditingEntityListener.class)
public class ClaimAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CLAIM_ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "CLAIM_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name = "CLAIM_ATTACHMENT_ID_GENERATOR", sequenceName = "CLAIM_ATTACHMENT_ID_GENERATOR", allocationSize = 1)
	private Integer claimAttachmentId;
	
	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;
	
	@Column(name = "TYPE_CODE")
	private String typeCode;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "DOCUMENT_STATUS_CODE")
	private String documentStatusCode;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "DOCUMENT_ID")
	private Integer documentId;
	
	@Column(name = "FILE_DATA_ID")
	private String fileDataId;
	
	@Column(name = "FILE_NAME")
	private String fileName;
	
	@Column(name = "MIME_TYPE")
	private String mimeType;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_ATTACHMENT_FK2"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ClaimAttachmentType attachmentType;
	
	private transient String updateUserName;
	
	public Integer getClaimAttachmentId() {
		return claimAttachmentId;
	}

	public void setClaimAttachmentId(Integer claimAttachmentId) {
		this.claimAttachmentId = claimAttachmentId;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
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

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public ClaimAttachmentType getAttachmentType() {
		return attachmentType;
	}

	public void setAttachmentType(ClaimAttachmentType attachmentType) {
		this.attachmentType = attachmentType;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}
}
