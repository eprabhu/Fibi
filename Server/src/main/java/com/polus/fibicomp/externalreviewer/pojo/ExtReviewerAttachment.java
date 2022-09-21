package com.polus.fibicomp.externalreviewer.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "EXTERNAL_REVIEWER_ATTACHMENTS")
public class ExtReviewerAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EXTERNAL_REVIEWER_ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer externalReviewerAttachmentId;

	@Column(name = "ATTACHMENT_TYPE_CODE")
	private String attachmentTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_ATTACHMENTS_FK1"), name = "ATTACHMENT_TYPE_CODE", referencedColumnName = "ATTACHMENT_TYPE_CODE", insertable = false, updatable = false)
	private ExternalReviewerAttachmentType externalReviewerAttachmentType;

    @Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "EXTERNAL_REVIEWER_ID")
	private Integer externalReviewerId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_ATTACHMENTS_FK2"), name = "EXTERNAL_REVIEWER_ID", referencedColumnName = "EXTERNAL_REVIEWER_ID", insertable = false, updatable = false)
	private ExternalReviewer externalReviewer;

	public Integer getExternalReviewerAttachmentId() {
		return externalReviewerAttachmentId;
	}

	public void setExternalReviewerAttachmentId(Integer externalReviewerAttachmentId) {
		this.externalReviewerAttachmentId = externalReviewerAttachmentId;
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

	public ExternalReviewerAttachmentType getExternalReviewerAttachmentType() {
		return externalReviewerAttachmentType;
	}

	public void setExternalReviewerAttachmentType(ExternalReviewerAttachmentType externalReviewerAttachmentType) {
		this.externalReviewerAttachmentType = externalReviewerAttachmentType;
	}

	public Integer getExternalReviewerId() {
		return externalReviewerId;
	}

	public void setExternalReviewerId(Integer externalReviewerId) {
		this.externalReviewerId = externalReviewerId;
	}

	public ExternalReviewer getExternalReviewer() {
		return externalReviewer;
	}

	public void setExternalReviewer(ExternalReviewer externalReviewer) {
		this.externalReviewer = externalReviewer;
	}

}