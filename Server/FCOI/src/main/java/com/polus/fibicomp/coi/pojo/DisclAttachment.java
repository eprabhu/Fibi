package com.polus.fibicomp.coi.pojo;

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
@Table(name = "DISCL_ATTACHMENT")
@EntityListeners(AuditingEntityListener.class)
public class DisclAttachment implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer attachmentId;
	
	@Column(name = "COMMENT_ID")
	private Integer commentId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_ATTACHMENT_FK1"), name = "COMMENT_ID", referencedColumnName = "COMMENT_ID", insertable = false, updatable = false)
	private DisclComment disclComment;
	
	@Column(name = "COMPONENT_TYPE_CODE")
	private String componentTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_ATTACHMENT_FK2"), name = "COMPONENT_TYPE_CODE", referencedColumnName = "COMPONENT_TYPE_CODE", insertable = false, updatable = false)
	private DisclComponentType disclComponentType;
	
	@Column(name = "COMPONENT_REFERENCE_ID")
	private Integer componentReferenceId;
	
	@Column(name = "COMPONENT_REFERENCE_NUMBER")
	private String componentReferenceNumber;
	
	@Column(name = "ATTACHMENT_NUMBER")
	private Integer attachmentNumber;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@Column(name = "VERSION_STATUS")
	private String versionStatus;
	
	@Column(name = "ATTA_TYPE_CODE")
	private String attaTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_ATTACHMENT_FK3"), name = "ATTA_TYPE_CODE", referencedColumnName = "ATTA_TYPE_CODE", insertable = false, updatable = false)
	private DisclAttaType disclAttaType;
	
	@Column(name = "ATTA_STATUS_CODE")
	private String attaStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_ATTACHMENT_FK4"), name = "ATTA_STATUS_CODE", referencedColumnName = "ATTA_STATUS_CODE", insertable = false, updatable = false)
	private DisclAttaStatus disclAttaStatus;
	
	@Column(name = "DOCUMENT_OWNER_PERSON_ID")
	private String documentOwnerPersonId;
	
	@Column(name = "FILE_NAME")
	private String fileName;
	
	@Column(name = "MIME_TYPE")
	private String mimeType;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "FILE_DATA_ID")
	private Integer fileDataId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_ATTACHMENT_FK5"), name = "FILE_DATA_ID", referencedColumnName = "FILE_DATA_ID", insertable = false, updatable = false)
	private DisclFileData disclFileData;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

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

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public DisclComment getDisclComment() {
		return disclComment;
	}

	public void setDisclComment(DisclComment disclComment) {
		this.disclComment = disclComment;
	}

	public String getComponentTypeCode() {
		return componentTypeCode;
	}

	public void setComponentTypeCode(String componentTypeCode) {
		this.componentTypeCode = componentTypeCode;
	}

	public DisclComponentType getDisclComponentType() {
		return disclComponentType;
	}

	public void setDisclComponentType(DisclComponentType disclComponentType) {
		this.disclComponentType = disclComponentType;
	}

	public Integer getComponentReferenceId() {
		return componentReferenceId;
	}

	public void setComponentReferenceId(Integer componentReferenceId) {
		this.componentReferenceId = componentReferenceId;
	}

	public String getComponentReferenceNumber() {
		return componentReferenceNumber;
	}

	public void setComponentReferenceNumber(String componentReferenceNumber) {
		this.componentReferenceNumber = componentReferenceNumber;
	}

	public Integer getAttachmentNumber() {
		return attachmentNumber;
	}

	public void setAttachmentNumber(Integer attachmentNumber) {
		this.attachmentNumber = attachmentNumber;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public String getAttaTypeCode() {
		return attaTypeCode;
	}

	public void setAttaTypeCode(String attaTypeCode) {
		this.attaTypeCode = attaTypeCode;
	}

	public DisclAttaType getDisclAttaType() {
		return disclAttaType;
	}

	public void setDisclAttaType(DisclAttaType disclAttaType) {
		this.disclAttaType = disclAttaType;
	}

	public String getAttaStatusCode() {
		return attaStatusCode;
	}

	public void setAttaStatusCode(String attaStatusCode) {
		this.attaStatusCode = attaStatusCode;
	}

	public DisclAttaStatus getDisclAttaStatus() {
		return disclAttaStatus;
	}

	public void setDisclAttaStatus(DisclAttaStatus disclAttaStatus) {
		this.disclAttaStatus = disclAttaStatus;
	}

	public String getDocumentOwnerPersonId() {
		return documentOwnerPersonId;
	}

	public void setDocumentOwnerPersonId(String documentOwnerPersonId) {
		this.documentOwnerPersonId = documentOwnerPersonId;
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

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(Integer fileDataId) {
		this.fileDataId = fileDataId;
	}

	public DisclFileData getDisclFileData() {
		return disclFileData;
	}

	public void setDisclFileData(DisclFileData disclFileData) {
		this.disclFileData = disclFileData;
	}
	
}
