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
@Table(name = "COI_DISCLOSURE_ATTACHMENTS")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclosureOldAttachments implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_DISCLOSURE_ATTACHMENTS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer CoiDisclosureOldAttachmentId;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;
	
	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "ATTACHMENT_VERSION_NUMBER")
	private Integer attachmentVersionNumber;
	
	@Column(name = "ATTACHMENT_VERSION_STATUS")
	private String attachmentVersionStatus;

	@Column(name = "COI_ATTA_CATEGORY_TYPE_CODE")
	private String coiAttachmentCategoryTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_ATTACHMENTS_FK1"), name = "COI_ATTA_CATEGORY_TYPE_CODE", referencedColumnName = "COI_ATTA_CATEGORY_TYPE_CODE", insertable = false, updatable = false)
	private CoiAttachmentCategoryType coiAttachmentCategoryType;

	@Column(name = "COI_ATTA_TYPE_CODE")
	private String coiAttachmentTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_ATTACHMENTS_FK2"), name = "COI_ATTA_TYPE_CODE", referencedColumnName = "COI_ATTA_TYPE_CODE", insertable = false, updatable = false)
	private CoiAttachmentType coiAttachmentType;
	
	@Column(name = "COI_ATTA_CATEGORY_VALUE")
	private String coiAttachmentCategoryValue;
	
	@Column(name = "COI_ATTA_STATUS_CODE")
	private String coiAttachmentStatusCode;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getCoiDisclosureOldAttachmentId() {
		return CoiDisclosureOldAttachmentId;
	}

	public void setCoiDisclosureOldAttachmentId(Integer CoiDisclosureOldAttachmentId) {
		this.CoiDisclosureOldAttachmentId = CoiDisclosureOldAttachmentId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public Integer getAttachmentVersionNumber() {
		return attachmentVersionNumber;
	}

	public void setAttachmentVersionNumber(Integer attachmentVersionNumber) {
		this.attachmentVersionNumber = attachmentVersionNumber;
	}

	public String getAttachmentVersionStatus() {
		return attachmentVersionStatus;
	}

	public void setAttachmentVersionStatus(String attachmentVersionStatus) {
		this.attachmentVersionStatus = attachmentVersionStatus;
	}

	public String getCoiAttachmentCategoryTypeCode() {
		return coiAttachmentCategoryTypeCode;
	}

	public void setCoiAttachmentCategoryTypeCode(String coiAttachmentCategoryTypeCode) {
		this.coiAttachmentCategoryTypeCode = coiAttachmentCategoryTypeCode;
	}

	public CoiAttachmentCategoryType getCoiAttachmentCategoryType() {
		return coiAttachmentCategoryType;
	}

	public void setCoiAttachmentCategoryType(CoiAttachmentCategoryType coiAttachmentCategoryType) {
		this.coiAttachmentCategoryType = coiAttachmentCategoryType;
	}

	public String getCoiAttachmentTypeCode() {
		return coiAttachmentTypeCode;
	}

	public void setCoiAttachmentTypeCode(String coiAttachmentTypeCode) {
		this.coiAttachmentTypeCode = coiAttachmentTypeCode;
	}

	public CoiAttachmentType getCoiAttachmentType() {
		return coiAttachmentType;
	}

	public void setCoiAttachmentType(CoiAttachmentType coiAttachmentType) {
		this.coiAttachmentType = coiAttachmentType;
	}

	public String getCoiAttachmentCategoryValue() {
		return coiAttachmentCategoryValue;
	}

	public void setCoiAttachmentCategoryValue(String coiAttachmentCategoryValue) {
		this.coiAttachmentCategoryValue = coiAttachmentCategoryValue;
	}

	public String getCoiAttachmentStatusCode() {
		return coiAttachmentStatusCode;
	}

	public void setCoiAttachmentStatusCode(String coiAttachmentStatusCode) {
		this.coiAttachmentStatusCode = coiAttachmentStatusCode;
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

}
