package com.polus.fibicomp.agreements.pojo;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.DocumentStatus;


@Entity
@Table(name = "AGREEMENT_ATTACHMENT")
public class AgreementAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_ATTACHMENT")
	@SequenceGenerator(name="SEQ_AGREEMENT_ATTACHMENT", sequenceName = "SEQ_AGREEMENT_ATTACHMENT", allocationSize=1)
	@Column(name = "AGREEMENT_ATTACHMENT_ID")
	private Integer agreementAttachmentId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ATTACHMENT_FK2"), name = "ACTION_LOG_ID", referencedColumnName = "ACTION_LOG_ID", insertable = false, updatable = false)
	private AgreementActionLog agreementActionLog;

	@Column(name = "AGREEMENT_ATTACHMENT_TYPE_CODE")
	private String agreementAttachmentTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ATTACHMENT_FK3"), name = "AGREEMENT_ATTACHMENT_TYPE_CODE", referencedColumnName = "AGREEMENT_ATTACHMENT_TYPE_CODE", insertable = false, updatable = false)
	private AgreementAttachmentType agreementAttachmentType;

	@Column(name = "AGREEMENT_ATTACHMENT_FILE_ID")
	private String agreementAttachmentFileId;

//	@JsonIgnore
//	@ManyToOne(optional = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
//	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ATTACHMENT_FK4"), name = "AGREEMENT_ATTACHMENT_FILE_ID", referencedColumnName = "AGREEMENT_ATTACHMENT_FILE_ID", insertable = false, updatable = false)
//	private AgreementAttachmentFile agreementAttachmentFile;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "CONTENT_TYPE")
	private String contentType;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private Integer documentStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ATTACHMENT_FK5"), name = "DOCUMENT_STATUS_CODE", referencedColumnName = "DOCUMENT_STATUS_CODE", insertable = false, updatable = false)
	private DocumentStatus documentStatus;

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "AGRMNT_ATTACH_STATUS_CODE")
	private String agreementAttachStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ATTACHMENT_FK6"), name = "AGRMNT_ATTACH_STATUS_CODE", referencedColumnName = "AGRMNT_ATTACH_STATUS_CODE", insertable = false, updatable = false)
	private AgreementAttachmentStatus agreementAttachmentStatus;

	@Transient
	private String updateUserFullName;

	@Transient
	private byte[] attachment;

	public Integer getAgreementAttachmentId() {
		return agreementAttachmentId;
	}

	public void setAgreementAttachmentId(Integer agreementAttachmentId) {
		this.agreementAttachmentId = agreementAttachmentId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public AgreementActionLog getAgreementActionLog() {
		return agreementActionLog;
	}

	public void setAgreementActionLog(AgreementActionLog agreementActionLog) {
		this.agreementActionLog = agreementActionLog;
	}

	public AgreementAttachmentType getAgreementAttachmentType() {
		return agreementAttachmentType;
	}

	public void setAgreementAttachmentType(AgreementAttachmentType agreementAttachmentType) {
		this.agreementAttachmentType = agreementAttachmentType;
	}

	public String getAgreementAttachmentFileId() {
		return agreementAttachmentFileId;
	}

	public void setAgreementAttachmentFileId(String agreementAttachmentFileId) {
		this.agreementAttachmentFileId = agreementAttachmentFileId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getAgreementAttachmentTypeCode() {
		return agreementAttachmentTypeCode;
	}

	public void setAgreementAttachmentTypeCode(String agreementAttachmentTypeCode) {
		this.agreementAttachmentTypeCode = agreementAttachmentTypeCode;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getAgreementAttachStatusCode() {
		return agreementAttachStatusCode;
	}

	public void setAgreementAttachStatusCode(String agreementAttachStatusCode) {
		this.agreementAttachStatusCode = agreementAttachStatusCode;
	}

	public AgreementAttachmentStatus getAgreementAttachmentStatus() {
		return agreementAttachmentStatus;
	}

	public void setAgreementAttachmentStatus(AgreementAttachmentStatus agreementAttachmentStatus) {
		this.agreementAttachmentStatus = agreementAttachmentStatus;
	}


}
