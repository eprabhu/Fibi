package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.FileData;

@Entity
@Table(name = "NEGOTIATION_ATTACHMENT")
public class NegotiationsAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "negotiationsAttachmentIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "negotiationsAttachmentIdGenerator")
	@Column(name = "NEGOTIATIONS_ATTACHMENT_ID")
	private Integer negotiationsAttachmentId;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ATTACHMENT_FK4"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "NEGOTIATION_ACTIVITY_ID")
	private Integer negotiationsActivityId;

	@Column(name = "ATTACHMENT_TYPE_CODE")
	private String attachmentTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ATTACHMENT_FK2"), name = "ATTACHMENT_TYPE_CODE", referencedColumnName = "ATTACHMENT_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsAttachmentType negotiationsAttachmentType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@ManyToOne(optional = true, cascade = { CascadeType.ALL })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ATTACHMENT_FK3"), name = "FILE_ID", referencedColumnName = "ID", insertable = false, updatable = false)
	private FileData fileData;

	@Column(name = "FILE_ID")
	private String fileId;

	@Column(name = "CONTENT_TYPE")
	private String contentType;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "RESTRICTED")
	private String restricted;

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;

	@Transient
	private String acType;

	@Transient
	private String updateUserFullName;

	@Transient
	private NegotiationsActivity negotiationsActivity;

	public Integer getNegotiationsAttachmentId() {
		return negotiationsAttachmentId;
	}

	public void setNegotiationsAttachmentId(Integer negotiationsAttachmentId) {
		this.negotiationsAttachmentId = negotiationsAttachmentId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public Integer getNegotiationsActivityId() {
		return negotiationsActivityId;
	}

	public void setNegotiationsActivityId(Integer negotiationsActivityId) {
		this.negotiationsActivityId = negotiationsActivityId;
	}

	public String getAttachmentTypeCode() {
		return attachmentTypeCode;
	}

	public void setAttachmentTypeCode(String attachmentTypeCode) {
		this.attachmentTypeCode = attachmentTypeCode;
	}

	public NegotiationsAttachmentType getNegotiationsAttachmentType() {
		return negotiationsAttachmentType;
	}

	public void setNegotiationsAttachmentType(NegotiationsAttachmentType negotiationsAttachmentType) {
		this.negotiationsAttachmentType = negotiationsAttachmentType;
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

	public FileData getFileData() {
		return fileData;
	}

	public void setFileData(FileData fileData) {
		this.fileData = fileData;
	}

	public String getFileId() {
		return fileId;
	}

	public void setFileId(String fileId) {
		this.fileId = fileId;
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

	public String getRestricted() {
		return restricted;
	}

	public void setRestricted(String restricted) {
		this.restricted = restricted;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public NegotiationsActivity getNegotiationsActivity() {
		return negotiationsActivity;
	}

	public void setNegotiationsActivity(NegotiationsActivity negotiationsActivity) {
		this.negotiationsActivity = negotiationsActivity;
	}

}