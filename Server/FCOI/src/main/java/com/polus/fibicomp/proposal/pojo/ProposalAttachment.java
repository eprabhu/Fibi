package com.polus.fibicomp.proposal.pojo;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.DocumentStatus;

@Entity
@Table(name = "EPS_PROPOSAL_ATTACHMENTS")
@EntityListeners(AuditingEntityListener.class)
public class ProposalAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_ATTACH_ID_GENERATOR")
	@SequenceGenerator(name = "EPS_PROP_ATTACH_ID_GENERATOR", sequenceName = "EPS_PROP_ATTACH_ID_GENERATOR", allocationSize = 1)
	private Integer attachmentId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "ATTACHMNT_TYPE_CODE")
	private Integer attachmentTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_ATTACHMENTS_FK2"), name = "ATTACHMNT_TYPE_CODE", referencedColumnName = "ATTACHMNT_TYPE_CODE", insertable = false, updatable = false)
	private ProposalAttachmentType attachmentType;

	@Column(name = "DESCRIPTION")
	private String description;

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

	@Column(name = "NARRATIVE_STATUS_CODE")
	private String narrativeStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_ATTACHMENTS_FK3"), name = "NARRATIVE_STATUS_CODE", referencedColumnName = "NARRATIVE_STATUS_CODE", insertable = false, updatable = false)
	private NarrativeStatus narrativeStatus;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private Integer documentStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_ATTACHMENTS_FK5"), name = "DOCUMENT_STATUS_CODE", referencedColumnName = "DOCUMENT_STATUS_CODE", insertable = false, updatable = false)
	private DocumentStatus documentStatus;

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;

	@Transient
	private byte[] attachment;

	@Transient
	private String lastUpdateUserFullName;

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public Integer getAttachmentTypeCode() {
		return attachmentTypeCode;
	}

	public void setAttachmentTypeCode(Integer attachmentTypeCode) {
		this.attachmentTypeCode = attachmentTypeCode;
	}

	public ProposalAttachmentType getAttachmentType() {
		return attachmentType;
	}

	public void setAttachmentType(ProposalAttachmentType attachmentType) {
		this.attachmentType = attachmentType;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public NarrativeStatus getNarrativeStatus() {
		return narrativeStatus;
	}

	public void setNarrativeStatus(NarrativeStatus narrativeStatus) {
		this.narrativeStatus = narrativeStatus;
	}

	public String getNarrativeStatusCode() {
		return narrativeStatusCode;
	}

	public void setNarrativeStatusCode(String narrativeStatusCode) {
		this.narrativeStatusCode = narrativeStatusCode;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
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

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

}
