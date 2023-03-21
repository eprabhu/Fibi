package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.pojo.DocumentStatus;

@Entity
@Table(name = "EPS_PROPOSAL_PERSON_ATTACHMNT")
public class ProposalPersonAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_PRSN_ATTACH_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_PRSN_ATTACH_ID_GENERATOR", sequenceName = "EPS_PROP_PRSN_ATTACH_ID_GENERATOR", allocationSize=1)
	private Integer attachmentId;

	@Column(name = "PROPOSAL_PERSON_ID")
	private Integer proposalPersonId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_PERSON_ATTACHMENT_FK1"), name = "PROPOSAL_PERSON_ID", referencedColumnName = "PROPOSAL_PERSON_ID", insertable = false, updatable = false)
	private ProposalPerson proposalPerson;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ATTACHMNT_TYPE_CODE")
	private Integer attachmentTypeCode;

	@ManyToOne(optional = false, cascade = CascadeType.REFRESH)
	@JoinColumn( name = "ATTACHMNT_TYPE_CODE", insertable = false, updatable = false)
	private ProposalKeyPersonnelAttachmentType attachmentType;

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private Integer documentStatusCode;

	@ManyToOne(optional = false, cascade = CascadeType.REFRESH)
	@JoinColumn(name = "DOCUMENT_STATUS_CODE", referencedColumnName = "DOCUMENT_STATUS_CODE", insertable = false, updatable = false)
	private DocumentStatus documentStatus;

	@JsonIgnore
	@Transient
	private byte[] attachment;

	@Transient
	private Integer replaceAttachmentId;

	@Transient
	private String proposalPersonName;

	@Transient
	private String lastUpdateUserFullName;

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public ProposalPerson getProposalPerson() {
		return proposalPerson;
	}

	public void setProposalPerson(ProposalPerson proposalPerson) {
		this.proposalPerson = proposalPerson;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public Integer getReplaceAttachmentId() {
		return replaceAttachmentId;
	}

	public void setReplaceAttachmentId(Integer replaceAttachmentId) {
		this.replaceAttachmentId = replaceAttachmentId;
	}

	public String getProposalPersonName() {
		return proposalPersonName;
	}

	public void setProposalPersonName(String proposalPersonName) {
		this.proposalPersonName = proposalPersonName;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

	public Integer getAttachmentTypeCode() {
		return attachmentTypeCode;
	}

	public void setAttachmentTypeCode(Integer attachmentTypeCode) {
		this.attachmentTypeCode = attachmentTypeCode;
	}

	public ProposalKeyPersonnelAttachmentType getAttachmentType() {
		return attachmentType;
	}

	public void setAttachmentType(ProposalKeyPersonnelAttachmentType attachmentType) {
		this.attachmentType = attachmentType;
	}

	public Integer getProposalPersonId() {
		return proposalPersonId;
	}

	public void setProposalPersonId(Integer proposalPersonId) {
		this.proposalPersonId = proposalPersonId;
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
}
