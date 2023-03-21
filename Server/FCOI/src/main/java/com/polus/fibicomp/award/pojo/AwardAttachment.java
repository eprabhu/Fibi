package com.polus.fibicomp.award.pojo;

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

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.polus.fibicomp.pojo.DocumentStatus;
import com.polus.fibicomp.proposal.pojo.NarrativeStatus;
import com.polus.fibicomp.proposal.pojo.ProposalAttachmentType;

@SuppressWarnings("unused")
@Entity
@Table(name = "AWARD_ATTACHMENT")
public class AwardAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_ATTACHMENT_ID_GENERATOR", sequenceName = "AWARD_ATTACHMENT_ID_GENERATOR", allocationSize=1)
	private Integer awardAttachmentId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "TYPE_CODE")
	private String typeCode;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private Integer documentStatusCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "NARRATIVE_STATUS_CODE")
	private String narrativeStatusCode;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "DOCUMENT_ID")
	private Integer documentId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ATTACHMENT_FK1"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private AwardAttachmentType attachmentType;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ATTACHMENT_FK2"), name = "NARRATIVE_STATUS_CODE", referencedColumnName = "NARRATIVE_STATUS_CODE", insertable = false, updatable = false)
	private NarrativeStatus narrativeStatus;

	@Transient
	private String lastUpdateUserFullName;
	/*
	 * @ManyToOne(optional = false)
	 * 
	 * @JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ATTACHMENT_FK3"), name =
	 * "DOCUMENT_STATUS_CODE", referencedColumnName = "DOCUMENT_STATUS_CODE",
	 * insertable = false, updatable = false) private DocumentStatus documentStatus;
	 */

	public Integer getAwardAttachmentId() {
		return awardAttachmentId;
	}

	public void setAwardAttachmentId(Integer awardAttachmentId) {
		this.awardAttachmentId = awardAttachmentId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
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

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(Integer documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public AwardAttachmentType getAttachmentType() {
		return attachmentType;
	}

	public void setAttachmentType(AwardAttachmentType attachmentType) {
		this.attachmentType = attachmentType;
	}

	public NarrativeStatus getNarrativeStatus() {
		return narrativeStatus;
	}

	public void setNarrativeStatus(NarrativeStatus narrativeStatus) {
		this.narrativeStatus = narrativeStatus;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

	/*
	 * public DocumentStatus getDocumentStatus() { return documentStatus; }
	 * 
	 * public void setDocumentStatus(DocumentStatus documentStatus) {
	 * this.documentStatus = documentStatus; }
	 */

}
