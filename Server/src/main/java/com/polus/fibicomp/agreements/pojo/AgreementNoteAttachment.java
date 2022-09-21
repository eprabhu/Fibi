package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "AGREEMENT_NOTE_ATTACHMENT")
public class AgreementNoteAttachment implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_NOTE_ATTACHMENT")
	@SequenceGenerator(name="SEQ_AGREEMENT_NOTE_ATTACHMENT", sequenceName = "SEQ_AGREEMENT_NOTE_ATTACHMENT", allocationSize=1)
	@Column(name = "AGREEMENT_NOTE_ATTACHMENT_ID")
	private Integer agreementNoteAttachmentId;

	@JsonBackReference
	@ManyToOne(optional = false, cascade = CascadeType.REFRESH)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_NOTE_ATTACHMENT_FK1"), name = "AGREEMENT_NOTE_ID", referencedColumnName = "AGREEMENT_NOTE_ID")
	private AgreementNote agreementNote;

	@Column(name = "AGREEMENT_ATTACHMENT_FILE_ID")
	private String agreementNoteFileId;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String updateUserFullName;

	@Transient
	private byte[] attachment;

	public Integer getAgreementNoteAttachmentId() {
		return agreementNoteAttachmentId;
	}

	public void setAgreementNoteAttachmentId(Integer agreementNoteAttachmentId) {
		this.agreementNoteAttachmentId = agreementNoteAttachmentId;
	}

	public AgreementNote getAgreementNote() {
		return agreementNote;
	}

	public void setAgreementNote(AgreementNote agreementNote) {
		this.agreementNote = agreementNote;
	}

	public String getAgreementNoteFileId() {
		return agreementNoteFileId;
	}

	public void setAgreementNoteFileId(String agreementNoteFileId) {
		this.agreementNoteFileId = agreementNoteFileId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
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

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String mimeType) {
		this.mimeType = mimeType;
	}

}
