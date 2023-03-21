package com.polus.fibicomp.negotiation.pojo;

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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "NEGOTIATION_COMMENT_ATTACHMENT")
public class NegotiationCommentAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_NEGO_COMMENT_ATTACH_ID")
	@SequenceGenerator(name = "SEQ_NEGO_COMMENT_ATTACH_ID", sequenceName = "SEQ_NEGO_COMMENT_ATTACH_ID", allocationSize = 1)
	@Column(name = "NEGOTIATION_COMMENT_ATTACH_ID")
	private Integer negotiationCommentAttachmentId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_COMMENT_ATTACH_FK1"), name = "NEGOTIATION_COMMENT_ID", referencedColumnName = "NEGOTIATION_COMMENT_ID")
	private NegotiationsComment negotiationsComment;

	@Column(name = "NEGOTIATION_ATTACHMENT_FILE_ID")
	private String negotiationAttachmentFileId;

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

	public NegotiationsComment getNegotiationsComment() {
		return negotiationsComment;
	}

	public void setNegotiationsComment(NegotiationsComment negotiationsComment) {
		this.negotiationsComment = negotiationsComment;
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

	public String getNegotiationAttachmentFileId() {
		return negotiationAttachmentFileId;
	}

	public void setNegotiationAttachmentFileId(String negotiationAttachmentFileId) {
		this.negotiationAttachmentFileId = negotiationAttachmentFileId;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String mimeType) {
		this.mimeType = mimeType;
	}

	public Integer getNegotiationCommentAttachmentId() {
		return negotiationCommentAttachmentId;
	}

	public void setNegotiationCommentAttachmentId(Integer negotiationCommentAttachmentId) {
		this.negotiationCommentAttachmentId = negotiationCommentAttachmentId;
	}

}
