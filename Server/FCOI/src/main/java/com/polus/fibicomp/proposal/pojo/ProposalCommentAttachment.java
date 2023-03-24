package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.*;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "EPS_PROPOSAL_COMMENT_ATTACHMENTS")
@EntityListeners(AuditingEntityListener.class)
public class ProposalCommentAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COMMENT_ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "COMMENT_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name="COMMENT_ATTACHMENT_ID_GENERATOR", sequenceName = "COMMENT_ATTACHMENT_ID_GENERATOR", allocationSize=1)
	private Integer commentAttachmentId;
	
	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "EPS_PROPOSAL_COMMENT_ID")
	private Integer proposalCommentId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_COMMENT_ATTACHMENT_FK1"), name = "EPS_PROPOSAL_COMMENT_ID", referencedColumnName = "EPS_PROPOSAL_COMMENT_ID", insertable = false, updatable = false)
	private ProposalComment proposalComment;

	@JsonIgnore
	@Transient
	private byte[] attachment;

	@Transient
	private String lastUpdateUserFullName;

	public Integer getCommentAttachmentId() {
		return commentAttachmentId;
	}

	public void setCommentAttachmentId(Integer commentAttachmentId) {
		this.commentAttachmentId = commentAttachmentId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
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

	public Integer getProposalCommentId() {
		return proposalCommentId;
	}

	public void setProposalCommentId(Integer proposalCommentId) {
		this.proposalCommentId = proposalCommentId;
	}

	public ProposalComment getProposalComment() {
		return proposalComment;
	}

	public void setProposalComment(ProposalComment proposalComment) {
		this.proposalComment = proposalComment;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

	
}
