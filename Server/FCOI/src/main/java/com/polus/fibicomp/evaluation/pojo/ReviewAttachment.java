package com.polus.fibicomp.evaluation.pojo;

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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "PROPOSAL_REVIEW_ATTACHMENT")
public class ReviewAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REVIEW_ATTACHMENT_ID", length = 10)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PROP_EVAL_ATCHMNT_ID_GENERATOR")
	@SequenceGenerator(name="PROP_EVAL_ATCHMNT_ID_GENERATOR", sequenceName = "PROP_EVAL_ATCHMNT_ID_GENERATOR", allocationSize=1)
	private Integer attachmentId;

	@JsonBackReference
	@ManyToOne(optional = false, cascade = CascadeType.REFRESH)
	@JoinColumn(foreignKey = @ForeignKey(name = "REVIEWER_ATTACHMENT_FK1"), name = "REVIEW_COMMENT_ID", referencedColumnName = "REVIEW_COMMENT_ID")
	private ReviewComment reviewComment;

	@Column(name = "FILE_NAME", length = 255)
	private String fileName;

	@Column(name = "MIME_TYPE", length = 255)
	private String mimeType;

	@Column(name = "PROPOSAL_ID", length = 10)
	private Integer proposalId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	@Column(name = "FILE_DATA_ID", length = 255)
	private String fileDataId;

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
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

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
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

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public ReviewComment getReviewComment() {
		return reviewComment;
	}

	public void setReviewComment(ReviewComment reviewComment) {
		this.reviewComment = reviewComment;
	}

}
