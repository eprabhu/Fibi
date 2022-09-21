package com.polus.fibicomp.prereview.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
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
@Table(name = "PRE_REVIEW_ATTACHMENT")
public class PreReviewAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PRE_REVIEW_ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PRE_REVIEW_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name="PRE_REVIEW_ATTACHMENT_ID_GENERATOR", sequenceName = "PRE_REVIEW_ATTACHMENT_ID_GENERATOR", allocationSize=1)
	private Integer preReviewAttachmentId;

	@JsonBackReference
	@ManyToOne(optional = false, fetch = FetchType.EAGER)
	@JoinColumn(foreignKey = @ForeignKey(name = "PRE_REVIEW_ATTACHMENT_FK1"), name = "PRE_REVIEW_COMMENT_ID", referencedColumnName = "PRE_REVIEW_COMMENT_ID")
	private PreReviewComment preReviewComment;

	@Column(name = "PRE_REVIEW_ID")
	private Integer preReviewId;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getPreReviewAttachmentId() {
		return preReviewAttachmentId;
	}

	public void setPreReviewAttachmentId(Integer preReviewAttachmentId) {
		this.preReviewAttachmentId = preReviewAttachmentId;
	}

	public Integer getPreReviewId() {
		return preReviewId;
	}

	public void setPreReviewId(Integer preReviewId) {
		this.preReviewId = preReviewId;
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

	public PreReviewComment getPreReviewComment() {
		return preReviewComment;
	}

	public void setPreReviewComment(PreReviewComment preReviewComment) {
		this.preReviewComment = preReviewComment;
	}

}
