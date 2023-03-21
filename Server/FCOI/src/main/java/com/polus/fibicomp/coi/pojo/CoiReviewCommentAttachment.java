package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "COI_REVIEW_COMMENT_ATTACHMENT")
@EntityListeners(AuditingEntityListener.class)
public class CoiReviewCommentAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_REVIEW_COMMENT_ATT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiReviewCommentAttId;
	
	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;
	
	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "COI_REVIEW_ID")
	private Integer coiReviewId;
	
	@Column(name = "COI_REVIEW_COMMENT_ID")
	private Integer coiReviewCommentId;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getCoiReviewCommentAttId() {
		return coiReviewCommentAttId;
	}

	public void setCoiReviewCommentAttId(Integer coiReviewCommentAttId) {
		this.coiReviewCommentAttId = coiReviewCommentAttId;
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

	public Integer getCoiReviewId() {
		return coiReviewId;
	}

	public void setCoiReviewId(Integer coiReviewId) {
		this.coiReviewId = coiReviewId;
	}

	public Integer getCoiReviewCommentId() {
		return coiReviewCommentId;
	}

	public void setCoiReviewCommentId(Integer coiReviewCommentId) {
		this.coiReviewCommentId = coiReviewCommentId;
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

}
