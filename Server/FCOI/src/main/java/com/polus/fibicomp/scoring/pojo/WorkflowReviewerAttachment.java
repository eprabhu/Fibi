package com.polus.fibicomp.scoring.pojo;

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
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "WORKFLOW_REVIEWER_ATTMNTS")
public class WorkflowReviewerAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKFLOW_REVIEWER_ATTMNTS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer workflowReviewerAttmntsId;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_REVIEWER_ATTMNTS_FK2"), name = "WORKFLOW_REVIEWER_COMMENTS_ID", referencedColumnName = "WORKFLOW_REVIEWER_COMMENTS_ID")
	private WorkflowReviewerComment workflowReviewerComments;

	@JsonIgnore
	@Transient
	private byte[] attachment;

	public Integer getWorkflowReviewerAttmntsId() {
		return workflowReviewerAttmntsId;
	}

	public void setWorkflowReviewerAttmntsId(Integer workflowReviewerAttmntsId) {
		this.workflowReviewerAttmntsId = workflowReviewerAttmntsId;
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

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public WorkflowReviewerComment getWorkflowReviewerComments() {
		return workflowReviewerComments;
	}

	public void setWorkflowReviewerComments(WorkflowReviewerComment workflowReviewerComments) {
		this.workflowReviewerComments = workflowReviewerComments;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

}
