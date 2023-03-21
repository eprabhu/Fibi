package com.polus.fibicomp.workflow.pojo;

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

import org.hibernate.annotations.GenericGenerator;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "WORKFLOW_ATTACHMENT")
public class WorkflowAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ATTACHMENT_ID", updatable = false, nullable = false)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "WORKFLOW_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name="WORKFLOW_ATTACHMENT_ID_GENERATOR", sequenceName = "WORKFLOW_ATTACHMENT_ID_GENERATOR", allocationSize=1)
	private Integer attachmentId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_ATTACHMENT_FK1"), name = "WORKFLOW_DETAIL_ID", referencedColumnName = "WORKFLOW_DETAIL_ID")
	private WorkflowDetail workflowDetail;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_ATTACHMENT_FK2"), name = "REVIEWER_DETAILS_ID", referencedColumnName = "REVIEWER_DETAILS_ID")
	private WorkflowReviewerDetail workflowReviewerDetail;

	@JsonIgnore
	@Column(name = "ATTACHMENT")
	private byte[] attachment;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@GeneratedValue(generator = "system-uuid")
	@GenericGenerator(name = "system-uuid", strategy = "uuid2")
	@Column(name = "FILE_DATA_ID", unique = true)
	private String fileDataId;

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
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

	public WorkflowDetail getWorkflowDetail() {
		return workflowDetail;
	}

	public void setWorkflowDetail(WorkflowDetail workflowDetail) {
		this.workflowDetail = workflowDetail;
	}

	public WorkflowReviewerDetail getWorkflowReviewerDetail() {
		return workflowReviewerDetail;
	}

	public void setWorkflowReviewerDetail(WorkflowReviewerDetail workflowReviewerDetail) {
		this.workflowReviewerDetail = workflowReviewerDetail;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

}
