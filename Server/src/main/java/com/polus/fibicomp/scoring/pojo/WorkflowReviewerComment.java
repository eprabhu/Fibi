package com.polus.fibicomp.scoring.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "WORKFLOW_REVIEWER_COMMENTS")
public class WorkflowReviewerComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKFLOW_REVIEWER_COMMENTS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer workflowReviewerCommentsId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_REVIEWER_COMMENTS_FK1"), name = "WORKFLOW_REVIEWER_SCORE_ID", referencedColumnName = "WORKFLOW_REVIEWER_SCORE_ID")
	private WorkflowReviewerScore workflowReviewerScore;

	@Column(name = "IS_PRIVATE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPrivate = false;

	@Column(name = "COMMENT")
	private String comment;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonManagedReference
	@OneToMany(mappedBy = "workflowReviewerComments", orphanRemoval = true, cascade = {
			CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<WorkflowReviewerAttachment> workflowReviewerAttachments;

	public WorkflowReviewerComment() {
		workflowReviewerAttachments = new ArrayList<WorkflowReviewerAttachment>();
	}

	public Integer getWorkflowReviewerCommentsId() {
		return workflowReviewerCommentsId;
	}

	public void setWorkflowReviewerCommentsId(Integer workflowReviewerCommentsId) {
		this.workflowReviewerCommentsId = workflowReviewerCommentsId;
	}

	public WorkflowReviewerScore getWorkflowReviewerScore() {
		return workflowReviewerScore;
	}

	public void setWorkflowReviewerScore(WorkflowReviewerScore workflowReviewerScore) {
		this.workflowReviewerScore = workflowReviewerScore;
	}

	public Boolean getIsPrivate() {
		return isPrivate;
	}

	public void setIsPrivate(Boolean isPrivate) {
		this.isPrivate = isPrivate;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
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

	public List<WorkflowReviewerAttachment> getWorkflowReviewerAttachments() {
		return workflowReviewerAttachments;
	}

	public void setWorkflowReviewerAttachments(List<WorkflowReviewerAttachment> workflowReviewerAttachments) {
		this.workflowReviewerAttachments = workflowReviewerAttachments;
	}

}
