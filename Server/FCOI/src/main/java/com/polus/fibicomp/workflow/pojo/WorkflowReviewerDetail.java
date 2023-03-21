package com.polus.fibicomp.workflow.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "WORKFLOW_REVIEWER_DETAIL")
public class WorkflowReviewerDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "reviewerDetailIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "reviewerDetailIdGenerator")
	@Column(name = "REVIEWER_DETAILS_ID", updatable = false, nullable = false)
	private Integer reviewerDetailId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "WRKFLW_REVIWER_DTL_FK1"), name = "WORKFLOW_DETAIL_ID", referencedColumnName = "WORKFLOW_DETAIL_ID")
	private WorkflowDetail workflowDetail;

	@Column(name = "REVIEWER_PERSON_ID")
	private String reviewerPersonId;

	@Column(name = "REVIEWER_PERSON_NAME")
	private String reviewerPersonName;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "REVIEW_DATE")
	private Timestamp reviewDate;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "APPROVAL_STATUS")
	private String approvalStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WRKFLW_REVIWER_DTL_FK2"), name = "APPROVAL_STATUS", referencedColumnName = "APPROVAL_STATUS", insertable = false, updatable = false)
	private WorkflowStatus workflowStatus;

	//@JsonManagedReference
	@OneToMany(mappedBy = "workflowReviewerDetail", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<WorkflowAttachment> workflowAttachments;

	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;

	public WorkflowReviewerDetail() {
		workflowAttachments = new ArrayList<WorkflowAttachment>();
	}

	public Integer getReviewerDetailId() {
		return reviewerDetailId;
	}

	public void setReviewerDetailId(Integer reviewerDetailId) {
		this.reviewerDetailId = reviewerDetailId;
	}

	public WorkflowDetail getWorkflowDetail() {
		return workflowDetail;
	}

	public void setWorkflowDetail(WorkflowDetail workflowDetail) {
		this.workflowDetail = workflowDetail;
	}

	public String getReviewerPersonId() {
		return reviewerPersonId;
	}

	public void setReviewerPersonId(String reviewerPersonId) {
		this.reviewerPersonId = reviewerPersonId;
	}

	public String getReviewerPersonName() {
		return reviewerPersonName;
	}

	public void setReviewerPersonName(String reviewerPersonName) {
		this.reviewerPersonName = reviewerPersonName;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public Timestamp getReviewDate() {
		return reviewDate;
	}

	public void setReviewDate(Timestamp reviewDate) {
		this.reviewDate = reviewDate;
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

	public String getApprovalStatusCode() {
		return approvalStatusCode;
	}

	public void setApprovalStatusCode(String approvalStatusCode) {
		this.approvalStatusCode = approvalStatusCode;
	}

	public WorkflowStatus getWorkflowStatus() {
		return workflowStatus;
	}

	public void setWorkflowStatus(WorkflowStatus workflowStatus) {
		this.workflowStatus = workflowStatus;
	}

	public List<WorkflowAttachment> getWorkflowAttachments() {
		return workflowAttachments;
	}

	public void setWorkflowAttachments(List<WorkflowAttachment> workflowAttachments) {
		this.workflowAttachments = workflowAttachments;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}
}
