package com.polus.fibicomp.scoring.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
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
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.grantcall.pojo.ScoringCriteria;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Entity
@Table(name = "WORKFLOW_REVIEWER_SCORE")
public class WorkflowReviewerScore implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKFLOW_REVIEWER_SCORE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer workflowReviewerScoreId;

	@Column(name = "WORKFLOW_DETAIL_ID")
	private Integer workflowDetailId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_REVIEWER_SCORE_FK1"), name = "WORKFLOW_DETAIL_ID", referencedColumnName = "WORKFLOW_DETAIL_ID", insertable = false, updatable = false)
	private WorkflowDetail workflowDetail;

	@Column(name = "SCORE")
	private BigDecimal score;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "SCORING_CRITERIA_TYPE_CODE")
	private String scoringCriteriaTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_REVIEWER_SCORE_FK2"), name = "SCORING_CRITERIA_TYPE_CODE", referencedColumnName = "SCORING_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
	private ScoringCriteria scoringCriteria;

	@Transient
	private String description;

	@JsonManagedReference
	@OneToMany(mappedBy = "workflowReviewerScore", orphanRemoval = true, cascade = {CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<WorkflowReviewerComment> workflowReviewerComments;

	@Transient
	private String updatedUserFullName;

	public WorkflowReviewerScore() {
		workflowReviewerComments = new ArrayList<WorkflowReviewerComment>();
	}

	public Integer getWorkflowReviewerScoreId() {
		return workflowReviewerScoreId;
	}

	public void setWorkflowReviewerScoreId(Integer workflowReviewerScoreId) {
		this.workflowReviewerScoreId = workflowReviewerScoreId;
	}

	public Integer getWorkflowDetailId() {
		return workflowDetailId;
	}

	public void setWorkflowDetailId(Integer workflowDetailId) {
		this.workflowDetailId = workflowDetailId;
	}

	public WorkflowDetail getWorkflowDetail() {
		return workflowDetail;
	}

	public void setWorkflowDetail(WorkflowDetail workflowDetail) {
		this.workflowDetail = workflowDetail;
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

	public String getScoringCriteriaTypeCode() {
		return scoringCriteriaTypeCode;
	}

	public void setScoringCriteriaTypeCode(String scoringCriteriaTypeCode) {
		this.scoringCriteriaTypeCode = scoringCriteriaTypeCode;
	}

	public ScoringCriteria getScoringCriteria() {
		return scoringCriteria;
	}

	public void setScoringCriteria(ScoringCriteria scoringCriteria) {
		this.scoringCriteria = scoringCriteria;
	}

	public List<WorkflowReviewerComment> getWorkflowReviewerComments() {
		return workflowReviewerComments;
	}

	public void setWorkflowReviewerComments(List<WorkflowReviewerComment> workflowReviewerComments) {
		this.workflowReviewerComments = workflowReviewerComments;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdatedUserFullName() {
		return updatedUserFullName;
	}

	public void setUpdatedUserFullName(String updatedUserFullName) {
		this.updatedUserFullName = updatedUserFullName;
	}

	public BigDecimal getScore() {
		return score;
	}

	public void setScore(BigDecimal score) {
		this.score = score;
	}

}
