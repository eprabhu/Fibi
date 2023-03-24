package com.polus.fibicomp.grantcall.dto;

import java.math.BigDecimal;
import java.util.List;

import com.polus.fibicomp.scoring.pojo.WorkflowReviewerComment;

public class ScoredPersonDTO {
	private BigDecimal score;
	private String personId;
	private String personName;
	private List<WorkflowReviewerComment> workflowReviewerComments;
	private Boolean isPersonCanScore;
	private String evaluationMapName;

	public Boolean getIsPersonCanScore() {
		return isPersonCanScore;
	}

	public void setIsPersonCanScore(Boolean isPersonCanScore) {
		this.isPersonCanScore = isPersonCanScore;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public List<WorkflowReviewerComment> getWorkflowReviewerComments() {
		return workflowReviewerComments;
	}

	public void setWorkflowReviewerComments(List<WorkflowReviewerComment> workflowReviewerComments) {
		this.workflowReviewerComments = workflowReviewerComments;
	}

	public BigDecimal getScore() {
		return score;
	}

	public void setScore(BigDecimal score) {
		this.score = score;
	}

	public String getEvaluationMapName() {
		return evaluationMapName;
	}

	public void setEvaluationMapName(String evaluationMapName) {
		this.evaluationMapName = evaluationMapName;
	}

}
