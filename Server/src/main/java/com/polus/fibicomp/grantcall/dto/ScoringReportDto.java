package com.polus.fibicomp.grantcall.dto;

import java.sql.Timestamp;

public class ScoringReportDto {

	private Integer reviewerScoreId;
	private String scoringCriteriaCode;
	private String scoringCriteriaDescription;
	private ScoredPersonDTO person;
	private Timestamp updateTimeStamp;

	public String getScoringCriteriaCode() {
		return scoringCriteriaCode;
	}

	public void setScoringCriteriaCode(String scoringCriteriaCode) {
		this.scoringCriteriaCode = scoringCriteriaCode;
	}

	public String getScoringCriteriaDescription() {
		return scoringCriteriaDescription;
	}

	public void setScoringCriteriaDescription(String scoringCriteriaDescription) {
		this.scoringCriteriaDescription = scoringCriteriaDescription;
	}

	public Integer getReviewerScoreId() {
		return reviewerScoreId;
	}

	public void setReviewerScoreId(Integer reviewerScoreId) {
		this.reviewerScoreId = reviewerScoreId;
	}

	public ScoredPersonDTO getPerson() {
		return person;
	}

	public void setPerson(ScoredPersonDTO person) {
		this.person = person;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

}
