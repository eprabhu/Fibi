package com.polus.fibicomp.evaluation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.evaluation.comparator.EvaluvationCompartorByReviewComment;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PROPOSAL_REVIEW")
public class ProposalReview implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REVIEW_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PROPOSAL_EVAL_ID_GENERATOR")
	@SequenceGenerator(name="PROPOSAL_EVAL_ID_GENERATOR", sequenceName = "PROPOSAL_EVAL_ID_GENERATOR", allocationSize=1)
	private Integer reviewId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "REVIEW_STATUS_CODE", length = 3)
	private Integer reviewStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_REVIEW_FK2"), name = "REVIEW_STATUS_CODE", referencedColumnName = "REVIEW_STATUS_CODE", insertable = false, updatable = false)
	private ReviewStatus reviewStatus;

	@Column(name = "REVIEWER_EMAIL", length = 200)
	private String reviewerEmail;

	@Column(name = "REVIEWER_FULLNAME", length = 90)
	private String reviewerFullName;

	@Column(name = "REVIEWER_PERSON_ID", length = 90)
	private String reviewerPersonId;

	@Column(name = "REVIEW_START_DATE")
	private Timestamp reviewStartDate;

	@Column(name = "REVIEW_END_DATE")
	private Timestamp reviewEndDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "proposalReview", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<ReviewComment> reviewComments;

	@Column(name = "COMPLETE_REVIEWER_EMAIL", length = 200)
	private String completeReviewerEmail;

	@Column(name = "COMPLETE_REVIEWER_FULLNAME", length = 90)
	private String completeReviewerFullName;

	@Column(name = "COMPLETE_REVIEWER_PERSON_ID", length = 90)
	private String completeReviewerPersonId;

	@Column(name = "REVIEW_DEADLINE_DATE")
	private Timestamp reviewDeadLineDate;

	@Column(name = "PI_REVIEW_DEADLINE_DATE")
	private Timestamp piReviewDeadLineDate;

	@Column(name = "HAS_ENDORSE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasEndorsed = false;

	@Column(name = "HAS_RANK", length = 200)
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasRank = false;

	@Column(name = "IS_FINAL", length = 200)
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isFinal = false;

	@Column(name = "HAS_QUESTIONNAIRE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasQuestionnaire = false;

	@Column(name = "HAS_RECOMMENDATION")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasRecommendation = false;

	@Column(name = "EVALUATION_STOP_CODE")
	private Integer evaluationStopCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_REVIEW_FK4"), name = "EVALUATION_STOP_CODE", referencedColumnName = "EVALUATION_STOP_CODE", insertable = false, updatable = false)
	private EvaluationStop evaluationStop;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_REVIEW_FK5"), name = "ROLE_ID", referencedColumnName = "ROLE_ID", insertable = false, updatable = false)
	private Role role;

	@Transient
	private Boolean isReturned = false;

	public ProposalReview() {
		reviewComments = new ArrayList<>();
	}

	public Integer getReviewId() {
		return reviewId;
	}

	public void setReviewId(Integer reviewId) {
		this.reviewId = reviewId;
	}

	public Integer getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(Integer reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public ReviewStatus getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(ReviewStatus reviewStatus) {
		this.reviewStatus = reviewStatus;
	}

	public String getReviewerEmail() {
		return reviewerEmail;
	}

	public void setReviewerEmail(String reviewerEmail) {
		this.reviewerEmail = reviewerEmail;
	}

	public String getReviewerFullName() {
		return reviewerFullName;
	}

	public void setReviewerFullName(String reviewerFullName) {
		this.reviewerFullName = reviewerFullName;
	}

	public String getReviewerPersonId() {
		return reviewerPersonId;
	}

	public void setReviewerPersonId(String reviewerPersonId) {
		this.reviewerPersonId = reviewerPersonId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<ReviewComment> getReviewComments() {
		if (reviewComments != null && !reviewComments.isEmpty()) {
			Collections.sort(reviewComments, new EvaluvationCompartorByReviewComment());
		}
		return reviewComments;
	}

	public void setReviewComments(List<ReviewComment> reviewComments) {
		this.reviewComments = reviewComments;
	}

	public Timestamp getReviewStartDate() {
		return reviewStartDate;
	}

	public void setReviewStartDate(Timestamp reviewStartDate) {
		this.reviewStartDate = reviewStartDate;
	}

	public Timestamp getReviewEndDate() {
		return reviewEndDate;
	}

	public void setReviewEndDate(Timestamp reviewEndDate) {
		this.reviewEndDate = reviewEndDate;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Timestamp getReviewDeadLineDate() {
		return reviewDeadLineDate;
	}

	public void setReviewDeadLineDate(Timestamp reviewDeadLineDate) {
		this.reviewDeadLineDate = reviewDeadLineDate;
	}

	public Timestamp getPiReviewDeadLineDate() {
		return piReviewDeadLineDate;
	}

	public void setPiReviewDeadLineDate(Timestamp piReviewDeadLineDate) {
		this.piReviewDeadLineDate = piReviewDeadLineDate;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Boolean getHasEndorsed() {
		return hasEndorsed;
	}

	public void setHasEndorsed(Boolean hasEndorsed) {
		this.hasEndorsed = hasEndorsed;
	}

	public Boolean getHasRank() {
		return hasRank;
	}

	public void setHasRank(Boolean hasRank) {
		this.hasRank = hasRank;
	}

	public Boolean getIsFinal() {
		return isFinal;
	}

	public void setIsFinal(Boolean isFinal) {
		this.isFinal = isFinal;
	}

	public String getCompleteReviewerEmail() {
		return completeReviewerEmail;
	}

	public void setCompleteReviewerEmail(String completeReviewerEmail) {
		this.completeReviewerEmail = completeReviewerEmail;
	}

	public String getCompleteReviewerFullName() {
		return completeReviewerFullName;
	}

	public void setCompleteReviewerFullName(String completeReviewerFullName) {
		this.completeReviewerFullName = completeReviewerFullName;
	}

	public String getCompleteReviewerPersonId() {
		return completeReviewerPersonId;
	}

	public void setCompleteReviewerPersonId(String completeReviewerPersonId) {
		this.completeReviewerPersonId = completeReviewerPersonId;
	}

	public Boolean getIsReturned() {
		return isReturned;
	}

	public void setIsReturned(Boolean isReturned) {
		this.isReturned = isReturned;
	}

	public Integer getRoleId() {
		return roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

	public Role getRole() {
		return role;
	}

	public void setRole(Role role) {
		this.role = role;
	}

	public EvaluationStop getEvaluationStop() {
		return evaluationStop;
	}

	public void setEvaluationStop(EvaluationStop evaluationStop) {
		this.evaluationStop = evaluationStop;
	}

	public Integer getEvaluationStopCode() {
		return evaluationStopCode;
	}

	public void setEvaluationStopCode(Integer evaluationStopCode) {
		this.evaluationStopCode = evaluationStopCode;
	}

	public Boolean getHasQuestionnaire() {
		return hasQuestionnaire;
	}

	public void setHasQuestionnaire(Boolean hasQuestionnaire) {
		this.hasQuestionnaire = hasQuestionnaire;
	}

	public Boolean getHasRecommendation() {
		return hasRecommendation;
	}

	public void setHasRecommendation(Boolean hasRecommendation) {
		this.hasRecommendation = hasRecommendation;
	}

}
