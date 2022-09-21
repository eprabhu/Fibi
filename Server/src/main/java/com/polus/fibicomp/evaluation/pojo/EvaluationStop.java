package com.polus.fibicomp.evaluation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.proposal.pojo.ProposalStatus;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EPS_PROP_EVALUATION_STOP")
public class EvaluationStop implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EVALUATION_STOP_CODE")
	private Integer evaluationStopCode;

	@Column(name = "DESCRIPTION", length = 200)
	private String description;

	@Column(name = "STOP_NUMBER")
	private Integer stopNumber;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVALUATION_STOP_FK1"), name = "ROLE_ID", referencedColumnName = "ROLE_ID", insertable = false, updatable = false)
	private Role role;

	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVALUATION_STOP_FK2"), name = "ACTIVITY_TYPE_CODE", referencedColumnName = "ACTIVITY_TYPE_CODE", insertable = false, updatable = false)
	private ActivityType activityType;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVALUATION_STOP_FK3"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ProposalStatus proposalStatus;

	@Column(name = "HAS_ENDORSE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasEndorsed = false;

	@Column(name = "HAS_RANK")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasRank = false;

	@Column(name = "IS_FINAL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isFinal = false;

	@Column(name = "HAS_QUESTIONNAIRE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasQuestionnaire = false;

	@Column(name = "HAS_RECOMMENDATION")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasRecommendation = false;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getStopNumber() {
		return stopNumber;
	}

	public void setStopNumber(Integer stopNumber) {
		this.stopNumber = stopNumber;
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

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public ActivityType getActivityType() {
		return activityType;
	}

	public void setActivityType(ActivityType activityType) {
		this.activityType = activityType;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public ProposalStatus getProposalStatus() {
		return proposalStatus;
	}

	public void setProposalStatus(ProposalStatus proposalStatus) {
		this.proposalStatus = proposalStatus;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
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
