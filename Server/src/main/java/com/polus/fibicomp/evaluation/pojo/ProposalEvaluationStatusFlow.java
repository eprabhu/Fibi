package com.polus.fibicomp.evaluation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.proposal.pojo.ProposalStatus;
import com.polus.fibicomp.roles.pojo.Role;

@Entity
@Table(name = "EPS_PROP_EVALUATN_STATUS_FLOW")
public class ProposalEvaluationStatusFlow implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EPS_PROP_EVAL_STATUS_FLOW_CODE")
	private Integer proposalEvaluationStatusCode;

	@Column(name = "STOP_NUMBER")
	private Integer stopNumber;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVAL_STATUS_FLOW_FK1"), name = "ROLE_ID", referencedColumnName = "ROLE_ID", insertable = false, updatable = false)
	private Role role;

	@Column(name = "NEXT_EVALUATION_STOP")
	private Integer nextEvaluationStopCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVAL_STATUS_FLOW_FK4"), name = "NEXT_EVALUATION_STOP", referencedColumnName = "EVALUATION_STOP_CODE", insertable = false, updatable = false)
	private EvaluationStop evaluationStop;

	@Column(name = "REVIEW_STATUS_CODE")
	private Integer reviewStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVAL_STATUS_FLOW_FK2"), name = "REVIEW_STATUS_CODE", referencedColumnName = "REVIEW_STATUS_CODE", insertable = false, updatable = false)
	private ReviewStatus reviewStatus;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVAL_STATUS_FLOW_FK3"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ProposalStatus proposalStatus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	public Integer getStopNumber() {
		return stopNumber;
	}

	public void setStopNumber(Integer stopNumber) {
		this.stopNumber = stopNumber;
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

	public Integer getProposalEvaluationStatusCode() {
		return proposalEvaluationStatusCode;
	}

	public void setProposalEvaluationStatusCode(Integer proposalEvaluationStatusCode) {
		this.proposalEvaluationStatusCode = proposalEvaluationStatusCode;
	}

	public Integer getNextEvaluationStopCode() {
		return nextEvaluationStopCode;
	}

	public void setNextEvaluationStopCode(Integer nextEvaluationStopCode) {
		this.nextEvaluationStopCode = nextEvaluationStopCode;
	}

	public EvaluationStop getEvaluationStop() {
		return evaluationStop;
	}

	public void setEvaluationStop(EvaluationStop evaluationStop) {
		this.evaluationStop = evaluationStop;
	}

}
