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

@Entity
@Table(name = "FINAL_EVALUATION_STATUS")
public class FinalEvaluationStatus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FINAL_EVALUATION_STATUS_CODE")
	private Integer finalEvaluationStatusCode;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "FINAL_EVALUATION_STATUS_FK1"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ProposalStatus proposalStatus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getFinalEvaluationStatusCode() {
		return finalEvaluationStatusCode;
	}

	public void setFinalEvaluationStatusCode(Integer finalEvaluationStatusCode) {
		this.finalEvaluationStatusCode = finalEvaluationStatusCode;
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

}
