package com.polus.fibicomp.evaluation.pojo;

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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "EPS_PROP_EVALPANEL_PERSONS")
public class ProposalEvaluationPanelPersons implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EVALPANEL_PERSONS_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EVALPANEL_PERSONS_ID_GENERATOR")
	@SequenceGenerator(name = "EVALPANEL_PERSONS_ID_GENERATOR", sequenceName = "EVALPANEL_PERSONS_ID_GENERATOR", allocationSize = 1)
	private Integer evaluationPanelPersonId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVALPANEL_PERSONS_FK1"), name = "PROPOSAL_EVALUATION_ID", referencedColumnName = "PROPOSAL_EVALUATION_ID")
	private ProposalEvaluationPanel proposalEvaluationPanel;

	@Column(name = "APPROVER_NUMBER")
	private Integer approverNumber;

	@Column(name = "APPROVER_PERSON_ID")
	private String approverPersonId;

	@Column(name = "APPROVER_PERSON_NAME")
	private String approverPersonName;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Integer getEvaluationPanelPersonId() {
		return evaluationPanelPersonId;
	}

	public void setEvaluationPanelPersonId(Integer evaluationPanelPersonId) {
		this.evaluationPanelPersonId = evaluationPanelPersonId;
	}

	public ProposalEvaluationPanel getProposalEvaluationPanel() {
		return proposalEvaluationPanel;
	}

	public void setProposalEvaluationPanel(ProposalEvaluationPanel proposalEvaluationPanel) {
		this.proposalEvaluationPanel = proposalEvaluationPanel;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public String getApproverPersonId() {
		return approverPersonId;
	}

	public void setApproverPersonId(String approverPersonId) {
		this.approverPersonId = approverPersonId;
	}

	public String getApproverPersonName() {
		return approverPersonName;
	}

	public void setApproverPersonName(String approverPersonName) {
		this.approverPersonName = approverPersonName;
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

}
