package com.polus.fibicomp.evaluation.vo;

import java.util.List;

import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanelPersons;

public class EvaluationVO {

	private Integer proposalId;

	private Integer proposalEvaluationPanelId;

	private ProposalEvaluationPanelPersons proposalEvaluationPanelPerson;

	private Integer proposalEvaluationPanelPersonId;

	private String actionType;

	private String personId;

	private String updateUser;

	private String message;

	private Integer grantCallId;

	private List<ProposalEvaluationPanel> proposalEvaluationPanelsList;

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Integer getProposalEvaluationPanelId() {
		return proposalEvaluationPanelId;
	}

	public void setProposalEvaluationPanelId(Integer proposalEvaluationPanelId) {
		this.proposalEvaluationPanelId = proposalEvaluationPanelId;
	}

	public ProposalEvaluationPanelPersons getProposalEvaluationPanelPerson() {
		return proposalEvaluationPanelPerson;
	}

	public void setProposalEvaluationPanelPerson(ProposalEvaluationPanelPersons proposalEvaluationPanelPerson) {
		this.proposalEvaluationPanelPerson = proposalEvaluationPanelPerson;
	}

	public Integer getProposalEvaluationPanelPersonId() {
		return proposalEvaluationPanelPersonId;
	}

	public void setProposalEvaluationPanelPersonId(Integer proposalEvaluationPanelPersonId) {
		this.proposalEvaluationPanelPersonId = proposalEvaluationPanelPersonId;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<ProposalEvaluationPanel> getProposalEvaluationPanelsList() {
		return proposalEvaluationPanelsList;
	}

	public void setProposalEvaluationPanelsList(List<ProposalEvaluationPanel> proposalEvaluationPanelsList) {
		this.proposalEvaluationPanelsList = proposalEvaluationPanelsList;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

}
