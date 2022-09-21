package com.polus.fibicomp.grantcall.vo;

import java.util.List;
import java.util.Map;

import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;
import com.polus.fibicomp.proposal.pojo.ProposalStatus;

public class EvaluationMainPanelVO {

	private Integer grantCallId;

	private List<Proposal> submittedProposals;

	private List<ProposalEvaluationScore> proposalEvaluationScores;

	private String statusType;

	private String loginPersonId;

	private String updateUser;

	private ProposalEvaluationScore proposalEvaluationScore;

	private List<ProposalStatus> proposalStatuses;

	private List<String> keys;

	private List<Map<String, Object>> evaluationDatas;

	private Integer scoringColorEndAt;

	private String exportIndex;

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public List<ProposalEvaluationScore> getProposalEvaluationScores() {
		return proposalEvaluationScores;
	}

	public void setProposalEvaluationScores(List<ProposalEvaluationScore> proposalEvaluationScores) {
		this.proposalEvaluationScores = proposalEvaluationScores;
	}

	public List<Proposal> getSubmittedProposals() {
		return submittedProposals;
	}

	public void setSubmittedProposals(List<Proposal> submittedProposals) {
		this.submittedProposals = submittedProposals;
	}

	public String getLoginPersonId() {
		return loginPersonId;
	}

	public void setLoginPersonId(String loginPersonId) {
		this.loginPersonId = loginPersonId;
	}

	public String getStatusType() {
		return statusType;
	}

	public void setStatusType(String statusType) {
		this.statusType = statusType;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public ProposalEvaluationScore getProposalEvaluationScore() {
		return proposalEvaluationScore;
	}

	public void setProposalEvaluationScore(ProposalEvaluationScore proposalEvaluationScore) {
		this.proposalEvaluationScore = proposalEvaluationScore;
	}

	public List<ProposalStatus> getProposalStatuses() {
		return proposalStatuses;
	}

	public void setProposalStatuses(List<ProposalStatus> proposalStatuses) {
		this.proposalStatuses = proposalStatuses;
	}

	public List<String> getKeys() {
		return keys;
	}

	public void setKeys(List<String> keys) {
		this.keys = keys;
	}

	public Integer getScoringColorEndAt() {
		return scoringColorEndAt;
	}

	public void setScoringColorEndAt(Integer scoringColorEndAt) {
		this.scoringColorEndAt = scoringColorEndAt;
	}

	public List<Map<String, Object>> getEvaluationDatas() {
		return evaluationDatas;
	}

	public void setEvaluationDatas(List<Map<String, Object>> evaluationDatas) {
		this.evaluationDatas = evaluationDatas;
	}

	public String getExportIndex() {
		return exportIndex;
	}

	public void setExportIndex(String exportIndex) {
		this.exportIndex = exportIndex;
	}

}
