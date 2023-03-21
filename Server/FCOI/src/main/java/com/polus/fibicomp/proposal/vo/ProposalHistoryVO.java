package com.polus.fibicomp.proposal.vo;

import java.util.List;

import com.polus.fibicomp.proposal.pojo.ProposalHistory;
import com.polus.fibicomp.vo.ParameterVO;

public class ProposalHistoryVO {

	private List<ProposalHistory> proposalHistory;

	private ParameterVO parameterValue;

	public ParameterVO getParameterValue() {
		return parameterValue;
	}

	public void setParameterValue(ParameterVO parameterValue) {
		this.parameterValue = parameterValue;
	}

	public List<ProposalHistory> getProposalHistory() {
		return proposalHistory;
	}

	public void setProposalHistory(List<ProposalHistory> proposalHistory) {
		this.proposalHistory = proposalHistory;
	}
}
