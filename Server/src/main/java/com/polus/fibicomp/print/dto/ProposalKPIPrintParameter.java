package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.List;

public class ProposalKPIPrintParameter {

	private String KPIType;
	private List<ProposalKPICriteriaType> proposalKPICriteriaTypes;

	public ProposalKPIPrintParameter() {
		proposalKPICriteriaTypes = new ArrayList<>();
	}

	public String getKPIType() {
		return KPIType;
	}

	public void setKPIType(String kPIType) {
		KPIType = kPIType;
	}

	public List<ProposalKPICriteriaType> getProposalKPICriteriaTypes() {
		return proposalKPICriteriaTypes;
	}

	public void setProposalKPICriteriaTypes(List<ProposalKPICriteriaType> proposalKPICriteriaTypes) {
		this.proposalKPICriteriaTypes = proposalKPICriteriaTypes;
	}

}
