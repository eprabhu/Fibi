package com.polus.fibicomp.grantcall.dto;

import java.math.BigDecimal;
import java.util.List;

import com.polus.fibicomp.proposal.pojo.ProposalPerson;

public class ProposalDataObject {

	private Integer proposalId;
	private String proposalTitle;
	private String leadUnitName;
	private String leadUnitNumber;
	private List<ProposalPerson> principleInvestigator;
	private BigDecimal score;
	private Integer statusCode;
	private String status;

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getProposalTitle() {
		return proposalTitle;
	}

	public void setProposalTitle(String proposalTitle) {
		this.proposalTitle = proposalTitle;
	}

	public String getLeadUnitName() {
		return leadUnitName;
	}

	public void setLeadUnitName(String leadUnitName) {
		this.leadUnitName = leadUnitName;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public List<ProposalPerson> getPrincipleInvestigator() {
		return principleInvestigator;
	}

	public void setPrincipleInvestigator(List<ProposalPerson> principleInvestigator) {
		this.principleInvestigator = principleInvestigator;
	}

	public BigDecimal getScore() {
		return score;
	}

	public void setScore(BigDecimal score) {
		this.score = score;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

}
