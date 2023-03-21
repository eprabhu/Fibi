package com.polus.fibicomp.proposal.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;

public class ProposalPersonRoleVO {
	
	Integer proposalId;

	private ProposalPersonRoles proposalPersonRole;

	private List<ProposalPersonRoles> proposalPersonRoles;
		
	private String acType;
	
	private String message;

	private List<ModuleDerivedRoles> moduleDerivedRoles;

	public List<ModuleDerivedRoles> getModuleDerivedRoles() {
		return moduleDerivedRoles;
	}

	public void setModuleDerivedRoles(List<ModuleDerivedRoles> moduleDerivedRoles) {
		this.moduleDerivedRoles = moduleDerivedRoles;
	}

	public ProposalPersonRoleVO() {
		proposalPersonRoles = new ArrayList<>();
	}
	
	public ProposalPersonRoles getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRoles proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<ProposalPersonRoles> getProposalPersonRoles() {
		return proposalPersonRoles;
	}

	public void setProposalPersonRoles(List<ProposalPersonRoles> proposalPersonRoles) {
		this.proposalPersonRoles = proposalPersonRoles;
	}

}
