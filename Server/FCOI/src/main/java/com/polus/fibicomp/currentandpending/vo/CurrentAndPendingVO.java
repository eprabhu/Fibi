package com.polus.fibicomp.currentandpending.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingPersonDTO;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetail;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetailExt;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.proposal.pojo.ProposalFundingStatus;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;

public class CurrentAndPendingVO {

	private String loginPersonId ;

	private Integer moduleCode;

	private String moduleItemKey;

	private List<ProposalPerson> proposalPersons;

	private String createUser;

	private String updateUser;

	private List<CurrentAndPendingPersonDTO> selectedPersons;

	private List<ProposalPersonRole> proposalPersonRoles;

	private Integer cpReportHeaderId;

	private Boolean isExcluded;

	private CPReportProjectDetailExt cpReportProjectDetailExt;

	private Integer cpReportProjectDetailId;

	private String personId;

	private List<Currency> currencyDetails;

	private List<ProposalFundingStatus> proposalFundingStatus;

	private List<SponsorType> sponsorTypes;

	private CPReportProjectDetail externalProjectDetail;

	private String sponsorName;

	private String message;

	public CurrentAndPendingVO() {
		selectedPersons = new ArrayList<>();
		proposalPersons = new ArrayList<>();
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getLoginPersonId() {
		return loginPersonId;
	}

	public void setLoginPersonId(String loginPersonId) {
		this.loginPersonId = loginPersonId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public List<ProposalPerson> getProposalPersons() {
		return proposalPersons;
	}

	public void setProposalPersons(List<ProposalPerson> proposalPersons) {
		this.proposalPersons = proposalPersons;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<CurrentAndPendingPersonDTO> getSelectedPersons() {
		return selectedPersons;
	}

	public void setSelectedPersons(List<CurrentAndPendingPersonDTO> selectedPersons) {
		this.selectedPersons = selectedPersons;
	}

	public List<ProposalPersonRole> getProposalPersonRoles() {
		return proposalPersonRoles;
	}

	public void setProposalPersonRoles(List<ProposalPersonRole> proposalPersonRoles) {
		this.proposalPersonRoles = proposalPersonRoles;
	}

	public Integer getCpReportHeaderId() {
		return cpReportHeaderId;
	}

	public void setCpReportHeaderId(Integer cpReportHeaderId) {
		this.cpReportHeaderId = cpReportHeaderId;
	}

	public Boolean getIsExcluded() {
		return isExcluded;
	}

	public void setIsExcluded(Boolean isExcluded) {
		this.isExcluded = isExcluded;
	}

	public CPReportProjectDetailExt getCpReportProjectDetailExt() {
		return cpReportProjectDetailExt;
	}

	public void setCpReportProjectDetailExt(CPReportProjectDetailExt cpReportProjectDetailExt) {
		this.cpReportProjectDetailExt = cpReportProjectDetailExt;
	}

	public Integer getCpReportProjectDetailId() {
		return cpReportProjectDetailId;
	}

	public void setCpReportProjectDetailId(Integer cpReportProjectDetailId) {
		this.cpReportProjectDetailId = cpReportProjectDetailId;
	}

	public List<Currency> getCurrencyDetails() {
		return currencyDetails;
	}

	public void setCurrencyDetails(List<Currency> currencyDetails) {
		this.currencyDetails = currencyDetails;
	}

	public List<ProposalFundingStatus> getProposalFundingStatus() {
		return proposalFundingStatus;
	}

	public void setProposalFundingStatus(List<ProposalFundingStatus> proposalFundingStatus) {
		this.proposalFundingStatus = proposalFundingStatus;
	}

	public List<SponsorType> getSponsorTypes() {
		return sponsorTypes;
	}

	public void setSponsorTypes(List<SponsorType> sponsorTypes) {
		this.sponsorTypes = sponsorTypes;
	}

	public CPReportProjectDetail getExternalProjectDetail() {
		return externalProjectDetail;
	}

	public void setExternalProjectDetail(CPReportProjectDetail externalProjectDetail) {
		this.externalProjectDetail = externalProjectDetail;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

}
