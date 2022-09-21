package com.polus.fibicomp.award.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.award.pojo.AccountType;
import com.polus.fibicomp.award.pojo.AwardContactType;
import com.polus.fibicomp.award.pojo.AwardStatus;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.award.pojo.CostShareType;
import com.polus.fibicomp.award.pojo.MilestoneStatus;
import com.polus.fibicomp.budget.pojo.AwardWorkflowStatus;
import com.polus.fibicomp.compilance.pojo.AcProtocolStatus;
import com.polus.fibicomp.compilance.pojo.IrbProtocolStatus;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ResearchType;
import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.pojo.SpecialReviewType;

public class AwardGeneralDetailsVO {

	private List<AccountType> accountTypes;

	private List<AwardType> awardTypes;

	private List<ActivityType> activitytypes;

	private List<AwardStatus> awardStatus;

	private List<ProposalPersonRole> proposalPersonRoles;

	private List<AwardContactType> awardContactTypeList;

	private List<SpecialReviewType> reviewTypes;

	private List<SpecialReviewApprovalType> approvalStatusTypes;

	private List<CostShareType> costShareTypes;

	private List<String> availableRights;

	private AwardWorkflowStatus awardWorkflowStatus;

	private List<ResearchType> researchTypes;

	private Boolean isGenerateWBSNumber = false;

	private List<MilestoneStatus> milestoneStatus;

	private List<AcProtocolStatus> acProtocolStatuses;

	private List<IrbProtocolStatus> irbProtocolStatuses;

	public AwardGeneralDetailsVO() {
		super();
		this.awardStatus = new ArrayList<>();
		this.activitytypes = new ArrayList<>();
		this.awardTypes = new ArrayList<>();
		this.accountTypes = new ArrayList<>();
		this.costShareTypes = new ArrayList<>();
		this.approvalStatusTypes = new ArrayList<>();
		this.reviewTypes = new ArrayList<>();
		this.proposalPersonRoles = new ArrayList<>();
		this.awardContactTypeList = new ArrayList<>();
	}

	public List<AwardStatus> getAwardStatus() {
		return awardStatus;
	}

	public void setAwardStatus(List<AwardStatus> awardStatus) {
		this.awardStatus = awardStatus;
	}

	public List<ActivityType> getActivitytypes() {
		return activitytypes;
	}

	public void setActivitytypes(List<ActivityType> activitytypes) {
		this.activitytypes = activitytypes;
	}

	public List<AwardType> getAwardTypes() {
		return awardTypes;
	}

	public void setAwardTypes(List<AwardType> awardTypes) {
		this.awardTypes = awardTypes;
	}

	public List<AccountType> getAccountTypes() {
		return accountTypes;
	}

	public void setAccountTypes(List<AccountType> accountTypes) {
		this.accountTypes = accountTypes;
	}

	public List<CostShareType> getCostShareTypes() {
		return costShareTypes;
	}

	public void setCostShareTypes(List<CostShareType> costShareTypes) {
		this.costShareTypes = costShareTypes;
	}

	public List<SpecialReviewApprovalType> getApprovalStatusTypes() {
		return approvalStatusTypes;
	}

	public void setApprovalStatusTypes(List<SpecialReviewApprovalType> approvalStatusTypes) {
		this.approvalStatusTypes = approvalStatusTypes;
	}

	public List<SpecialReviewType> getReviewTypes() {
		return reviewTypes;
	}

	public void setReviewTypes(List<SpecialReviewType> reviewTypes) {
		this.reviewTypes = reviewTypes;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public List<ProposalPersonRole> getProposalPersonRoles() {
		return proposalPersonRoles;
	}

	public void setProposalPersonRoles(List<ProposalPersonRole> proposalPersonRoles) {
		this.proposalPersonRoles = proposalPersonRoles;
	}

	public List<AwardContactType> getAwardContactTypeList() {
		return awardContactTypeList;
	}

	public void setAwardContactTypeList(List<AwardContactType> awardContactTypeList) {
		this.awardContactTypeList = awardContactTypeList;
	}

	public AwardWorkflowStatus getAwardWorkflowStatus() {
		return awardWorkflowStatus;
	}

	public void setAwardWorkflowStatus(AwardWorkflowStatus awardWorkflowStatus) {
		this.awardWorkflowStatus = awardWorkflowStatus;
	}

	public Boolean getIsGenerateWBSNumber() {
		return isGenerateWBSNumber;
	}

	public void setIsGenerateWBSNumber(Boolean isGenerateWBSNumber) {
		this.isGenerateWBSNumber = isGenerateWBSNumber;
	}

	public List<ResearchType> getResearchTypes() {
		return researchTypes;
	}

	public void setResearchTypes(List<ResearchType> researchTypes) {
		this.researchTypes = researchTypes;
	}

	public List<MilestoneStatus> getMilestoneStatus() {
		return milestoneStatus;
	}

	public void setMilestoneStatus(List<MilestoneStatus> milestoneStatus) {
		this.milestoneStatus = milestoneStatus;
	}

	public List<AcProtocolStatus> getAcProtocolStatuses() {
		return acProtocolStatuses;
	}

	public void setAcProtocolStatuses(List<AcProtocolStatus> acProtocolStatuses) {
		this.acProtocolStatuses = acProtocolStatuses;
	}

	public List<IrbProtocolStatus> getIrbProtocolStatuses() {
		return irbProtocolStatuses;
	}

	public void setIrbProtocolStatuses(List<IrbProtocolStatus> irbProtocolStatuses) {
		this.irbProtocolStatuses = irbProtocolStatuses;
	}

}
