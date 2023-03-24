package com.polus.fibicomp.ip.vo;

import java.util.List;
import java.util.Map;

import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.budget.pojo.BudgetStatus;
import com.polus.fibicomp.compilance.pojo.AcProtocolStatus;
import com.polus.fibicomp.compilance.pojo.IrbProtocolStatus;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.ip.dto.InstituteProposalHistoryDto;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionLog;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachments;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetHeader;
import com.polus.fibicomp.ip.pojo.InstituteProposalComment;
import com.polus.fibicomp.ip.pojo.InstituteProposalKeywords;
import com.polus.fibicomp.ip.pojo.InstituteProposalPerson;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonAttachment;
import com.polus.fibicomp.ip.pojo.InstituteProposalResearchArea;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.ip.pojo.InstituteProposalStatus;
import com.polus.fibicomp.ip.pojo.InstituteProposalType;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ResearchType;
import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.proposal.pojo.CommentType;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.proposal.pojo.DisciplineCluster;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;
import com.polus.fibicomp.vo.ParameterVO;

public class InstProposalVO {
		
	private InstituteProposal instProposal;

	private Integer proposalId;

	private String userName;

	private String personId;

	private Integer devProposalId;

	private List<Integer> devProposalIds;

	private Integer statusCode;

	private List<InstituteProposalStatus> statusCodes;

	private List<String> availableRights;

	private Boolean isFundingSupportDeclarationRequired = false;

	private Boolean isReplaceAttachmentEnabled = Boolean.FALSE;

	private String sortBy;

	private String reverse;

	private List<InstituteProposalAttachments> instituteProposalAttachments;

	private List<InstituteProposalPerson> instituteProposalPersons;

	private List<InstituteProposalResearchArea> instituteProposalResearchAreas;

	private List<InstituteProposalSpecialReview> instituteProposalSpecialReviews;

	private InstituteProposalPerson instituteProposalPerson;

	private InstituteProposalSpecialReview instituteProposalSpecialReview;

	private InstituteProposalResearchArea instituteProposalResearchArea;

	private InstituteProposalBudgetHeader instituteProposalBudgetHeader;

	private InstituteProposalAttachments instituteProposalAttachment;

	private String instPropPersonId;

	private List<InstituteProposalPersonAttachment> newIPPersonAttachments;

	private List<GrantCallType> grantCallTypes;

	private List<ProposalPersonRole> proposalPersonRoles;

	private List<InstituteProposalAttachType> proposalAttachmentTypes;

	private List<InstituteProposalType> proposalTypes;

	private List<AwardType> awardTypes;

	private List<ActivityType> activityTypes;

	private InstituteProposalKeywords instituteProposalKeyword;

	private List<InstituteProposalKeywords> instituteProposalKeywords;

	private List<SpecialReviewType> reviewTypes;

	private List<SpecialReviewApprovalType> specialReviewApprovalTypes;

	private List<ResearchType> researchTypes;

	private String researchDescription;

	private String multiDisciplinaryDescription;

	private List<InstituteProposalActionType> instituteProposalActionTypes;

	private InstituteProposalActionType instituteProposalActionType;

	private List<InstituteProposalActionLog> instituteProposalActionLogs;

	private Map<Integer, List<ProposalAttachment>> proposalAttachments;

	private Boolean isShowInKind = false;
	
	private Boolean isShowCostShareAndUnderrecovery = false;

	private List<BudgetStatus> budgetStatus;

	private List<RateType> rateTypes;

	private Boolean isShowModifiedDirectCost = false;

	private Boolean isCampusFlagEnabled = false;

	private String description;

	private Boolean isAwarded;

	private String proposalNumber;

	private Boolean isKeyPersonMerge = false;

	private Boolean isSpecialReviewMerge = false;

	private Boolean isBudgetMerge = false;

	private List<DisciplineCluster> disciplineClusters;

	private Boolean isShowBudgetOHRatePercentage;

	private Boolean enableActivityGrantCallMapping;

	private List<CommentType> commentType;

	private InstituteProposalComment instituteProposalComment;

	private List<InstituteProposalComment> instituteProposalComments;

	private List<AcProtocolStatus> acProtocolStatuses;

	private List<IrbProtocolStatus> irbProtocolStatuses;
	
	private List<CostSharingType> costSharingTypes;

	private Integer costShareTypeCode;
	
	private boolean enableCostShareStatus;

	private Boolean instituteProposalDateChanged;

	private ParameterVO parameterValue;

	private List<InstituteProposalHistoryDto> instituteProposalHistories;

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public InstituteProposal getInstProposal() {
		return instProposal;
	}

	public void setInstProposal(InstituteProposal instProposal) {
		this.instProposal = instProposal;
	}

	public Integer getDevProposalId() {
		return devProposalId;
	}

	public void setDevProposalId(Integer devProposalId) {
		this.devProposalId = devProposalId;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public List<InstituteProposalStatus> getStatusCodes() {
		return statusCodes;
	}

	public void setStatusCodes(List<InstituteProposalStatus> statusCodes) {
		this.statusCodes = statusCodes;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public Boolean getIsFundingSupportDeclarationRequired() {
		return isFundingSupportDeclarationRequired;
	}

	public void setIsFundingSupportDeclarationRequired(Boolean isFundingSupportDeclarationRequired) {
		this.isFundingSupportDeclarationRequired = isFundingSupportDeclarationRequired;
	}

	public Boolean getIsReplaceAttachmentEnabled() {
		return isReplaceAttachmentEnabled;
	}

	public void setIsReplaceAttachmentEnabled(Boolean isReplaceAttachmentEnabled) {
		this.isReplaceAttachmentEnabled = isReplaceAttachmentEnabled;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public List<InstituteProposalAttachments> getInstituteProposalAttachments() {
		return instituteProposalAttachments;
	}

	public void setInstituteProposalAttachments(List<InstituteProposalAttachments> instituteProposalAttachments) {
		this.instituteProposalAttachments = instituteProposalAttachments;
	}

	public List<Integer> getDevProposalIds() {
		return devProposalIds;
	}

	public void setDevProposalIds(List<Integer> devProposalIds) {
		this.devProposalIds = devProposalIds;
	}

	public List<InstituteProposalPerson> getInstituteProposalPersons() {
		return instituteProposalPersons;
	}

	public void setInstituteProposalPersons(List<InstituteProposalPerson> instituteProposalPersons) {
		this.instituteProposalPersons = instituteProposalPersons;
	}

	public List<InstituteProposalResearchArea> getInstituteProposalResearchAreas() {
		return instituteProposalResearchAreas;
	}

	public void setInstituteProposalResearchAreas(List<InstituteProposalResearchArea> instituteProposalResearchAreas) {
		this.instituteProposalResearchAreas = instituteProposalResearchAreas;
	}

	public List<InstituteProposalSpecialReview> getInstituteProposalSpecialReviews() {
		return instituteProposalSpecialReviews;
	}

	public void setInstituteProposalSpecialReviews(List<InstituteProposalSpecialReview> instituteProposalSpecialReviews) {
		this.instituteProposalSpecialReviews = instituteProposalSpecialReviews;
	}

	public InstituteProposalPerson getInstituteProposalPerson() {
		return instituteProposalPerson;
	}

	public void setInstituteProposalPerson(InstituteProposalPerson instituteProposalPerson) {
		this.instituteProposalPerson = instituteProposalPerson;
	}

	public String getInstPropPersonId() {
		return instPropPersonId;
	}

	public void setInstPropPersonId(String instPropPersonId) {
		this.instPropPersonId = instPropPersonId;
	}

	public List<InstituteProposalPersonAttachment> getNewIPPersonAttachments() {
		return newIPPersonAttachments;
	}

	public void setNewIPPersonAttachments(List<InstituteProposalPersonAttachment> newIPPersonAttachments) {
		this.newIPPersonAttachments = newIPPersonAttachments;
	}

	public InstituteProposalSpecialReview getInstituteProposalSpecialReview() {
		return instituteProposalSpecialReview;
	}

	public void setInstituteProposalSpecialReview(InstituteProposalSpecialReview instituteProposalSpecialReview) {
		this.instituteProposalSpecialReview = instituteProposalSpecialReview;
	}

	public InstituteProposalResearchArea getInstituteProposalResearchArea() {
		return instituteProposalResearchArea;
	}

	public void setInstituteProposalResearchArea(InstituteProposalResearchArea instituteProposalResearchArea) {
		this.instituteProposalResearchArea = instituteProposalResearchArea;
	}

	public InstituteProposalBudgetHeader getInstituteProposalBudgetHeader() {
		return instituteProposalBudgetHeader;
	}

	public void setInstituteProposalBudgetHeader(InstituteProposalBudgetHeader instituteProposalBudgetHeader) {
		this.instituteProposalBudgetHeader = instituteProposalBudgetHeader;
	}

	public InstituteProposalAttachments getInstituteProposalAttachment() {
		return instituteProposalAttachment;
	}

	public void setInstituteProposalAttachment(InstituteProposalAttachments instituteProposalAttachment) {
		this.instituteProposalAttachment = instituteProposalAttachment;
	}

	public List<GrantCallType> getGrantCallTypes() {
		return grantCallTypes;
	}

	public void setGrantCallTypes(List<GrantCallType> grantCallTypes) {
		this.grantCallTypes = grantCallTypes;
	}

	public List<ProposalPersonRole> getProposalPersonRoles() {
		return proposalPersonRoles;
	}

	public void setProposalPersonRoles(List<ProposalPersonRole> proposalPersonRoles) {
		this.proposalPersonRoles = proposalPersonRoles;
	}

	public List<AwardType> getAwardTypes() {
		return awardTypes;
	}

	public void setAwardTypes(List<AwardType> awardTypes) {
		this.awardTypes = awardTypes;
	}

	public List<ActivityType> getActivityTypes() {
		return activityTypes;
	}

	public void setActivityTypes(List<ActivityType> activityTypes) {
		this.activityTypes = activityTypes;
	}

	public InstituteProposalKeywords getInstituteProposalKeyword() {
		return instituteProposalKeyword;
	}

	public void setInstituteProposalKeyword(InstituteProposalKeywords instituteProposalKeyword) {
		this.instituteProposalKeyword = instituteProposalKeyword;
	}

	public List<InstituteProposalKeywords> getInstituteProposalKeywords() {
		return instituteProposalKeywords;
	}

	public void setInstituteProposalKeywords(List<InstituteProposalKeywords> instituteProposalKeywords) {
		this.instituteProposalKeywords = instituteProposalKeywords;
	}

	public List<SpecialReviewType> getReviewTypes() {
		return reviewTypes;
	}

	public void setReviewTypes(List<SpecialReviewType> reviewTypes) {
		this.reviewTypes = reviewTypes;
	}

	public List<SpecialReviewApprovalType> getSpecialReviewApprovalTypes() {
		return specialReviewApprovalTypes;
	}

	public void setSpecialReviewApprovalTypes(List<SpecialReviewApprovalType> specialReviewApprovalTypes) {
		this.specialReviewApprovalTypes = specialReviewApprovalTypes;
	}

	public List<ResearchType> getResearchTypes() {
		return researchTypes;
	}

	public void setResearchTypes(List<ResearchType> researchTypes) {
		this.researchTypes = researchTypes;
	}

	public String getResearchDescription() {
		return researchDescription;
	}

	public void setResearchDescription(String researchDescription) {
		this.researchDescription = researchDescription;
	}

	public String getMultiDisciplinaryDescription() {
		return multiDisciplinaryDescription;
	}

	public void setMultiDisciplinaryDescription(String multiDisciplinaryDescription) {
		this.multiDisciplinaryDescription = multiDisciplinaryDescription;
	}

	public List<InstituteProposalAttachType> getProposalAttachmentTypes() {
		return proposalAttachmentTypes;
	}

	public void setProposalAttachmentTypes(List<InstituteProposalAttachType> proposalAttachmentTypes) {
		this.proposalAttachmentTypes = proposalAttachmentTypes;
	}

	public List<InstituteProposalType> getProposalTypes() {
		return proposalTypes;
	}

	public void setProposalTypes(List<InstituteProposalType> proposalTypes) {
		this.proposalTypes = proposalTypes;
	}

	public List<InstituteProposalActionType> getInstituteProposalActionTypes() {
		return instituteProposalActionTypes;
	}

	public void setInstituteProposalActionTypes(List<InstituteProposalActionType> instituteProposalActionTypes) {
		this.instituteProposalActionTypes = instituteProposalActionTypes;
	}

	public InstituteProposalActionType getInstituteProposalActionType() {
		return instituteProposalActionType;
	}

	public void setInstituteProposalActionType(InstituteProposalActionType instituteProposalActionType) {
		this.instituteProposalActionType = instituteProposalActionType;
	}

	public List<InstituteProposalActionLog> getInstituteProposalActionLogs() {
		return instituteProposalActionLogs;
	}

	public void setInstituteProposalActionLogs(List<InstituteProposalActionLog> instituteProposalActionLogs) {
		this.instituteProposalActionLogs = instituteProposalActionLogs;
	}

	public Map<Integer, List<ProposalAttachment>> getProposalAttachments() {
		return proposalAttachments;
	}

	public void setProposalAttachments(Map<Integer, List<ProposalAttachment>> proposalAttachments) {
		this.proposalAttachments = proposalAttachments;
	}

	public Boolean getIsShowInKind() {
		return isShowInKind;
	}

	public void setIsShowInKind(Boolean isShowInKind) {
		this.isShowInKind = isShowInKind;
	}

	public Boolean getIsShowCostShareAndUnderrecovery() {
		return isShowCostShareAndUnderrecovery;
	}

	public void setIsShowCostShareAndUnderrecovery(Boolean isShowCostShareAndUnderrecovery) {
		this.isShowCostShareAndUnderrecovery = isShowCostShareAndUnderrecovery;
	}

	public List<BudgetStatus> getBudgetStatus() {
		return budgetStatus;
	}

	public void setBudgetStatus(List<BudgetStatus> budgetStatus) {
		this.budgetStatus = budgetStatus;
	}

	public List<RateType> getRateTypes() {
		return rateTypes;
	}

	public void setRateTypes(List<RateType> rateTypes) {
		this.rateTypes = rateTypes;
	}

	public Boolean getIsShowModifiedDirectCost() {
		return isShowModifiedDirectCost;
	}

	public void setIsShowModifiedDirectCost(Boolean isShowModifiedDirectCost) {
		this.isShowModifiedDirectCost = isShowModifiedDirectCost;
	}

	public Boolean getIsCampusFlagEnabled() {
		return isCampusFlagEnabled;
	}

	public void setIsCampusFlagEnabled(Boolean isCampusFlagEnabled) {
		this.isCampusFlagEnabled = isCampusFlagEnabled;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getIsAwarded() {
		return isAwarded;
	}

	public void setIsAwarded(Boolean isAwarded) {
		this.isAwarded = isAwarded;
	}

	public String getProposalNumber() {
		return proposalNumber;
	}

	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}

	public Boolean getIsKeyPersonMerge() {
		return isKeyPersonMerge;
	}

	public void setIsKeyPersonMerge(Boolean isKeyPersonMerge) {
		this.isKeyPersonMerge = isKeyPersonMerge;
	}

	public Boolean getIsSpecialReviewMerge() {
		return isSpecialReviewMerge;
	}

	public void setIsSpecialReviewMerge(Boolean isSpecialReviewMerge) {
		this.isSpecialReviewMerge = isSpecialReviewMerge;
	}

	public Boolean getIsBudgetMerge() {
		return isBudgetMerge;
	}

	public void setIsBudgetMerge(Boolean isBudgetMerge) {
		this.isBudgetMerge = isBudgetMerge;
	}

	public List<DisciplineCluster> getDisciplineClusters() {
		return disciplineClusters;
	}

	public void setDisciplineClusters(List<DisciplineCluster> disciplineClusters) {
		this.disciplineClusters = disciplineClusters;
	}

	public Boolean getShowBudgetOHRatePercentage() {
		return isShowBudgetOHRatePercentage;
	}

	public void setShowBudgetOHRatePercentage(Boolean showBudgetOHRatePercentage) {
		isShowBudgetOHRatePercentage = showBudgetOHRatePercentage;
	}

	public Boolean getEnableActivityGrantCallMapping() {
		return enableActivityGrantCallMapping;
	}

	public void setEnableActivityGrantCallMapping(Boolean enableActivityGrantCallMapping) {
		this.enableActivityGrantCallMapping = enableActivityGrantCallMapping;
	}

	public List<CommentType> getCommentType() {
		return commentType;
	}

	public void setCommentType(List<CommentType> commentType) {
		this.commentType = commentType;
	}

	public InstituteProposalComment getInstituteProposalComment() {
		return instituteProposalComment;
	}

	public void setInstituteProposalComment(InstituteProposalComment instituteProposalComment) {
		this.instituteProposalComment = instituteProposalComment;
	}

	public List<InstituteProposalComment> getInstituteProposalComments() {
		return instituteProposalComments;
	}

	public void setInstituteProposalComments(List<InstituteProposalComment> instituteProposalComments) {
		this.instituteProposalComments = instituteProposalComments;
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
	
	public List<CostSharingType> getCostSharingTypes() {
		return costSharingTypes;
	}

	public void setCostSharingTypes(List<CostSharingType> costSharingTypes) {
		this.costSharingTypes = costSharingTypes;
	}

	public Integer getCostShareTypeCode() {
		return costShareTypeCode;
	}

	public void setCostShareTypeCode(Integer costShareTypeCode) {
		this.costShareTypeCode = costShareTypeCode;
	}
	
	public boolean isEnableCostShareStatus() {
		return enableCostShareStatus;
	}

	public void setEnableCostShareStatus(boolean enableCostShareStatus) {
		this.enableCostShareStatus = enableCostShareStatus;
	}

	public Boolean getInstituteProposalDateChanged() {
		return instituteProposalDateChanged;
	}

	public void setInstituteProposalDateChanged(Boolean instituteProposalDateChanged) {
		this.instituteProposalDateChanged = instituteProposalDateChanged;
	}

	public ParameterVO getParameterValue() {
		return parameterValue;
	}

	public void setParameterValue(ParameterVO parameterValue) {
		this.parameterValue = parameterValue;
	}

	public List<InstituteProposalHistoryDto> getInstituteProposalHistories() {
		return instituteProposalHistories;
	}

	public void setInstituteProposalHistories(List<InstituteProposalHistoryDto> instituteProposalHistories) {
		this.instituteProposalHistories = instituteProposalHistories;
	}

}
