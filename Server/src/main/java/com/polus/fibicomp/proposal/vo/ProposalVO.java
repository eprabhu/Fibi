package com.polus.fibicomp.proposal.vo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetStatus;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.budget.vo.BudgetVO;
import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;
import com.polus.fibicomp.compilance.pojo.AcProtocolStatus;
import com.polus.fibicomp.compilance.pojo.IrbProtocolStatus;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.evaluation.pojo.EvaluationRecommendation;
import com.polus.fibicomp.evaluation.pojo.EvaluationStop;
import com.polus.fibicomp.evaluation.pojo.FinalEvaluationStatus;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanelPersons;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.evaluation.pojo.ReviewComment;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.grantcall.pojo.KPIType;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.FundingSourceType;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ResearchType;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewComment;
import com.polus.fibicomp.prereview.pojo.PreReviewSectionType;
import com.polus.fibicomp.prereview.pojo.PreReviewStatus;
import com.polus.fibicomp.prereview.pojo.PreReviewType;
import com.polus.fibicomp.prereview.pojo.PreReviewer;
import com.polus.fibicomp.proposal.pojo.CommentType;
import com.polus.fibicomp.proposal.pojo.CongressionalDistrict;
import com.polus.fibicomp.proposal.pojo.DisciplineCluster;
import com.polus.fibicomp.proposal.pojo.NarrativeStatus;
import com.polus.fibicomp.proposal.pojo.OrganizationType;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalAttachmentType;
import com.polus.fibicomp.proposal.pojo.ProposalComment;
import com.polus.fibicomp.proposal.pojo.ProposalCommentAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalExtension;
import com.polus.fibicomp.proposal.pojo.ProposalFundingStatus;
import com.polus.fibicomp.proposal.pojo.ProposalHistory;
import com.polus.fibicomp.proposal.pojo.ProposalIrbProtocol;
import com.polus.fibicomp.proposal.pojo.ProposalKPI;
import com.polus.fibicomp.proposal.pojo.ProposalMileStone;
import com.polus.fibicomp.proposal.pojo.ProposalOrganization;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.pojo.ProposalPersonAssignedRoles;
import com.polus.fibicomp.proposal.pojo.ProposalPersonAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalPersonDegree;
import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.proposal.pojo.ProposalProjectTeam;
import com.polus.fibicomp.proposal.pojo.ProposalResearchArea;
import com.polus.fibicomp.proposal.pojo.ProposalSponsor;
import com.polus.fibicomp.proposal.pojo.ProposalType;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerAttachment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerComment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerScore;
import com.polus.fibicomp.vo.BaseVO;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowMapDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowStatus;

public class ProposalVO extends BaseVO {

	private Integer grantCallId;

	private Proposal proposal;

	private List<ActivityType> activityTypes;

	private List<ScienceKeyword> scienceKeywords;

	private List<FundingSourceType> fundingSourceTypes;

	private List<ProposalAttachmentType> proposalAttachmentTypes;

	private List<GrantCall> grantCalls;

	private List<GrantCallType> grantCallTypes;

	private List<ProposalPersonRole> proposalPersonRoles;

	private List<ProposalAttachment> newAttachments;

	private List<SponsorType> sponsorTypes;

	private List<Sponsor> sponsors;

	private Integer keywordId;

	private Integer researchAreaId;

	private Integer attachmentId;

	private Integer budgetId;

	private Integer proposalPersonId;

	private Integer irbProtocolId;

	private Integer sponsorId;

	private Boolean status;

	private String message;

	private String updateType;

	private Integer proposalId;

	private String budgetCategoryCode;

	private String sponsorTypeCode;

	private List<ProposalType> proposalTypes;

	private Workflow workflow;

	private String actionType;

	private String userName;

	private String userFullName;

	private String personId;

	private String approveComment;

	private Boolean isApprover = false;

	private Boolean isApproved = false;

	private Boolean isReviewed = false;

	private Boolean isGrantAdmin = false;

	private Integer approverStopNumber;

	private Integer proposalStatusCode;

	private Boolean finalApprover = false;

	private GrantCallType defaultGrantCallType;

	private List<WorkflowMapDetail> availableReviewers;

	private WorkflowDetail loggedInWorkflowDetail;

	private Map<String, WorkflowStatus> workflowStatusMap;

	private String reviewerId;

	private List<Unit> homeUnits;

	private List<CostElement> costElements;

	private Integer budgetPeriodId;

	private List<CostElement> sysGeneratedCostElements;

	private Integer budgetDetailId;

	private Set<String> rateClassTypes;

	private List<BudgetCategory> budgetCategories;

	private Integer copyPeriodId;

	private Integer currentPeriodId;

	private List<SpecialReviewType> reviewTypes;

	private List<SpecialReviewApprovalType> specialReviewApprovalTypes;

	private List<TbnPerson> tbnPersons;

	private Integer proposalSpecialReviewId;

	private Boolean isDeclarationSectionRequired = false;

	private Boolean isPreReviewCompletionRequired = false;

	private List<Unit> departments;

	private List<NarrativeStatus> narrativeStatus;

	private List<PreReviewType> preReviewTypes;

	private List<PreReviewStatus> preReviewStatus;

	private PreReview newProposalPreReview;

	private List<Workflow> workflowList;

	private Boolean isProposalPerson = false;

	private boolean isSuperUser = false;

	private List<PreReviewer> preReviewers;

	private Integer documentId;

	private Integer moduleCode;

	private Integer subModuleCode;

	private Integer ruleId;

	private String moduleItemKey;

	private String updateUser;

	private String logginPersonId;

	private String canApproveRouting;

	private String isFinalApprover;

	private String isSubmit;

	private Integer budgetPersonDetailId;

	private List<WorkFlowResultDto> validationError;

	private String scienceKeyword;

	private Integer preReviewId;

	private PreReviewComment preNewReviewComment;

	private String sortBy;

	private String reverse;

	private List<PreReview> proposalPreReviews;

	private String budgetDescription;

	private List<PreReviewSectionType> preReviewClarifications;

	private List<PreReviewSectionType> preReviewRoutingReview;

	private List<BudgetStatus> budgetStatus;

	private List<RateType> rateTypes;

	private List<AwardType> awardType;

	private Integer reviewId;

	private ReviewComment newReviewComment;

	private Integer reviewCommentId;

	private List<ProposalReview> proposalReviews;

	private List<Integer> proposalIds;

	private ProposalReview newProposalReview;

	private Timestamp piReviewDeadLineDate;

	private List<EvaluationStop> evaluationReviewStop;

	private String homeUnitNumber;

	private List<ProposalFundingStatus> proposalFundingStatus;

	private List<DisciplineCluster> disciplineClusters;

	private Integer grantTypeCode;

	private Integer proposalPersonAssignedRoleId;

	private List<ProposalPersonAttachment> newPersonAttachments;

	private Boolean isCalculationWithPredefinedSalary;

	private Boolean isFundingSupportDeclarationRequired = false;

	private List<ProposalAttachment> proposalAttachments;

	private List<ProposalResearchArea> proposalResearchAreas;

	private List<ProposalPerson> proposalPersons;

	private List<ProposalSponsor> proposalSponsors;

	private List<ProposalIrbProtocol> proposalIrbProtocols;

	private List<ProposalSpecialReview> proposalSpecialReviews;

	private List<ProposalPersonAssignedRoles> proposalPersonAssignedRoles;

	private List<ProposalProjectTeam> proposalProjectTeams;

	private List<BudgetHeader> budgetHeaders;

	private ProposalAttachment proposalAttachment;

	private ProposalResearchArea proposalResearchArea;

	private ProposalPerson proposalPerson;

	private ProposalSponsor proposalSponsor;

	private ProposalIrbProtocol proposalIrbProtocol;

	private ProposalSpecialReview proposalSpecialReview;

	private ProposalProjectTeam proposalProjectTeam;

	private BudgetHeader budgetHeader;

	private String researchDescription;

	private String multiDisciplinaryDescription;

	private GrantCall grantCall;

	private BudgetVO budgetVO;

	private String attachmentDescription;

	private List<Integer> attachmentIds;

	private String completeReviewerEmail;

	private String completeReviewerFullName;

	private String completeReviewerPersonId;

	private List<String> availableRights;

	private Boolean isBudgetHeaderFound = false;

	private String reviewerEmail;

	private String reviewerFullName;

	private String reviewerPersonId;

	private List<PersonRoles> personRoles;

	private Boolean hasRank = false;

	private List<FinalEvaluationStatus> finalEvaluationStatus;

	private List<EvaluationRecommendation> evaluationRecommendation;

	private Boolean isRcbf = false;

	private String piPersonId;

	private Boolean hasRecommendation = false;

	private Boolean showOtherInformation;

	private Integer reviewStatusCode;

	private Integer numberOfApplications;

	private String reviewerRole;

	private Timestamp reviewDeadLineDate;

	private String proposalPersonRole;

	private String narrativeStatusCode;

	private ProposalMileStone proposalMileStone;

	private List<ProposalMileStone> proposalMileStones;

	private Integer proposalMileStoneId;

	private Timestamp reviewDeadLine;

	private ProposalKPI proposalKpi;

	private List<ProposalKPI> proposalKpis;

	private List<KPIType> kpiTypes;

	private ProposalExtension proposalExtension;

	private String userRole;

	private List<ProposalPersonRoles> proposalRoles;

	private List<String> ipNumbers;

	private Integer workflowDetailId;

	private Integer workflowReviewerScoreId;

	private WorkflowReviewerScore workflowReviewerScore;

	private List<WorkflowReviewerScore> workflowReviewerScores;

	private List<WorkflowReviewerAttachment> workflowReviewerAttachments;

	private Integer workflowReviewerAttmntsId;

	private Boolean isPersonCanScore;

	private String workFlowPersonId;

	private Integer categoryCode;

	private List<ProposalEvaluationPanel> proposalEvaluationPanelsList;

	private List<GrantCallEvaluationPanel> grantCallEvaluationPanelsList;

	private Integer proposalEvaluationPanelId;

	private ProposalEvaluationPanelPersons proposalEvaluationPanelPerson;

	private Integer proposalEvaluationPanelPersonId;

	private List<WorkflowReviewerComment> workflowReviewerComments;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private ProposalPersonAttachment proposalPersonAttachment;

	private String contentType;

	private String fileName;

	private Boolean isLastUploadedFile;

	private List<Currency> currencyDetails;

	private ProposalComment comment;

	private List<ProposalComment> proposalComments;

	private String commentTypeCode;

	private String isPublic;

	private List<CommentType> commentType;

	private String fileTimestamp;

	private boolean isNonEmployee = false;

	private List<SponsorFundingScheme> sponsorFundingSchemes;

	private Integer mapId;

	private Integer mapNumber;

	private Integer approverNumber;

	private String previousProposalPersonId;

	private Boolean previousNonEmployeeFlag;

	private Boolean isPINameAutoFilledRequired = Boolean.FALSE;

	private String loginPersonUnitNumber;

	private Integer workflowReviewerCommentsId;

	private Boolean isReplaceAttachmentEnabled = Boolean.FALSE;

	private String isBudgetSummaryPrint;

	private String isSimpleBudgetPrint;

	private String isDetailedBudgetPrint;

	private String isPersonnelBudgetPrint;

	private List<ProposalOrganization> proposalOrganizations;

	private ProposalOrganization proposalOrganization;

	private List<OrganizationType> organizationType;

	private List<CongressionalDistrict> congressionalDistricts;

	private Organization organization;

	private CongressionalDistrict congressionalDistrict;

	private List<ResearchType> researchTypes;

	private String rightName;

	private String unitNumber;

	private Boolean isRightExist = Boolean.FALSE;

	private Boolean isAutoCalculateEnabled = Boolean.FALSE;
	
	private String rcbfFundingStatusCode;
	
	private String rcbfTypeCode;

	private Boolean isProposalArchiveCreation = Boolean.FALSE;

	private Boolean isProposalAdminCorrection = Boolean.FALSE;

	private List<ProposalHistory> proposalHistory;

	private Boolean copyAllBudgetVersion = Boolean.FALSE;

	private Boolean copyFinalBudgetVersion = Boolean.FALSE;

	private Boolean copyQuestionnaire = Boolean.FALSE;

	private Boolean copyOtherInformation = Boolean.FALSE;

	private Boolean copyAttachment = Boolean.FALSE;

	private Boolean isProposalComparison = Boolean.FALSE;

	private Boolean isBudgetVersionEnabled = Boolean.FALSE;

	private Boolean ipGenerationOnly = Boolean.FALSE;

	private Map<String, Object>  grantEligibilityStatus;

	private String fileDataId;

	private List<ProposalPersonAttachment> proposalPersonAttachments;

	private Boolean enableOrganizationLocation = Boolean.FALSE;

	private Boolean canDeleteProposal;

	private String createUser;

	private Boolean enableActivityGrantCallMapping;

	private List<ProposalCommentAttachment> newCommentAttachments;

	private Boolean enableClosedGrantCallLinkingInProposal = Boolean.FALSE;
	
	private Integer noOfDays;

	private List<AcProtocolStatus> acProtocolStatuses;

	private List<IrbProtocolStatus> irbProtocolStatuses;
	
	private ProposalPersonDegree proposalPersonDegree;
	
	private Integer proposalPersonDegreeId;
	
	private List<ProposalPersonDegree> proposalPersonDegrees;

	private Boolean personCertified;

	private Boolean proposalEditable;

	public ProposalVO() {
		proposal = new Proposal();
		proposalAttachments = new ArrayList<>();
		proposalPersons = new ArrayList<>();
		proposalIrbProtocols = new ArrayList<>();
		proposalResearchAreas = new ArrayList<>();
		proposalSponsors = new ArrayList<>();
		proposalSpecialReviews = new ArrayList<>();
		proposalProjectTeams = new ArrayList<>();
		proposalReviews = new ArrayList<>();
		proposalPersonAssignedRoles = new ArrayList<>();
		proposalMileStones = new ArrayList<>();
		proposalKpis = new ArrayList<>();
		proposalRoles = new ArrayList<>();
		proposalHistory = new ArrayList<>();
	}


	public List<ProposalPersonDegree> getProposalPersonDegrees() {
		return proposalPersonDegrees;
	}

	public void setProposalPersonDegrees(List<ProposalPersonDegree> proposalPersonDegrees) {
		this.proposalPersonDegrees = proposalPersonDegrees;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public Proposal getProposal() {
		return proposal;
	}

	public void setProposal(Proposal proposal) {
		this.proposal = proposal;
	}

	public List<ScienceKeyword> getScienceKeywords() {
		return scienceKeywords;
	}

	public void setScienceKeywords(List<ScienceKeyword> scienceKeywords) {
		this.scienceKeywords = scienceKeywords;
	}

	public List<FundingSourceType> getFundingSourceTypes() {
		return fundingSourceTypes;
	}

	public void setFundingSourceTypes(List<FundingSourceType> fundingSourceTypes) {
		this.fundingSourceTypes = fundingSourceTypes;
	}

	public List<ProposalAttachmentType> getProposalAttachmentTypes() {
		return proposalAttachmentTypes;
	}

	public void setProposalAttachmentTypes(List<ProposalAttachmentType> proposalAttachmentTypes) {
		this.proposalAttachmentTypes = proposalAttachmentTypes;
	}

	public List<GrantCall> getGrantCalls() {
		return grantCalls;
	}

	public void setGrantCalls(List<GrantCall> grantCalls) {
		this.grantCalls = grantCalls;
	}

	public List<ProposalPersonRole> getProposalPersonRoles() {
		return proposalPersonRoles;
	}

	public void setProposalPersonRoles(List<ProposalPersonRole> proposalPersonRoles) {
		this.proposalPersonRoles = proposalPersonRoles;
	}

	public List<SponsorType> getSponsorTypes() {
		return sponsorTypes;
	}

	public void setSponsorTypes(List<SponsorType> sponsorTypes) {
		this.sponsorTypes = sponsorTypes;
	}

	public List<Sponsor> getSponsors() {
		return sponsors;
	}

	public void setSponsors(List<Sponsor> sponsors) {
		this.sponsors = sponsors;
	}

	public Integer getKeywordId() {
		return keywordId;
	}

	public void setKeywordId(Integer keywordId) {
		this.keywordId = keywordId;
	}

	public Integer getResearchAreaId() {
		return researchAreaId;
	}

	public void setResearchAreaId(Integer researchAreaId) {
		this.researchAreaId = researchAreaId;
	}

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public Integer getProposalPersonId() {
		return proposalPersonId;
	}

	public void setProposalPersonId(Integer proposalPersonId) {
		this.proposalPersonId = proposalPersonId;
	}

	public Integer getIrbProtocolId() {
		return irbProtocolId;
	}

	public void setIrbProtocolId(Integer irbProtocolId) {
		this.irbProtocolId = irbProtocolId;
	}

	public Integer getSponsorId() {
		return sponsorId;
	}

	public void setSponsorId(Integer sponsorId) {
		this.sponsorId = sponsorId;
	}

	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateType() {
		return updateType;
	}

	public void setUpdateType(String updateType) {
		this.updateType = updateType;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public List<ProposalType> getProposalTypes() {
		return proposalTypes;
	}

	public void setProposalTypes(List<ProposalType> proposalTypes) {
		this.proposalTypes = proposalTypes;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
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

	public String getApproveComment() {
		return approveComment;
	}

	public void setApproveComment(String approveComment) {
		this.approveComment = approveComment;
	}

	public Boolean getIsApprover() {
		return isApprover;
	}

	public void setIsApprover(Boolean isApprover) {
		this.isApprover = isApprover;
	}

	public Boolean getIsApproved() {
		return isApproved;
	}

	public void setIsApproved(Boolean isApproved) {
		this.isApproved = isApproved;
	}

	public Boolean getIsReviewed() {
		return isReviewed;
	}

	public void setIsReviewed(Boolean isReviewed) {
		this.isReviewed = isReviewed;
	}

	public Boolean getIsGrantAdmin() {
		return isGrantAdmin;
	}

	public void setIsGrantAdmin(Boolean isGrantAdmin) {
		this.isGrantAdmin = isGrantAdmin;
	}

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
	}

	public Integer getProposalStatusCode() {
		return proposalStatusCode;
	}

	public void setProposalStatusCode(Integer proposalStatusCode) {
		this.proposalStatusCode = proposalStatusCode;
	}

	public Boolean getFinalApprover() {
		return finalApprover;
	}

	public void setFinalApprover(Boolean finalApprover) {
		this.finalApprover = finalApprover;
	}

	public GrantCallType getDefaultGrantCallType() {
		return defaultGrantCallType;
	}

	public void setDefaultGrantCallType(GrantCallType defaultGrantCallType) {
		this.defaultGrantCallType = defaultGrantCallType;
	}

	public WorkflowDetail getLoggedInWorkflowDetail() {
		return loggedInWorkflowDetail;
	}

	public void setLoggedInWorkflowDetail(WorkflowDetail loggedInWorkflowDetail) {
		this.loggedInWorkflowDetail = loggedInWorkflowDetail;
	}

	public List<WorkflowMapDetail> getAvailableReviewers() {
		return availableReviewers;
	}

	public void setAvailableReviewers(List<WorkflowMapDetail> availableReviewers) {
		this.availableReviewers = availableReviewers;
	}

	public Map<String, WorkflowStatus> getWorkflowStatusMap() {
		return workflowStatusMap;
	}

	public void setWorkflowStatusMap(Map<String, WorkflowStatus> workflowStatusMap) {
		this.workflowStatusMap = workflowStatusMap;
	}

	public String getReviewerId() {
		return reviewerId;
	}

	public void setReviewerId(String reviewerId) {
		this.reviewerId = reviewerId;
	}

	public List<Unit> getHomeUnits() {
		return homeUnits;
	}

	public void setHomeUnits(List<Unit> homeUnits) {
		this.homeUnits = homeUnits;
	}

	public List<CostElement> getCostElements() {
		return costElements;
	}

	public void setCostElements(List<CostElement> costElements) {
		this.costElements = costElements;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}

	public List<ActivityType> getActivityTypes() {
		return activityTypes;
	}

	public void setActivityTypes(List<ActivityType> activityTypes) {
		this.activityTypes = activityTypes;
	}

	public List<CostElement> getSysGeneratedCostElements() {
		return sysGeneratedCostElements;
	}

	public void setSysGeneratedCostElements(List<CostElement> sysGeneratedCostElements) {
		this.sysGeneratedCostElements = sysGeneratedCostElements;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
	}

	public Set<String> getRateClassTypes() {
		return rateClassTypes;
	}

	public void setRateClassTypes(Set<String> rateClassTypes) {
		this.rateClassTypes = rateClassTypes;
	}

	public List<BudgetCategory> getBudgetCategories() {
		return budgetCategories;
	}

	public void setBudgetCategories(List<BudgetCategory> budgetCategories) {
		this.budgetCategories = budgetCategories;
	}

	public Integer getCopyPeriodId() {
		return copyPeriodId;
	}

	public void setCopyPeriodId(Integer copyPeriodId) {
		this.copyPeriodId = copyPeriodId;
	}

	public Integer getCurrentPeriodId() {
		return currentPeriodId;
	}

	public void setCurrentPeriodId(Integer currentPeriodId) {
		this.currentPeriodId = currentPeriodId;
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

	public List<TbnPerson> getTbnPersons() {
		return tbnPersons;
	}

	public void setTbnPersons(List<TbnPerson> tbnPersons) {
		this.tbnPersons = tbnPersons;
	}

	public Integer getProposalSpecialReviewId() {
		return proposalSpecialReviewId;
	}

	public void setProposalSpecialReviewId(Integer proposalSpecialReviewId) {
		this.proposalSpecialReviewId = proposalSpecialReviewId;
	}

	public Boolean getIsDeclarationSectionRequired() {
		return isDeclarationSectionRequired;
	}

	public void setIsDeclarationSectionRequired(Boolean isDeclarationSectionRequired) {
		this.isDeclarationSectionRequired = isDeclarationSectionRequired;
	}

	public List<Unit> getDepartments() {
		return departments;
	}

	public void setDepartments(List<Unit> departments) {
		this.departments = departments;
	}

	public List<NarrativeStatus> getNarrativeStatus() {
		return narrativeStatus;
	}

	public void setNarrativeStatus(List<NarrativeStatus> narrativeStatus) {
		this.narrativeStatus = narrativeStatus;
	}

	public List<ProposalAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<ProposalAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	public List<PreReviewType> getPreReviewTypes() {
		return preReviewTypes;
	}

	public void setPreReviewTypes(List<PreReviewType> preReviewTypes) {
		this.preReviewTypes = preReviewTypes;
	}

	public List<PreReviewStatus> getPreReviewStatus() {
		return preReviewStatus;
	}

	public void setPreReviewStatus(List<PreReviewStatus> preReviewStatus) {
		this.preReviewStatus = preReviewStatus;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public Boolean getIsPreReviewCompletionRequired() {
		return isPreReviewCompletionRequired;
	}

	public void setIsPreReviewCompletionRequired(Boolean isPreReviewCompletionRequired) {
		this.isPreReviewCompletionRequired = isPreReviewCompletionRequired;
	}

	public Boolean getIsProposalPerson() {
		return isProposalPerson;
	}

	public void setIsProposalPerson(Boolean isProposalPerson) {
		this.isProposalPerson = isProposalPerson;
	}

	public List<PreReviewer> getPreReviewers() {
		return preReviewers;
	}

	public void setPreReviewers(List<PreReviewer> preReviewers) {
		this.preReviewers = preReviewers;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public Integer getRuleId() {
		return ruleId;
	}

	public void setRuleId(Integer ruleId) {
		this.ruleId = ruleId;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getLogginPersonId() {
		return logginPersonId;
	}

	public void setLogginPersonId(String logginPersonId) {
		this.logginPersonId = logginPersonId;
	}

	public List<WorkFlowResultDto> getValidationError() {
		return validationError;
	}

	public void setValidationError(List<WorkFlowResultDto> validationError) {
		this.validationError = validationError;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public String getIsSubmit() {
		return isSubmit;
	}

	public void setIsSubmit(String isSubmit) {
		this.isSubmit = isSubmit;
	}

	public String getScienceKeyword() {
		return scienceKeyword;
	}

	public void setScienceKeyword(String scienceKeyword) {
		this.scienceKeyword = scienceKeyword;
	}

	public Integer getPreReviewId() {
		return preReviewId;
	}

	public void setPreReviewId(Integer preReviewId) {
		this.preReviewId = preReviewId;
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

	public String getBudgetDescription() {
		return budgetDescription;
	}

	public void setBudgetDescription(String budgetDescription) {
		this.budgetDescription = budgetDescription;
	}

	public PreReview getNewProposalPreReview() {
		return newProposalPreReview;
	}

	public void setNewProposalPreReview(PreReview newProposalPreReview) {
		this.newProposalPreReview = newProposalPreReview;
	}

	public PreReviewComment getPreNewReviewComment() {
		return preNewReviewComment;
	}

	public void setPreNewReviewComment(PreReviewComment preNewReviewComment) {
		this.preNewReviewComment = preNewReviewComment;
	}

	public List<PreReview> getProposalPreReviews() {
		return proposalPreReviews;
	}

	public void setProposalPreReviews(List<PreReview> proposalPreReviews) {
		this.proposalPreReviews = proposalPreReviews;
	}

	public List<PreReviewSectionType> getPreReviewClarifications() {
		return preReviewClarifications;
	}

	public void setPreReviewClarifications(List<PreReviewSectionType> preReviewClarifications) {
		this.preReviewClarifications = preReviewClarifications;
	}

	public List<PreReviewSectionType> getPreReviewRoutingReview() {
		return preReviewRoutingReview;
	}

	public void setPreReviewRoutingReview(List<PreReviewSectionType> preReviewRoutingReview) {
		this.preReviewRoutingReview = preReviewRoutingReview;
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

	public List<AwardType> getAwardType() {
		return awardType;
	}

	public void setAwardType(List<AwardType> awardType) {
		this.awardType = awardType;
	}

	public Integer getReviewId() {
		return reviewId;
	}

	public void setReviewId(Integer reviewId) {
		this.reviewId = reviewId;
	}

	public ReviewComment getNewReviewComment() {
		return newReviewComment;
	}

	public void setNewReviewComment(ReviewComment newReviewComment) {
		this.newReviewComment = newReviewComment;
	}

	public Integer getReviewCommentId() {
		return reviewCommentId;
	}

	public void setReviewCommentId(Integer reviewCommentId) {
		this.reviewCommentId = reviewCommentId;
	}

	public List<ProposalReview> getProposalReviews() {
		return proposalReviews;
	}

	public void setProposalReviews(List<ProposalReview> proposalReviews) {
		this.proposalReviews = proposalReviews;
	}

	public List<Integer> getProposalIds() {
		return proposalIds;
	}

	public void setProposalIds(List<Integer> proposalIds) {
		this.proposalIds = proposalIds;
	}

	public ProposalReview getNewProposalReview() {
		return newProposalReview;
	}

	public void setNewProposalReview(ProposalReview newProposalReview) {
		this.newProposalReview = newProposalReview;
	}

	public Timestamp getPiReviewDeadLineDate() {
		return piReviewDeadLineDate;
	}

	public void setPiReviewDeadLineDate(Timestamp piReviewDeadLineDate) {
		this.piReviewDeadLineDate = piReviewDeadLineDate;
	}

	public String getHomeUnitNumber() {
		return homeUnitNumber;
	}

	public void setHomeUnitNumber(String homeUnitNumber) {
		this.homeUnitNumber = homeUnitNumber;
	}

	public List<ProposalFundingStatus> getProposalFundingStatus() {
		return proposalFundingStatus;
	}

	public void setProposalFundingStatus(List<ProposalFundingStatus> proposalFundingStatus) {
		this.proposalFundingStatus = proposalFundingStatus;
	}

	public List<DisciplineCluster> getDisciplineClusters() {
		return disciplineClusters;
	}

	public void setDisciplineClusters(List<DisciplineCluster> disciplineClusters) {
		this.disciplineClusters = disciplineClusters;
	}

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public Integer getProposalPersonAssignedRoleId() {
		return proposalPersonAssignedRoleId;
	}

	public void setProposalPersonAssignedRoleId(Integer proposalPersonAssignedRoleId) {
		this.proposalPersonAssignedRoleId = proposalPersonAssignedRoleId;
	}

	public List<ProposalPersonAttachment> getNewPersonAttachments() {
		return newPersonAttachments;
	}

	public void setNewPersonAttachments(List<ProposalPersonAttachment> newPersonAttachments) {
		this.newPersonAttachments = newPersonAttachments;
	}

	public Boolean getIsFundingSupportDeclarationRequired() {
		return isFundingSupportDeclarationRequired;
	}

	public void setIsFundingSupportDeclarationRequired(Boolean isFundingSupportDeclarationRequired) {
		this.isFundingSupportDeclarationRequired = isFundingSupportDeclarationRequired;
	}

	public List<GrantCallType> getGrantCallTypes() {
		return grantCallTypes;
	}

	public void setGrantCallTypes(List<GrantCallType> grantCallTypes) {
		this.grantCallTypes = grantCallTypes;
	}

	public List<ProposalAttachment> getProposalAttachments() {
		return proposalAttachments;
	}

	public void setProposalAttachments(List<ProposalAttachment> proposalAttachments) {
		this.proposalAttachments = proposalAttachments;
	}

	public List<ProposalResearchArea> getProposalResearchAreas() {
		return proposalResearchAreas;
	}

	public void setProposalResearchAreas(List<ProposalResearchArea> proposalResearchAreas) {
		this.proposalResearchAreas = proposalResearchAreas;
	}

	public List<ProposalPerson> getProposalPersons() {
		return proposalPersons;
	}

	public void setProposalPersons(List<ProposalPerson> proposalPersons) {
		this.proposalPersons = proposalPersons;
	}

	public List<ProposalSponsor> getProposalSponsors() {
		return proposalSponsors;
	}

	public void setProposalSponsors(List<ProposalSponsor> proposalSponsors) {
		this.proposalSponsors = proposalSponsors;
	}

	public List<ProposalIrbProtocol> getProposalIrbProtocols() {
		return proposalIrbProtocols;
	}

	public void setProposalIrbProtocols(List<ProposalIrbProtocol> proposalIrbProtocols) {
		this.proposalIrbProtocols = proposalIrbProtocols;
	}

	public List<ProposalSpecialReview> getProposalSpecialReviews() {
		return proposalSpecialReviews;
	}

	public void setProposalSpecialReviews(List<ProposalSpecialReview> proposalSpecialReviews) {
		this.proposalSpecialReviews = proposalSpecialReviews;
	}

	public ProposalAttachment getProposalAttachment() {
		return proposalAttachment;
	}

	public void setProposalAttachment(ProposalAttachment proposalAttachment) {
		this.proposalAttachment = proposalAttachment;
	}

	public ProposalResearchArea getProposalResearchArea() {
		return proposalResearchArea;
	}

	public void setProposalResearchArea(ProposalResearchArea proposalResearchArea) {
		this.proposalResearchArea = proposalResearchArea;
	}

	public ProposalPerson getProposalPerson() {
		return proposalPerson;
	}

	public void setProposalPerson(ProposalPerson proposalPerson) {
		this.proposalPerson = proposalPerson;
	}

	public ProposalSponsor getProposalSponsor() {
		return proposalSponsor;
	}

	public void setProposalSponsor(ProposalSponsor proposalSponsor) {
		this.proposalSponsor = proposalSponsor;
	}

	public ProposalIrbProtocol getProposalIrbProtocol() {
		return proposalIrbProtocol;
	}

	public void setProposalIrbProtocol(ProposalIrbProtocol proposalIrbProtocol) {
		this.proposalIrbProtocol = proposalIrbProtocol;
	}

	public ProposalSpecialReview getProposalSpecialReview() {
		return proposalSpecialReview;
	}

	public void setProposalSpecialReview(ProposalSpecialReview proposalSpecialReview) {
		this.proposalSpecialReview = proposalSpecialReview;
	}

	public ProposalProjectTeam getProposalProjectTeam() {
		return proposalProjectTeam;
	}

	public void setProposalProjectTeam(ProposalProjectTeam proposalProjectTeam) {
		this.proposalProjectTeam = proposalProjectTeam;
	}

	public BudgetHeader getBudgetHeader() {
		return budgetHeader;
	}

	public void setBudgetHeader(BudgetHeader budgetHeader) {
		this.budgetHeader = budgetHeader;
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

	public List<ProposalProjectTeam> getProposalProjectTeams() {
		return proposalProjectTeams;
	}

	public void setProposalProjectTeams(List<ProposalProjectTeam> proposalProjectTeams) {
		this.proposalProjectTeams = proposalProjectTeams;
	}

	public List<ProposalPersonAssignedRoles> getProposalPersonAssignedRoles() {
		return proposalPersonAssignedRoles;
	}

	public void setProposalPersonAssignedRoles(List<ProposalPersonAssignedRoles> proposalPersonAssignedRoles) {
		this.proposalPersonAssignedRoles = proposalPersonAssignedRoles;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public BudgetVO getBudgetVO() {
		return budgetVO;
	}

	public void setBudgetVO(BudgetVO budgetVO) {
		this.budgetVO = budgetVO;
	}

	public String getAttachmentDescription() {
		return attachmentDescription;
	}

	public void setAttachmentDescription(String attachmentDescription) {
		this.attachmentDescription = attachmentDescription;
	}

	public List<Integer> getAttachmentIds() {
		return attachmentIds;
	}

	public void setAttachmentIds(List<Integer> attachmentIds) {
		this.attachmentIds = attachmentIds;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public List<BudgetHeader> getBudgetHeaders() {
		return budgetHeaders;
	}

	public void setBudgetHeaders(List<BudgetHeader> budgetHeaders) {
		this.budgetHeaders = budgetHeaders;
	}

	public Boolean getIsBudgetHeaderFound() {
		return isBudgetHeaderFound;
	}

	public void setIsBudgetHeaderFound(Boolean isBudgetHeaderFound) {
		this.isBudgetHeaderFound = isBudgetHeaderFound;
	}

	public String getCompleteReviewerEmail() {
		return completeReviewerEmail;
	}

	public void setCompleteReviewerEmail(String completeReviewerEmail) {
		this.completeReviewerEmail = completeReviewerEmail;
	}

	public String getCompleteReviewerFullName() {
		return completeReviewerFullName;
	}

	public void setCompleteReviewerFullName(String completeReviewerFullName) {
		this.completeReviewerFullName = completeReviewerFullName;
	}

	public String getCompleteReviewerPersonId() {
		return completeReviewerPersonId;
	}

	public void setCompleteReviewerPersonId(String completeReviewerPersonId) {
		this.completeReviewerPersonId = completeReviewerPersonId;
	}

	public String getReviewerEmail() {
		return reviewerEmail;
	}

	public void setReviewerEmail(String reviewerEmail) {
		this.reviewerEmail = reviewerEmail;
	}

	public String getReviewerFullName() {
		return reviewerFullName;
	}

	public void setReviewerFullName(String reviewerFullName) {
		this.reviewerFullName = reviewerFullName;
	}

	public String getReviewerPersonId() {
		return reviewerPersonId;
	}

	public void setReviewerPersonId(String reviewerPersonId) {
		this.reviewerPersonId = reviewerPersonId;
	}

	public List<EvaluationStop> getEvaluationReviewStop() {
		return evaluationReviewStop;
	}

	public void setEvaluationReviewStop(List<EvaluationStop> evaluationReviewStop) {
		this.evaluationReviewStop = evaluationReviewStop;
	}

	public List<PersonRoles> getPersonRoles() {
		return personRoles;
	}

	public void setPersonRoles(List<PersonRoles> personRoles) {
		this.personRoles = personRoles;
	}

	public Boolean getHasRank() {
		return hasRank;
	}

	public void setHasRank(Boolean hasRank) {
		this.hasRank = hasRank;
	}

	public List<FinalEvaluationStatus> getFinalEvaluationStatus() {
		return finalEvaluationStatus;
	}

	public void setFinalEvaluationStatus(List<FinalEvaluationStatus> finalEvaluationStatus) {
		this.finalEvaluationStatus = finalEvaluationStatus;
	}

	public List<EvaluationRecommendation> getEvaluationRecommendation() {
		return evaluationRecommendation;
	}

	public void setEvaluationRecommendation(List<EvaluationRecommendation> evaluationRecommendation) {
		this.evaluationRecommendation = evaluationRecommendation;
	}

	public Boolean getIsRcbf() {
		return isRcbf;
	}

	public void setIsRcbf(Boolean isRcbf) {
		this.isRcbf = isRcbf;
	}

	public String getPiPersonId() {
		return piPersonId;
	}

	public void setPiPersonId(String piPersonId) {
		this.piPersonId = piPersonId;
	}

	public Boolean getHasRecommendation() {
		return hasRecommendation;
	}

	public void setHasRecommendation(Boolean hasRecommendation) {
		this.hasRecommendation = hasRecommendation;
	}

	public Boolean getShowOtherInformation() {
		return showOtherInformation;
	}

	public void setShowOtherInformation(Boolean showOtherInformation) {
		this.showOtherInformation = showOtherInformation;
	}

	public Integer getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(Integer reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public Integer getNumberOfApplications() {
		return numberOfApplications;
	}

	public void setNumberOfApplications(Integer numberOfApplications) {
		this.numberOfApplications = numberOfApplications;
	}

	public String getReviewerRole() {
		return reviewerRole;
	}

	public void setReviewerRole(String reviewerRole) {
		this.reviewerRole = reviewerRole;
	}

	public Timestamp getReviewDeadLineDate() {
		return reviewDeadLineDate;
	}

	public void setReviewDeadLineDate(Timestamp reviewDeadLineDate) {
		this.reviewDeadLineDate = reviewDeadLineDate;
	}

	public String getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(String proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public String getNarrativeStatusCode() {
		return narrativeStatusCode;
	}

	public void setNarrativeStatusCode(String narrativeStatusCode) {
		this.narrativeStatusCode = narrativeStatusCode;
	}

	public ProposalMileStone getProposalMileStone() {
		return proposalMileStone;
	}

	public void setProposalMileStone(ProposalMileStone proposalMileStone) {
		this.proposalMileStone = proposalMileStone;
	}

	public List<ProposalMileStone> getProposalMileStones() {
		return proposalMileStones;
	}

	public void setProposalMileStones(List<ProposalMileStone> proposalMileStones) {
		this.proposalMileStones = proposalMileStones;
	}

	public Integer getProposalMileStoneId() {
		return proposalMileStoneId;
	}

	public void setProposalMileStoneId(Integer proposalMileStoneId) {
		this.proposalMileStoneId = proposalMileStoneId;
	}

	public Timestamp getReviewDeadLine() {
		return reviewDeadLine;
	}

	public void setReviewDeadLine(Timestamp reviewDeadLine) {
		this.reviewDeadLine = reviewDeadLine;
	}

	public ProposalKPI getProposalKpi() {
		return proposalKpi;
	}

	public void setProposalKpi(ProposalKPI proposalKpi) {
		this.proposalKpi = proposalKpi;
	}

	public List<ProposalKPI> getProposalKpis() {
		return proposalKpis;
	}

	public void setProposalKpis(List<ProposalKPI> proposalKpis) {
		this.proposalKpis = proposalKpis;
	}

	public List<KPIType> getKpiTypes() {
		return kpiTypes;
	}

	public void setKpiTypes(List<KPIType> kpiTypes) {
		this.kpiTypes = kpiTypes;
	}

	public ProposalExtension getProposalExtension() {
		return proposalExtension;
	}

	public void setProposalExtension(ProposalExtension proposalExtension) {
		this.proposalExtension = proposalExtension;
	}

	public String getUserRole() {
		return userRole;
	}

	public void setUserRole(String userRole) {
		this.userRole = userRole;
	}

	public List<ProposalPersonRoles> getProposalRoles() {
		return proposalRoles;
	}

	public void setProposalRoles(List<ProposalPersonRoles> proposalRoles) {
		this.proposalRoles = proposalRoles;
	}

	public List<String> getIpNumbers() {
		return ipNumbers;
	}

	public void setIpNumbers(List<String> ipNumbers) {
		this.ipNumbers = ipNumbers;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public Integer getWorkflowDetailId() {
		return workflowDetailId;
	}

	public void setWorkflowDetailId(Integer workflowDetailId) {
		this.workflowDetailId = workflowDetailId;
	}

	public Integer getWorkflowReviewerScoreId() {
		return workflowReviewerScoreId;
	}

	public void setWorkflowReviewerScoreId(Integer workflowReviewerScoreId) {
		this.workflowReviewerScoreId = workflowReviewerScoreId;
	}

	public WorkflowReviewerScore getWorkflowReviewerScore() {
		return workflowReviewerScore;
	}

	public void setWorkflowReviewerScore(WorkflowReviewerScore workflowReviewerScore) {
		this.workflowReviewerScore = workflowReviewerScore;
	}

	public List<WorkflowReviewerScore> getWorkflowReviewerScores() {
		return workflowReviewerScores;
	}

	public void setWorkflowReviewerScores(List<WorkflowReviewerScore> workflowReviewerScores) {
		this.workflowReviewerScores = workflowReviewerScores;
	}

	public List<WorkflowReviewerAttachment> getWorkflowReviewerAttachments() {
		return workflowReviewerAttachments;
	}

	public void setWorkflowReviewerAttachments(List<WorkflowReviewerAttachment> workflowReviewerAttachments) {
		this.workflowReviewerAttachments = workflowReviewerAttachments;
	}

	public Integer getWorkflowReviewerAttmntsId() {
		return workflowReviewerAttmntsId;
	}

	public void setWorkflowReviewerAttmntsId(Integer workflowReviewerAttmntsId) {
		this.workflowReviewerAttmntsId = workflowReviewerAttmntsId;
	}

	public Boolean getIsPersonCanScore() {
		return isPersonCanScore;
	}

	public void setIsPersonCanScore(Boolean isPersonCanScore) {
		this.isPersonCanScore = isPersonCanScore;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public List<ProposalEvaluationPanel> getProposalEvaluationPanelsList() {
		return proposalEvaluationPanelsList;
	}

	public void setProposalEvaluationPanelsList(List<ProposalEvaluationPanel> proposalEvaluationPanelsList) {
		this.proposalEvaluationPanelsList = proposalEvaluationPanelsList;
	}

	public List<GrantCallEvaluationPanel> getGrantCallEvaluationPanelsList() {
		return grantCallEvaluationPanelsList;
	}

	public void setGrantCallEvaluationPanelsList(List<GrantCallEvaluationPanel> grantCallEvaluationPanelsList) {
		this.grantCallEvaluationPanelsList = grantCallEvaluationPanelsList;
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

	public List<WorkflowReviewerComment> getWorkflowReviewerComments() {
		return workflowReviewerComments;
	}

	public void setWorkflowReviewerComments(List<WorkflowReviewerComment> workflowReviewerComments) {
		this.workflowReviewerComments = workflowReviewerComments;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public ProposalPersonAttachment getProposalPersonAttachment() {
		return proposalPersonAttachment;
	}

	public void setProposalPersonAttachment(ProposalPersonAttachment proposalPersonAttachment) {
		this.proposalPersonAttachment = proposalPersonAttachment;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public Boolean getIsLastUploadedFile() {
		return isLastUploadedFile;
	}

	public void setIsLastUploadedFile(Boolean isLastUploadedFile) {
		this.isLastUploadedFile = isLastUploadedFile;
	}

	public List<Currency> getCurrencyDetails() {
		return currencyDetails;
	}

	public void setCurrencyDetails(List<Currency> currencyDetails) {
		this.currencyDetails = currencyDetails;
	}

	public ProposalComment getComment() {
		return comment;
	}

	public void setProposalComment(ProposalComment comment) {
		this.comment = comment;
	}

	public List<ProposalComment> getProposalComments() {
		return proposalComments;
	}

	public void setProposalComments(List<ProposalComment> proposalComments) {
		this.proposalComments = proposalComments;
	}

	public String getCommentTypeCode() {
		return commentTypeCode;
	}

	public void setCommentTypeCode(String commentTypeCode) {
		this.commentTypeCode = commentTypeCode;
	}

	public String getIsPublic() {
		return isPublic;
	}

	public void setIsPublic(String isPublic) {
		this.isPublic = isPublic;
	}

	public List<CommentType> getCommentType() {
		return commentType;
	}

	public void setCommentType(List<CommentType> commentType) {
		this.commentType = commentType;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public boolean getIsSuperUser() {
		return isSuperUser;
	}

	public void setIsSuperUser(boolean isSuperUser) {
		this.isSuperUser = isSuperUser;
	}

	public boolean getIsNonEmployee() {
		return isNonEmployee;
	}

	public void setIsNonEmployee(boolean isNonEmployee) {
		this.isNonEmployee = isNonEmployee;
	}

	public List<SponsorFundingScheme> getSponsorFundingSchemes() {
		return sponsorFundingSchemes;
	}

	public void setSponsorFundingSchemes(List<SponsorFundingScheme> sponsorFundingSchemes) {
		this.sponsorFundingSchemes = sponsorFundingSchemes;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public Integer getMapNumber() {
		return mapNumber;
	}

	public void setMapNumber(Integer mapNumber) {
		this.mapNumber = mapNumber;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public String getPreviousProposalPersonId() {
		return previousProposalPersonId;
	}

	public void setPreviousProposalPersonId(String previousProposalPersonId) {
		this.previousProposalPersonId = previousProposalPersonId;
	}

	public Boolean getPreviousNonEmployeeFlag() {
		return previousNonEmployeeFlag;
	}

	public void setPreviousNonEmployeeFlag(Boolean previousNonEmployeeFlag) {
		this.previousNonEmployeeFlag = previousNonEmployeeFlag;
	}

	public Boolean getIsCalculationWithPredefinedSalary() {
		return isCalculationWithPredefinedSalary;
	}

	public void setIsCalculationWithPredefinedSalary(Boolean isCalculationWithPredefinedSalary) {
		this.isCalculationWithPredefinedSalary = isCalculationWithPredefinedSalary;
	}

	public Boolean getIsPINameAutoFilledRequired() {
		return isPINameAutoFilledRequired;
	}

	public void setIsPINameAutoFilledRequired(Boolean isPINameAutoFilledRequired) {
		this.isPINameAutoFilledRequired = isPINameAutoFilledRequired;
	}

	public String getLoginPersonUnitNumber() {
		return loginPersonUnitNumber;
	}

	public void setLoginPersonUnitNumber(String loginPersonUnitNumber) {
		this.loginPersonUnitNumber = loginPersonUnitNumber;
	}

	public Integer getWorkflowReviewerCommentsId() {
		return workflowReviewerCommentsId;
	}

	public void setWorkflowReviewerCommentsId(Integer workflowReviewerCommentsId) {
		this.workflowReviewerCommentsId = workflowReviewerCommentsId;
	}

	public Boolean getIsReplaceAttachmentEnabled() {
		return isReplaceAttachmentEnabled;
	}

	public void setIsReplaceAttachmentEnabled(Boolean isReplaceAttachmentEnabled) {
		this.isReplaceAttachmentEnabled = isReplaceAttachmentEnabled;
	}

	public String getIsBudgetSummaryPrint() {
		return isBudgetSummaryPrint;
	}

	public void setIsBudgetSummaryPrint(String isBudgetSummaryPrint) {
		this.isBudgetSummaryPrint = isBudgetSummaryPrint;
	}

	public String getIsSimpleBudgetPrint() {
		return isSimpleBudgetPrint;
	}

	public void setIsSimpleBudgetPrint(String isSimpleBudgetPrint) {
		this.isSimpleBudgetPrint = isSimpleBudgetPrint;
	}

	public String getIsDetailedBudgetPrint() {
		return isDetailedBudgetPrint;
	}

	public void setIsDetailedBudgetPrint(String isDetailedBudgetPrint) {
		this.isDetailedBudgetPrint = isDetailedBudgetPrint;
	}

	public String getIsPersonnelBudgetPrint() {
		return isPersonnelBudgetPrint;
	}

	public void setIsPersonnelBudgetPrint(String isPersonnelBudgetPrint) {
		this.isPersonnelBudgetPrint = isPersonnelBudgetPrint;
	}

	public List<ProposalOrganization> getProposalOrganizations() {
		return proposalOrganizations;
	}

	public void setProposalOrganizations(List<ProposalOrganization> proposalOrganizations) {
		this.proposalOrganizations = proposalOrganizations;
	}

	public ProposalOrganization getProposalOrganization() {
		return proposalOrganization;
	}

	public void setProposalOrganization(ProposalOrganization proposalOrganization) {
		this.proposalOrganization = proposalOrganization;
	}

	public List<OrganizationType> getOrganizationType() {
		return organizationType;
	}

	public void setOrganizationType(List<OrganizationType> organizationType) {
		this.organizationType = organizationType;
	}

	public List<CongressionalDistrict> getCongressionalDistricts() {
		return congressionalDistricts;
	}

	public void setCongressionalDistricts(List<CongressionalDistrict> congressionalDistricts) {
		this.congressionalDistricts = congressionalDistricts;
	}

	public Organization getOrganization() {
		return organization;
	}

	public void setOrganization(Organization organization) {
		this.organization = organization;
	}

	public CongressionalDistrict getCongressionalDistrict() {
		return congressionalDistrict;
	}

	public void setCongressionalDistrict(CongressionalDistrict congressionalDistrict) {
		this.congressionalDistrict = congressionalDistrict;
	}

	public void setSuperUser(boolean isSuperUser) {
		this.isSuperUser = isSuperUser;
	}

	public void setNonEmployee(boolean isNonEmployee) {
		this.isNonEmployee = isNonEmployee;
	}

	public List<ResearchType> getResearchTypes() {
		return researchTypes;
	}

	public void setResearchTypes(List<ResearchType> researchTypes) {
		this.researchTypes = researchTypes;
	}

	public Boolean getIsRightExist() {
		return isRightExist;
	}

	public String getRightName() {
		return rightName;
	}

	public void setRightName(String rightName) {
		this.rightName = rightName;
	}

	public Boolean setIsRightExist(Boolean isRightExist) {
		return this.isRightExist = isRightExist;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Boolean getIsAutoCalculateEnabled() {
		return isAutoCalculateEnabled;
	}

	public void setIsAutoCalculateEnabled(Boolean isAutoCalculateEnabled) {
		this.isAutoCalculateEnabled = isAutoCalculateEnabled;
	}

	public String getRcbfFundingStatusCode() {
		return rcbfFundingStatusCode;
	}

	public void setRcbfFundingStatusCode(String rcbfFundingStatusCode) {
		this.rcbfFundingStatusCode = rcbfFundingStatusCode;
	}

	public String getRcbfTypeCode() {
		return rcbfTypeCode;
	}

	public void setRcbfTypeCode(String rcbfTypeCode) {
		this.rcbfTypeCode = rcbfTypeCode;
	}

	public Boolean getIsProposalArchiveCreation() {
		return isProposalArchiveCreation;
	}

	public void setIsProposalArchiveCreation(Boolean isProposalArchiveCreation) {
		this.isProposalArchiveCreation = isProposalArchiveCreation;
	}

	public Boolean getIsProposalAdminCorrection() {
		return isProposalAdminCorrection;
	}

	public void setIsProposalAdminCorrection(Boolean isProposalAdminCorrection) {
		this.isProposalAdminCorrection = isProposalAdminCorrection;
	}

	public List<ProposalHistory> getProposalHistory() {
		return proposalHistory;
	}

	public void setProposalHistory(List<ProposalHistory> proposalHistory) {
		this.proposalHistory = proposalHistory;
	}

	public Boolean getIsProposalComparison() {
		return isProposalComparison;
	}

	public void setIsProposalComparison(Boolean isProposalComparison) {
		this.isProposalComparison = isProposalComparison;
	}

	public Boolean getCopyAllBudgetVersion() {
		return copyAllBudgetVersion;
	}

	public void setCopyAllBudgetVersion(Boolean copyAllBudgetVersion) {
		this.copyAllBudgetVersion = copyAllBudgetVersion;
	}

	public Boolean getCopyFinalBudgetVersion() {
		return copyFinalBudgetVersion;
	}

	public void setCopyFinalBudgetVersion(Boolean copyFinalBudgetVersion) {
		this.copyFinalBudgetVersion = copyFinalBudgetVersion;
	}

	public Boolean getCopyQuestionnaire() {
		return copyQuestionnaire;
	}

	public void setCopyQuestionnaire(Boolean copyQuestionnaire) {
		this.copyQuestionnaire = copyQuestionnaire;
	}

	public Boolean getCopyOtherInformation() {
		return copyOtherInformation;
	}

	public void setCopyOtherInformation(Boolean copyOtherInformation) {
		this.copyOtherInformation = copyOtherInformation;
	}

	public Boolean getCopyAttachment() {
		return copyAttachment;
	}

	public void setCopyAttachment(Boolean copyAttachment) {
		this.copyAttachment = copyAttachment;
	}

	public Boolean getIsBudgetVersionEnabled() {
		return isBudgetVersionEnabled;
	}

	public void setIsBudgetVersionEnabled(Boolean isBudgetVersionEnabled) {
		this.isBudgetVersionEnabled = isBudgetVersionEnabled;
	}

	public Boolean getIpGenerationOnly() {
		return ipGenerationOnly;
	}

	public void setIpGenerationOnly(Boolean ipGenerationOnly) {
		this.ipGenerationOnly = ipGenerationOnly;
	}

	public Map<String, Object> getGrantEligibilityStatus() {
		return grantEligibilityStatus;
	}

	public void setGrantEligibilityStatus(Map<String, Object> grantEligibilityStatus) {
		this.grantEligibilityStatus = grantEligibilityStatus;
	}
	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public List<ProposalPersonAttachment> getProposalPersonAttachments() {
		return proposalPersonAttachments;
	}

	public void setProposalPersonAttachments(List<ProposalPersonAttachment> proposalPersonAttachments) {
		this.proposalPersonAttachments = proposalPersonAttachments;
	}

	public Boolean getEnableOrganizationLocation() {
		return enableOrganizationLocation;
	}

	public void setEnableOrganizationLocation(Boolean enableOrganizationLocation) {
		this.enableOrganizationLocation = enableOrganizationLocation;
	}

	public Boolean getCanDeleteProposal() {
		return canDeleteProposal;
	}

	public void setCanDeleteProposal(Boolean canDeleteProposal) {
		this.canDeleteProposal = canDeleteProposal;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Boolean getEnableActivityGrantCallMapping() {
		return enableActivityGrantCallMapping;
	}

	public void setEnableActivityGrantCallMapping(Boolean enableActivityGrantCallMapping) {
		this.enableActivityGrantCallMapping = enableActivityGrantCallMapping;
	}

	public List<ProposalCommentAttachment> getNewCommentAttachments() {
		return newCommentAttachments;
	}

	public void setNewCommentAttachments(List<ProposalCommentAttachment> newCommentAttachments) {
		this.newCommentAttachments = newCommentAttachments;
	}

	public Boolean getEnableClosedGrantCallLinkingInProposal() {
		return enableClosedGrantCallLinkingInProposal;
	}

	public void setEnableClosedGrantCallLinkingInProposal(Boolean enableClosedGrantCallLinkingInProposal) {
		this.enableClosedGrantCallLinkingInProposal = enableClosedGrantCallLinkingInProposal;
	}
	
	public Integer getNoOfDays() {
		return noOfDays;
	}

	public void setNoOfDays(Integer noOfDays) {
		this.noOfDays = noOfDays;
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

	public ProposalPersonDegree getProposalPersonDegree() {
		return proposalPersonDegree;
	}

	public void setProposalPersonDegree(ProposalPersonDegree proposalPersonDegree) {
		this.proposalPersonDegree = proposalPersonDegree;
	}

	public Integer getProposalPersonDegreeId() {
		return proposalPersonDegreeId;
	}

	public void setProposalPersonDegreeId(Integer proposalPersonDegreeId) {
		this.proposalPersonDegreeId = proposalPersonDegreeId;
	}


	public Boolean getPersonCertified() {
		return personCertified;
	}


	public void setPersonCertified(Boolean personCertified) {
		this.personCertified = personCertified;
	}


	public Boolean getProposalEditable() {
		return proposalEditable;
	}


	public void setProposalEditable(Boolean proposalEditable) {
		this.proposalEditable = proposalEditable;
	}

}
