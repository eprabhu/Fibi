package com.polus.fibicomp.award.vo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationDetail;
import com.polus.fibicomp.award.pojo.AwardAssociationType;
import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonAttachment;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.award.pojo.AwardReportTrackingFile;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardReviewComment;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.pojo.CostShareType;
import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.compilance.pojo.AcProtocolStatus;
import com.polus.fibicomp.compilance.pojo.IrbProtocolStatus;
import com.polus.fibicomp.grantcall.pojo.KPIType;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.scopusintegration.pojo.AwardScopus;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.vo.BaseVO;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

public class AwardVO extends BaseVO {

	private List<AwardFundingProposal> awardFundingProposals;

	private Award award;

	private List<AwardPerson> awardPersons;

	private List<AwardProjectTeam> awardProjectTeams;

	private List<AwardContact> awardContacts;

	private List<AwardSpecialReview> awardSpecialReviews;

	private List<AwardSubContract> awardSubContracts;

	private List<AwardCostShare> awardCostShares;

	private Integer awardCostShareId;

	private Integer subContractId;

	private Integer awardSpecailReviewId;

	private Integer awardKeywordId;

	private String updateType;

	private String message;

	private String personId;

	private Integer awardId;

	private String awardSequenceStatus;

	private String budgetCategoryCode;

	private List<CostElement> costElements;

	private Integer budgetPeriodId;

	private List<CostElement> sysGeneratedCostElements;

	private Integer budgetDetailId;

	private Set<String> rateClassTypes;

	private List<BudgetCategory> budgetCategories;

	private List<TbnPerson> tbnPersons;

	private boolean status;

	private Integer copyPeriodId;

	private Integer currentPeriodId;

	private String userName;

	private Integer budgetPersonDetailId;

	private List<WorkFlowResultDto> validationError;

	private String leadUnitNumber;

	private String actionType;

	private String updateUser;

	private String approveComment;

	private String isFinalApprover;

	private List<Workflow> workflowList;

	private String canApproveRouting;

	private Workflow workflow;

	private Integer workflowDetailId;

	private WorkflowDetail workflowDetail;

	private Boolean finalApprover;

	private Boolean isApproved;

	private Boolean isApprover;

	private String isSubmit;

	private List<AwardPersonAttachment> newPersonAttachments;

	private String userFullName;

	private Boolean isSuperUser = false;

	private Integer approverStopNumber;

	private Integer workFlowId;

	private Integer mapId;

	private Integer mapNumber;

	private Integer approvalStopNumber;

	private Integer approverNumber;

	private String approverPersonId;

	private String approvalStatus;

	private String awardNumber;

	private String subject;

	private String description;

	private List<AwardAttachment> awardAttachments;

	private ServiceRequestType serviceRequestType;

	private String serviceRequestTypeCode;

	private List<ServiceRequestAttachment> newAttachments;

	private String workFlowPersonId;

	private ServiceRequest serviceRequest;

	private Boolean isVariationRequest = false;

	private Boolean isAwardModification = false;

	private Boolean isProjectClosure = false;

	private Boolean isAwardOutcome = false;

	private List<String> availableRights;

	private List<AwardPublications> awardPublications;

	private AwardPublications awardPublication;

	private List<AwardAssociation> awardAssociations;

	private AwardAssociation awardAssociation;

	private List<AwardAcheivements> awardAcheivements;

	private AwardAcheivements awardAcheivement;

	private List<AwardAssociationType> associationTypes;

	private List<ModuleVariableSection> sectionTypeCodes;

	private boolean isPrimaryApprover = false;

	private String approverFlag;

	private List<Award> awards;

	private Integer moduleCode;

	private Integer moduleItemKey;

	private Proposal proposal;

	private List<AwardReportTrackingFile> awardReportTrackingFiles;

	private Integer awardReportTrackingId;

	private Integer awardReportTrackingFileId;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private AwardAttachment awardAttachment;

	private Boolean isAwardHierarchy = Boolean.FALSE;

	private List<AwardAmountInfo> awardAmountInfos;

	private List<CostShareType> costShareTypes;

	private boolean isCancelRequest = false;

	private String awardStatusCode;

	private String fileTimestamp;

	private Integer taskCount;

	private String contentType;

	private String fileName;

	private AwardReportTrackingFile awardReportTrackingFile;

	private ServiceRequestAttachment serviceRequestAttachment;

	private Integer serviceRequestId;

	private List<Integer> attachmentIds;

	private Boolean isBudgetCreated;

	private Boolean isNonEmployee = false;

	private Boolean isCopyAward = false;

	private String createUser;

	private AwardAssociationDetail awardAssociationDetail;

	private Integer awardAssociationDetailId;

	private AwardResearchArea awardResearchArea;

	private List<AwardResearchArea> awardResearchAreas;

	private Integer researchAreaId;

	private String researchDescription;

	private String multiDisciplinaryDescription;

	private Integer subModuleCode;

	private Integer subModuleItemKey;

	private Task task;

	private Integer questionnaireId;

	private Integer questionnaireAnswerHeaderId;

	private Boolean isGenerateWBSNumber = false;

	private String accountNumber;

	private String stopName;

	private List<AwardKPI> awardKpis;

	private List<KPIType> kpiTypes;

	private List<AwardMileStone> awardMileStones;

	private Boolean isFeededAwardId = false;

	private Timestamp previousExpirationDate;

	private String userRole;

	private Integer previousActiveAwardId;

	private String commentTypeCode;

	private Boolean isLastUploadedFile;

	private Integer pendingAwardId;

	private Timestamp updateTimestamp;

	private String awardPublicationId;

	private String awardAssociationId;

	private String awardAcheivementId;

	private List<AwardReviewComment> awardReviewComments;

	private Claim claim;

	private Integer originalAwardId;

	private Boolean isMasterAwardCreation = false;

	private AwardSummaryVO pendingAwardsSummary;

	private Boolean canCreateVariationRequest = true;

	private String budgetStatusCode;

	private Integer awardSequenceNumber;

	private Award activeAward;

	private Timestamp awardEffectiveDate;

	private Timestamp finalExpirationDate;

	private Integer awardPersonId;

	private String awardKeyPersonTimesheetType;

	private Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails;

	private Set<String> editableSectionCodes;

	private List<AwardKeyPersonTimesheet> awardKeyPersonTimesheet;

	private AwardProgressReport awardProgressReport;

	private Integer latestAwardId;

	private Boolean isFirstTimeCreation = false;

	private Boolean isLastRequest = false;

    private Timestamp funderApprovalDate;
    
    private Boolean isGrantCallChanged = Boolean.FALSE;
    
    private Boolean isReportTermsExist = Boolean.FALSE;

    private Boolean isManpowerIntegrationRequired = false;

    private Boolean copyQuestionnaire = Boolean.FALSE;

    private Boolean copyOtherInformation = Boolean.FALSE;

	private List<String> publicationTypes;

	private List<Publication> publications;

    private Boolean canEnableMilestoneStatus;
    
    private AgreementHeader agreementHeader;

    private List<AcProtocolStatus> acProtocolStatuses;

	private List<IrbProtocolStatus> irbProtocolStatuses;
	
	private Boolean isPersonCanScore;

	private Boolean isAwardHistoryTab;

	private String mapName;

	private String mapDescription;

	private List<AwardScopus> awardScopuses;

	public String getMapDescription() {
		return mapDescription;
	}

	public void setMapDescription(String mapDescription) {
		this.mapDescription = mapDescription;
	}

	public String getMapName() {
		return mapName;
	}

	public void setMapName(String mapName) {
		this.mapName = mapName;
	}

	public AgreementHeader getAgreementHeader() {
		return agreementHeader;
	}

	public void setAgreementHeader(AgreementHeader agreementHeader) {
		this.agreementHeader = agreementHeader;
	}

	public AwardVO() {
		award = new Award();
		awardContacts = new ArrayList<>();
		awardCostShares = new ArrayList<>();
		awardFundingProposals = new ArrayList<>();
		awardProjectTeams = new ArrayList<>();
		awardPersons = new ArrayList<>();
		awardSpecialReviews = new ArrayList<>();
		awardSubContracts = new ArrayList<>();
		sectionTypeCodes = new ArrayList<>();
		awardAmountInfos = new ArrayList<>();
		attachmentIds = new ArrayList<>();
		awardResearchAreas = new ArrayList<>();
		awardKpis = new ArrayList<>();
		awardMileStones = new ArrayList<>();
		awardReviewComments = new ArrayList<>();
		editableSectionCodes = new HashSet<>();
		awardScopuses = new ArrayList<>();
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public String getUpdateType() {
		return updateType;
	}

	public void setUpdateType(String updateType) {
		this.updateType = updateType;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Integer getAwardCostShareId() {
		return awardCostShareId;
	}

	public void setAwardCostShareId(Integer awardCostShareId) {
		this.awardCostShareId = awardCostShareId;
	}

	public Integer getAwardSpecailReviewId() {
		return awardSpecailReviewId;
	}

	public void setAwardSpecailReviewId(Integer awardSpecailReviewId) {
		this.awardSpecailReviewId = awardSpecailReviewId;
	}

	public Integer getSubContractId() {
		return subContractId;
	}

	public void setSubContractId(Integer subContractId) {
		this.subContractId = subContractId;
	}

	public Integer getAwardKeywordId() {
		return awardKeywordId;
	}

	public void setAwardKeywordId(Integer awardKeywordId) {
		this.awardKeywordId = awardKeywordId;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public List<CostElement> getCostElements() {
		return costElements;
	}

	public void setCostElements(List<CostElement> costElements) {
		this.costElements = costElements;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public List<CostElement> getSysGeneratedCostElements() {
		return sysGeneratedCostElements;
	}

	public void setSysGeneratedCostElements(List<CostElement> sysGeneratedCostElements) {
		this.sysGeneratedCostElements = sysGeneratedCostElements;
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

	public List<TbnPerson> getTbnPersons() {
		return tbnPersons;
	}

	public void setTbnPersons(List<TbnPerson> tbnPersons) {
		this.tbnPersons = tbnPersons;
	}

	public boolean isStatus() {
		return status;
	}

	public void setStatus(boolean status) {
		this.status = status;
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

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public List<WorkFlowResultDto> getValidationError() {
		return validationError;
	}

	public void setValidationError(List<WorkFlowResultDto> validationError) {
		this.validationError = validationError;
	}

	public List<AwardPerson> getAwardPersons() {
		return awardPersons;
	}

	public void setAwardPersons(List<AwardPerson> awardPersons) {
		this.awardPersons = awardPersons;
	}

	public List<AwardProjectTeam> getAwardProjectTeams() {
		return awardProjectTeams;
	}

	public void setAwardProjectTeams(List<AwardProjectTeam> awardProjectTeams) {
		this.awardProjectTeams = awardProjectTeams;
	}

	public List<AwardContact> getAwardContacts() {
		return awardContacts;
	}

	public void setAwardContacts(List<AwardContact> awardContacts) {
		this.awardContacts = awardContacts;
	}

	public List<AwardSpecialReview> getAwardSpecialReviews() {
		return awardSpecialReviews;
	}

	public void setAwardSpecialReviews(List<AwardSpecialReview> awardSpecialReviews) {
		this.awardSpecialReviews = awardSpecialReviews;
	}

	public List<AwardSubContract> getAwardSubContracts() {
		return awardSubContracts;
	}

	public void setAwardSubContracts(List<AwardSubContract> awardSubContracts) {
		this.awardSubContracts = awardSubContracts;
	}

	public List<AwardCostShare> getAwardCostShares() {
		return awardCostShares;
	}

	public void setAwardCostShares(List<AwardCostShare> awardCostShares) {
		this.awardCostShares = awardCostShares;
	}

	public List<AwardFundingProposal> getAwardFundingProposals() {
		return awardFundingProposals;
	}

	public void setAwardFundingProposals(List<AwardFundingProposal> awardFundingProposals) {
		this.awardFundingProposals = awardFundingProposals;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getApproveComment() {
		return approveComment;
	}

	public void setApproveComment(String approveComment) {
		this.approveComment = approveComment;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public Integer getWorkflowDetailId() {
		return workflowDetailId;
	}

	public void setWorkflowDetailId(Integer workflowDetailId) {
		this.workflowDetailId = workflowDetailId;
	}

	public WorkflowDetail getWorkflowDetail() {
		return workflowDetail;
	}

	public void setWorkflowDetail(WorkflowDetail workflowDetail) {
		this.workflowDetail = workflowDetail;
	}

	public Boolean getFinalApprover() {
		return finalApprover;
	}

	public void setFinalApprover(Boolean finalApprover) {
		this.finalApprover = finalApprover;
	}

	public Boolean getIsApproved() {
		return isApproved;
	}

	public void setIsApproved(Boolean isApproved) {
		this.isApproved = isApproved;
	}

	public Boolean getIsApprover() {
		return isApprover;
	}

	public void setIsApprover(Boolean isApprover) {
		this.isApprover = isApprover;
	}

	public String getIsSubmit() {
		return isSubmit;
	}

	public void setIsSubmit(String isSubmit) {
		this.isSubmit = isSubmit;
	}

	public List<AwardPersonAttachment> getNewPersonAttachments() {
		return newPersonAttachments;
	}

	public void setNewPersonAttachments(List<AwardPersonAttachment> newPersonAttachments) {
		this.newPersonAttachments = newPersonAttachments;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}

	public Boolean getIsSuperUser() {
		return isSuperUser;
	}

	public void setIsSuperUser(Boolean isSuperUser) {
		this.isSuperUser = isSuperUser;
	}

	public Integer getWorkFlowId() {
		return workFlowId;
	}

	public void setWorkFlowId(Integer workFlowId) {
		this.workFlowId = workFlowId;
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

	public Integer getApprovalStopNumber() {
		return approvalStopNumber;
	}

	public void setApprovalStopNumber(Integer approvalStopNumber) {
		this.approvalStopNumber = approvalStopNumber;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public String getApproverPersonId() {
		return approverPersonId;
	}

	public void setApproverPersonId(String approverPersonId) {
		this.approverPersonId = approverPersonId;
	}

	public String getApprovalStatus() {
		return approvalStatus;
	}

	public void setApprovalStatus(String approvalStatus) {
		this.approvalStatus = approvalStatus;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public List<AwardAttachment> getAwardAttachments() {
		return awardAttachments;
	}

	public void setAwardAttachments(List<AwardAttachment> awardAttachments) {
		this.awardAttachments = awardAttachments;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public List<ServiceRequestAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<ServiceRequestAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public ServiceRequest getServiceRequest() {
		return serviceRequest;
	}

	public void setServiceRequest(ServiceRequest serviceRequest) {
		this.serviceRequest = serviceRequest;
	}

	public Boolean getIsVariationRequest() {
		return isVariationRequest;
	}

	public void setIsVariationRequest(Boolean isVariationRequest) {
		this.isVariationRequest = isVariationRequest;
	}

	public Boolean getIsAwardModification() {
		return isAwardModification;
	}

	public void setIsAwardModification(Boolean isAwardModification) {
		this.isAwardModification = isAwardModification;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public Boolean getIsProjectClosure() {
		return isProjectClosure;
	}

	public void setIsProjectClosure(Boolean isProjectClosure) {
		this.isProjectClosure = isProjectClosure;
	}

	public List<AwardPublications> getAwardPublications() {
		return awardPublications;
	}

	public void setAwardPublications(List<AwardPublications> awardPublications) {
		this.awardPublications = awardPublications;
	}

	public AwardPublications getAwardPublication() {
		return awardPublication;
	}

	public void setAwardPublication(AwardPublications awardPublication) {
		this.awardPublication = awardPublication;
	}

	public List<AwardAssociation> getAwardAssociations() {
		return awardAssociations;
	}

	public void setAwardAssociations(List<AwardAssociation> awardAssociations) {
		this.awardAssociations = awardAssociations;
	}

	public AwardAssociation getAwardAssociation() {
		return awardAssociation;
	}

	public void setAwardAssociation(AwardAssociation awardAssociation) {
		this.awardAssociation = awardAssociation;
	}

	public List<AwardAcheivements> getAwardAcheivements() {
		return awardAcheivements;
	}

	public void setAwardAcheivements(List<AwardAcheivements> awardAcheivements) {
		this.awardAcheivements = awardAcheivements;
	}

	public AwardAcheivements getAwardAcheivement() {
		return awardAcheivement;
	}

	public void setAwardAcheivement(AwardAcheivements awardAcheivement) {
		this.awardAcheivement = awardAcheivement;
	}

	public List<AwardAssociationType> getAssociationTypes() {
		return associationTypes;
	}

	public void setAssociationTypes(List<AwardAssociationType> associationTypes) {
		this.associationTypes = associationTypes;
	}

	public boolean isPrimaryApprover() {
		return isPrimaryApprover;
	}

	public void setPrimaryApprover(boolean isPrimaryApprover) {
		this.isPrimaryApprover = isPrimaryApprover;
	}

	public String getApproverFlag() {
		return approverFlag;
	}

	public void setApproverFlag(String approverFlag) {
		this.approverFlag = approverFlag;
	}

	public List<Award> getAwards() {
		return awards;
	}

	public void setAwards(List<Award> awards) {
		this.awards = awards;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	/**
	 * @return the moduleItemKey
	 */
	public Integer getModuleItemKey() {
		return moduleItemKey;
	}

	/**
	 * @param moduleItemKey the moduleItemKey to set
	 */
	public void setModuleItemKey(Integer moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	/**
	 * @return the proposal
	 */
	public Proposal getProposal() {
		return proposal;
	}

	/**
	 * @param proposal the proposal to set
	 */
	public void setProposal(Proposal proposal) {
		this.proposal = proposal;
	}

	public List<AwardReportTrackingFile> getAwardReportTrackingFiles() {
		return awardReportTrackingFiles;
	}

	public void setAwardReportTrackingFiles(List<AwardReportTrackingFile> awardReportTrackingFiles) {
		this.awardReportTrackingFiles = awardReportTrackingFiles;
	}

	public Integer getAwardReportTrackingId() {
		return awardReportTrackingId;
	}

	public void setAwardReportTrackingId(Integer awardReportTrackingId) {
		this.awardReportTrackingId = awardReportTrackingId;
	}

	public Integer getAwardReportTrackingFileId() {
		return awardReportTrackingFileId;
	}

	public void setAwardReportTrackingFileId(Integer awardReportTrackingFileId) {
		this.awardReportTrackingFileId = awardReportTrackingFileId;
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

	public Boolean getIsAwardOutcome() {
		return isAwardOutcome;
	}

	public void setIsAwardOutcome(Boolean isAwardOutcome) {
		this.isAwardOutcome = isAwardOutcome;
	}

	public AwardAttachment getAwardAttachment() {
		return awardAttachment;
	}

	public void setAwardAttachment(AwardAttachment awardAttachment) {
		this.awardAttachment = awardAttachment;
	}

	public List<AwardAmountInfo> getAwardAmountInfos() {
		return awardAmountInfos;
	}

	public void setAwardAmountInfos(List<AwardAmountInfo> awardAmountInfos) {
		this.awardAmountInfos = awardAmountInfos;
	}

	public List<CostShareType> getCostShareTypes() {
		return costShareTypes;
	}

	public void setCostShareTypes(List<CostShareType> costShareTypes) {
		this.costShareTypes = costShareTypes;
	}

	public boolean isCancelRequest() {
		return isCancelRequest;
	}

	public void setCancelRequest(boolean isCancelRequest) {
		this.isCancelRequest = isCancelRequest;
	}

	public String getAwardStatusCode() {
		return awardStatusCode;
	}

	public void setAwardStatusCode(String awardStatusCode) {
		this.awardStatusCode = awardStatusCode;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public Integer getTaskCount() {
		return taskCount;
	}

	public void setTaskCount(Integer taskCount) {
		this.taskCount = taskCount;
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

	public AwardReportTrackingFile getAwardReportTrackingFile() {
		return awardReportTrackingFile;
	}

	public void setAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile) {
		this.awardReportTrackingFile = awardReportTrackingFile;
	}

	public ServiceRequestAttachment getServiceRequestAttachment() {
		return serviceRequestAttachment;
	}

	public void setServiceRequestAttachment(ServiceRequestAttachment serviceRequestAttachment) {
		this.serviceRequestAttachment = serviceRequestAttachment;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public List<Integer> getAttachmentIds() {
		return attachmentIds;
	}

	public void setAttachmentIds(List<Integer> attachmentIds) {
		this.attachmentIds = attachmentIds;
	}

	public List<ModuleVariableSection> getSectionTypeCodes() {
		return sectionTypeCodes;
	}

	public void setSectionTypeCodes(List<ModuleVariableSection> sectionTypeCodes) {
		this.sectionTypeCodes = sectionTypeCodes;
	}

	public Boolean getIsBudgetCreated() {
		return isBudgetCreated;
	}

	public void setIsBudgetCreated(Boolean isBudgetCreated) {
		this.isBudgetCreated = isBudgetCreated;
	}

	public Boolean getIsNonEmployee() {
		return isNonEmployee;
	}

	public void setIsNonEmployee(Boolean isNonEmployee) {
		this.isNonEmployee = isNonEmployee;
	}

	public Boolean getIsCopyAward() {
		return isCopyAward;
	}

	public void setIsCopyAward(Boolean isCopyAward) {
		this.isCopyAward = isCopyAward;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public AwardAssociationDetail getAwardAssociationDetail() {
		return awardAssociationDetail;
	}

	public void setAwardAssociationDetail(AwardAssociationDetail awardAssociationDetail) {
		this.awardAssociationDetail = awardAssociationDetail;
	}

	public Integer getAwardAssociationDetailId() {
		return awardAssociationDetailId;
	}

	public void setAwardAssociationDetailId(Integer awardAssociationDetailId) {
		this.awardAssociationDetailId = awardAssociationDetailId;
	}

	public AwardResearchArea getAwardResearchArea() {
		return awardResearchArea;
	}

	public void setAwardResearchArea(AwardResearchArea awardResearchArea) {
		this.awardResearchArea = awardResearchArea;
	}

	public List<AwardResearchArea> getAwardResearchAreas() {
		return awardResearchAreas;
	}

	public void setAwardResearchAreas(List<AwardResearchArea> awardResearchAreas) {
		this.awardResearchAreas = awardResearchAreas;
	}

	public Integer getResearchAreaId() {
		return researchAreaId;
	}

	public void setResearchAreaId(Integer researchAreaId) {
		this.researchAreaId = researchAreaId;
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

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public Integer getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(Integer subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public Task getTask() {
		return task;
	}

	public void setTask(Task task) {
		this.task = task;
	}

	public Integer getQuestionnaireId() {
		return questionnaireId;
	}

	public void setQuestionnaireId(Integer questionnaireId) {
		this.questionnaireId = questionnaireId;
	}

	public Integer getQuestionnaireAnswerHeaderId() {
		return questionnaireAnswerHeaderId;
	}

	public void setQuestionnaireAnswerHeaderId(Integer questionnaireAnswerHeaderId) {
		this.questionnaireAnswerHeaderId = questionnaireAnswerHeaderId;
	}

	public Boolean getIsGenerateWBSNumber() {
		return isGenerateWBSNumber;
	}

	public void setIsGenerateWBSNumber(Boolean isGenerateWBSNumber) {
		this.isGenerateWBSNumber = isGenerateWBSNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getStopName() {
		return stopName;
	}

	public void setStopName(String stopName) {
		this.stopName = stopName;
	}

	public String getServiceRequestTypeCode() {
		return serviceRequestTypeCode;
	}

	public void setServiceRequestTypeCode(String serviceRequestTypeCode) {
		this.serviceRequestTypeCode = serviceRequestTypeCode;
	}

	public List<AwardKPI> getAwardKpis() {
		return awardKpis;
	}

	public void setAwardKpis(List<AwardKPI> awardKpis) {
		this.awardKpis = awardKpis;
	}

	public List<KPIType> getKpiTypes() {
		return kpiTypes;
	}

	public void setKpiTypes(List<KPIType> kpiTypes) {
		this.kpiTypes = kpiTypes;
	}

	public List<AwardMileStone> getAwardMileStones() {
		return awardMileStones;
	}

	public void setAwardMileStones(List<AwardMileStone> awardMileStones) {
		this.awardMileStones = awardMileStones;
	}

	public Boolean getIsFeededAwardId() {
		return isFeededAwardId;
	}

	public void setIsFeededAwardId(Boolean isFeededAwardId) {
		this.isFeededAwardId = isFeededAwardId;
	}

	public Timestamp getPreviousExpirationDate() {
		return previousExpirationDate;
	}

	public void setPreviousExpirationDate(Timestamp previousExpirationDate) {
		this.previousExpirationDate = previousExpirationDate;
	}

	public String getUserRole() {
		return userRole;
	}

	public void setUserRole(String userRole) {
		this.userRole = userRole;
	}

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
	}

	public Integer getPreviousActiveAwardId() {
		return previousActiveAwardId;
	}

	public void setPreviousActiveAwardId(Integer previousActiveAwardId) {
		this.previousActiveAwardId = previousActiveAwardId;
	}

	public String getCommentTypeCode() {
		return commentTypeCode;
	}

	public void setCommentTypeCode(String commentTypeCode) {
		this.commentTypeCode = commentTypeCode;
	}

	public Boolean getIsLastUploadedFile() {
		return isLastUploadedFile;
	}

	public void setIsLastUploadedFile(Boolean isLastUploadedFile) {
		this.isLastUploadedFile = isLastUploadedFile;
	}

	public Integer getPendingAwardId() {
		return pendingAwardId;
	}

	public void setPendingAwardId(Integer pendingAwardId) {
		this.pendingAwardId = pendingAwardId;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getAwardPublicationId() {
		return awardPublicationId;
	}

	public void setAwardPublicationId(String awardPublicationId) {
		this.awardPublicationId = awardPublicationId;
	}

	public String getAwardAssociationId() {
		return awardAssociationId;
	}

	public void setAwardAssociationId(String awardAssociationId) {
		this.awardAssociationId = awardAssociationId;
	}

	public String getAwardAcheivementId() {
		return awardAcheivementId;
	}

	public void setAwardAcheivementId(String awardAcheivementId) {
		this.awardAcheivementId = awardAcheivementId;
	}

	public List<AwardReviewComment> getAwardReviewComments() {
		return awardReviewComments;
	}

	public void setAwardReviewComments(List<AwardReviewComment> awardReviewComments) {
		this.awardReviewComments = awardReviewComments;
	}

	public Claim getClaim() {
		return claim;
	}

	public void setClaim(Claim claim) {
		this.claim = claim;
	}

	public Integer getOriginalAwardId() {
		return originalAwardId;
	}

	public void setOriginalAwardId(Integer originalAwardId) {
		this.originalAwardId = originalAwardId;
	}

	public Boolean getIsMasterAwardCreation() {
		return isMasterAwardCreation;
	}

	public void setIsMasterAwardCreation(Boolean isMasterAwardCreation) {
		this.isMasterAwardCreation = isMasterAwardCreation;
	}

	public AwardSummaryVO getPendingAwardsSummary() {
		return pendingAwardsSummary;
	}

	public void setPendingAwardsSummary(AwardSummaryVO pendingAwardsSummary) {
		this.pendingAwardsSummary = pendingAwardsSummary;
	}

	public Boolean getCanCreateVariationRequest() {
		return canCreateVariationRequest;
	}

	public void setCanCreateVariationRequest(Boolean canCreateVariationRequest) {
		this.canCreateVariationRequest = canCreateVariationRequest;
	}

	public String getBudgetStatusCode() {
		return budgetStatusCode;
	}

	public void setBudgetStatusCode(String budgetStatusCode) {
		this.budgetStatusCode = budgetStatusCode;
	}

	public Integer getAwardSequenceNumber() {
		return awardSequenceNumber;
	}

	public void setAwardSequenceNumber(Integer awardSequenceNumber) {
		this.awardSequenceNumber = awardSequenceNumber;
	}

	public Award getActiveAward() {
		return activeAward;
	}

	public void setActiveAward(Award activeAward) {
		this.activeAward = activeAward;
	}

	public Integer getAwardPersonId() {
		return awardPersonId;
	}

	public void setAwardPersonId(Integer awardPersonId) {
		this.awardPersonId = awardPersonId;
	}

	public Timestamp getAwardEffectiveDate() {
		return awardEffectiveDate;
	}

	public void setAwardEffectiveDate(Timestamp awardEffectiveDate) {
		this.awardEffectiveDate = awardEffectiveDate;
	}

	public Timestamp getFinalExpirationDate() {
		return finalExpirationDate;
	}

	public void setFinalExpirationDate(Timestamp finalExpirationDate) {
		this.finalExpirationDate = finalExpirationDate;
	}

	public String getAwardKeyPersonTimesheetType() {
		return awardKeyPersonTimesheetType;
	}

	public void setAwardKeyPersonTimesheetType(String awardKeyPersonTimesheetType) {
		this.awardKeyPersonTimesheetType = awardKeyPersonTimesheetType;
	}

	public Map<String, List<AwardKeyPersonTimesheet>> getAwardKeyPersonTimesheetDetails() {
		return awardKeyPersonTimesheetDetails;
	}

	public void setAwardKeyPersonTimesheetDetails(
			Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails) {
		this.awardKeyPersonTimesheetDetails = awardKeyPersonTimesheetDetails;
	}

	public AwardProgressReport getAwardProgressReport() {
		return awardProgressReport;
	}

	public void setAwardProgressReport(AwardProgressReport awardProgressReport) {
		this.awardProgressReport = awardProgressReport;
	}

	public Boolean getIsAwardHierarchy() {
		return isAwardHierarchy;
	}

	public void setIsAwardHierarchy(Boolean isAwardHierarchy) {
		this.isAwardHierarchy = isAwardHierarchy;
	}

	public List<AwardKeyPersonTimesheet> getAwardKeyPersonTimesheet() {
		return awardKeyPersonTimesheet;
	}

	public void setAwardKeyPersonTimesheet(List<AwardKeyPersonTimesheet> awardKeyPersonTimesheet) {
		this.awardKeyPersonTimesheet = awardKeyPersonTimesheet;
	}

	public Timestamp getFunderApprovalDate() {
		return funderApprovalDate;
	}

	public void setFunderApprovalDate(Timestamp funderApprovalDate) {
		this.funderApprovalDate = funderApprovalDate;
  }
  
	public Set<String> getEditableSectionCodes() {
		return editableSectionCodes;
	}

	public void setEditableSectionCodes(Set<String> editableSectionCodes) {
		this.editableSectionCodes = editableSectionCodes;
	}

	public Integer getLatestAwardId() {
		return latestAwardId;
	}

	public void setLatestAwardId(Integer latestAwardId) {
		this.latestAwardId = latestAwardId;
	}

	public Boolean getIsFirstTimeCreation() {
		return isFirstTimeCreation;
	}

	public void setIsFirstTimeCreation(Boolean isFirstTimeCreation) {
		this.isFirstTimeCreation = isFirstTimeCreation;
	}

	public Boolean getIsLastRequest() {
		return isLastRequest;
	}

	public void setIsLastRequest(Boolean isLastRequest) {
		this.isLastRequest = isLastRequest;
	}

	public Boolean getIsManpowerIntegrationRequired() {
		return isManpowerIntegrationRequired;
	}

	public void setIsManpowerIntegrationRequired(Boolean isManpowerIntegrationRequired) {
		this.isManpowerIntegrationRequired = isManpowerIntegrationRequired;
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
	
	public Boolean getIsGrantCallChanged() {
		return isGrantCallChanged;
	}

	public void setIsGrantCallChanged(Boolean isGrantCallChanged) {
		this.isGrantCallChanged = isGrantCallChanged;
	}

	public Boolean getIsReportTermsExist() {
		return isReportTermsExist;
	}

	public void setIsReportTermsExist(Boolean isReportTermsExist) {
		this.isReportTermsExist = isReportTermsExist;
	}

	public List<String> getPublicationTypes() {
		return publicationTypes;
	}

	public void setPublicationTypes(List<String> publicationTypes) {
		this.publicationTypes = publicationTypes;
	}

	public List<Publication> getPublications() {
		return publications;
	}

	public void setPublications(List<Publication> publications) {
		this.publications = publications;
	}

	public Boolean getCanEnableMilestoneStatus() {
		return canEnableMilestoneStatus;
	}

	public void setCanEnableMilestoneStatus(Boolean canEnableMilestoneStatus) {
		this.canEnableMilestoneStatus = canEnableMilestoneStatus;
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

	public Boolean getIsPersonCanScore() {
		return isPersonCanScore;
	}

	public void setIsPersonCanScore(Boolean isPersonCanScore) {
		this.isPersonCanScore = isPersonCanScore;
	}

	public Boolean getIsAwardHistoryTab() {
		return isAwardHistoryTab;
	}

	public void setIsAwardHistoryTab(Boolean isAwardHistoryTab) {
		this.isAwardHistoryTab = isAwardHistoryTab;
	}

	public List<AwardScopus> getAwardScopuses() {
		return awardScopuses;
	}

	public void setAwardScopuses(List<AwardScopus> awardScopuses) {
		this.awardScopuses = awardScopuses;
	}

}
