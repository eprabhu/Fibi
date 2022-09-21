package com.polus.fibicomp.progressreport.vo;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.polus.fibicomp.award.pojo.MilestoneStatus;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAchievement;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAttachment;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportKPISummary;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportMilestone;
import com.polus.fibicomp.progressreport.pojo.ProgressReportAttachmentType;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICashFunding;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICollaborationProjects;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICompetitiveGrants;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIConferencePresentation;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIGrantSpecific;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIHealthSpecificOutcomes;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIImpactPublications;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIInkindContributions;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPILicenses;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIManpowerDevelopment;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPatents;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPostDocsEmployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPISuccessfulStartups;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologiesDeployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologyDisclosure;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIUndergraduateStudent;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetailExt;
import com.polus.fibicomp.workflow.pojo.WorkflowFeedbackType;

public class ProgressReportVO {

	private Integer awardId;

	private String awardNumber;

	private String title;

	private Integer sequenceNumber;

	private String frequencyCode;

	private AwardProgressReport awardProgressReport;

	private List<AwardProgressReportMilestone> awardProgressReportMilestones;

	private AwardProgressReportMilestone awardProgressReportMilestone;

	private Integer progressReportId;

	private String message;

	private Workflow workflow;

	private String isFinalApprover;

	private String canApproveRouting;

	private List<Workflow> workflowList;

	private String workFlowPersonId;

	private String actionType;

	private String approveComment;

	private Boolean finalApprover = false;

	private Boolean isApproved = false;

	private Boolean isApprover = false;

	private Integer approverStopNumber;

	private String subModuleItemKey;

	private Integer workFlowDetailId;

	private String isSubmit;

	private Integer mapNumber;

	private Integer mapId;

	private Integer approverNumber;

	private List<String> availableRights;

	private String fileName;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String fileTimestamp;

	private String contentType;

	private String personId;

	private String updateUser;

	private List<MilestoneStatus> progressReportMilestoneStatuses;

	private List<AwardProgressReportAttachment> awardProgressReportAttachments;

	private AwardProgressReportAttachment awardProgressReportAttachment;

	private Integer documentId;

	private List<ProgressReportAttachmentType> progressReportAttachmentTypes;

	private List<AwardProgressReportAchievement> awardProgressReportAchievements;

	private List<AwardProgressReportKPISummary> awardProgressReportKPISummary;

	private Object summaryDetail;

	private String sectionCode;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIImpactPublications progressReportKPIImpactPublications;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPICollaborationProjects progressReportKPICollaborationProjects;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPITechnologyDisclosure progressReportKPITechnologyDisclosure;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIManpowerDevelopment progressReportKPIManpowerDevelopment;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIUndergraduateStudent progressReportKPIUndergraduateStudent;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIConferencePresentation progressReportKPIConferencePresentation;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPICompetitiveGrants progressReportKPICompetitiveGrants;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIPatents progressReportKPIPatents;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPILicenses progressReportKPILicenses;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPISuccessfulStartups progressReportKPISuccessfulStartups;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIHealthSpecificOutcomes progressReportKPIHealthSpecificOutcomes;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIPostDocsEmployed progressReportKPIPostDocsEmployed;
	
	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIGrantSpecific progressReportKPIGrantSpecific;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPICashFunding progressReportKPICashFunding;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPITechnologiesDeployed progressReportKPITechnologiesDeployed;

	@JsonInclude(Include.NON_NULL)
	private ProgressReportKPIInkindContributions progressReportKPIInkindContributions;
	
	private List<String> progressReportNumbers;

	private List<Map<Object, Object>> summaryHistoryData;

	private List<WorkflowFeedbackType> workflowFeedbackTypes;

	private WorkflowDetailExt workflowDetailExt;
	
	private Boolean status;

	private Timestamp funderApprovalDate;

	private Boolean progressReportImported = Boolean.FALSE;

	private Timestamp reportStartDate;

	private Timestamp reportEndDate;

	private String sortBy;

	private String reverse;

	private String awardLeadUnitNumber;

	private AwardProgressReport lastProgressReport;
	
	public Timestamp getReportStartDate() {
		return reportStartDate;
	}

	public void setReportStartDate(Timestamp reportStartDate) {
		this.reportStartDate = reportStartDate;
	}

	public Timestamp getReportEndDate() {
		return reportEndDate;
	}

	public void setReportEndDate(Timestamp reportEndDate) {
		this.reportEndDate = reportEndDate;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public AwardProgressReport getAwardProgressReport() {
		return awardProgressReport;
	}

	public void setAwardProgressReport(AwardProgressReport awardProgressReport) {
		this.awardProgressReport = awardProgressReport;
	}

	public String getFrequencyCode() {
		return frequencyCode;
	}

	public void setFrequencyCode(String frequencyCode) {
		this.frequencyCode = frequencyCode;
	}

	public List<AwardProgressReportMilestone> getAwardProgressReportMilestones() {
		return awardProgressReportMilestones;
	}

	public void setAwardProgressReportMilestones(List<AwardProgressReportMilestone> awardProgressReportMilestones) {
		this.awardProgressReportMilestones = awardProgressReportMilestones;
	}

	public AwardProgressReportMilestone getAwardProgressReportMilestone() {
		return awardProgressReportMilestone;
	}

	public void setAwardProgressReportMilestone(AwardProgressReportMilestone awardProgressReportMilestone) {
		this.awardProgressReportMilestone = awardProgressReportMilestone;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public String getApproveComment() {
		return approveComment;
	}

	public void setApproveComment(String approveComment) {
		this.approveComment = approveComment;
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

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public Integer getWorkFlowDetailId() {
		return workFlowDetailId;
	}

	public void setWorkFlowDetailId(Integer workFlowDetailId) {
		this.workFlowDetailId = workFlowDetailId;
	}

	public String getIsSubmit() {
		return isSubmit;
	}

	public void setIsSubmit(String isSubmit) {
		this.isSubmit = isSubmit;
	}

	public Integer getMapNumber() {
		return mapNumber;
	}

	public void setMapNumber(Integer mapNumber) {
		this.mapNumber = mapNumber;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
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

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<AwardProgressReportAttachment> getAwardProgressReportAttachments() {
		return awardProgressReportAttachments;
	}

	public void setAwardProgressReportAttachments(List<AwardProgressReportAttachment> awardProgressReportAttachments) {
		this.awardProgressReportAttachments = awardProgressReportAttachments;
	}

	public AwardProgressReportAttachment getAwardProgressReportAttachment() {
		return awardProgressReportAttachment;
	}

	public void setAwardProgressReportAttachment(AwardProgressReportAttachment awardProgressReportAttachment) {
		this.awardProgressReportAttachment = awardProgressReportAttachment;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public List<ProgressReportAttachmentType> getProgressReportAttachmentTypes() {
		return progressReportAttachmentTypes;
	}

	public void setProgressReportAttachmentTypes(List<ProgressReportAttachmentType> progressReportAttachmentTypes) {
		this.progressReportAttachmentTypes = progressReportAttachmentTypes;
	}

	public List<AwardProgressReportAchievement> getAwardProgressReportAchievements() {
		return awardProgressReportAchievements;
	}

	public void setAwardProgressReportAchievements(List<AwardProgressReportAchievement> awardProgressReportAchievements) {
		this.awardProgressReportAchievements = awardProgressReportAchievements;
	}

	public List<AwardProgressReportKPISummary> getAwardProgressReportKPISummary() {
		return awardProgressReportKPISummary;
	}

	public void setAwardProgressReportKPISummary(List<AwardProgressReportKPISummary> awardProgressReportKPISummary) {
		this.awardProgressReportKPISummary = awardProgressReportKPISummary;
	}

	public Object getSummaryDetail() {
		return summaryDetail;
	}

	public void setSummaryDetail(Object summaryDetail) {
		this.summaryDetail = summaryDetail;
	}

	public String getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(String sectionCode) {
		this.sectionCode = sectionCode;
	}

	public ProgressReportKPIImpactPublications getProgressReportKPIImpactPublications() {
		return progressReportKPIImpactPublications;
	}

	public void setProgressReportKPIImpactPublications(
			ProgressReportKPIImpactPublications progressReportKPIImpactPublications) {
		this.progressReportKPIImpactPublications = progressReportKPIImpactPublications;
	}

	public ProgressReportKPICollaborationProjects getProgressReportKPICollaborationProjects() {
		return progressReportKPICollaborationProjects;
	}

	public void setProgressReportKPICollaborationProjects(
			ProgressReportKPICollaborationProjects progressReportKPICollaborationProjects) {
		this.progressReportKPICollaborationProjects = progressReportKPICollaborationProjects;
	}

	public ProgressReportKPITechnologyDisclosure getProgressReportKPITechnologyDisclosure() {
		return progressReportKPITechnologyDisclosure;
	}

	public void setProgressReportKPITechnologyDisclosure(
			ProgressReportKPITechnologyDisclosure progressReportKPITechnologyDisclosure) {
		this.progressReportKPITechnologyDisclosure = progressReportKPITechnologyDisclosure;
	}

	public ProgressReportKPIManpowerDevelopment getProgressReportKPIManpowerDevelopment() {
		return progressReportKPIManpowerDevelopment;
	}

	public void setProgressReportKPIManpowerDevelopment(
			ProgressReportKPIManpowerDevelopment progressReportKPIManpowerDevelopment) {
		this.progressReportKPIManpowerDevelopment = progressReportKPIManpowerDevelopment;
	}

	public ProgressReportKPIUndergraduateStudent getProgressReportKPIUndergraduateStudent() {
		return progressReportKPIUndergraduateStudent;
	}

	public void setProgressReportKPIUndergraduateStudent(
			ProgressReportKPIUndergraduateStudent progressReportKPIUndergraduateStudent) {
		this.progressReportKPIUndergraduateStudent = progressReportKPIUndergraduateStudent;
	}

	public ProgressReportKPIConferencePresentation getProgressReportKPIConferencePresentation() {
		return progressReportKPIConferencePresentation;
	}

	public void setProgressReportKPIConferencePresentation(
			ProgressReportKPIConferencePresentation progressReportKPIConferencePresentation) {
		this.progressReportKPIConferencePresentation = progressReportKPIConferencePresentation;
	}

	public ProgressReportKPICompetitiveGrants getProgressReportKPICompetitiveGrants() {
		return progressReportKPICompetitiveGrants;
	}

	public void setProgressReportKPICompetitiveGrants(
			ProgressReportKPICompetitiveGrants progressReportKPICompetitiveGrants) {
		this.progressReportKPICompetitiveGrants = progressReportKPICompetitiveGrants;
	}

	public ProgressReportKPIPatents getProgressReportKPIPatents() {
		return progressReportKPIPatents;
	}

	public void setProgressReportKPIPatents(ProgressReportKPIPatents progressReportKPIPatents) {
		this.progressReportKPIPatents = progressReportKPIPatents;
	}

	public ProgressReportKPILicenses getProgressReportKPILicenses() {
		return progressReportKPILicenses;
	}

	public void setProgressReportKPILicenses(ProgressReportKPILicenses progressReportKPILicenses) {
		this.progressReportKPILicenses = progressReportKPILicenses;
	}

	public ProgressReportKPISuccessfulStartups getProgressReportKPISuccessfulStartups() {
		return progressReportKPISuccessfulStartups;
	}

	public void setProgressReportKPISuccessfulStartups(
			ProgressReportKPISuccessfulStartups progressReportKPISuccessfulStartups) {
		this.progressReportKPISuccessfulStartups = progressReportKPISuccessfulStartups;
	}

	public ProgressReportKPIHealthSpecificOutcomes getProgressReportKPIHealthSpecificOutcomes() {
		return progressReportKPIHealthSpecificOutcomes;
	}

	public void setProgressReportKPIHealthSpecificOutcomes(
			ProgressReportKPIHealthSpecificOutcomes progressReportKPIHealthSpecificOutcomes) {
		this.progressReportKPIHealthSpecificOutcomes = progressReportKPIHealthSpecificOutcomes;
	}

	public ProgressReportKPIPostDocsEmployed getProgressReportKPIPostDocsEmployed() {
		return progressReportKPIPostDocsEmployed;
	}

	public void setProgressReportKPIPostDocsEmployed(ProgressReportKPIPostDocsEmployed progressReportKPIPostDocsEmployed) {
		this.progressReportKPIPostDocsEmployed = progressReportKPIPostDocsEmployed;
	}

	public ProgressReportKPIGrantSpecific getProgressReportKPIGrantSpecific() {
		return progressReportKPIGrantSpecific;
	}

	public void setProgressReportKPIGrantSpecific(ProgressReportKPIGrantSpecific progressReportKPIGrantSpecific) {
		this.progressReportKPIGrantSpecific = progressReportKPIGrantSpecific;
	}

	public ProgressReportKPICashFunding getProgressReportKPICashFunding() {
		return progressReportKPICashFunding;
	}

	public void setProgressReportKPICashFunding(ProgressReportKPICashFunding progressReportKPICashFunding) {
		this.progressReportKPICashFunding = progressReportKPICashFunding;
	}

	public ProgressReportKPITechnologiesDeployed getProgressReportKPITechnologiesDeployed() {
		return progressReportKPITechnologiesDeployed;
	}

	public void setProgressReportKPITechnologiesDeployed(
			ProgressReportKPITechnologiesDeployed progressReportKPITechnologiesDeployed) {
		this.progressReportKPITechnologiesDeployed = progressReportKPITechnologiesDeployed;
	}

	public ProgressReportKPIInkindContributions getProgressReportKPIInkindContributions() {
		return progressReportKPIInkindContributions;
	}

	public void setProgressReportKPIInkindContributions(
			ProgressReportKPIInkindContributions progressReportKPIInkindContributions) {
		this.progressReportKPIInkindContributions = progressReportKPIInkindContributions;
	}

	public List<String> getProgressReportNumbers() {
		return progressReportNumbers;
	}

	public void setProgressReportNumbers(List<String> progressReportNumbers) {
		this.progressReportNumbers = progressReportNumbers;
	}

	public List<Map<Object, Object>> getSummaryHistoryData() {
		return summaryHistoryData;
	}

	public void setSummaryHistoryData(List<Map<Object, Object>> summaryHistoryData) {
		this.summaryHistoryData = summaryHistoryData;
	}

	public List<WorkflowFeedbackType> getWorkflowFeedbackTypes() {
		return workflowFeedbackTypes;
	}

	public void setWorkflowFeedbackTypes(List<WorkflowFeedbackType> workflowFeedbackTypes) {
		this.workflowFeedbackTypes = workflowFeedbackTypes;
	}

	public WorkflowDetailExt getWorkflowDetailExt() {
		return workflowDetailExt;
	}

	public void setWorkflowDetailExt(WorkflowDetailExt workflowDetailExt) {
		this.workflowDetailExt = workflowDetailExt;
	}

	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public Timestamp getFunderApprovalDate() {
		return funderApprovalDate;
	}

	public void setFunderApprovalDate(Timestamp funderApprovalDate) {
		this.funderApprovalDate = funderApprovalDate;
	}

	public Boolean getProgressReportImported() {
		return progressReportImported;
	}

	public void setProgressReportImported(Boolean progressReportImported) {
		this.progressReportImported = progressReportImported;
	}

	public String getSortBy() {
		return sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public String getAwardLeadUnitNumber() {
		return awardLeadUnitNumber;
	}

	public void setAwardLeadUnitNumber(String awardLeadUnitNumber) {
		this.awardLeadUnitNumber = awardLeadUnitNumber;
	}

	public AwardProgressReport getLastProgressReport() {
		return lastProgressReport;
	}

	public void setLastProgressReport(AwardProgressReport lastProgressReport) {
		this.lastProgressReport = lastProgressReport;
	}

	public List<MilestoneStatus> getProgressReportMilestoneStatuses() {
		return progressReportMilestoneStatuses;
	}

	public void setProgressReportMilestoneStatuses(List<MilestoneStatus> progressReportMilestoneStatuses) {
		this.progressReportMilestoneStatuses = progressReportMilestoneStatuses;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

}
