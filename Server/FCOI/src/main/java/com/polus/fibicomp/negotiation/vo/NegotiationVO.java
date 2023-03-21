package com.polus.fibicomp.negotiation.vo;

import java.util.HashMap;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;
import com.polus.fibicomp.negotiation.dto.NegotiationProjectDetailsDto;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivityType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementValue;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachmentType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsNegotiatorHistory;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnel;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnelType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsStatus;
import com.polus.fibicomp.negotiation.pojo.NegotiationsStatusHistory;
import com.polus.fibicomp.negotiation.pojo.NegotiationsWorkflowStatus;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.vo.BaseVO;
import com.polus.fibicomp.vo.SponsorSearchResult;
import com.polus.fibicomp.workflow.pojo.Workflow;

public class NegotiationVO extends BaseVO {
	
	private String message;
	
	private String actionType;
	
	private String approveComment;

	private Integer negotiationId;
	
	private Integer notificationTypeId;
	
	private Negotiations negotiations;
	
	private NegotiationMode negotiationMode;
	
	private NegotiationsActivity negotiationsActivity;
	
	private NegotiationsAttachment negotiationsAttachment;
	
	private NegotiationsPersonnel negotiationsPersonnel;

	private NegotiationsLocation negotiationsLocation;

	private NegotiationsAssociationDetails negotiationsAssociationDetail;

	private NegotiationsAgreementValue negotiationsAgreementValue;

	private LastLocationDetails lastLocationDetails;

	private List<NegotiationsAttachment> newAttachments;
	
	private List<NegotiationsLocation> negotiationsLocationHistoryList;
	
	private List<HashMap<String, Object>> negotiationLocationHistoryList;
	
	private List<NegotiationsAgreementType> negotiationsAgreementTypeList;
	
	private List<NegotiationsStatus> negotiationsStatusList;
	
	private List<NegotiationsActivityType> negotiationsActivityTypeList;
		
	private List<NegotiationsAttachmentType> negotiationsAttachmentTypeList;
	
	private List<NegotiationsLocationType> negotiationsLocationTypeList;
	
	private List<NegotiationsLocation> negotiationLocationList;
	
	private List<NegotiationsPersonnel> negotiationsPersonnelList;
	
	private List<NegotiationsPersonnelType> negotiationsPersonnelTypeList;
	
	private List<NegotiationsStatusHistory> negotiationsStatusHistoryList;
	
	private List<NegotiationsWorkflowStatus> negotiationsWorkflowStatusList;
	
	private List<NegotiationsNegotiatorHistory> negotiationsNegotiatorHistoryList;
	
	private List<NegotiationsAssociationType> negotiationsAssociationTypeList;
	
	private List<NegotiationsAgreementValue> negotiationsAgreementValueList;
	
	private List<NegotiationsLocation> negotiationsLocations;
	
	private List<NegotiationsActivity> negotiationActivitysById;

	private String acType;
	
	private List<NegotiationsAttachment> negotiationAttachments;
	
	private List<NegotiationsAttachment> attachmentFile; 
	
	private List<NegotiationsAssociationDetails> negotiationsAssociationDetailsList;
	
	private String subAwardOrganizationName;
	
	private String sponsorName;
	
	private String primeSponsorName;
	
	private List<NegotiationProjectDetailsDto> projectDetails;
	
	private Integer canApproveRouting;
	
	private String isFinalApprover;
	
	private String isSubmit;
	
	private String personId;
	
	private String moduleItemKey;
	
	private Integer moduleCode;
	
	private List<WorkFlowResultDto> validationError;
	
	private Workflow workflow;
	
	private List<Workflow> workflowList;
	
	private String loginPersonId;

	private Integer negotiationLocationId;

	@JsonIgnore
	private List<Unit> LeadUnits;
	
	@JsonIgnore
	private List<SponsorSearchResult> SponsorSearchResult;
	
	private List<Organization> subAwardOrgnizationList;
	
	private NegotiationsAssociationDetails negotiationsAssociationDetails;

	private List<Integer> attachmentIds;

	private Integer negotiationAttachmentId;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String fileTimestamp;

	private Integer workFlowDetailId;

	private String updateUser;

	private NegotiationsLocation negotiationLocation;

	private Integer agreementRequestId;

	private Integer approverStopNumber;

	private Integer mapId;

	private Integer mapNumber;

	private Integer approverNumber;

	private Integer negotiationActivityId;

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public List<NegotiationsStatus> getNegotiationsStatusList() {
		return negotiationsStatusList;
	}

	public void setNegotiationsStatusList(List<NegotiationsStatus> negotiationsStatusList) {
		this.negotiationsStatusList = negotiationsStatusList;
	}

	public List<NegotiationsAgreementType> getNegotiationsAgreementTypeList() {
		return negotiationsAgreementTypeList;
	}

	public void setNegotiationsAgreementTypeList(List<NegotiationsAgreementType> negotiationsAgreementTypeList) {
		this.negotiationsAgreementTypeList = negotiationsAgreementTypeList;
	}

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}
	 
	

	public List<NegotiationsStatusHistory> getNegotiationsStatusHistoryList() {
		return negotiationsStatusHistoryList;
	}

	public void setNegotiationsStatusHistoryList(List<NegotiationsStatusHistory> negotiationsStatusHistoryList) {
		this.negotiationsStatusHistoryList = negotiationsStatusHistoryList;
	}

	public List<NegotiationsWorkflowStatus> getNegotiationsWorkflowStatusList() {
		return negotiationsWorkflowStatusList;
	}

	public void setNegotiationsWorkflowStatusList(List<NegotiationsWorkflowStatus> negotiationsWorkflowStatusList) {
		this.negotiationsWorkflowStatusList = negotiationsWorkflowStatusList;
	}

	public List<NegotiationsNegotiatorHistory> getNegotiationsNegotiatorHistoryList() {
		return negotiationsNegotiatorHistoryList;
	}

	public void setNegotiationsNegotiatorHistoryList(
			List<NegotiationsNegotiatorHistory> negotiationsNegotiatorHistoryList) {
		this.negotiationsNegotiatorHistoryList = negotiationsNegotiatorHistoryList;
	}

	public NegotiationMode getNegotiationMode() {
		return negotiationMode;
	}

	public void setNegotiationMode(NegotiationMode negotiationMode) {
		this.negotiationMode = negotiationMode;
	}

	public List<NegotiationsAssociationType> getNegotiationsAssociationTypeList() {
		return negotiationsAssociationTypeList;
	}

	public void setNegotiationsAssociationTypeList(List<NegotiationsAssociationType> negotiationsAssociationTypeList) {
		this.negotiationsAssociationTypeList = negotiationsAssociationTypeList;
	}
	
	public NegotiationsActivity getNegotiationsActivity() {
		return negotiationsActivity;
	}

	public void setNegotiationsActivity(NegotiationsActivity negotiationsActivity) {
		this.negotiationsActivity = negotiationsActivity;
	}
	public List<NegotiationsAgreementValue> getNegotiationsAgreementValueList() {
		return negotiationsAgreementValueList;
	}

	public void setNegotiationsAgreementValueList(List<NegotiationsAgreementValue> negotiationsAgreementValueList) {
		this.negotiationsAgreementValueList = negotiationsAgreementValueList;
	}

	public List<NegotiationsLocationType> getNegotiationsLocationTypeList() {
		return negotiationsLocationTypeList;
	}

	public void setNegotiationsLocationTypeList(List<NegotiationsLocationType> negotiationsLocationTypeList) {
		this.negotiationsLocationTypeList = negotiationsLocationTypeList;
	}

	public LastLocationDetails getLastLocationDetails() {
		return lastLocationDetails;
	}

	public void setLastLocationDetails(LastLocationDetails lastLocationDetails) {
		this.lastLocationDetails = lastLocationDetails;
	}

	public List<NegotiationsLocation> getNegotiationsLocationHistoryList() {
		return negotiationsLocationHistoryList;
	}

	public void setNegotiationsLocationHistoryList(List<NegotiationsLocation> negotiationsLocationHistoryList) {
		this.negotiationsLocationHistoryList = negotiationsLocationHistoryList;
	}

	public NegotiationsPersonnel getNegotiationsPersonnel() {
		return negotiationsPersonnel;
	}

	public void setNegotiationsPersonnel(NegotiationsPersonnel negotiationsPersonnel) {
		this.negotiationsPersonnel = negotiationsPersonnel;
	}

	public List<NegotiationsActivityType> getNegotiationsActivityTypeList() {
		return negotiationsActivityTypeList;
	}

	public void setNegotiationsActivityTypeList(List<NegotiationsActivityType> negotiationsActivityTypeList) {
		this.negotiationsActivityTypeList = negotiationsActivityTypeList;
	}

	public List<NegotiationsAttachmentType> getNegotiationsAttachmentTypeList() {
		return negotiationsAttachmentTypeList;
	}

	public void setNegotiationsAttachmentTypeList(List<NegotiationsAttachmentType> negotiationsAttachmentTypeList) {
		this.negotiationsAttachmentTypeList = negotiationsAttachmentTypeList;
	}

	public List<NegotiationsPersonnelType> getNegotiationsPersonnelTypeList() {
		return negotiationsPersonnelTypeList;
	}

	public void setNegotiationsPersonnelTypeList(List<NegotiationsPersonnelType> negotiationsPersonnelTypeList) {
		this.negotiationsPersonnelTypeList = negotiationsPersonnelTypeList;
	}

	public List<NegotiationsActivity> getNegotiationActivitysById() {
		return negotiationActivitysById;
	}

	public void setNegotiationActivitysById(List<NegotiationsActivity> negotiationActivitysById) {
		this.negotiationActivitysById = negotiationActivitysById;
	}

	public List<NegotiationsPersonnel> getNegotiationsPersonnelList() {
		return negotiationsPersonnelList;
	}

	public void setNegotiationsPersonnelList(List<NegotiationsPersonnel> negotiationsPersonnelList) {
		this.negotiationsPersonnelList = negotiationsPersonnelList;
	}

	public List<NegotiationsAttachment> getAttachmentFile() {
		return attachmentFile;
	}

	public void setAttachmentFile(List<NegotiationsAttachment> attachmentFile) {
		this.attachmentFile = attachmentFile;
	}

	public List<NegotiationsAssociationDetails> getNegotiationsAssociationDetailsList() {
		return negotiationsAssociationDetailsList;
	}

	public void setNegotiationsAssociationDetailsList(
			List<NegotiationsAssociationDetails> negotiationsAssociationDetailsList) {
		this.negotiationsAssociationDetailsList = negotiationsAssociationDetailsList;
	}

	public List<SponsorSearchResult> getSponsorSearchResult() {
		return SponsorSearchResult;
	}

	public void setSponsorSearchResult(List<SponsorSearchResult> sponsorSearchResult) {
		SponsorSearchResult = sponsorSearchResult;
	}

	public NegotiationsAssociationDetails getNegotiationsAssociationDetails() {
		return negotiationsAssociationDetails;
	}

	public void setNegotiationsAssociationDetails(NegotiationsAssociationDetails negotiationsAssociationDetails) {
		this.negotiationsAssociationDetails = negotiationsAssociationDetails;
	}

	public List<Organization> getSubAwardOrgnizationList() {
		return subAwardOrgnizationList;
	}

	public void setSubAwardOrgnizationList(List<Organization> subAwardOrgnizationList) {
		this.subAwardOrgnizationList = subAwardOrgnizationList;
	}

	public List<Unit> getLeadUnits() {
		return LeadUnits;
	}

	public void setLeadUnits(List<Unit> leadUnits) {
		LeadUnits = leadUnits;
	}

	public String getSubAwardOrganizationName() {
		return subAwardOrganizationName;
	}

	public void setSubAwardOrganizationName(String subAwardOrganizationName) {
		this.subAwardOrganizationName = subAwardOrganizationName;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getPrimeSponsorName() {
		return primeSponsorName;
	}

	public void setPrimeSponsorName(String primeSponsorName) {
		this.primeSponsorName = primeSponsorName;
	}

	

	public List<NegotiationsAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<NegotiationsAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	

	public NegotiationsAssociationDetails getNegotiationsAssociationDetail() {
		return negotiationsAssociationDetail;
	}

	public void setNegotiationsAssociationDetail(NegotiationsAssociationDetails negotiationsAssociationDetail) {
		this.negotiationsAssociationDetail = negotiationsAssociationDetail;
	}

	public NegotiationsAgreementValue getNegotiationsAgreementValue() {
		return negotiationsAgreementValue;
	}

	public void setNegotiationsAgreementValue(NegotiationsAgreementValue negotiationsAgreementValue) {
		this.negotiationsAgreementValue = negotiationsAgreementValue;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<NegotiationProjectDetailsDto> getProjectDetails() {
		return projectDetails;
	}

	public void setProjectDetails(List<NegotiationProjectDetailsDto> projectDetails) {
		this.projectDetails = projectDetails;
	}

	public List<HashMap<String, Object>> getNegotiationLocationHistoryList() {
		return negotiationLocationHistoryList;
	}

	public void setNegotiationLocationHistoryList(List<HashMap<String, Object>> negotiationLocationHistoryList) {
		this.negotiationLocationHistoryList = negotiationLocationHistoryList;
	}

	public NegotiationsAttachment getNegotiationsAttachment() {
		return negotiationsAttachment;
	}

	public void setNegotiationsAttachment(NegotiationsAttachment negotiationsAttachment) {
		this.negotiationsAttachment = negotiationsAttachment;
	}

	public List<NegotiationsLocation> getNegotiationLocationList() {
		return negotiationLocationList;
	}

	public void setNegotiationLocationList(List<NegotiationsLocation> negotiationLocationList) {
		this.negotiationLocationList = negotiationLocationList;
	}

	public NegotiationsLocation getNegotiationsLocation() {
		return negotiationsLocation;
	}

	public void setNegotiationsLocation(NegotiationsLocation negotiationsLocation) {
		this.negotiationsLocation = negotiationsLocation;
	}

	public Integer getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(Integer canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public List<WorkFlowResultDto> getValidationError() {
		return validationError;
	}

	public void setValidationError(List<WorkFlowResultDto> validationError) {
		this.validationError = validationError;
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

	public Integer getNotificationTypeId() {
		return notificationTypeId;
	}

	public void setNotificationTypeId(Integer notificationTypeId) {
		this.notificationTypeId = notificationTypeId;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getIsSubmit() {
		return isSubmit;
	}

	public void setIsSubmit(String isSubmit) {
		this.isSubmit = isSubmit;
	}

	public String getLoginPersonId() {
		return loginPersonId;
	}

	public void setLoginPersonId(String loginPersonId) {
		this.loginPersonId = loginPersonId;
	}

	public List<NegotiationsAttachment> getNegotiationAttachments() {
		return negotiationAttachments;
	}

	public void setNegotiationAttachments(List<NegotiationsAttachment> negotiationAttachments) {
		this.negotiationAttachments = negotiationAttachments;
	}

	public List<Integer> getAttachmentIds() {
		return attachmentIds;
	}

	public void setAttachmentIds(List<Integer> attachmentIds) {
		this.attachmentIds = attachmentIds;
	}

	public Integer getNegotiationAttachmentId() {
		return negotiationAttachmentId;
	}

	public void setNegotiationAttachmentId(Integer negotiationAttachmentId) {
		this.negotiationAttachmentId = negotiationAttachmentId;
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

	public Integer getWorkFlowDetailId() {
		return workFlowDetailId;
	}

	public void setWorkFlowDetailId(Integer workFlowDetailId) {
		this.workFlowDetailId = workFlowDetailId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public NegotiationsLocation getNegotiationLocation() {
		return negotiationLocation;
	}

	public void setNegotiationLocation(NegotiationsLocation negotiationLocation) {
		this.negotiationLocation = negotiationLocation;
	}

	public List<NegotiationsLocation> getNegotiationsLocations() {
		return negotiationsLocations;
	}

	public void setNegotiationsLocations(List<NegotiationsLocation> negotiationsLocations) {
		this.negotiationsLocations = negotiationsLocations;
	}

	public Integer getNegotiationLocationId() {
		return negotiationLocationId;
	}

	public void setNegotiationLocationId(Integer negotiationLocationId) {
		this.negotiationLocationId = negotiationLocationId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
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

	public Integer getNegotiationActivityId() {
		return negotiationActivityId;
	}

	public void setNegotiationActivityId(Integer negotiationActivityId) {
		this.negotiationActivityId = negotiationActivityId;
	}

}
