package com.polus.fibicomp.servicerequest.vo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.pojo.Module;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionLog;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestComment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestPriority;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestReporterChangeHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestRoleType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatusHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestWatcher;
import com.polus.fibicomp.vo.BaseVO;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

public class ServiceRequestVO extends BaseVO {

	private Integer serviceRequestId;

	private String personId;

	private String updateType;

	private String message;
	
	private String acType;

	private ServiceRequest serviceRequest;

	private List<Module> moduleList;

	private List<ServiceRequestType> serviceRequestTypes;

	private List<ServiceRequestActionType> serviceRequestActionTypes;

	private List<ServiceRequestRoleType> serviceRequestRoleTypes;

	private ServiceRequestComment serviceRequestComment;

	private String moduleItemKey;

	private Integer moduleCode;

	private String personName;

	private String assignActionType;

	private Integer actionCode;

	private List<ServiceRequestAttachment> newAttachments;

	private Integer attachmentId;

	private Map<String, String> sort = new HashMap<>();

	private String serviceRequestTabName;

	private Integer currentPage;

	private Integer pageNumber;

	private Integer commentId;

	private String tabIndex;

	private List<String> availableRights;

	private String leadUnitNumber;

	private List<ServiceRequestActionLog> serviceRequestActionLogs;

	private String loginPersonUnitNumber;

	private String actionType;

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

	private Integer approverStopNumber;

	private String subModuleItemKey;

	private String isSubmit;

	private Integer mapNumber;

	private Integer mapId;

	private Integer approverNumber;

	private String updateUser;

	private String workFlowPersonId;

	private String serviceRequestSubject;

	private String serviceRequestDescription;

	private ServiceRequestWatcher serviceRequestWatcher;

	private List<ServiceRequestStatusHistory> serviceRequestStatusHistories;

	private List<ServiceRequestWatcher> serviceRequestWatchers;

	private List<ServiceRequestPriority> serviceRequestPriorities;

	private List<AdminGroup> adminGroups;

	private String assigneePersonId;

	private Integer adminGroupId;

	private Boolean isReturnServiceRequest;

	private ServiceRequestReporterChangeHistory sRReporterChangeHistory;

	private String projectTitle;

	private String projectId;

	private String principalInvestigator;

	private String serviceRequestType;

	private String userRole;

	private String sponsorAwardNumber;

	private String accountNumber;

	private String serviceRequestOutCome;

	private String htmlTable;

	private String notifyActionComment;

	private ServiceRequestHistory serviceRequestHistory;

	private Boolean isAddAsWatcher;

	public ServiceRequestVO() {
		serviceRequest = new ServiceRequest();
		newAttachments = new ArrayList<>();
		serviceRequestStatusHistories = new ArrayList<>();
		serviceRequestWatchers = new ArrayList<>();
		serviceRequestPriorities = new ArrayList<>();
		adminGroups = new ArrayList<>();
		availableRights = new ArrayList<>();
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public ServiceRequest getServiceRequest() {
		return serviceRequest;
	}

	public void setServiceRequest(ServiceRequest serviceRequest) {
		this.serviceRequest = serviceRequest;
	}

	public List<Module> getModuleList() {
		return moduleList;
	}

	public void setModuleList(List<Module> moduleList) {
		this.moduleList = moduleList;
	}

	public List<ServiceRequestType> getServiceRequestTypes() {
		return serviceRequestTypes;
	}

	public void setServiceRequestTypes(List<ServiceRequestType> serviceRequestTypes) {
		this.serviceRequestTypes = serviceRequestTypes;
	}

	public List<ServiceRequestActionType> getServiceRequestActionTypes() {
		return serviceRequestActionTypes;
	}

	public void setServiceRequestActionTypes(List<ServiceRequestActionType> serviceRequestActionTypes) {
		this.serviceRequestActionTypes = serviceRequestActionTypes;
	}

	public List<ServiceRequestRoleType> getServiceRequestRoleTypes() {
		return serviceRequestRoleTypes;
	}

	public void setServiceRequestRoleTypes(List<ServiceRequestRoleType> serviceRequestRoleTypes) {
		this.serviceRequestRoleTypes = serviceRequestRoleTypes;
	}

	public ServiceRequestComment getServiceRequestComment() {
		return serviceRequestComment;
	}

	public void setServiceRequestComment(ServiceRequestComment serviceRequestComment) {
		this.serviceRequestComment = serviceRequestComment;
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

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public String getAssignActionType() {
		return assignActionType;
	}

	public void setAssignActionType(String assignActionType) {
		this.assignActionType = assignActionType;
	}

	public Integer getActionCode() {
		return actionCode;
	}

	public void setActionCode(Integer actionCode) {
		this.actionCode = actionCode;
	}

	public List<ServiceRequestAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<ServiceRequestAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public String getServiceRequestTabName() {
		return serviceRequestTabName;
	}

	public void setServiceRequestTabName(String serviceRequestTabName) {
		this.serviceRequestTabName = serviceRequestTabName;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public String getTabIndex() {
		return tabIndex;
	}

	public void setTabIndex(String tabIndex) {
		this.tabIndex = tabIndex;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public List<ServiceRequestActionLog> getServiceRequestActionLogs() {
		return serviceRequestActionLogs;
	}

	public void setServiceRequestActionLogs(List<ServiceRequestActionLog> serviceRequestActionLogs) {
		this.serviceRequestActionLogs = serviceRequestActionLogs;
	}

	public String getLoginPersonUnitNumber() {
		return loginPersonUnitNumber;
	}

	public void setLoginPersonUnitNumber(String loginPersonUnitNumber) {
		this.loginPersonUnitNumber = loginPersonUnitNumber;
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

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public String getServiceRequestSubject() {
		return serviceRequestSubject;
	}

	public void setServiceRequestSubject(String serviceRequestSubject) {
		this.serviceRequestSubject = serviceRequestSubject;
	}

	public String getServiceRequestDescription() {
		return serviceRequestDescription;
	}

	public void setServiceRequestDescription(String serviceRequestDescription) {
		this.serviceRequestDescription = serviceRequestDescription;
	}

	public ServiceRequestWatcher getServiceRequestWatcher() {
		return serviceRequestWatcher;
	}

	public void setServiceRequestWatcher(ServiceRequestWatcher serviceRequestWatcher) {
		this.serviceRequestWatcher = serviceRequestWatcher;
	}

	public List<ServiceRequestStatusHistory> getServiceRequestStatusHistories() {
		return serviceRequestStatusHistories;
	}

	public void setServiceRequestStatusHistories(List<ServiceRequestStatusHistory> serviceRequestStatusHistories) {
		this.serviceRequestStatusHistories = serviceRequestStatusHistories;
	}

	public List<ServiceRequestWatcher> getServiceRequestWatchers() {
		return serviceRequestWatchers;
	}

	public void setServiceRequestWatchers(List<ServiceRequestWatcher> serviceRequestWatchers) {
		this.serviceRequestWatchers = serviceRequestWatchers;
	}

	public List<ServiceRequestPriority> getServiceRequestPriorities() {
		return serviceRequestPriorities;
	}

	public void setServiceRequestPriorities(List<ServiceRequestPriority> serviceRequestPriorities) {
		this.serviceRequestPriorities = serviceRequestPriorities;
	}

	public List<AdminGroup> getAdminGroups() {
		return adminGroups;
	}

	public void setAdminGroups(List<AdminGroup> adminGroups) {
		this.adminGroups = adminGroups;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
	}

	public Boolean getIsReturnServiceRequest() {
		return isReturnServiceRequest;
	}

	public void setIsReturnServiceRequest(Boolean isReturnServiceRequest) {
		this.isReturnServiceRequest = isReturnServiceRequest;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public ServiceRequestReporterChangeHistory getsRReporterChangeHistory() {
		return sRReporterChangeHistory;
	}

	public void setsRReporterChangeHistory(ServiceRequestReporterChangeHistory sRReporterChangeHistory) {
		this.sRReporterChangeHistory = sRReporterChangeHistory;
	}

	public String getProjectTitle() {
		return projectTitle;
	}

	public void setProjectTitle(String projectTitle) {
		this.projectTitle = projectTitle;
	}

	public String getProjectId() {
		return projectId;
	}

	public void setProjectId(String projectId) {
		this.projectId = projectId;
	}

	public String getPrincipalInvestigator() {
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
	}

	public String getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(String serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public String getUserRole() {
		return userRole;
	}

	public void setUserRole(String userRole) {
		this.userRole = userRole;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getServiceRequestOutCome() {
		return serviceRequestOutCome;
	}

	public void setServiceRequestOutCome(String serviceRequestOutCome) {
		this.serviceRequestOutCome = serviceRequestOutCome;
	}

	public String getHtmlTable() {
		return htmlTable;
	}

	public void setHtmlTable(String htmlTable) {
		this.htmlTable = htmlTable;
	}

	public String getNotifyActionComment() {
		return notifyActionComment;
	}

	public void setNotifyActionComment(String notifyActionComment) {
		this.notifyActionComment = notifyActionComment;
	}

	public ServiceRequestHistory getServiceRequestHistory() {
		return serviceRequestHistory;
	}

	public void setServiceRequestHistory(ServiceRequestHistory serviceRequestHistory) {
		this.serviceRequestHistory = serviceRequestHistory;
	}

	public Boolean getIsAddAsWatcher() {
		return isAddAsWatcher;
	}

	public void setIsAddAsWatcher(Boolean isAddAsWatcher) {
		this.isAddAsWatcher = isAddAsWatcher;
	}

}