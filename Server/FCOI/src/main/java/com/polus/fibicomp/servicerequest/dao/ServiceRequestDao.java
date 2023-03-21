package com.polus.fibicomp.servicerequest.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.pojo.Module;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionLog;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestComment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestPriority;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestProcessFlow;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestProject;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestReporterChangeHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestRoleType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatus;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatusHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestWatcher;

/**
 * @author sajith.m
 *
 */
@Service
public interface ServiceRequestDao {

	/**
	 * This method is used to create ticket.
	 * @param proposal - Object of Proposal class.
	 * @return Set of values to create ticket.
	 */
	public ServiceRequest saveOrUpdateServiceRequest(ServiceRequest serviceRequest);

	/**
	 * This method is used to create action log.
	 * @param serviceRequestActionLog - Object of serviceRequestActionLog class.
	 * @return serviceRequestActionLog object.
	 */
	public ServiceRequestActionLog saveActionLog(ServiceRequestActionLog serviceRequestActionLog);

	/**
	 * This method is used to get ticket details based on id.
	 * @param ticketId - ticket id.
	 * @return ticket details.
	 */
	public ServiceRequest fetchServiceRequestById(Integer ticketId);

	/**
	 * This method is used to get list of service request types.
	 * @return service request types.
	 */
	public List<ServiceRequestType> getServiceRequestTypes();

	/**
	 * This method is used to get list of service request action types.
	 * @return service request types.
	 */
	public List<ServiceRequestActionType> getServiceRequestActionTypes();

	/**
	 * This method is used to fetch status based on status code.
	 * @param statusCode - status code of the proposal.
	 * @return An object of proposal status.
	 */
	public ServiceRequestStatus fetchStatusByStatusCode(Integer statusCode);

	/**
	 * This method is used to fetch user role based on role code.
	 * @param statusCode - role code of the proposal.
	 * @return An object of user role.
	 */
	public ServiceRequestRoleType fetchUserRoleByRoleCode(String roleCode);

	/**
	 * This method is used to fetch attachment based on Id.
	 * @param attachmentId - Id of the attachment.
	 * @return An attachment object.
	 */
	public ServiceRequestAttachment getAttachmentById(Integer attachmentId);

	/**
	 * This method is used to delete service request.
	 * @param serviceRequest
	 */
	public ServiceRequest deleteServiceRequest(ServiceRequest serviceRequest);

	/**
	 * This method is used to delete service request comment.
	 * @param serviceRequestComment - serviceRequestComment.
	 */	
	public void deleteServiceRequestComment(ServiceRequestComment serviceRequestComment);

	/**
	 * This method is used to get module details based on module code.
	 * @param moduleCode - moduleCode.
	 * return module details.
	 */
	public Module getServiceRequestModuleDetails(Integer moduleCode);

	/**
	 * This method is used to get attachments based on action log id.
	 * @param actionLogId - actionLogId.
	 * return attachment list.
	 */
	public List<ServiceRequestAttachment> getAttachmentsBasedOnActionLog(Integer actionLogId);

	/**
	 * This method is used to get comment based on action log id.
	 * @param actionLogId - actionLogId.
	 * return comment.
	 */
	public List<ServiceRequestComment> getCommentBasedOnActionLog(Integer actionLogId);

	/**
	 * This method is used to get action logs based on service request id.
	 * @param actionLogId - actionLogId.
	 * return action logs list.
	 */
	public List<ServiceRequestActionLog> fetchActionLogByServiceRequestId(Integer serviceRequestId);

	/**
	 * This method is used to get status history based on action log id.
	 * @param actionLogId - actionLogId.
	 * return status history.
	 */
	public ServiceRequestStatusHistory fetchstatusHistoryByActionLogId(Integer actionLogId);

	/**
	 * This method is used to get history based on action log id.
	 * @param actionLogId - actionLogId.
	 * return history.
	 */
	public ServiceRequestHistory fetchServiceRequestHistoryByActionLogId(Integer actionLogId);

	/**
	 * This method is used to get process flow based on action log id.
	 * @param actionLogId - actionLogId.
	 * return process flow.
	 */
	public ServiceRequestProcessFlow getProcessFlowByActionLogId(Integer actionLogId);

	/**
	 * This method is used to delete action log.
	 * @param actionLogId - actionLogId
	 */
	public void deleteActionLog(ServiceRequestActionLog serviceRequestActionLog);

	/**
	 * This method is used to get action log by id.
	 * @param actionLogId - actionLogId
	 */
	public ServiceRequestActionLog fetchActionLogById(Integer actionLogId);

	/**
	 * This method ServiceRequestType by id.
	 * @param typeCode - typeCode
	 * return ServiceRequestType object
	 */
	public ServiceRequestType fetchServiceRequestTypeById(String typeCode);

	/**
	 * This method is used to get service request by module item key and module code.
	 * @param moduleCode - moduleCode
	 * @param moduleItemKey - moduleItemKey
	 * return ServiceRequest object
	 */
	public ServiceRequest fetchServiceRequestByModuleCodeAndKey(Integer moduleCode, String moduleItemKey);

	/**
	 * This method is used to saveOrUpdateServiceRequestComment.
	 * @param proposal - Object of serviceRequestComment.
	 * @return Object of serviceRequestComment.
	 */
	public ServiceRequestComment saveOrUpdateServiceRequestComment(ServiceRequestComment serviceRequestComment);

	/**
	 * This method is used to get the service request by originated award id
	 * @param moduleCode
	 * @param originatedAwardId
	 * @return object of service request
	 */
	public ServiceRequest fetchServiceRequestByOriginatedAward(Integer moduleCode, String originatedAwardId);

	/**
	 * This method is used to deleteServiceRequestAttachment
	 * @param serviceRequestAttachment
	 */
	public ServiceRequestAttachment deleteServiceRequestAttachment(ServiceRequestAttachment serviceRequestAttachment);

	/**
	 * @param modulecode
	 * @return
	 */
	List<ServiceRequestType> getServiceRequestTypesBasedOnModuleCode(Integer modulecode);

	/**
	 * @param proposalId
	 * @return
	 */
	List<ServiceRequestAttachment> fetchServiceRequestAttachmentBasedOnSRId(Integer serviceRequestId);

	/**
	 * @param serviceRequestAttachment
	 * @return
	 */
	ServiceRequestAttachment saveOrUpdateServiceRequestAttachment(ServiceRequestAttachment serviceRequestAttachment);

	/**
	 * @param attachmentId
	 * @return
	 */
	ServiceRequestAttachment fetchServiceRequestAttachmentById(Integer attachmentId);

	/**
	 * @param serviceRequestStatusHistory
	 * @return
	 */
	ServiceRequestStatusHistory saveOrUpdateServiceRequestStatusHistory(ServiceRequestStatusHistory serviceRequestStatusHistory);

	/**
	 * @param serviceRequestHistory
	 * @return
	 */
	ServiceRequestHistory saveServiceRequestHistory(ServiceRequestHistory serviceRequestHistory);

	/**
	 * @param serviceRequestId
	 * @param actionTypeCodes
	 * @return
	 */
	public List<ServiceRequestActionLog> fetchActionLogByServiceRequestIdAndActionTypes(Integer serviceRequestId,
			List<Integer> actionTypeCodes);

	/**
	 * @param commentIds
	 * @return
	 */
	List<ServiceRequestComment> getCommentBasedOnActionLogIdIds(List<Integer> commentIds, Boolean isViewPrivateComment);

	/**
	 * @param attachmentIds
	 * @return
	 */
	List<ServiceRequestAttachment> getAttachmentBasedOnActionLogIdIds(List<Integer> attachmentIds);

	/**
	 * @param serviceRequestProcessFlow
	 * @return
	 */
	ServiceRequestProcessFlow saveOrUpdateServiceRequestProcessFlow(ServiceRequestProcessFlow serviceRequestProcessFlow);

	/**
	 * @param serviceRequestProject
	 * @return
	 */
	ServiceRequestProject saveOrUpdateServiceRequestProject(ServiceRequestProject serviceRequestProject);

	/**
	 * @param serviceRequestId
	 * @param statusCode
	 */
	void updateSRStatusAndAdminGroupId(Integer serviceRequestId, Integer statusCode, Integer adminGroupId);

	/**
	 * @param statusCode
	 * @return
	 */
	ServiceRequestStatus getServiceRequestStatus(Integer statusCode);

	/**
	 * @param serviceRequestWatcher
	 * @return object of ServiceRequestWatcher
	 */
	ServiceRequestWatcher saveServiceRequestWatcher(ServiceRequestWatcher serviceRequestWatcher);

	/**
	 * @param serviceRequestId
	 */
	 void updateStatusHistoryEndTime(Integer serviceRequestId);

	/**
	 * @param serviceRequestId
	 * @return
	 */
	List<ServiceRequestStatusHistory> fetchstatusHistoryBySRHeaderId(Integer serviceRequestId);

	/**
	 * @param serviceRequestId
	 * @return
	 */
	List<ServiceRequestWatcher> fetchSRWatchersBySRHeaderId(Integer serviceRequestId);

	/**
	 * @param serviceRequestWatcher
	 * @return
	 */
	ServiceRequestWatcher deleteServiceRequestWatcher(ServiceRequestWatcher serviceRequestWatcher);

	/**
	 * @param watcherId
	 * @return
	 */
	ServiceRequestWatcher fetchServiceRequestWatcherById(Integer watcherId);

	/**
	 * @return List of ServiceRequestPriority.
	 */
	List<ServiceRequestPriority> getServiceRequestPriority();

	/**
	 * @param actionTypeCode
	 * @return
	 */
	ServiceRequestActionType fetchServiceRequestActionTypeById(Integer actionTypeCode);

	/**
	 * @param typeCode
	 * @return
	 */
	Integer fetchAdminGroupIdBasedOnSRType(String typeCode);

	/**
	 * @param serviceRequestStatusHistory
	 * @return
	 */
	ServiceRequestReporterChangeHistory saveOrUpdateSRReporterChange(ServiceRequestReporterChangeHistory serviceRequestReporterChangeHistory);

	/**
	 * @param previousModuleItemKey
	 * @param moduleItemKey
	 * @param moduleCode
	 * @param serviceRequestId
	 */
	void updateSRProject(String previousModuleItemKey, String moduleItemKey, Integer moduleCode, Integer serviceRequestId);

	/**
	 * @param priorityId
	 * @return
	 */
	ServiceRequestPriority fetchSRPriorityById(Integer priorityId);

	/**
	 * This method is used to get action logs based on service request id.
	 * @param actionLogId - actionLogId.
	 * return action logs list.
	 */
	List<ServiceRequestHistory> fetchServiceRequestHistoryByActionLogIds(List<String> actionLogIds);

	/**
	 * @param serviceRequestId
	 * @return
	 */
	List<String> getSRWatcherIds(Integer serviceRequestId);

	/**
	 * @param serviceRequestId
	 * @return
	 */
	List<Integer> getPrivateCommentActionLogIds(Integer serviceRequestId);

	/**
	 * @param serviceRequestId
	 * @param statusCode
	 * @return
	 */
	String getPreviousServiceRequestStatus(Integer serviceRequestId, Integer statusCode);

	/**
	 * @param serviceRequestId
	 * @return
	 */
	Integer getDocumentIdOfSRAttachment(Integer serviceRequestId);

}
