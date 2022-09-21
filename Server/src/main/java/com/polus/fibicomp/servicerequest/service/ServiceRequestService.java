package com.polus.fibicomp.servicerequest.service;

import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;

@Service
public interface ServiceRequestService {

	/**
	 * This method is used to create new ticket.
	 * @return list of modules list values.
	 */
	public String createServiceRequest();

	/**
	 * This method is used to submit ticket.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @return ticket details.
	 */
	public String submitServiceRequest(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to get ticket details.
	 * @param vo - ServiceRequestVO
	 * @return ticket details.
	 */
	public String loadServiceRequestById(Integer serviceRequestId);

	/**
	 * This method is used to save or update service request.
	 * @param vo - Object of ServiceRequestVO class.
	 * @return A string of details of a service request.
	 */
	public String saveOrUpdateServiceRequest(ServiceRequestVO vo);

	/**
	 * This method is used to add attachments for a service request.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @param request 
	 * @return A String of details of service request data with list of attachments.
	 */
	public String addServiceRequestCommentAndAttachment(MultipartFile[] files, String formDataJSON, HttpServletRequest request);

	/**
	 * This method is used to delete attachments of a service request.
	 * @param vo - Object of ServiceRequestVO class.
	 * @return A String of details of service request data with list of attachments.
	 */
	public String deleteServiceRequestAttachment(ServiceRequestVO vo);

	/**
	 * This method is used to download service request attachment by id.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadServiceRequestAttachment(Integer attachmentId);

	/**
	 * This method is used to add watcher of a service request.
	 * @param vo - Object of ServiceRequestVO class.
	 * @return A String of details of service request data with list of watchers.
	 */
	public String saveServiceRequestWatcher(ServiceRequestVO vo);

	/**
	 * This method is used to get comments and attachment history based on update time stamp.
	 * @param serviceRequestId - Service request ID.
	 * @return Details of comments and attachments of a service request .
	 */
	public String getSRCommentsAndAttachments(Integer serviceRequestId);

	/**
	 * This method is used to delete watcher of a service request.
	 * @param vo - Object of ServiceRequestVO class.
	 * @return A String of details of service request data with list of attachments.
	 */
	public String deleteServiceRequestWatcher(Integer watcherId);

	/**
	 * This method is used to send service request notification.
	 * @param vo - Object of ServiceRequestVO class.
	 * @param notificationTypeId
	 * @param dynamicEmailrecipients
	 * @param approveComment 
	 * @return ServiceRequestVO.
	 */
	public ServiceRequestVO sendServiceRequestNotification(ServiceRequestVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailrecipients, String approveComment);

	/**
	 * This method is used to getServiceRequestTypes.
	 * @return A String of details of service request types.
	 */
	public String getServiceRequestTypes();

	/**
	 * This method is used to set service request details for award notification.
	 * @param award - Object of Award class.
	 * @param piAwardPerson - Object of AwardPerson class.
	 * @param userRole - userRole.
	 * @param notificationTypeId
	 * @param dynamicEmailrecipients
	 * @return ServiceRequestVO.
	 */
	public ServiceRequestVO setServiceRequestDetailsForAward(Award award, AwardPerson piAwardPerson, String userRole);

	/**
	 * This method is used to save Service Request Action logs.
	 * @param serviceRequest 
	 * @param actionTypeCode 
	 * @param statusCode
	 * @param updateUser
	 * @return A String response.
	 */
	public Integer saveServiceRequestActionLog(Integer actionTypeCode, ServiceRequest serviceRequest);

	/**
	 * @param moduleCode
	 * @return
	 */
	String getServiceRequestTypesBasedOnModule(Integer moduleCode);

	/**
	 * @param files
	 * @param serviceRequest
	 * @param newAttachments
	 * @param actionLogId
	 */
	void addServiceRequestAttachments(MultipartFile[] files, ServiceRequest serviceRequest,
			List<ServiceRequestAttachment> newAttachments, Integer actionLogId);

	/**
	 * @param newAttachment
	 * @param file
	 * @param fileName
	 * @param versionNumber
	 * @param documentId
	 * @return
	 */
	ServiceRequestAttachment addNewServiceRequestAttachment(ServiceRequestAttachment newAttachment, MultipartFile file,
			String fileName, Integer versionNumber, Integer documentId);

	/**
	 * @param serviceRequestVO
	 */
	void canServiceRequestTakeRoutingAction(ServiceRequestVO serviceRequestVO);

	/**
	 * @param serviceRequest
	 * @param actionLogId
	 */
	void updateSRStatusHistory(ServiceRequest serviceRequest, Integer actionLogId);

	/**
	 * @param serviceRequest
	 * @return
	 */
	ServiceRequest updateSRStatusAndGroupId(ServiceRequest serviceRequest);

	/**
	 * @param files
	 * @param formDataJSON
	 * @return
	 */
	String assignReviewer(MultipartFile[] files, String formDataJSON);

	/**
	 * @param files
	 * @param formDataJson
	 * @return
	 */
	String resolveServiceRequest(MultipartFile[] files, String formDataJson);

	/**
	 * @param vo
	 * @return
	 */
	String updateSRReporter(ServiceRequestVO vo);

	/**
	 * @param serviceRequestId
	 * @return
	 */
	String loadServiceRequestHistory(Integer serviceRequestId);

	/**
	 * @param vo
	 */
	void loadServiceRequestInDetail(ServiceRequestVO vo);

	/**
	 * @param notificationId
	 * @param emailIds
	 * @param userName
	 * @param comment
	 * @param serviceRequest
	 * @param moduleItemKey
	 */
	void sendMailForServiceRequestActions(Integer notificationId, Set<String> emailIds,ServiceRequestVO serviceRequestVO);

	/**
	 * @param emailRecepients
	 * @param serviceRequest
	 */
	void sendStatusUpdateMailForServiceRequest(Set<String> emailRecepients, ServiceRequest serviceRequest);

	/**
	 * @param serviceRequest
	 * @param actionTypeCode
	 * @param statusCode
	 */
	ServiceRequest updateSRStatusAndActionLog(ServiceRequest serviceRequest, Integer actionTypeCode, Integer statusCode);

	/**
	 * @param vo
	 * @return
	 */
	String deleteServiceRequestAttachmentFromAward(ServiceRequestVO vo);

	/**
	 * @param moduleCode
	 * @param moduleItemKey
	 * @return
	 */
	ModuleDetails prepareModuleDetails(Integer moduleCode, Integer moduleItemKey);

	/**
	 * @param vo
	 * @return
	 */
	ServiceRequestVO loadInitialData();

}
