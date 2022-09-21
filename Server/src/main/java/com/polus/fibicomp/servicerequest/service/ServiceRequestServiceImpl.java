package com.polus.fibicomp.servicerequest.service;

import java.io.File;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionLog;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestComment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestProcessFlow;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestProject;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatusHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestWatcher;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Service(value = "serviceRequestService")
public class ServiceRequestServiceImpl implements ServiceRequestService {

	protected static Logger logger = LogManager.getLogger(ServiceRequestServiceImpl.class.getName());

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	public BusinessRuleDao businessRuleDao;

	@Autowired
	private PrintService printService;

	@Autowired
	public AuthorizationService authorizationService;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	private InboxService inboxService;

	@Autowired
	private InboxDao inboxDao;

	@Override
	public String createServiceRequest() {
		ServiceRequestVO vo = new ServiceRequestVO();
		ServiceRequest serviceRequest = new ServiceRequest();
		serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT);
		serviceRequest.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT));
		serviceRequest.setReporterPersonId(AuthenticatedUser.getLoginPersonId());
		serviceRequest.setPriorityId(Constants.SERVICE_REQUEST_PRIORITY_NORMAL);
		serviceRequest.setServiceRequestPriority(serviceRequestDao.fetchSRPriorityById(Constants.SERVICE_REQUEST_PRIORITY_NORMAL));
		vo.setServiceRequest(serviceRequest);
		loadInitialData();
		return commonDao.convertObjectToJSON(vo);
	}


	@Override
	public String loadServiceRequestById(Integer serviceRequestId) {
		ServiceRequestVO vo = new ServiceRequestVO();
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setServiceRequestId(serviceRequestId);
		if (serviceRequestId != null) {
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(serviceRequestId);
			setServiceRequestFullNames(serviceRequest);
			vo.setServiceRequest(serviceRequest);
			vo.setAcType("U");
		}
		loadServiceRequestInDetail(vo);
		canServiceRequestTakeRoutingAction(vo);
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(serviceRequestId.toString(),Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			vo.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(serviceRequestId.toString(), Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				vo.setWorkflowList(workFlows);
			}
		}
		vo.setCanApproveRouting(businessRuleDao.canApproveRouting(serviceRequestId.toString(), vo.getPersonId(), Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE).toString());
		return commonDao.convertObjectToJSON(vo);
	}

	private ServiceRequest setServiceRequestFullNames(ServiceRequest serviceRequest) {
		if (serviceRequest.getCreateUser() != null) {
			serviceRequest.setCreateUserFullName(personDao.getUserFullNameByUserName(serviceRequest.getCreateUser()));
		}
		if (serviceRequest.getUpdateUser() != null) {
			serviceRequest.setUpdateUserFullName(personDao.getUserFullNameByUserName(serviceRequest.getUpdateUser()));
		}
		if (serviceRequest.getReporterPersonId() != null) {
			serviceRequest.setReporterPersonName(personDao.getPersonFullNameByPersonId(serviceRequest.getReporterPersonId()));
		}
		if (serviceRequest.getAssigneePersonId() != null) {
			serviceRequest.setAssigneePersonName(personDao.getPersonFullNameByPersonId(serviceRequest.getAssigneePersonId()));
		}
		return serviceRequest;
	}

	@Override
	public ServiceRequestVO loadInitialData() {
		ServiceRequestVO vo = new ServiceRequestVO();
		try {
			vo.setModuleList(commonDao.getModules());
			vo.setServiceRequestTypes(serviceRequestDao.getServiceRequestTypes());
			vo.setServiceRequestActionTypes(serviceRequestDao.getServiceRequestActionTypes());
			vo.setServiceRequestPriorities(serviceRequestDao.getServiceRequestPriority());
			vo.setAdminGroups(commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.SERVICE_REQUEST_MODULE_CODE));
		} catch (Exception e) {
			logger.error("Exception in loadInitialData :{}", e.getMessage());
		}
		return vo;
	}

	@Override
	public void loadServiceRequestInDetail(ServiceRequestVO vo) {
		vo.setServiceRequestStatusHistories(serviceRequestDao.fetchstatusHistoryBySRHeaderId(vo.getServiceRequest().getServiceRequestId()));
		vo.setServiceRequestWatchers(fetchSRWatchers(vo.getServiceRequest().getServiceRequestId()));
		if (vo.getServiceRequest().getModuleCode() != null && vo.getServiceRequest().getModuleItemKey() != null) {
			vo.getServiceRequest().setModuleDetails(prepareModuleDetails(vo.getServiceRequest().getModuleCode(), Integer.parseInt(vo.getServiceRequest().getModuleItemKey())));
		}
		if (AuthenticatedUser.getLoginPersonUnit() != null) {
			vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.SERVICE_REQUEST_MODULE_CODE, AuthenticatedUser.getLoginPersonId(), vo.getServiceRequest().getUnitNumber(), vo.getServiceRequestId()));
		}
	}

	private List<ServiceRequestWatcher> fetchSRWatchers(Integer serviceRequestId) {
		List<ServiceRequestWatcher> srWatchers = serviceRequestDao.fetchSRWatchersBySRHeaderId(serviceRequestId);
		Set<String> personIds = srWatchers.stream().map(ServiceRequestWatcher::getWatcherPersonId).collect(Collectors.toSet());
		if(!personIds.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByPersonId(new ArrayList<>(personIds));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(Person :: getPersonId, Person :: getFullName));
			srWatchers.stream().filter(srWatcher -> srWatcher.getWatcherPersonId() != null).filter(srWatcher -> collect.containsKey(srWatcher.getWatcherPersonId())).forEach(srWatcher -> srWatcher.setWatcherName(collect.get(srWatcher.getWatcherPersonId())));
		}
		return srWatchers;
	}

	@Override
	public String saveOrUpdateServiceRequest(ServiceRequestVO vo) {
		ServiceRequest serviceRequest = vo.getServiceRequest();
		if (serviceRequest.getServiceRequestId() == null) {
			serviceRequest.setIsSystemGenerated(Boolean.FALSE);
			serviceRequest.setReporterPersonId(AuthenticatedUser.getLoginPersonId());
			serviceRequest.setReporterPersonName(AuthenticatedUser.getLoginUserFullName());
			serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
			serviceRequest.setUnit(commonDao.getLeadUnitByUnitNumber(serviceRequest.getUnitNumber()));
			Integer actionLogId = saveServiceRequestActionLog(Constants.NEW_REQUEST_ACTION_CODE, serviceRequest);
			setProcessFlowData(actionLogId, serviceRequest);
			addServiceRequestStatusHistory(serviceRequest, actionLogId);
			if (serviceRequest.getModuleItemKey() != null) {
				addServiceRequestProject(serviceRequest);
			}
		} else {
			serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
			if (vo.getServiceRequestHistory() != null) {
				vo.getServiceRequestHistory().setActionLogId(saveServiceRequestActionLog(Constants.EDIT_REQUEST_ACTION_CODE, serviceRequest));
				if (vo.getServiceRequestHistory().getModuleItemKey() != null) {
					serviceRequestDao.updateSRProject(vo.getServiceRequestHistory().getPreviousModuleItemKey(), vo.getServiceRequestHistory().getModuleItemKey(), 
							vo.getServiceRequestHistory().getModuleCode(), vo.getServiceRequestHistory().getServiceRequestId());
				}
				serviceRequestDao.saveServiceRequestHistory(vo.getServiceRequestHistory());
			}
		}
		vo.setServiceRequest(serviceRequest);
		setServiceRequestFullNames(serviceRequest);
		loadServiceRequestInDetail(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void addServiceRequestProject(ServiceRequest serviceRequest) {
		ServiceRequestProject serviceRequestProject = new ServiceRequestProject();
		serviceRequestProject.setServiceRequestId(serviceRequest.getServiceRequestId());
		serviceRequestProject.setModuleCode(serviceRequest.getModuleCode());
		serviceRequestProject.setModuleItemKey(serviceRequest.getModuleItemKey());
		serviceRequestDao.saveOrUpdateServiceRequestProject(serviceRequestProject);
	}

	private void addServiceRequestStatusHistory(ServiceRequest serviceRequest, Integer actionLogId) {
		ServiceRequestStatusHistory serviceRequestStatusHistory = new ServiceRequestStatusHistory();
		serviceRequestStatusHistory.setActionLogId(actionLogId);
		serviceRequestStatusHistory.setActionStartTime(commonDao.getCurrentTimestamp());
		serviceRequestStatusHistory.setStatusCode(serviceRequest.getStatusCode());
		serviceRequestStatusHistory.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(serviceRequest.getStatusCode()));
		serviceRequestStatusHistory.setServiceRequestId(serviceRequest.getServiceRequestId());
		serviceRequestDao.saveOrUpdateServiceRequestStatusHistory(serviceRequestStatusHistory);		
	}

	@Override
	public String addServiceRequestCommentAndAttachment(MultipartFile[] files, String formDataJSON, HttpServletRequest request) {
		ServiceRequestVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ServiceRequestVO.class);
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId());
			Integer actionLogId = saveServiceRequestActionLog(Constants.ADD_COMMENT_ACTION_CODE, serviceRequest);
			if (vo.getServiceRequestComment() != null) {
				addServiceRequestComments(vo.getServiceRequestComment(), serviceRequest, actionLogId);
			}
			if (vo.getNewAttachments() != null && !vo.getNewAttachments().isEmpty()) {
				addServiceRequestAttachments(files, serviceRequest, vo.getNewAttachments(), actionLogId);
			}
		} catch (Exception e) {
			logger.info("Error while adding SR comments and attachments");
			e.printStackTrace();
		}
		return (vo != null && vo.getServiceRequestId() != null) ? getSRCommentsAndAttachments(vo.getServiceRequestId()): commonDao.convertObjectToJSON("No comments & attachments found");
	}

	private void addServiceRequestComments(ServiceRequestComment serviceRequestComment, ServiceRequest serviceRequest,
			Integer actionLogId) {
		serviceRequestComment.setActionLogId(actionLogId);
		serviceRequestComment.setServiceRequestId(serviceRequest.getServiceRequestId());
		serviceRequestDao.saveOrUpdateServiceRequestComment(serviceRequestComment);
	}
	
	@Override
	public void addServiceRequestAttachments(MultipartFile[] files, ServiceRequest serviceRequest,
			List<ServiceRequestAttachment> newAttachments, Integer actionLogId) {
		Integer documentId = serviceRequestDao.getDocumentIdOfSRAttachment(serviceRequest.getServiceRequestId());
		Integer versionNumber = 0;
		for (int i = 0; i < files.length; i++) {
			for (ServiceRequestAttachment newAttachment : newAttachments) {
				File file = new File(files[i].getOriginalFilename());
				String fileName = file.getName();
				if (newAttachment.getFileName().equals(fileName)) {
					documentId = documentId + 1;
					ServiceRequestAttachment serviceRequestAttachment = addNewServiceRequestAttachment(newAttachment,
							files[i], fileName, versionNumber, documentId);
					serviceRequestAttachment.setServiceRequestId(serviceRequest.getServiceRequestId());
					serviceRequestAttachment.setActionLogId(actionLogId);
					serviceRequestDao.saveOrUpdateServiceRequestAttachment(serviceRequestAttachment);
				}
			}
		}
	}

	@Override
	public ServiceRequestAttachment addNewServiceRequestAttachment(ServiceRequestAttachment newAttachment,
			MultipartFile file, String fileName, Integer versionNumber, Integer documentId) {
		ServiceRequestAttachment serviceRequestAttachment = new ServiceRequestAttachment();
		try {
			if (newAttachment.getFileName().equals(fileName)) {
				serviceRequestAttachment.setFileName(fileName);
				serviceRequestAttachment.setContentType(newAttachment.getContentType());
				serviceRequestAttachment.setVersionNumber(versionNumber + 1);
				FileData fileData = new FileData();
				fileData.setAttachment(file.getBytes());
				fileData = commonDao.saveFileData(fileData);
				serviceRequestAttachment.setFileDataId(fileData.getFileDataId());
				serviceRequestAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
				serviceRequestAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
				serviceRequestAttachment.setDocumentId(documentId);
			}
		} catch (Exception e) {
			logger.info("Error occured in addNewServiceRequestAttachment : {}", e.getMessage());
		}
		return serviceRequestAttachment;
	}

	private ServiceRequestStatusHistory setStatusHistory(ServiceRequest serviceRequest, Integer actionLogId) {
		ServiceRequestStatusHistory statusHistory = new ServiceRequestStatusHistory();
		statusHistory.setActionLogId(actionLogId);
		statusHistory.setStatusCode(serviceRequest.getStatusCode());
		statusHistory.setActionStartTime(commonDao.getCurrentTimestamp());
		statusHistory.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(serviceRequest.getStatusCode()));
		statusHistory.setServiceRequestId(serviceRequest.getServiceRequestId());
		if (serviceRequest.getStatusCode().equals(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVED)) {
			statusHistory.setAdminGroupId(serviceRequest.getAdminGroupId());
			statusHistory.setAdminGroup(serviceRequest.getAdminGroup());
		}
		return serviceRequestDao.saveOrUpdateServiceRequestStatusHistory(statusHistory);
	}

	private ServiceRequestProcessFlow setProcessFlowData(Integer actionLogId, ServiceRequest serviceRequest) {
		ServiceRequestProcessFlow processFlow = new ServiceRequestProcessFlow();
		processFlow.setProcessStartTimestamp(serviceRequest.getUpdateTimestamp());
		processFlow.setActionLogId(actionLogId);
		processFlow.setStatusCode(serviceRequest.getStatusCode());
		processFlow.setServiceRequestId(serviceRequest.getServiceRequestId());
		return serviceRequestDao.saveOrUpdateServiceRequestProcessFlow(processFlow);
	}

	@Override
	public String deleteServiceRequestAttachment(ServiceRequestVO vo) {
		try {
			ServiceRequestAttachment serviceRequestAttachment = serviceRequestDao
					.getAttachmentById(vo.getAttachmentId());
			if (serviceRequestAttachment != null) {
				if (serviceRequestAttachment.getFileDataId() != null) {
					commonDao.deleteFileData(commonDao.getFileDataById(serviceRequestAttachment.getFileDataId()));
				}
				serviceRequestDao.deleteServiceRequestAttachment(serviceRequestAttachment);
			}
			vo.setMessage("Attachment deleted sucessfully");
		} catch (Exception e) {
			vo.setMessage("Problem occurred in deleting attachment");
			logger.error("Exception in method deleteServiceRequestAttachment : {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<byte[]> downloadServiceRequestAttachment(Integer attachmentId) {
		logger.info("-------- downloadServiceRequestAttachment ---------");
		ServiceRequestAttachment attachment = serviceRequestDao.getAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		FileData fileData = null;
		if(attachment != null) {
			fileData = commonDao.getFileDataById(attachment.getFileDataId());
		}
		try {
			if(fileData != null) {
				attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
			}
		} catch (Exception e) {
			logger.error("Exception in method downloadServiceRequestAttachment : {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String getSRCommentsAndAttachments(Integer serviceRequestId) {
		List<Integer> actionTypeCodes = new ArrayList<>();
		actionTypeCodes.add(Constants.NEW_REQUEST_ACTION_CODE);
		actionTypeCodes.add(Constants.ADD_COMMENT_ACTION_CODE);
		actionTypeCodes.add(Constants.ADD_ATTACHMENT_ACTION_CODE);
		actionTypeCodes.add(Constants.SUBMIT_ACTION_CODE);
		actionTypeCodes.add(Constants.ASSIGN_ACTION_CODE);
		actionTypeCodes.add(Constants.ASSIGN_TO_ME_ACTION_CODE);
		actionTypeCodes.add(Constants.RETURN_BY_REPORTER_LOG_ACTION_CODE);
		actionTypeCodes.add(Constants.RESOLVED_ACTION_CODE);
		actionTypeCodes.add(Constants.RESUBMIT_ACTION_CODE);
		List<ServiceRequestActionLog> actionLogs = serviceRequestDao.fetchActionLogByServiceRequestIdAndActionTypes(serviceRequestId, actionTypeCodes);
		Set<Integer> srCommentActionLogIds = new HashSet<>();
		Set<Integer> srAttachmentActionLogIds = new HashSet<>();
		actionLogs.forEach(actionLog -> {
				srCommentActionLogIds.add(actionLog.getActionLogId());
				srAttachmentActionLogIds.add(actionLog.getActionLogId());
			});
		Map<Integer, List<ServiceRequestComment>> srComments = new HashMap<>();
		Map<Integer, List<ServiceRequestAttachment>> srAttachments = new HashMap<>();
		Map<Integer, Map<String, Object>> commentsAndAttachments = new HashMap<>();
		Map<Integer, String> actionLogMap = actionLogs.stream().collect
				(Collectors.toMap(ServiceRequestActionLog :: getActionLogId, 
						actionLog -> actionLog.getServiceRequestActionType().getDescription()));
		if (!srCommentActionLogIds.isEmpty()) {
			 srComments = getSRCommentsBasedOnCommentIds(new ArrayList<>(srCommentActionLogIds), serviceRequestId)
					.stream().collect(Collectors.groupingBy(ServiceRequestComment::getActionLogId));
		}
		if (!srAttachmentActionLogIds.isEmpty()) {
			srAttachments = getSRAttachmentsBasedOnAttachmenIds(new ArrayList<>(srAttachmentActionLogIds))
					.stream().collect(Collectors.groupingBy(ServiceRequestAttachment::getActionLogId));	
		}
		for (Map.Entry<Integer, List<ServiceRequestComment>> srComment : srComments.entrySet()) {
			Map<String, Object> commentsAndAttachment = new HashMap<>();
			commentsAndAttachment.put("comment", srComment.getValue());
			if (srAttachments.containsKey(srComment.getKey())) {
				commentsAndAttachment.put("attachment", srAttachments.get(srComment.getKey()));
			}
			commentsAndAttachment.put("actionTypeDescription", actionLogMap.get(srComment.getKey()));
			commentsAndAttachments.put(srComment.getKey(), commentsAndAttachment);
		}
		List<Integer> privateActionLogIds = serviceRequestDao.getPrivateCommentActionLogIds(serviceRequestId);
		for (Map.Entry<Integer, List<ServiceRequestAttachment>> srComment : srAttachments.entrySet()) {
			Map<String, Object> commentsAndAttachment = new HashMap<>();
			if (!commentsAndAttachments.containsKey(srComment.getKey()) && !privateActionLogIds.contains(srComment.getKey())) {
				commentsAndAttachment.put("comment", new ServiceRequestComment());
				commentsAndAttachment.put("attachment", srComment.getValue());
				commentsAndAttachment.put("actionTypeDescription", actionLogMap.get(srComment.getKey()));
				commentsAndAttachments.put(srComment.getKey(), commentsAndAttachment);
			}
		}
		return commonDao.convertObjectToJSON(commentsAndAttachments);
	}

	private void setStatusHistoryEndDate(ServiceRequest serviceRequest) {
		serviceRequestDao.updateStatusHistoryEndTime(serviceRequest.getServiceRequestId());
	}

	/*private void getFullNameOfUpdateUser(List<ServiceRequestActionLog> actionLogDetails) {
		Set<String> userName = actionLogDetails.stream().map(ServiceRequestActionLog::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			actionLogDetails.stream().filter(item -> item.getUpdateUser() != null).filter(actionLogDetail -> collect.containsKey(actionLogDetail.getUpdateUser().toUpperCase())).forEach(actionLogDetail -> actionLogDetail.setUpdateUserFullName(collect.get(actionLogDetail.getUpdateUser().toUpperCase())));
		}	
	}*/

	@Override
	public String deleteServiceRequestWatcher(Integer watcherId) {
		try {
			serviceRequestDao.deleteServiceRequestWatcher(serviceRequestDao.fetchServiceRequestWatcherById(watcherId));
			return commonDao.convertObjectToJSON("SR watcher deleted sucessfully");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("Failed to delete watcher");
		}
	}

	@Override
    public void sendMailForServiceRequestActions(Integer notificationId, Set<String> emailIds,ServiceRequestVO serviceRequestVO) {
        Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
        emailIds.forEach(emailId -> 
            commonService.setNotificationRecipients(emailId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients)
        );
        sendServiceRequestNotification(serviceRequestVO, notificationId, dynamicEmailrecipients, null);
    }
	
	@Override
	public ServiceRequestVO sendServiceRequestNotification(ServiceRequestVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients,String approveComment) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.SERVICE_REQUEST_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.getServiceRequest().getServiceRequestId().toString());
		emailServiceVO.setPlaceHolder(getServiceRequestPlaceholders(vo, approveComment));
		emailServiceVO.setSubModuleCode((Constants.SERVICE_REQUEST_SUBMODULE_CODE).toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && Boolean.TRUE.equals(emailServiceVO.getPrompted())) {
			vo.setNotificationTypeId(notificationTypeId);
			vo.setBody(emailServiceVO.getBody());
			vo.setSubject(emailServiceVO.getSubject());
		}
		return vo;
	}

	private Map<String, String> getServiceRequestPlaceholders(ServiceRequestVO vo, String approveComment) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{TABLE}", vo.getHtmlTable() == null ? "" : vo.getHtmlTable());
		placeHolder.put("{USER_NAME}", vo.getPersonName() == null ? "" : vo.getPersonName());
		placeHolder.put("{MESSAGE}", vo.getMessage() == null ? "" : vo.getMessage());
		placeHolder.put("{COMMENT}", vo.getNotifyActionComment() == null ? "" : vo.getNotifyActionComment());
		placeHolder.put("{MODULE_ITEM_KEY}", vo.getModuleItemKey() == null ? "" : vo.getModuleItemKey());
		placeHolder.put("{USER_ROLE}", vo.getUserRole() == null ? "" : vo.getUserRole());
		placeHolder.put("{PROJECT_TITLE}", vo.getProjectTitle() == null ? "" : vo.getProjectTitle());
		placeHolder.put("{PROJECT_ID}", vo.getProjectId() == null ? "" : vo.getProjectId());
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", vo.getPrincipalInvestigator() == null ? "" : vo.getPrincipalInvestigator());
		placeHolder.put("{SERVICE_REQUEST_TYPE}", vo.getServiceRequestType() == null ? "" : vo.getServiceRequestType());
		placeHolder.put("{SERVICE_REQUEST_OUTCOME}", vo.getServiceRequestOutCome() == null ? "" : vo.getServiceRequestOutCome());
		placeHolder.put("{SPONSOR_AWARD_NUMBER}", vo.getSponsorAwardNumber() == null ? "" :  vo.getSponsorAwardNumber());
		placeHolder.put("{ACCOUNT_NUMBER}", vo.getAccountNumber() == null ? "" :  vo.getAccountNumber());
		placeHolder.put("{WORKFLOW_COMMENT}", approveComment != null ? approveComment : "No Comments");
		String stopName = commonService.getPlaceHolderDataForRouting(vo.getApproverStopNumber(),vo.getMapId(),vo.getWorkflowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	@Override
	public String getServiceRequestTypesBasedOnModule(Integer moduleCode) {
		return commonDao.convertObjectToJSON(serviceRequestDao.getServiceRequestTypesBasedOnModuleCode(moduleCode));
	}

	@Override
	public ServiceRequestVO setServiceRequestDetailsForAward(Award award, AwardPerson piAwardPerson, String userRole) {
		ServiceRequestVO serviceRequestVO = new ServiceRequestVO();
		ServiceRequest serviceRequest = awardDao.getServiceRequestBasedOnOriginatedAwardId(award.getAwardId().toString());
		serviceRequestVO.setServiceRequest(serviceRequest);
		serviceRequestVO.setProjectId(award.getAwardNumber());
		serviceRequestVO.setProjectTitle(award.getTitle());
		serviceRequestVO.setPrincipalInvestigator(piAwardPerson.getFullName());
		serviceRequestVO.setUserRole(userRole);
		serviceRequestVO.setSponsorAwardNumber(award.getSponsorAwardNumber());
		serviceRequestVO.setAccountNumber(award.getAccountNumber());
		ServiceRequestType serviceRequestType = award.getServiceRequestType();
		if (serviceRequestType != null) {
			serviceRequestVO.setServiceRequestType(serviceRequestType.getDescription());
		}
		serviceRequestVO.setServiceRequestOutCome(award.getAwardStatus().getDescription());
		return serviceRequestVO;
	}

	@Override
	public Integer saveServiceRequestActionLog(Integer actionTypeCode, ServiceRequest serviceRequest) {
		ServiceRequestActionLog serviceRequestActionLog = new ServiceRequestActionLog();
		serviceRequestActionLog.setServiceRequestId(serviceRequest.getServiceRequestId());
		serviceRequestActionLog.setActionTypeCode(actionTypeCode);
		serviceRequestActionLog.setServiceRequestActionType(serviceRequestDao.fetchServiceRequestActionTypeById(actionTypeCode));
		serviceRequestActionLog.setAssigneePersonId(serviceRequest.getReporterPersonId());
		serviceRequestActionLog.setAssigneePersonName(serviceRequest.getReporterPersonName());
		serviceRequestActionLog.setStatusCode(serviceRequest.getStatusCode());
		return serviceRequestDao.saveActionLog(serviceRequestActionLog).getActionLogId();
	}

	@Override
	public String submitServiceRequest(MultipartFile[] files, String formDataJSON) {
		ServiceRequestVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ServiceRequestVO.class);
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId());
			Integer serviceRequestStatus = serviceRequest.getStatusCode();
			serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVAL_IN_PROGRESS);
			serviceRequest.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVAL_IN_PROGRESS));
			Integer actionLogId;
			if (!serviceRequestStatus.equals(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT)) {
				actionLogId = saveServiceRequestActionLog(Constants.RESUBMIT_ACTION_CODE, serviceRequest);
			} else {
				actionLogId = saveServiceRequestActionLog(Constants.SUBMIT_ACTION_CODE, serviceRequest);
			}
			if (vo.getServiceRequestComment() != null) {
				addServiceRequestComments(vo.getServiceRequestComment(), serviceRequest, actionLogId);
			}
			if (!vo.getNewAttachments().isEmpty()) {
				addServiceRequestAttachments(files, serviceRequest, vo.getNewAttachments(), actionLogId);
			}
			vo.setServiceRequest(serviceRequest);
			buildSRflow(vo);
			fetchPreviousWorkFlowsList(vo);
			if (vo.getWorkflow() == null || vo.getWorkflow().getWorkflowDetails().isEmpty()) {
				serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVED);
				serviceRequest.setServiceRequestStatus(serviceRequestDao
						.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVED));
				serviceRequest = updateSRStatusAndGroupId(serviceRequest);
				}
			updateSRStatusHistory(serviceRequest, actionLogId);
			vo.setServiceRequest(serviceRequest);
			loadServiceRequestInDetail(vo);
			Set<String> emailRecepients = new HashSet<>();
			if (!serviceRequestStatus.equals(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT)) {
				sendStatusUpdateMailForServiceRequest(emailRecepients, serviceRequest);
			}/* else {
				sendMailForServiceRequestActions(Constants.RESUBMIT_SERVICE_REQUEST_NOTIFICATION_CODE, addReporterAndWatcherAsRecepients(serviceRequest, emailRecepients), AuthenticatedUser.getLoginUserName(), null, serviceRequest);
			}*/
		} catch (Exception e) {
			e.printStackTrace();
			logger.info("Error while submitServiceRequest");
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private Set<String> addWatcherAsRecepients(ServiceRequest serviceRequest, Set<String> emailRecepients) {
		emailRecepients.addAll(serviceRequestDao.getSRWatcherIds(serviceRequest.getServiceRequestId()));
		return emailRecepients;
	}

	@Override
	public void updateSRStatusHistory(ServiceRequest serviceRequest, Integer actionLogId) {
		setStatusHistoryEndDate(serviceRequest);
		setStatusHistory(serviceRequest, actionLogId);	
	}

	@Override
	public ServiceRequest updateSRStatusAndActionLog(ServiceRequest serviceRequest, Integer actionTypeCode, Integer statusCode) {
		serviceRequest.setStatusCode(statusCode);
		serviceRequest.setServiceRequestStatus(serviceRequestDao.getServiceRequestStatus(statusCode));
		Integer actionLogId = saveServiceRequestActionLog(actionTypeCode, serviceRequest);
		setStatusHistoryEndDate(serviceRequest);
		setStatusHistory(serviceRequest, actionLogId);
		return serviceRequest;
	}

	@Override
	public String getServiceRequestTypes() {
		return commonDao.convertObjectToJSON(serviceRequestDao.getServiceRequestTypes());
	}

	private ServiceRequestVO buildSRflow(ServiceRequestVO vo) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Constants.SERVICE_REQUEST_MODULE_CODE);
		evaluateValidationRuleVO.setSubModuleCode(Constants.SUBMODULE_CODE);
		evaluateValidationRuleVO.setModuleItemKey(vo.getServiceRequest().getServiceRequestId().toString());
		evaluateValidationRuleVO.setLogginPersonId(AuthenticatedUser.getLoginPersonId());
		evaluateValidationRuleVO.setUpdateUser(AuthenticatedUser.getLoginUserName());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			vo.setWorkflow(workflowDao.fetchActiveWorkflowByParams(vo.getServiceRequest().getServiceRequestId().toString(), Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE);
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE);
		vo.setCanApproveRouting(canApproveRouting.toString());
		vo.setIsFinalApprover(isFinalApprover);
		Set<String> dynamicEmailrecipients = new HashSet<>();
		if (vo.getWorkflow() != null) {
			List<WorkflowDetail> workflowDetails = vo.getWorkflow().getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workflowDetails) {
					if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
						dynamicEmailrecipients.add(workflowDetail.getApproverPersonId());
						vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
						vo.setMapId(workflowDetail.getMapId());
					}
				}
			}
		}
		vo.setPersonName(AuthenticatedUser.getLoginUserName());
		if (vo.getServiceRequest().getServiceRequestId() != null) {
			vo.setModuleItemKey(vo.getServiceRequest().getServiceRequestId().toString());
		}
		sendMailForServiceRequestActions(Constants.SERVICE_REQUEST_APPROVAL_NOTIFICATION_CODE, dynamicEmailrecipients, vo);
		return vo;
	}

	private ServiceRequestVO fetchPreviousWorkFlowsList(ServiceRequestVO vo) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(vo.getServiceRequest().getServiceRequestId().toString(), Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			vo.setWorkflowList(workFlows);
		}
		return vo;
	}

	@Override
	public void canServiceRequestTakeRoutingAction(ServiceRequestVO serviceRequestVO) {
		Workflow workflow = serviceRequestVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(serviceRequestVO.getServiceRequestId().toString(),
					Constants.PROGRESS_REPORT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
					Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		}
		if (workflow != null) {
			serviceRequestVO.getServiceRequest()
					.setSubmitUserFullName(personDao.getPersonFullNameByPersonId(workflow.getWorkflowStartPerson()));
			serviceRequestVO.getServiceRequest().setCreatedPersonName(
					personDao.getUserFullNameByUserName(serviceRequestVO.getServiceRequest().getCreateUser()));
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(),
					maxApprovalStopNumber);
			for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
				if (finalWorkflowDetail.getApproverPersonId().equals(AuthenticatedUser.getLoginPersonId())
						|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
					serviceRequestVO.setFinalApprover(true);
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				if (serviceRequestVO.getServiceRequest().getStatusCode().equals(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVAL_IN_PROGRESS)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (workflowDetail.getApproverPersonId().equals(AuthenticatedUser.getLoginPersonId())) {
							if (workflowDetail.getApprovalStatusCode()
									.equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
								serviceRequestVO.setIsApproved(true);
							} else {
								serviceRequestVO.setIsApproved(false);
							}
							serviceRequestVO.setIsApprover(true);
						}
					}
				}
			}
		}
	}

	@Override
	public String saveServiceRequestWatcher(ServiceRequestVO vo) {
		ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId());
		Integer actionLogId = saveServiceRequestActionLog(Constants.ADD_WATCHER_ACTION_CODE, serviceRequest);
		vo.getServiceRequestWatcher().setActionLogId(actionLogId);
		vo.getServiceRequestWatcher().setServiceRequestId(serviceRequest.getServiceRequestId());
		serviceRequestDao.saveServiceRequestWatcher(vo.getServiceRequestWatcher());
		//String userName = personDao.getUserFullNameByUserName(serviceRequest.getUpdateUser());
		//sendNotification(Constants.ADD_WATCHER_SERVICE_REQUEST_NOTIFICATION_CODE, emailRecipients, userName, null, serviceRequest, null);
		vo.setMessage("Updated Service Request watchers successfully");
		vo.setServiceRequestWatcher(serviceRequestDao.saveServiceRequestWatcher(vo.getServiceRequestWatcher()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ModuleDetails prepareModuleDetails(Integer moduleCode, Integer moduleItemKey) {
		ModuleDetails moduleDetails = new ModuleDetails();
		try {
			Object[] entity = commonDao.fetchModuleDetailsBasedOnId(moduleCode, moduleItemKey);
			if (entity != null) {
				moduleDetails.setAccountNumber((entity[0] != null && !entity[0].toString().equals("")) ? entity[0].toString() : null);
				moduleDetails.setLeadUnitName(entity[1] != null ? entity[1].toString() : null);
				moduleDetails.setLeadUnitNumber(entity[2] != null ? entity[2].toString(): null);
				moduleDetails.setTitle(entity[3] != null ? entity[3].toString() : null);
				moduleDetails.setStatus(entity[4] != null ? entity[4].toString() : null);
				moduleDetails.setPiName(entity[5] != null ?entity[5].toString() : null);
				moduleDetails.setSponsorName(entity[6] != null ? entity[6].toString() : null);
				moduleDetails.setModuleItemKey(entity[7] != null ? entity[7].toString() : null);
				moduleDetails.setModuleCode(entity[8] != null ? Integer.parseInt(entity[8].toString()) : null);
				moduleDetails.setModuleItemId(entity[9] != null ? Integer.parseInt(entity[9].toString()) : null);
				moduleDetails.setPersonId(entity[10] != null ? entity[10].toString() : null);
				moduleDetails.setSponsorCode(entity[11] != null ? entity[11].toString() : null);
				moduleDetails.setStartDate(entity[12] != null ? Timestamp.valueOf(entity[12].toString()) : null);
				moduleDetails.setEndDate(entity[13] != null ? Timestamp.valueOf(entity[13].toString()) : null);
				moduleDetails.setPrimeSponsorCode((entity[14] != null && entity[14].toString().equals("")) ? entity[14].toString() : null);
				moduleDetails.setRolodexId(entity[15] != null ? entity[15].toString() : null);
				moduleDetails.setAnticipatedTotal(entity[16] != null ? entity[16].toString() : null);
				moduleDetails.setObligatedTotal(entity[17] != null ? entity[17].toString() : null);
				moduleDetails.setTotalCost(entity[18] != null ? entity[18].toString() : null);
				moduleDetails.setAgreementType(entity[19] != null ? entity[19].toString() : null);
				moduleDetails.setAgreementReqName(entity[20] != null ? entity[20].toString() : null);
				moduleDetails.setSponsorAwardNumber((entity[21] != null && !entity[21].toString().equals("")) ? entity[21].toString() : null);
			}
			return moduleDetails;
		} catch (Exception e) {
			e.printStackTrace();
			return moduleDetails;
		}
	}

	private List<ServiceRequestComment> getSRCommentsBasedOnCommentIds(List<Integer> commentIds, Integer serviceRequestId) {
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.VIEW_PRIVATE_COMMENTS_RIGHT);
		List<ServiceRequestComment> serviceRequestComments = serviceRequestDao.getCommentBasedOnActionLogIdIds(commentIds, commonDao.checkPersonHasRightInModule(Constants.SERVICE_REQUEST_MODULE_CODE, serviceRequestId, rightNames, AuthenticatedUser.getLoginPersonId()));
		Set<String> userName = serviceRequestComments.stream().map(ServiceRequestComment::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person :: getFullName));
			serviceRequestComments.stream().filter(item -> item.getUpdateUser() != null).filter(serviceRequestComment -> collect.containsKey(serviceRequestComment.getUpdateUser().toUpperCase())).forEach(serviceRequestComment -> serviceRequestComment.setUpdateUserFullName(collect.get(serviceRequestComment.getUpdateUser().toUpperCase())));
		}
		return serviceRequestComments;
	}

	private List<ServiceRequestAttachment> getSRAttachmentsBasedOnAttachmenIds(List<Integer> attachmentIds) {
		List<ServiceRequestAttachment> serviceRequestAttachments = serviceRequestDao.getAttachmentBasedOnActionLogIdIds(attachmentIds);
		Set<String> userName = serviceRequestAttachments.stream().map(ServiceRequestAttachment::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person :: getFullName));
			serviceRequestAttachments.stream().filter(item -> item.getUpdateUser() != null).filter(serviceRequestAttachment -> collect.containsKey(serviceRequestAttachment.getUpdateUser().toUpperCase())).forEach(serviceRequestAttachment -> serviceRequestAttachment.setUpdateUserFullName(collect.get(serviceRequestAttachment.getUpdateUser().toUpperCase())));
		}
		return serviceRequestAttachments;
	}

	@Override
	public ServiceRequest updateSRStatusAndGroupId(ServiceRequest serviceRequest) {
		Integer adminGroupId = serviceRequestDao.fetchAdminGroupIdBasedOnSRType(serviceRequest.getTypeCode());
		serviceRequest.setAdminGroupId(adminGroupId);
		if (adminGroupId != null) {
			serviceRequest.setAdminGroup(commonDao.getAdminGroupByGroupId(adminGroupId));
		}
		serviceRequestDao.updateSRStatusAndAdminGroupId(serviceRequest.getServiceRequestId(),serviceRequest.getStatusCode(), adminGroupId);
		sendMailforAdminGroup(adminGroupId, serviceRequest);
		return serviceRequest;
	}

	@Override
	public String assignReviewer(MultipartFile[] files, String formDataJSON) {
		ServiceRequestVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ServiceRequestVO.class);
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			Integer actionLogId;
			Set<String> emailRecepients = new HashSet<>();
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId());
			Integer serviceRequestId = serviceRequest.getServiceRequestId();
			inboxDao.markAsReadBasedOnParams(Constants.SERVICE_REQUEST_MODULE_CODE, serviceRequest.getServiceRequestId().toString(), Constants.MESSAGE_TYPE_SR_ASSIGN);
			if (Boolean.TRUE.equals(vo.getIsReturnServiceRequest())) {
				actionLogId = saveServiceRequestActionLog(Constants.RETURN_BY_REPORTER_LOG_ACTION_CODE, serviceRequest);
				serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_REJECT);
				serviceRequest.setServiceRequestStatus(serviceRequestDao
						.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_REJECT));
				updateSRStatusHistory(serviceRequest, actionLogId);
				sendStatusUpdateMailForServiceRequest(emailRecepients, serviceRequest);
				//sendMailForServiceRequestActions(Constants.RETURN_SERVICE_REQUEST_NOTIFICATION_CODE, addReporterAndWatcherAsRecepients(serviceRequest, emailRecepients), AuthenticatedUser.getLoginUserName(), null, serviceRequest);
			} else {
				if (vo.getAdminGroupId() != null) {
					sendMailforAdminGroup(vo.getAdminGroupId(), serviceRequest);
					}
				if (vo.getAssigneePersonId() != null) {
					String userMessage = "#" + serviceRequest.getServiceRequestId().toString() + " - "
							+ serviceRequest.getSubject();
					if (!vo.getAssigneePersonId().equals(AuthenticatedUser.getLoginUserName())) {
						emailRecepients.clear();
						emailRecepients.add(vo.getAssigneePersonId());
						vo.setPersonName(AuthenticatedUser.getLoginUserName());
						vo.setServiceRequest(serviceRequest);
						if (serviceRequest.getServiceRequestId() != null) {
							vo.setModuleItemKey(serviceRequest.getServiceRequestId().toString());
						}
						sendMailForServiceRequestActions(Constants.ASSIGN_TO_USER_SERVICE_REQUEST_NOTIFICATION_CODE, emailRecepients, vo);
					}
					inboxService.addMessageToInbox(serviceRequestId.toString(), vo.getAssigneePersonId(),
							Constants.MESSAGE_TYPE_SR_ASSIGN, 0, Constants.SUBMODULE_CODE,
							userMessage, Constants.SERVICE_REQUEST_MODULE_CODE);
				}
				if (vo.getAssigneePersonId() != null && vo.getAssigneePersonId().equals(AuthenticatedUser.getLoginPersonId())) {
					actionLogId = saveServiceRequestActionLog(Constants.ASSIGN_TO_ME_ACTION_CODE, serviceRequest);
				} else {
					actionLogId = saveServiceRequestActionLog(Constants.ASSIGN_ACTION_CODE, serviceRequest);
				}
				if (vo.getServiceRequestHistory() != null) {
					vo.getServiceRequestHistory().setActionLogId(actionLogId);
//					vo.getServiceRequestHistory().setActionLogId(saveServiceRequestActionLog(Constants.EDIT_REQUEST_ACTION_CODE, serviceRequest));
					serviceRequestDao.saveServiceRequestHistory(vo.getServiceRequestHistory());
				}
				serviceRequest.setAssigneePersonId(vo.getAssigneePersonId());
				if (vo.getAdminGroupId() != null) {
					serviceRequest.setAdminGroupId(vo.getAdminGroupId());
					serviceRequest.setAdminGroup(commonDao.getAdminGroupByGroupId(vo.getAdminGroupId()));
					updateSRStatusHistory(serviceRequest, actionLogId);
				}
			}
			if (vo.getServiceRequestComment() != null) {
				addServiceRequestComments(vo.getServiceRequestComment(), serviceRequest, actionLogId);
			}
			if (!vo.getNewAttachments().isEmpty()) {
				addServiceRequestAttachments(files, serviceRequest, vo.getNewAttachments(), actionLogId);
			}
			if (Boolean.TRUE.equals(vo.getIsAddAsWatcher())) {
				addMeAsWatcher(vo, serviceRequest.getServiceRequestId());
			}
			serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
			setServiceRequestFullNames(serviceRequest);
			vo.setServiceRequest(serviceRequest);
			loadServiceRequestInDetail(vo);
		} catch (Exception e) {
			e.printStackTrace();
			logger.info("Error while assignReviewer");
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void sendMailforAdminGroup(Integer adminGroupId, ServiceRequest serviceRequest) {
		String userMessage = "#" + serviceRequest.getServiceRequestId().toString() + " - "
				+ serviceRequest.getSubject();
		ServiceRequestVO vo = new ServiceRequestVO();
		Set<String> emailRecepients = new HashSet<>();
		emailRecepients.addAll(personDao.getGroupAdminPersonIdsByRightName(Constants.SR_ADMIN_GROUP_RIGHT, adminGroupId));
		if (!emailRecepients.isEmpty()) {
			emailRecepients.forEach(personId -> 
				inboxService.addMessageToInbox(serviceRequest.getServiceRequestId().toString(), personId,
						Constants.MESSAGE_TYPE_SR_ASSIGN, 0, Constants.SUBMODULE_CODE,
						userMessage, Constants.SERVICE_REQUEST_MODULE_CODE)
			);
		}
		vo.setPersonName(AuthenticatedUser.getLoginUserName());
		vo.setServiceRequest(serviceRequest);
		if (serviceRequest.getServiceRequestId() != null) {
			vo.setModuleItemKey(serviceRequest.getServiceRequestId().toString());
		}
		sendMailForServiceRequestActions(Constants.ASSIGN_SERVICE_REQUEST_NOTIFICATION_CODE_FOR_GROUP, addWatcherAsRecepients(serviceRequest, emailRecepients), vo);	
	}

	private void addMeAsWatcher(ServiceRequestVO vo, Integer serviceRequestId) {
		ServiceRequestWatcher serviceRequestWatcher = new ServiceRequestWatcher();
		serviceRequestWatcher.setServiceRequestId(serviceRequestId);
		serviceRequestWatcher.setWatcherPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setServiceRequestWatcher(serviceRequestWatcher);
		saveServiceRequestWatcher(vo);
	}

	@Override
	public String resolveServiceRequest(MultipartFile[] files, String formDataJSON) {
		ServiceRequestVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ServiceRequestVO.class);
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			Set<String> emailRecepients = new HashSet<>();
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId());
			serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_RESOLVED);
			serviceRequest.setServiceRequestStatus(
					serviceRequestDao.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_RESOLVED));
			Integer actionLogId = saveServiceRequestActionLog(Constants.RESOLVED_ACTION_CODE, serviceRequest);
			updateSRStatusHistory(serviceRequest, actionLogId);
			inboxDao.markAsReadBasedOnParams(Constants.SERVICE_REQUEST_MODULE_CODE, serviceRequest.getServiceRequestId().toString(), Constants.MESSAGE_TYPE_SR_ASSIGN);
			if (vo.getServiceRequestComment() != null) {
				addServiceRequestComments(vo.getServiceRequestComment(), serviceRequest, actionLogId);
			}
			if (!vo.getNewAttachments().isEmpty()) {
				addServiceRequestAttachments(files, serviceRequest, vo.getNewAttachments(), actionLogId);
			}
			serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
			setServiceRequestFullNames(serviceRequest);
			vo.setServiceRequest(serviceRequest);
			loadServiceRequestInDetail(vo);
			sendStatusUpdateMailForServiceRequest(emailRecepients, vo.getServiceRequest());
		} catch (Exception e) {
			e.printStackTrace();
			logger.info("Error while resolveServiceRequest");
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void sendStatusUpdateMailForServiceRequest(Set<String> emailRecepients, ServiceRequest serviceRequest) {
        ServiceRequestVO serviceRequestVO = new ServiceRequestVO();
        if (serviceRequest.getServiceRequestId() != null) {
            serviceRequestVO.setModuleItemKey(serviceRequest.getServiceRequestId().toString());
        }
		emailRecepients.add(serviceRequest.getReporterPersonId());
	    serviceRequestVO.setPersonName(AuthenticatedUser.getLoginUserName());
	    serviceRequestVO.setServiceRequest(serviceRequest);
		sendMailForServiceRequestActions(Constants.SERVICE_REQUEST_STATUS_UPDATE_REPORTER_NOTIFICATION_CODE, emailRecepients, serviceRequestVO);
		emailRecepients.clear();
		sendMailForServiceRequestActions(Constants.SERVICE_REQUEST_STATUS_UPDATE_WATCHER_NOTIFICATION_CODE, addWatcherAsRecepients(serviceRequest, emailRecepients), serviceRequestVO);
	}

	@Override
	public String updateSRReporter(ServiceRequestVO vo) {
		vo.getsRReporterChangeHistory().setActionLogId(saveServiceRequestActionLog(Constants.REPORTER_CHANGE_ACTION_CODE, vo.getServiceRequest()));
		serviceRequestDao.saveOrUpdateSRReporterChange(vo.getsRReporterChangeHistory());
		return commonDao.convertObjectToJSON(serviceRequestDao.saveOrUpdateServiceRequest(vo.getServiceRequest()));
	}

	@Override
	public String loadServiceRequestHistory(Integer serviceRequestId) {
		List<ServiceRequestActionLog> serviceRequestActionLogs = serviceRequestDao.fetchActionLogByServiceRequestId(serviceRequestId);
		getSRActionLogInDetail(serviceRequestActionLogs);
		return commonDao.convertObjectToJSON(serviceRequestActionLogs);
	}

	private List<ServiceRequestActionLog> getSRActionLogInDetail(
			List<ServiceRequestActionLog> serviceRequestActionLogs) {
		Set<String> userName = serviceRequestActionLogs.stream().map(ServiceRequestActionLog::getUpdateUser)
				.collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream()
					.collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person::getFullName));
			serviceRequestActionLogs.stream().filter(item -> item.getUpdateUser() != null)
					.filter(serviceRequestActionLog -> collect
							.containsKey(serviceRequestActionLog.getUpdateUser().toUpperCase()))
					.forEach(serviceRequestActionLog -> serviceRequestActionLog
							.setUpdateUserFullName(collect.get(serviceRequestActionLog.getUpdateUser().toUpperCase())));
		}
		List<Integer> filterActionTypeCodes = new ArrayList<>();
		filterActionTypeCodes.add(Constants.EDIT_REQUEST_ACTION_CODE);
		filterActionTypeCodes.add(Constants.ASSIGN_ACTION_CODE);
		filterActionTypeCodes.add(Constants.ASSIGN_TO_ME_ACTION_CODE);
		Set<String> actionLogIds = serviceRequestActionLogs.stream()
				.filter(serviceRequestActionLog -> filterActionTypeCodes.contains(serviceRequestActionLog.getActionTypeCode()))
				.map(serviceRequestActionLog -> serviceRequestActionLog.getActionLogId().toString())
				.collect(Collectors.toSet());
		if (!actionLogIds.isEmpty()) {
			List<ServiceRequestHistory> serviceRequestHistories = serviceRequestDao
					.fetchServiceRequestHistoryByActionLogIds(new ArrayList<>(actionLogIds));
			Map<Integer, ServiceRequestHistory> collect = serviceRequestHistories.stream().collect(Collectors
					.toMap(ServiceRequestHistory::getActionLogId, serviceRequestHistory -> serviceRequestHistory));
			serviceRequestActionLogs.stream()
					.filter(serviceRequestActionLog -> filterActionTypeCodes
							.contains(serviceRequestActionLog.getActionTypeCode()))
					.filter(serviceRequestActionLog -> collect.containsKey(serviceRequestActionLog.getActionLogId()))
					.forEach(serviceRequestActionLog -> serviceRequestActionLog
							.setServiceRequestHistory(collect.get(serviceRequestActionLog.getActionLogId())));
		}
		return serviceRequestActionLogs;
	}

	@Override
	public String deleteServiceRequestAttachmentFromAward(ServiceRequestVO vo) {
		ServiceRequestAttachment serviceRequestAttachment = serviceRequestDao.getAttachmentById(vo.getAttachmentId());
		if (serviceRequestAttachment != null) {
			if (serviceRequestAttachment.getFileDataId() != null) {
				commonDao.deleteFileData(commonDao.getFileDataById(serviceRequestAttachment.getFileDataId()));
			}
			serviceRequestDao.deleteServiceRequestAttachment(serviceRequestAttachment);
		}
		vo.setMessage("Attachment deleted sucessfully");
		return commonDao.convertObjectToJSON(vo);
	}

}
