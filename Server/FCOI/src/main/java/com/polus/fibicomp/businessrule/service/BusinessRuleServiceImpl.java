package com.polus.fibicomp.businessrule.service;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.service.AgreementService;
import com.polus.fibicomp.agreements.service.AgreementWorkflowService;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.awardworkflow.dao.AwardWorkflowDao;
import com.polus.fibicomp.award.awardworkflow.service.AwardWorkflowService;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.claims.dao.ClaimsDao;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.claims.service.ClaimsService;
import com.polus.fibicomp.claims.vo.ClaimsVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.dao.EvaluationDao;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.inbox.vo.InboxVO;
import com.polus.fibicomp.ip.service.InstitutionalProposalService;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.manpowerintegration.scheduler.ManpowerIntegrationSchedulerService;
import com.polus.fibicomp.negotiation.dao.NegotiationAgreementDao;
import com.polus.fibicomp.negotiation.dao.NegotiationDao;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.service.NegotiationService;
import com.polus.fibicomp.negotiation.vo.NegotiationMode;
import com.polus.fibicomp.negotiation.vo.NegotiationVO;
import com.polus.fibicomp.notification.email.dao.EmailMaintenanceDao;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.progressreport.dao.ProgressReportDao;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportKPISummary;
import com.polus.fibicomp.progressreport.service.ProgressReportService;
import com.polus.fibicomp.progressreport.vo.ProgressReportVO;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;
import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.proposal.service.ProposalCopyService;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.sectionwiseedit.dao.SectionWiseEditDao;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.service.ServiceRequestService;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;
import com.polus.fibicomp.task.dao.TaskDao;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.service.TaskService;
import com.polus.fibicomp.task.vo.TaskVO;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowAttachment;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowDetailExt;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Service(value = "businessRuleService")
public class BusinessRuleServiceImpl implements BusinessRuleService {

	protected static Logger logger = LogManager.getLogger(BusinessRuleServiceImpl.class.getName());

	@Autowired
	private BusinessRuleDao businessRuleDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private NegotiationDao negotiationDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private PrintService printService;

	@Autowired
	private EmailMaintenanceDao emailMaintenanceDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private InstitutionalProposalService institutionalProposalService;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardWorkflowDao awardWorkflowDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private EvaluationDao evaluationDao;

	@Autowired
	private NegotiationService negotiationService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;
	
	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private ServiceRequestService serviceRequestService;

	@Autowired
	private AwardWorkflowService awardWorkflowService;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private TaskService taskService;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private ClaimsDao claimsDao;

	@Autowired
	private ClaimsService claimsService;

	@Value("${spring.application.name}")
	private String context;

	@Autowired
	private ManpowerIntegrationSchedulerService manpowerIntegrationSchedulerService;
	
	@Autowired
	private ProgressReportDao progressReportDao;
	
	@Autowired
	private ProgressReportService progressReportService;

	@Autowired
	private ProposalCopyService proposalCopyService;
	
	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private ManpowerService manpowerService;
	
	@Autowired
	private NegotiationAgreementDao negotiationAgreementDao;

	@Autowired
	private AgreementService agreementService;
	
	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private AgreementWorkflowService agreementWorkflowService;

	@Autowired
	private InboxService inboxService;

	private static final String IS_FINAL_APPROVER = "isFinalApprover : {}";
	private static final String PROPOSAL_ID = "proposalId : {}";
	private static final String UPDATE_USER = "updatedUser : {}";
	private static final String MODULE_ITEM_KEY = "moduleItemKey : {}";
	private static final String LOGIN_PERSON_ID = "loginPersonId : {}";
	private static final String SUB_MODULE_CODE = "subModuleCode : {}";
	private static final String ACTION_TYPE = "actionType : {}";
	private static final String APPROVER_COMMENT = "approverComment : {}";
	private static final String CAN_APPROVE_ROUTING = "canApproveRouting : {}";
	private static final String APPROVAL_REJECTED = "approval_rejected";
	private static final String APPROVAL_SUCCESS = "approval_success";
	private static final String AWARD_NUMBER = "awardNumber : {}";
	private static final String WORKFLOW_PERSON_ID = "workFlowPersonId : {}";
	private static final String FINAL_APPROVAL_REJECTED = "final_approval_rejected";
	private static final String FINAL_APPROVAL_SUCCESS = "final_approval_success";
	private static final String APPROVAL_FAILED = "approval_failed";

	@Override
	public Integer buildWorkFlow(EvaluateValidationRuleVO evaluateValidationRuleVO) {
		Integer workflowStatus = businessRuleDao.buildWorkFlow(evaluateValidationRuleVO);
		List<HashMap<String, Object>> details = businessRuleDao.getRoleNameAndMapName(evaluateValidationRuleVO.getModuleCode(), evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode(), evaluateValidationRuleVO.getSubModuleItemKey());
		sendNotificationForNoRoleBasedPersonAssigned(details, evaluateValidationRuleVO);
		return workflowStatus;
	}

	@Override
	public String evaluateValidationRule(EvaluateValidationRuleVO evaluateValidationRuleVO) {
		evaluateValidationRuleVO.setWorkFlowResultList(businessRuleDao.evaluateValidationRule(evaluateValidationRuleVO));
		return commonDao.convertObjectToJSON(evaluateValidationRuleVO.getWorkFlowResultList());
	}

	@Override
	public String evaluateNotificationRule(EvaluateValidationRuleVO evaluateValidationRuleVO) {
		return commonDao.convertObjectToJSON(businessRuleDao.evaluateNotificationRule(evaluateValidationRuleVO));
	}

	@Override
	public String ruleEvaluate(EvaluateValidationRuleVO evaluateValidationRuleVO) {
		return businessRuleDao.ruleEvaluate(evaluateValidationRuleVO);
	}

	@Override
	public Integer canApproveRouting(String moduleItemKey, String loginPersonId, Integer moduleCode, String subModuleItemKey, Integer subModuleCode) {
		return businessRuleDao.canApproveRouting(moduleItemKey, loginPersonId, moduleCode, subModuleItemKey, subModuleCode);
	}

	@Override
	public String getWorkFlowRouteLog(String moduleItemId, Integer moduleCode) {
		return businessRuleDao.getWorkFlowRouteLog(moduleItemId, moduleCode);
	}

	@Override
	public String workflowfinalApproval(String moduleItemKey, String personId, Integer moduleCode, Integer subModuleCode, String subModuleItemKey) {
		return businessRuleDao.workflowfinalApproval(moduleItemKey, personId, moduleCode, subModuleItemKey, subModuleCode);
	}

	@Override
	public String approveOrRejectWorkflow(MultipartFile[] files, String formDataJson, String moduleCode, String subModuleCode) {
		if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_NEGOTIATIONS))) {
			return negotiationWorkflowApproval(files, formDataJson, moduleCode);
		} else if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL))) {
			return proposalWorkflowApproval(files, formDataJson, moduleCode);
		} else if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_AWARD))) {
			if (subModuleCode.equals(String.valueOf(Constants.AWARD_TASK_SUBMODULE_CODE))) {
				return taskWorkflowApproval(files, formDataJson, moduleCode, subModuleCode);
			} else {
				return commonDao.convertObjectToJSON(awardWorkflowApproval(files, formDataJson, moduleCode));
			}
		} else if (moduleCode.equals(String.valueOf(Constants.CLAIM_MODULE_CODE))) {
			return claimWorkflowApproval(files, formDataJson, moduleCode);
		} else if (moduleCode.equals(String.valueOf(Constants.PROGRESS_REPORT_MODULE_CODE))) {
			return progressReportWorkflowApproval(files, formDataJson, moduleCode);
		}  else if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_AGREEMENT))) {
			return agreementWorkflowApproval(files, formDataJson, moduleCode);
		} else if (moduleCode.equals(String.valueOf(Constants.SERVICE_REQUEST_MODULE_CODE))) {
			return serviceRequestWorkflowApproval(files, formDataJson, moduleCode);
		}
		return commonDao.convertObjectToJSON("");
	}

	private String proposalWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		ProposalVO proposalVO = new ProposalVO();
		try {
			proposalVO = mapper.readValue(formDataJson, ProposalVO.class);
			Integer proposalId = proposalVO.getProposalId();
			String updatedUser = proposalVO.getUpdateUser();
			String moduleItemKey = proposalId.toString();
			String logginPersonId = proposalVO.getPersonId();
			String workFlowPersonId = proposalVO.getWorkFlowPersonId();
			Integer subModuleCode = Constants.DEV_PROPOSAL_SUBMODULE_CODE;
			Proposal proposal = proposalDao.fetchProposalById(proposalId);
			proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId));
			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposalVO.setProposal(proposal);
			proposalVO.setProposalStatusCode(proposal.getStatusCode());
			String actionType = proposalVO.getActionType();
			String approverComment = proposalVO.getApproveComment();
			logger.info(PROPOSAL_ID, proposalId);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B") || actionType.equals("C")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			proposalVO = approveOrRejectProposal(proposalVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, files);
			proposal = proposalDao.saveOrUpdateProposal(proposal);
			if (proposal.getGrantCallId() != null) {
				GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
				String grantCallCreateUser = grantCall.getCreateUser();
				if (proposal.getStatusCode().equals(Constants.ADMIN_CHECK_COMPLETED)) {
					String personId = personDao.getPersonIdByUserName(grantCallCreateUser);
					addMessageToInbox(proposal, personId, Constants.MESSAGE_TYPE_START_EVALUATION, Constants.SUBJECT_TYPE_CODE, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				}
			}
			if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING) 
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_RETURNED) 
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)
					|| proposal.getStatusCode().equals(Constants.COMPLETED)
					|| proposal.getStatusCode().equals(Constants.ADMIN_CHECK_COMPLETED)
					|| proposal.getStatusCode().equals(Constants.REVIEW_IN_PROGRESS)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_NOT_AWARDED)) {
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				workflowService.prepareWorkflowDetails(workflow);
				proposalVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					proposalVO.setWorkflowList(workFlows);
				}
			}
			proposalVO.setProposal(proposal);
			proposalVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
			proposalVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
			if (!isFinalApprover.equals("1") || actionType.equals("R")) {
				sendApproveOrDisApproveNotificationForProposal(proposalVO, actionType);
			}
			proposalService.loadInitialData(proposalVO);
			proposalService.loadProposalHomeData(proposalVO);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("exception in proposalWorkflowApproval: {} ", e.getMessage());					
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private void addMessageToInbox(Proposal proposal, String personId, String messageTypeCode, String subjectTypeCode, Integer subModuleCode) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
		inbox.setSubModuleCode(subModuleCode);
		inbox.setModuleItemKey(proposal.getProposalId().toString());
		inbox.setSubModuleItemKey("0");
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType(subjectTypeCode);
		inbox.setToPersonId(personId);
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(proposal.getCreateUser());
		inbox.setUserMessage(proposal.getProposalId().toString() + " - " + proposal.getTitle() + " - Review");
		inboxDao.saveToInbox(inbox);	
	}

	private String negotiationWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		NegotiationVO negotiationVO = new NegotiationVO();
		try {
			negotiationVO = mapper.readValue(formDataJson, NegotiationVO.class);
			String updatedUser = negotiationVO.getUpdateUser();
			String moduleItemKey = negotiationVO.getNegotiationId().toString();
			String logginPersonId = negotiationVO.getPersonId();
			Integer subModuleCode = 0;
			String actionType = negotiationVO.getActionType();
			String approverComment = negotiationVO.getApproveComment();
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			Integer canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			negotiationVO = approveOrRejectedNegotiation(negotiationVO, moduleItemKey, moduleCode, logginPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files);
			Negotiations negotiation = negotiationAgreementDao.fetchNegotiationById(negotiationVO.getNegotiationId());
			negotiationService.loadNegotiationUserFullNames(negotiation);
			negotiationVO.setNegotiations(negotiation);
			NegotiationMode negotiationsMode = negotiationService.getNegotiationMode(negotiation);
			if (negotiationsMode.getMode().equalsIgnoreCase(Constants.NEGOTIATION_EDIT_MODE)) {
			negotiationVO.setIsSubmit(Constants.NEGOTIATION_SHOW_SUBMIT_BUTTON);
		     }
			negotiationVO.setNegotiationMode(negotiationsMode);
			negotiationVO.setSubAwardOrganizationName(negotiationDao.getSubAwardOrganizationName(negotiationVO.getNegotiationId()));
			if (negotiationVO.getNegotiations().getNegotiationStatusCode().equals(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString())) {
				negotiationVO.getNegotiationMode().setMode("EDIT");
				negotiationVO.getNegotiationMode().setStatus("1");
				negotiationVO.setIsSubmit("1");
			} else {
				negotiationVO.getNegotiationMode().setMode(Constants.NEGOTIATION_VIEW_MODE);
				negotiationVO.setIsSubmit("0");
			}
			if (negotiationVO.getNegotiations().getNegotiationStatusCode()
					.equals(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString())
					|| negotiationVO.getNegotiations().getNegotiationStatusCode()
							.equals(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString())
					|| negotiationVO.getNegotiations().getNegotiationStatusCode()
							.equals(Constants.NEGOTIATION_STATUS_CODE_APPROVED.toString())) {
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(
						negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
				workflowService.prepareWorkflowDetails(workflow);
				negotiationVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					negotiationVO.setWorkflowList(workFlows);
				}
			}
			sendNegotiationApproveOrDisapproveNotification(negotiationVO, actionType, isFinalApprover);
			isFinalApprover = businessRuleDao.workflowfinalApproval(negotiationVO.getNegotiations().getNegotiationId().toString(), negotiationVO.getNegotiations().getNegotiatorPersonId(), Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			canApproveRouting = businessRuleDao.canApproveRouting(negotiationVO.getNegotiations().getNegotiationId().toString(), negotiationVO.getNegotiations().getNegotiatorPersonId(), Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			negotiationVO.setCanApproveRouting(canApproveRouting);
			negotiationVO.setIsFinalApprover(isFinalApprover);
		} catch (Exception e) {
			logger.error("exception in negotiationWorkflowApproval: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(negotiationVO);

	}

	public String getNegotiatorEmailAddress(String negotiatorId) {
		String emailAddress = "";
		emailAddress = businessRuleDao.getEmailIdByName(negotiatorId);
		return commonDao.convertObjectToJSON(emailAddress);
	}

	@Override
	public Workflow getWorkFlow(String moduleItemKey, Integer moduleCode) {
		return businessRuleDao.getWorkFlow(moduleItemKey, moduleCode);
	}

	public void uploadAttachment(MultipartFile[] files, String updatedUser, Integer workflowDetailId) {
		List<WorkflowAttachment> workflowAttachments = new ArrayList<>();
		WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
		try {
			for (int i = 0; i < files.length; i++) {
				WorkflowAttachment attachment = new WorkflowAttachment();
				File file = new File(files[i].getOriginalFilename());
				attachment.setAttachment(files[i].getBytes());
				attachment.setFileName(file.getName());
				attachment.setMimeType(files[i].getContentType());
				attachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				attachment.setUpdateUser(updatedUser);
				attachment.setWorkflowDetail(workflowDetail);
				workflowAttachments.add(attachment);
			}
			workflowDetail.getWorkflowAttachments().addAll(workflowAttachments);
			workflowDao.saveWorkflowDetail(workflowDetail);
		} catch (Exception e) {
			logger.error("exception in uploadAttachment: {} ", e.getMessage());
		}
	}

	@SuppressWarnings("unused")
	private NegotiationVO sendNegotiationNotificationIfApproved(NegotiationVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailrecipients) {
		NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(notificationTypeId);
		commonService.setNotificationRecipients("", "TO", dynamicEmailrecipients);
		negotiationService.sendNegotiationNotification(vo, notificationType.getNotificationTypeId(), dynamicEmailrecipients);
		return vo;
	}

	private NegotiationVO approveOrRejectedNegotiation(NegotiationVO negotiationVO, String moduleItemKey,
			String moduleCode, String logginPersonId, Integer subModuleCode, String updatedUser, String actionType,
			String approverComment, String isFinalApprover, Integer canApproveRouting, MultipartFile[] files) {
		String workflowDetailId = null;
		try {
			Negotiations negotiations = negotiationAgreementDao.fetchNegotiationById(negotiationVO.getNegotiationId());
			if (canApproveRouting.toString().equals("0")) {
				negotiationVO.setMessage(APPROVAL_FAILED);
				return negotiationVO;
			} else {
				if (negotiationVO.getActionType().equals("A")) {
					negotiationVO.setMessage(APPROVAL_SUCCESS);
				} else if (actionType.equals("R")) {
					negotiations.setNegotiationStatusCode(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString());
				} else {
					return negotiationVO;
				}
			}
			if (isFinalApprover.equals("1")) {
				if (negotiationVO.getActionType().equals("A")) {
					negotiations.setNegotiationStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVED.toString());
					negotiations.setNegotiationsStatus(negotiationAgreementDao.fetchStatusByStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVED.toString()));
					negotiationVO.setMessage(FINAL_APPROVAL_SUCCESS);
				} else if (negotiationVO.getActionType().equals("R")) {
					negotiations.setNegotiationStatusCode(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString());
					negotiations.setNegotiationsStatus(negotiationAgreementDao.fetchStatusByStatusCode(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString()));
					negotiationVO.setMessage(FINAL_APPROVAL_REJECTED);
				}
			}
			workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, logginPersonId, updatedUser,
					actionType, approverComment, Constants.NEGOTIATION_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY, 
					negotiationVO.getMapId(), negotiationVO.getMapNumber(), negotiationVO.getApproverStopNumber(), negotiationVO.getApproverNumber());
			if (workflowDetailId != null) {
				negotiationVO.setWorkFlowDetailId(Integer.parseInt(workflowDetailId));
			}
			if (files != null) {
				Integer id = Integer.parseInt(workflowDetailId);
				uploadAttachment(files, updatedUser, id);
			}
			if(isFinalApprover.equals("0")) {
				negotiationVO.setIsFinalApprover("0");
			} else {
				negotiationVO.setIsFinalApprover("1");
			}
			negotiationVO.setNegotiations(negotiationAgreementDao.saveNegotiationInfo(negotiations));
			negotiationVO.setActionType(negotiationVO.getActionType());
		} catch (Exception e) {
			logger.error("exception in approveOrRejectedNegotiation: {} ", e.getMessage());
		}
		return negotiationVO;
	}


	private ProposalVO approveOrRejectProposal(ProposalVO proposalVO, String moduleItemKey, String moduleCode,
			String logginPersonId, Integer subModuleCode, String updatedUser, String actionType, String approverComment,
			String isFinalApprover, MultipartFile[] files) {
		Proposal proposal = proposalVO.getProposal();
		String workflowDetailId = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Integer.parseInt(moduleCode));
		evaluateValidationRuleVO.setLogginPersonId(logginPersonId);
		evaluateValidationRuleVO.setSubModuleCode(subModuleCode);
		evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
		evaluateValidationRuleVO.setUpdateUser(updatedUser);
		evaluateValidationRuleVO.setAcType(proposalVO.getActionType());
		evaluateValidationRuleVO.setComments(proposalVO.getApproveComment());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
			if (proposalVO.getActionType().equals("A") || proposalVO.getActionType().equals("B")) {
				proposalVO.setMessage(APPROVAL_SUCCESS);
			} else if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS) && proposalVO.getActionType().equals("R")) {
				proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED);
				proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED));
				proposal.setDocumentStatusCode(Constants.PROPOSAL_DOCUMENT_STATUS_ACTIVE);
				proposal.setDocumentStatus(proposalDao.fetchProposalDocumentStatus(Constants.PROPOSAL_DOCUMENT_STATUS_ACTIVE));
				proposalService.sendProposalNotification(proposalVO, Constants.APPLICATION_REVISION_NOTIFICATION_CODE, new HashSet<>());
				proposalVO.setMessage(APPROVAL_REJECTED);
				if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_DEV_PROP_VERSIONS)) {
					ProposalVO vo = new ProposalVO();
					vo.setIsProposalArchiveCreation(true);
					vo.setProposalId(proposal.getProposalId());
					vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
					vo.setProposalStatusCode(proposalVO.getProposalStatusCode());
					proposalCopyService.copyProposal(vo);
				}
			} else if (proposalVO.getActionType().equals("C")) {
				proposalVO.setMessage("WITHDRAWAL");
			} else {
				proposalVO.setMessage(APPROVAL_FAILED);
				return proposalVO;
			}
		proposalVO.setProposal(proposal);
		workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, logginPersonId, updatedUser,
				actionType, approverComment, subModuleCode, Constants.SUBMODULE_ITEM_KEY, proposalVO.getMapId(),
				proposalVO.getMapNumber(), proposalVO.getApproverStopNumber(), proposalVO.getApproverNumber());
		if (files != null && workflowDetailId != null) {
			uploadAttachment(files, updatedUser, Integer.parseInt(workflowDetailId));
		}
		if (workflowDetailId != null) {
			proposalVO.setWorkflowDetailId(Integer.parseInt(workflowDetailId));
			if (isFinalApprover.equals("1")) {
				if (proposalVO.getActionType().equals("A") || proposalVO.getActionType().equals("B")) {
					if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.MAP_ROUTING)) {
						if (proposal.getGrantCallId() != null) {
							if (proposal.getGrantCallType() != null && (proposal.getGrantCallType().getGrantTypeCode().equals(1)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(2)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(3))) {
								if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLED_ADMIN_CHK)) {
									proposalVO.getProposal().setStatusCode(Constants.COMPLETED);
									proposalVO.getProposal().setProposalStatus(
											proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.COMPLETED));
									sendCompleteReviewNotification(proposalVO, Constants.ADMIN_CHECK_COMPLETE_NOTIFICATION_CODE);
								} else {
									proposalVO.getProposal().setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
									proposalVO.getProposal().setProposalStatus(proposalLookUpDao
											.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
									proposal = generateInstitutionalProposal(proposalVO);
									proposalService.sendProposalNotification(proposalVO,
											Constants.PROPOSAL_AWARDED_NOTIFICATION_CODE, new HashSet<>());
								}
							}
							if (proposal.getGrantCallType() !=null && (proposal.getGrantCallType().getGrantTypeCode().equals(4)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(5)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(6)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(7)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(8)
									|| proposal.getGrantCallType().getGrantTypeCode().equals(9))) {
								if (proposal.getProposalStatus().getStatusCode().equals(Constants.REVIEW_IN_PROGRESS)) {
									proposal.setStatusCode(Constants.COMPLETED);
									proposal.setProposalStatus(
											proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.COMPLETED));
									ProposalEvaluationScore proposalEvaluationScore = new ProposalEvaluationScore();
									proposalEvaluationScore.setGrantHeaderId(proposal.getGrantCallId());
									proposalEvaluationScore.setProposalId(proposal.getProposalId());
									proposalEvaluationScore.setAdjustedScore(new BigDecimal(0));
									proposalEvaluationScore.setIsShortListed("N");
									grantCallDao.saveOrUpdateProposalEvalautionScore(proposalEvaluationScore);
									proposalVO.setMessage(APPROVAL_SUCCESS);
								} else {
									proposalVO.getProposal().setStatusCode(Constants.ADMIN_CHECK_COMPLETED);
									proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.ADMIN_CHECK_COMPLETED));
									sendCompleteReviewNotification(proposalVO, Constants.ADMIN_CHECK_COMPLETE_NOTIFICATION_CODE);								}
							}
						} else {
							proposalVO.getProposal().setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
							proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
							proposal = generateInstitutionalProposal(proposalVO);
							proposalService.sendProposalNotification(proposalVO, Constants.PROPOSAL_AWARDED_NOTIFICATION_CODE, new HashSet<>());
						}
					}
					if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE)
							.equals(Constants.EVALUATION_MAP_ROUTING)) {
						proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW);
						proposal.setProposalStatus(proposalLookUpDao
								.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW));
					}
					proposalVO.setMessage(FINAL_APPROVAL_SUCCESS);
					proposalVO.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(),
							proposal.getActivityTypeCode()));
				} else if (proposalVO.getActionType().equals("R")) {
					proposalVO.getProposal().setStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED);
					proposalVO.getProposal().setProposalStatus(
							proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED));
					proposalService.sendProposalNotification(proposalVO, Constants.APPLICATION_REVISION_NOTIFICATION_CODE, new HashSet<>());
					proposalVO.setMessage(FINAL_APPROVAL_REJECTED);
				} else if (proposalVO.getActionType().equals("C")) {
					proposalVO.getProposal().setStatusCode(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
					proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_WITHDRAW));
					proposalVO.setMessage(FINAL_APPROVAL_REJECTED);
				}
			}
		}
		if (proposalVO.getProposal().getStatusCode().equals(Constants.COMPLETED)) {
			sendCompleteReviewNotification(proposalVO, Constants.PROPOSAL_FINAL_ENDORSEMENT_NOTIFICATION_CODE);
			List<String> personIds = proposalDao.fetchMainPanelPersonIdsByGrantCallId(proposal.getGrantCallId());
			List<ProposalPersonRoles> proposalPersons = proposalLookUpDao.fetchProposalPersonRoles(proposal.getProposalId(), Constants.REVIEW_PROPOSAL_ROLE_ID);
			if (proposalPersons == null || proposalPersons.isEmpty() && personIds != null && !personIds.isEmpty()) {
				for (String personId : personIds) {
					proposalService.saveProposalPersonRole(personId, proposal.getProposalId(), proposal.getUpdateUser(), Constants.REVIEW_PROPOSAL_ROLE_ID);
					addMessageToInbox(proposal, personId, Constants.MESSAGE_TYPE_MAIN_PANEL_REVIEW, Constants.SUBJECT_TYPE_CODE, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				}
			}
			sendMainPanelReviewNotification(proposalVO);
		}
		return proposalVO;
	}

	@Override
	public Proposal generateInstitutionalProposal(ProposalVO proposalVO) {
		Proposal proposal = proposalVO.getProposal();
		if (canCreateInstituteProposal(proposal, proposalVO).equals(Boolean.TRUE)) {
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
		String ipNumber = "";
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED) && proposal.getApplicationId() != null) {
			ipNumber = proposal.getApplicationId();
		} else {
			ipNumber = institutionalProposalService.generateInstitutionalProposalNumber();
		}
		logger.info("Initial IP Number : {}", ipNumber);
		boolean isIPCreated = institutionalProposalService.createInstitutionalProposal(proposalVO.getProposalId(), ipNumber, proposalVO.getUpdateUser());
		logger.info("isIPCreated : {}", isIPCreated);
		if (isIPCreated) {
			logger.info("Generated IP Number : {}", ipNumber);
			proposal.setIpNumber(ipNumber);
			proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
			proposal.setDocumentStatusCode(Constants.PROPOSAL_DOCUMENT_STATUS_ACTIVE);
			proposal.setDocumentStatus(proposalDao.fetchProposalDocumentStatus(Constants.PROPOSAL_DOCUMENT_STATUS_ACTIVE));
			proposalService.sendProposalNotification(proposalVO, Constants.NOTIFICATION_IP_GENERATED, new HashSet<>());
		}
		} else {
			if (personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), "CREATE_INST_PROPOSAL", proposal.getHomeUnitNumber())) {
				proposalVO.setIpGenerationOnly(Boolean.FALSE);
			} else {
				proposalVO.setIpGenerationOnly(Boolean.TRUE);
			}
			if (proposal.getIpNumber() == null || (proposal.getIpNumber() != null && proposal.getIpNumber().equals(""))) {
				proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_NOT_AWARDED);
				proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_NOT_AWARDED));
			}
		}
		return proposal;
	}

	@Override
	public Boolean canCreateInstituteProposal(Proposal proposal, ProposalVO proposalVO) {
		return ((proposal.getIpNumber() == null || (proposal.getIpNumber() != null && proposal.getIpNumber().equals(""))) && (proposalVO.getIpGenerationOnly().equals(Boolean.TRUE) || (proposal.getProposalType().getCanMergeToIp().equals(Boolean.FALSE)) || 
				(!commonDao.getParameterValueAsBoolean(Constants.ENABLE_DEV_PROP_VERSIONS))) && (personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), "CREATE_INST_PROPOSAL", proposal.getHomeUnitNumber())));
	}

	public Integer buildWorkFlows(String moduleItemKey, String moduleCode, String logginPersonId, Integer subModuleCode, String updatedUser) {
		Integer workflowResult = 0;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Integer.parseInt(moduleCode));
		evaluateValidationRuleVO.setLogginPersonId(logginPersonId);
		evaluateValidationRuleVO.setSubModuleCode(subModuleCode);
		evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
		evaluateValidationRuleVO.setUpdateUser(updatedUser);
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		workflowResult = businessRuleDao.buildWorkFlow(evaluateValidationRuleVO);
		return workflowResult;
	}

	@Override
	public ResponseEntity<byte[]> downloadWorkflowsAttachments(Integer workflowAttachmentId) {
		logger.info("-------- downloadNegotiationAttachment serviceimpl ---------");
		WorkflowAttachment attachment = businessRuleDao.fetchAttachmentById(workflowAttachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), attachment.getAttachment());
		} catch (Exception e) {
			logger.error("exception in downloadWorkflowsAttachments: {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public AwardVO awardWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		AwardVO awardVO = new AwardVO();
		try {
			awardVO = mapper.readValue(formDataJson, AwardVO.class);
			Integer awardId = awardVO.getAwardId();
			String awardNumber = awardVO.getAwardNumber();
			String updatedUser = awardVO.getUpdateUser();
			String moduleItemKey = awardId.toString();
			String logginPersonId = awardVO.getPersonId();
			String workFlowPersonId = awardVO.getWorkFlowPersonId();
			Integer subModuleCode = Constants.AWARD_SUBMODULE_CODE;
			Award award = awardDao.getAwardDetailsById(awardId);
			if (award.getSubmitUser() != null) {
				award.setSubmitUserFullName(personDao.getUserFullNameByUserName(award.getSubmitUser()));
			}
			award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardVO.setAward(award);
			String actionType = awardVO.getActionType();
			String approverComment = awardVO.getApproveComment();

			if (award.getUpdateUser() != null) {
				award.setUpdateUserFullName(personDao.getUserFullNameByUserName(award.getUpdateUser()));
			}
			if (award.getCreateUser() != null) {
				award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
			}
			logger.info("awardId : {}", awardId);
			logger.info(AWARD_NUMBER, awardNumber);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(WORKFLOW_PERSON_ID, workFlowPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);

			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B") || actionType.equals("C")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			awardVO = approveOrRejectAward(awardVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files, logginPersonId);
			award.setDocumentUpdateUser(updatedUser);
			award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award = awardDao.saveOrUpdateAwardDetails(award);
			awardService.setSapFeedStatus(awardVO);
			awardVO.setAward(award);
			if (awardVO != null && awardVO.getAward() != null) {
				awardService.getPendingAwardDetails(awardVO, awardVO.getAward().getAwardNumber(), awardVO.getAward().getAwardId());
			}
			if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)
					|| award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED)
					|| award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_ACTIVE)
					|| award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_HOLD_FOR_SAP)) {
				awardService.canTakeRoutingAction(awardVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					awardVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						awardVO.setWorkflowList(workFlows);
					}
					awardVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
					awardVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
					if (workflow != null && workflow.getCurrentStopName() != null && award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
						award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription() + " : " + workflow.getCurrentStopName());
					} else {
						award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription());
					}
					if (Boolean.TRUE.equals(awardVO.getIsMasterAwardCreation())) {
						awardWorkflowService.checkAndFeedSapAwardDetail(awardVO.getAward(), awardVO.getUpdateUser());
					}
					commonDao.doflush();
					if ((!isFinalApprover.equals("1")) || (isFinalApprover.equals("1") && actionType.equals("R"))) {
						sendApproveOrDisApproveNotificationForAwardAndServiceRequest(awardVO, actionType);
					} else {
						if (actionType.equals("B")) {
							sendAwardBypassNotification(awardVO);
						}
						if (!award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_HOLD_FOR_SAP)) {
							sendFinalApprovalNotification(awardVO);
						}
						if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
							awardService.sendAwardNotification(awardVO, Constants.LIVE_AWARD_NOTIFICATION_FOR_CLAIM_PREPARER_CODE, new HashSet<>());
						}
					}
				}
			}
		} catch (JsonMappingException e) {
			logger.error("JsonMappingException in awardWorkflowApproval: {} ", e.getMessage());
		} catch (JsonParseException e) {
			logger.error("JsonParseException in awardWorkflowApproval: {} ", e.getMessage());
		} catch (IOException e) {
			logger.error("IOException in awardWorkflowApproval: {} ", e.getMessage());
		} catch (Exception e) {
			throw e;
		}
		return awardVO;
	}

	private AwardVO approveOrRejectAward(AwardVO awardVO, String moduleItemKey, String moduleCode,
			String workFlowPersonId, Integer subModuleCode, String updatedUser, String actionType, String approverComment,
			String isFinalApprover, Integer canApproveRouting, MultipartFile[] files, String logginPersonId) {
			Award award = awardVO.getAward();
			String awardId = award.getAwardId().toString();
			Award activeAward = awardDao.fetchActiveAwardByAwardNumber(award.getAwardNumber());
			Award currentActiveAward = null;
			AwardPerson newAwardPIPerson = manpowerIntegrationDao.getAwardPIPersonByAwardId(award.getAwardId());
			AwardPerson activeAwardPIPerson = null;
			String activeAwardId = "";
			String activeAwardSuperiorSupOrg = null;
			if (activeAward != null) {
				activeAwardId = activeAward.getAwardId().toString();
				activeAwardPIPerson = manpowerIntegrationDao.getAwardPIPersonByAwardId(activeAward.getAwardId());
				currentActiveAward = activeAward;
				try {
					activeAwardSuperiorSupOrg = manpowerService.findSuperiorSupOrgForAward(activeAward);
				} catch (Exception e ) {
					logger.error("Error in findSuperiorSupOrgForAward {} ", e.getMessage());
					throw new ApplicationException("error occured while find superior sup org", e, Constants.JAVA_ERROR);
				}
				
			}
			String workflowDetailId = null;
			if (awardVO.getWorkflowDetailId() == null) {
				if (canApproveRouting.toString().equals("0")) {
					awardVO.setMessage(APPROVAL_FAILED);
					return awardVO;
				} else {
						if (awardVO.getWorkflowDetailId() == null) {
							workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, workFlowPersonId,
									updatedUser, actionType, approverComment, subModuleCode, Constants.SUBMODULE_ITEM_KEY,
									awardVO.getMapId(), awardVO.getMapNumber(), awardVO.getApproverStopNumber(),
									awardVO.getApproverNumber());
							awardVO.setWorkflowDetailId(Integer.parseInt(workflowDetailId));
						} else {
							workflowDetailId = awardVO.getWorkflowDetailId().toString();
						}
					if (awardVO.getActionType().equals("A") || awardVO.getActionType().equals("B")) {
						logger.info("In case of approve or bypass in approveOrRejectAward");
						awardVO.setMessage(APPROVAL_SUCCESS);
						awardVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO,
								Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
						List<Integer> taskIds = taskDao.getPersonTaskIds(awardId, Constants.AWARD_MODULE_CODE,
								logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
						if (taskIds != null && !taskIds.isEmpty()) {
							for (Integer taskId : taskIds) {
								List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao
										.getEditableSections(awardId, taskId.toString(), Constants.AWARD_MODULE_CODE,
												logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
								if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
									awardVO.getSectionTypeCodes().addAll(moduleVariableSections);
								}
							}
						}
						if (activeAward != null) {
							ServiceRequest serviceRequest = awardDao.getServiceRequestBasedOnAwardId(activeAwardId,
									awardId);
							if (serviceRequest != null && isFinalApprover.equals("1")) {
								serviceRequest = serviceRequestService.updateSRStatusAndActionLog(serviceRequest,Constants.RESOLVED_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_RESOLVED);
								awardVO.setServiceRequest(serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest));
							}
						}
					} else if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS) && awardVO.getActionType().equals("R")) {
						logger.info("In case of reject in approveOrRejectAward");
						award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED);
						award.setAwardWorkflowStatus(awardWorkflowDao
								.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED));
						awardVO.setMessage(APPROVAL_REJECTED);
						awardVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO,
								Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
						List<Integer> taskIds = taskDao.getPersonTaskIds(awardId, Constants.AWARD_MODULE_CODE,
								logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
						if (taskIds != null && !taskIds.isEmpty()) {
							for (Integer taskId : taskIds) {
								List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao
										.getEditableSections(awardId, taskId.toString(), Constants.AWARD_MODULE_CODE,
												logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
								if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
									awardVO.getSectionTypeCodes().addAll(moduleVariableSections);
								}
							}
						}
						if (activeAward != null) {
							ServiceRequest serviceRequest = awardDao.getServiceRequestBasedOnAwardId(activeAwardId,
									awardId);
							if (serviceRequest != null) {
								serviceRequest = serviceRequestService.updateSRStatusAndActionLog(serviceRequest,Constants.RETURN_IN_ROUTE_LOG_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_RETURNED);
								awardVO.setServiceRequest(serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest));
							}
						}
					} else if (!awardVO.getActionType().equals("C")) {
						awardVO.setMessage(APPROVAL_FAILED);
						logger.info("In case of approval failed in approveOrRejectAward");
						awardVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO,
								Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
						List<Integer> taskIds = taskDao.getPersonTaskIds(awardId, Constants.AWARD_MODULE_CODE,
								logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
						if (taskIds != null && !taskIds.isEmpty()) {
							for (Integer taskId : taskIds) {
								List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao
										.getEditableSections(awardId, taskId.toString(), Constants.AWARD_MODULE_CODE,
												logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
								if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
									awardVO.getSectionTypeCodes().addAll(moduleVariableSections);
								}
							}
						}
						return awardVO;
					}
				}
			}
			awardVO.setAward(award);
			if (files != null && workflowDetailId != null) {
				Integer id = Integer.parseInt(workflowDetailId);
				uploadAttachment(files, updatedUser, id);
			}
			if (workflowDetailId != null && isFinalApprover.equals("1")) {
					if (awardVO.getActionType().equals("A") || awardVO.getActionType().equals("B")) {
						logger.info("In case of final approver approve or bypass routelog in approveOrRejectAward");
						String awardStatusCode = commonDao.getParameterValueAsString(Constants.AWARD_STATUS_ON_APPROVAL);
						String awardVariationTypeCode = award.getAwardVariationTypeCode();
						String awardDocumentTypeCode = award.getAwardDocumentTypeCode();
						awardWorkflowService.sapBasedWorkflowStatusAndSequenceStatus(award, awardVO.getUpdateUser());
						award.setIsLatest(false);
						if (awardDocumentTypeCode.equals(Constants.AWARD_VARIATION) && awardVariationTypeCode.equals(Constants.PROJECT_CLOSURE_TYPE_CODE)) {
							award.setStatusCode(Constants.AWARD_STATUS_CODE_CLOSED);
							award.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_CLOSED));
							if (activeAward != null) {
								awardDao.updateAwardStatusByStatusCode(activeAward.getAwardId(), Constants.AWARD_STATUS_CODE_CLOSED);
							}
						awardDao.saveOrUpdateAwardDetails(award);
						} else {
							if (awardDocumentTypeCode.equals(Constants.AWARD_SETUP) && award.getBeginDate().after(commonDao.getCurrentTimestamp())) {
								award.setStatusCode(awardStatusCode);
								award.setAwardStatus(awardDao.fetchAwardStatusByCode(awardStatusCode));
							} else {
								award.setStatusCode(Constants.AWARD_STATUS_CODE_AWARDED);
								award.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_AWARDED));
							}
							awardDao.saveOrUpdateAwardDetails(award);
						}
						awardVO.setIsMasterAwardCreation(true);
						awardVO.setMessage(FINAL_APPROVAL_SUCCESS);
						if (commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED)){
							manpowerIntegrationSchedulerService.checkForManpowerIntegration(award, currentActiveAward, newAwardPIPerson, activeAwardPIPerson, activeAwardSuperiorSupOrg);
						}
					} else if (awardVO.getActionType().equals("R")) {
						logger.info("In case of final approver reject routelog in approveOrRejectAward");
						award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED);
						award.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED));
						awardDao.saveOrUpdateAwardDetails(award);
						awardVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO, Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
						List<Integer> taskIds = taskDao.getPersonTaskIds(awardId, Constants.AWARD_MODULE_CODE, logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
						if (taskIds != null && !taskIds.isEmpty()) {
							taskIds.stream().forEach(taskId -> {
								List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(awardId, taskId.toString(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
								if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
									awardVO.getSectionTypeCodes().addAll(moduleVariableSections);
								}
							});
						}
						awardVO.setMessage(FINAL_APPROVAL_REJECTED);
					}
			}
			return awardVO;
	}

	private void exceptionAppendToFile(Exception e) {
		try {
			Timestamp currentDate = commonDao.getCurrentTimestamp();
			BufferedWriter out = new BufferedWriter(new FileWriter("/opt/tomcat/ExceptionAwardWorkflowApproval.txt", true));
			out.write(currentDate.toString() + "\r\n" + StringUtils.repeat("-", 100) + "\r\n");
			PrintWriter pWriter = new PrintWriter(out, true);
			e.printStackTrace(pWriter);
		} catch (Exception ie) {
			throw new RuntimeException("Could not write Exception to file", ie);
		}
	}

	private void sendApproveOrDisApproveNotificationForProposal(ProposalVO vo, String approvalStatus) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0)) {
			if ("A".equals(approvalStatus) || "B".equals(approvalStatus) && vo.getWorkflow() != null) {
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
					vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
					vo.setMapId(workflowDetail.getMapId());
				}
				if (workFlowDetails != null && !workFlowDetails.isEmpty()) {
					proposalService.sendProposalNotification(vo, Constants.PROPOSAL_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
				}
				if ("B".equals(approvalStatus)) {
					sendProposalBypassNotification(vo);
				}
			} else if ("R".equals(approvalStatus)) {
				proposalService.sendProposalNotification(vo, Constants.NOTIFICATION_PROPOSAL_REJECTED, dynamicEmailrecipients);
			}
		}
	}

	private void sendApproveOrDisApproveNotificationForAwardAndServiceRequest(AwardVO vo, String approvalStatus) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0) && vo.getWorkflow() != null) {
			if ("A".equals(approvalStatus) || "B".equals(approvalStatus)) {
				sendAwardNotificationBasedOnUserRole(vo.getWorkflow(), dynamicEmailrecipients, vo);
				if ("B".equals(approvalStatus)) {
					sendAwardBypassNotification(vo);
				}
			} else if ("R".equals(approvalStatus)) {
				Award award = vo.getAward();
				setRecipientWithUserName(award.getSubmitUser(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
				AwardPerson piAwardPerson = awardWorkflowService.getAwardPrincipalInvestigator(award.getAwardPersons());
				if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
					awardService.sendAwardNotification(vo, Constants.AWARD_REJECT_NOTIFICATION_CODE, dynamicEmailrecipients);
				} else {
						ServiceRequestVO serviceRequestVO = serviceRequestService.setServiceRequestDetailsForAward(award, piAwardPerson, "");
						serviceRequestVO.setWorkflowDetailId(vo.getWorkflowDetailId());
						serviceRequestVO.setActionType(vo.getActionType());
						serviceRequestService.sendServiceRequestNotification(serviceRequestVO, Constants.SERVICE_REQUEST_REVISION_NOTIFICATION_CODE, dynamicEmailrecipients, vo.getApproveComment());
						serviceRequestService.sendServiceRequestNotification(serviceRequestVO, Constants.SERVICE_REQUEST_REVISION_TO_PI_NOTIFICATION_CODE, new HashSet<>(), vo.getApproveComment());
				}
			}
		}
	}

	private void setRecipientWithUserName(String submitUser, String notificationRecipientType,Set<NotificationRecipient> dynamicEmailrecipients) {
		String submittedUser = personDao.getPersonIdByUserName(submitUser);
		if (submittedUser != null) {
			commonService.setNotificationRecipients(submittedUser, notificationRecipientType, dynamicEmailrecipients);	
		}
	}

	private void sendAwardBypassNotification(AwardVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0) && vo.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				vo.setMapId(workflowDetail.getMapId());
			}
			awardService.sendAwardNotification(vo, Constants.AWARD_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
	}

	private void sendProposalBypassNotification(ProposalVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0) && vo.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				vo.setMapId(workflowDetail.getMapId());
			}
			if (workFlowDetails != null && !workFlowDetails.isEmpty()) {
				proposalService.sendProposalNotification(vo, Constants.PROPOSAL_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients);
			}
		}
	}

	private void sendNegotiationApproveOrDisapproveNotification(NegotiationVO negotiationVO, String actionType, String isFinalApprover) {
		if (negotiationVO.getWorkFlowDetailId() != null && !negotiationVO.getWorkFlowDetailId().equals(0)) {
			if (actionType.equalsIgnoreCase("A") && (isFinalApprover.equalsIgnoreCase("0")) && negotiationVO.getWorkflow() != null) {
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(negotiationVO.getWorkflow().getWorkflowDetails(), null, null, negotiationVO.getWorkFlowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), "TO", dynamicEmailrecipients);
				}
				if (!dynamicEmailrecipients.isEmpty()) {
					negotiationService.sendNegotiationNotification(negotiationVO, Constants.NOTIFICATION_NEGOTIATION_APPROVED, dynamicEmailrecipients);
				}
			} else if (actionType.equalsIgnoreCase("A") && (isFinalApprover.equalsIgnoreCase("1"))) {
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				commonService.setNotificationRecipients(negotiationVO.getNegotiations().getNegotiatorPersonId(), "TO", dynamicEmailrecipients);
				negotiationService.sendNegotiationNotification(negotiationVO, Constants.NOTIFICATION_NEGOTIATION_FINAL_APPROVED, dynamicEmailrecipients);
			} else if (actionType.equalsIgnoreCase("R")) {
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				commonService.setNotificationRecipients(negotiationVO.getNegotiations().getNegotiatorPersonId(), "TO", dynamicEmailrecipients);
				negotiationService.sendNegotiationNotification(negotiationVO, Constants.NOTIFICATION_NEGOTIATION_REJECTED, dynamicEmailrecipients);
			}
		}
	}

	private void sendAwardNotificationBasedOnUserRole(Workflow workFlow, Set<NotificationRecipient> dynamicEmailrecipients, AwardVO vo) {
		Integer assigneePersonRoleCode = null;
		List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(workFlow.getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
		for (WorkflowDetail workflowDetail : workFlowDetails) {
			if (workflowDetail.getPersonRoleType() != null) {
				assigneePersonRoleCode = workflowDetail.getPersonRoleType().getRoleTypeCode();
			}
			commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
			vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
			vo.setMapId(workflowDetail.getMapId());
		}
		if (vo.getAward().getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
			if (Constants.FINANCE_ROLE_TYPE_CODE.equals(assigneePersonRoleCode) ) {
				awardService.sendAwardNotification(vo, Constants.FINANCE_AWARD_ASSIGN_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else if (Constants.GRANT_ADMINISTRATOR_ROLE_TYPE_CODE.equals(assigneePersonRoleCode)) {
				awardService.sendAwardNotification(vo, Constants.GRANT_ADMIN_AWARD_ASSIGN_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else {
				awardService.sendAwardNotification(vo, Constants.AWARD_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
			}
		} else {
			awardService.sendAwardNotification(vo, Constants.AWARD_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
		}
	}

	private List<WorkflowDetail> waitingWorkflowDetails(List<WorkflowDetail> workFlowDetails, Integer mapNumber, Integer approverStopNumber, Integer workflowDetailId ) {
		if (approverStopNumber == null && mapNumber == null && workflowDetailId != null && !workflowDetailId.equals(0)) {
			Optional<WorkflowDetail> workflowDetail = workFlowDetails.stream().filter(detail -> detail.getWorkflowDetailId().equals(workflowDetailId)).findFirst();
			if (workflowDetail.isPresent()) {
				mapNumber = workflowDetail.get().getMapNumber();
				approverStopNumber = workflowDetail.get().getApprovalStopNumber();
			}
		}
		if (approverStopNumber != null && mapNumber != null) {
			AtomicInteger map = new AtomicInteger(mapNumber);
			AtomicInteger stop = new AtomicInteger(approverStopNumber);
			return workFlowDetails.stream().filter(workflowDetail -> workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)
						&& ((workflowDetail.getMapNumber() > map.intValue())
								|| ((workflowDetail.getMapNumber().equals(map.intValue()))
										&& (workflowDetail.getApprovalStopNumber() > stop.intValue()))) && !workflowDetail.getWorkflowMap().getMapType().equals("E")).collect(Collectors.toList());
		} else {
			return new ArrayList<>();
		}
		
	}

	private List<WorkflowDetail> bypassedWorkflowDetails(List<WorkflowDetail> workFlowDetails, Integer mapNumber, Integer approverStopNumber, Integer workflowDetailId) {
		Integer approverNumber = null;
		if (workflowDetailId != null && !workflowDetailId.equals(0)) {
			Optional<WorkflowDetail> workflowDetail = workFlowDetails.stream().filter(detail -> detail.getWorkflowDetailId().equals(workflowDetailId)).findFirst();
			if (workflowDetail.isPresent()) {
				mapNumber = workflowDetail.get().getMapNumber();
				approverStopNumber = workflowDetail.get().getApprovalStopNumber();
				approverNumber = workflowDetail.get().getApproverNumber();
			}
		}
		if (approverStopNumber != null && mapNumber != null && approverNumber != null) {
			AtomicInteger map = new AtomicInteger(mapNumber);
			AtomicInteger stop = new AtomicInteger(approverStopNumber);
			AtomicInteger approver = new AtomicInteger(approverNumber);
			return workFlowDetails.stream().filter(workflowDetail -> workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_BYPASSED)
					&& workflowDetail.getMapNumber().equals(map.intValue())
					&& (workflowDetail.getApprovalStopNumber().equals(stop.intValue()))
					&& (workflowDetail.getApproverNumber().equals(approver.intValue()))).collect(Collectors.toList());
		}
		return new ArrayList<>();
	}

	@Override
	public void sendFinalApprovalNotification(AwardVO vo) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		commonService.setNotificationRecipients(AuthenticatedUser.getLoginPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_CC, dynamicEmailrecipients);
		Award award = vo.getAward();
		AwardPerson piAwardPerson = awardWorkflowService.getAwardPrincipalInvestigator(award.getAwardPersons());
		if (piAwardPerson != null) {
			if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
				awardService.sendAwardNotification(vo, Constants.LIVE_AWARD_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else {
				ServiceRequestVO serviceRequestVO = serviceRequestService.setServiceRequestDetailsForAward(award, piAwardPerson, "");
				List<String> srTypes = new ArrayList<>();
				srTypes.add("9");
				srTypes.add("19");
				srTypes.add("20");
				Integer notificationId = srTypes.contains(award.getAwardVariationTypeCode()) ?  Constants.MANPOWER_VARIATION_FINAL_APPROVAL: Constants.SERVICE_REQUEST_OUTCOME_NOTIFICATION_CODE ;
				serviceRequestService.sendServiceRequestNotification(serviceRequestVO, notificationId, dynamicEmailrecipients, vo.getApproveComment());
			}
		}
	}

	@Override
	public ProposalVO endorseProposal(Integer proposalId, String updatedUser, String moduleItemKey,
			String logginPersonId, Integer subModuleCode) {
		ProposalVO proposalVO = new ProposalVO();
		proposalVO.setProposalId(proposalId);
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		proposal.setUpdateUser(updatedUser);
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalVO.setProposal(proposal);
		String actionType = "A";
		String workFlowPersonId = logginPersonId;
		proposalVO.setActionType(actionType);
		proposalVO.setPersonId(logginPersonId);
		logger.info(PROPOSAL_ID, proposalId);
		logger.info(UPDATE_USER, updatedUser);
		logger.info(MODULE_ITEM_KEY, moduleItemKey);
		logger.info(LOGIN_PERSON_ID, logginPersonId);
		logger.info(SUB_MODULE_CODE, subModuleCode);
		logger.info(ACTION_TYPE, actionType);

		String isFinalApprover = businessRuleDao.workflowfinalApproval(proposalId.toString(), workFlowPersonId,	Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, subModuleCode);
		logger.info(IS_FINAL_APPROVER, isFinalApprover);
		Integer canApproveRouting = businessRuleDao.canApproveRouting(proposalId.toString(), workFlowPersonId,
				Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, subModuleCode);
		logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
		MultipartFile[] files = null;
		proposalVO = approveOrRejectProposal(proposalVO, proposalId.toString(),
				Constants.DEV_PROPOSAL_MODULE_CODE.toString(), workFlowPersonId, subModuleCode, updatedUser, actionType,
				null, isFinalApprover, files);
		proposal = proposalDao.saveOrUpdateProposal(proposal);

		if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING)
				|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)
				|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_RETURNED)
				|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)
				|| proposal.getStatusCode().equals(Constants.COMPLETED)
				|| proposal.getStatusCode().equals(Constants.ADMIN_CHECK_COMPLETED)
				|| proposal.getStatusCode().equals(Constants.REVIEW_IN_PROGRESS)) {
			Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
			workflowService.prepareWorkflowDetails(workflow);
			proposalVO.setWorkflow(workflow);
		}
		return proposalVO;
	}

	private void sendCompleteReviewNotification(ProposalVO proposalVO, Integer notificationId) {
		proposalService.sendProposalNotification(proposalVO, notificationId, new HashSet<>());
	}

	private void sendMainPanelReviewNotification(ProposalVO proposalVO) {
		List<String> mainPanelPersonIds = proposalDao.fetchMainPanelPersonIdsByGrantCallId(proposalVO.getProposal().getGrantCallId());
		for (String personId : mainPanelPersonIds) {
			proposalVO.setUserFullName(personDao.getPersonFullNameByPersonId(personId));
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			commonService.setNotificationRecipients(personId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
			proposalService.sendProposalNotification(proposalVO, Constants.MAIN_PANEL_EVALUATION_NOTIFICATION_CODE, dynamicEmailrecipients);
		}
	}

	private String taskWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode, String subModuleCode) {
		ObjectMapper mapper = new ObjectMapper();
		TaskVO taskVO = new TaskVO();
		try {
			taskVO = mapper.readValue(formDataJson, TaskVO.class);
			Integer taskId = taskVO.getTaskId();
			String awardNumber = taskVO.getModuleItemKey();
			String updatedUser = taskVO.getUpdateUser();
			String moduleItemKey = taskVO.getModuleItemId().toString();
			String logginPersonId = taskVO.getPersonId();
			String workFlowPersonId = taskVO.getWorkFlowPersonId();
			Task task = taskDao.fetchTaskByTaskId(taskId);
			task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			taskVO.setModuleCode(Integer.parseInt(moduleCode));
			taskVO.setSubModuleCode(Integer.parseInt(subModuleCode));
			taskVO.setTask(task);
			String actionType = taskVO.getActionType();
			String approverComment = taskVO.getApproveComment();
			if (task.getUpdateUser() != null) {
				task.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(task.getUpdateUser()));
				task.setSubmitUserFullName(personDao.getUserFullNameByUserName(task.getUpdateUser()));
			}
			if (task.getCreateUser() != null) {
				task.setCreateUserFullName(personDao.getUserFullNameByUserName(task.getCreateUser()));
			}
			if (task.getAssigneePersonId() != null) {
				task.setAssigneeFullName(personDao.getPersonFullNameByPersonId(task.getAssigneePersonId()));
			}
			logger.info("taskId : {}", taskId);
			logger.info(AWARD_NUMBER, awardNumber);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(WORKFLOW_PERSON_ID, workFlowPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), taskId.toString(), Integer.parseInt(subModuleCode));
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), taskId.toString(), Integer.parseInt(subModuleCode));
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			taskVO = approveOrRejectTask(taskVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files, logginPersonId);
			task = taskDao.saveOrUpdateTask(task);
			if (task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS) || task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_RETURNED) || task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_COMPLETED)) {
				taskService.canTaskTakeRoutingAction(taskVO);
				Workflow workFlow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey, Integer.parseInt(moduleCode), taskId.toString(), Integer.parseInt(subModuleCode));
				if (workFlow != null) {
					workflowService.prepareWorkflowDetails(workFlow);
					taskVO.setWorkflow(workFlow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey, taskVO.getModuleCode(), taskId.toString(), taskVO.getSubModuleCode());
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						taskVO.setWorkflowList(workFlows);
					}
				}
				if (workFlow !=null && workFlow.getCurrentStopName() != null && task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS)) {
					task.setWorkFlowStatusName(task.getTaskStatus().getDescription() + " : " +workFlow.getCurrentStopName());
				} else {
					task.setWorkFlowStatusName(task.getTaskStatus().getDescription());
				}
			}
			taskVO.setTask(task);
			taskVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), task.getTaskId().toString(), Integer.parseInt(subModuleCode)).toString());
			taskVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), task.getTaskId().toString(), Integer.parseInt(subModuleCode)));
			taskVO.setTask(taskDao.fetchTaskByTaskId(taskVO.getTaskId()));
			String leadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(Integer.parseInt(moduleItemKey));
			if (leadUnitNumber != null) {
				taskVO.setLeadUnitNumber(leadUnitNumber);
			}
			taskVO.setTaskCount(taskService.fetchTaskCountByModuleItemKey(task.getModuleItemKey()));
			awardService.updateAwardDocumentUpdateUserAndTimestamp(taskVO.getModuleItemId(), updatedUser);
			commonDao.doflush();
			if (!isFinalApprover.equals("1") || "R".equals(actionType)) {
				sendApproveOrDisApproveNotificationForTask(taskVO, actionType);
			} else if (isFinalApprover.equals("1") && !"R".equals(actionType)) {
				sendCompleteReviewNotificationForTask(taskVO);
				if (task.getTaskTypeCode().equals(Constants.TASK_CONFIRM_PROJECT_DETAILS)) {
					taskVO.setTaskTypeCode(task.getTaskTypeCode());
					taskService.sendNotficationForFinanceAssignee(taskVO, Constants.TASK_CONFIRM_PROJECT_DETAIS_COMPLETES_NOTIFICTION_CODE);
				}
			}
		} catch (Exception e) {
			logger.error("exception in taskWorkflowApproval: {} ", e.getMessage());
			throw new ApplicationException("exception in taskWorkflowApproval", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(taskVO);
	}

	private TaskVO approveOrRejectTask(TaskVO taskVO, String moduleItemKey, String moduleCode, String workFlowPersonId,
			String subModuleCode, String updatedUser, String actionType, String approverComment,
			String isFinalApprover, Integer canApproveRouting, MultipartFile[] files, String logginPersonId) {
		Task task = taskVO.getTask();
		String workflowDetailId = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Integer.parseInt(moduleCode));
		evaluateValidationRuleVO.setLogginPersonId(workFlowPersonId);
		evaluateValidationRuleVO.setSubModuleCode(Integer.parseInt(subModuleCode));
		evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
		evaluateValidationRuleVO.setUpdateUser(updatedUser);
		evaluateValidationRuleVO.setAcType(taskVO.getActionType());
		evaluateValidationRuleVO.setComments(taskVO.getApproveComment());
		if (canApproveRouting.toString().equals("0")) {
			taskVO.setMessage(APPROVAL_FAILED);
			return taskVO;
		} else {
			if (taskVO.getActionType().equals("A") || taskVO.getActionType().equals("B")) {
				taskVO.setMessage(APPROVAL_SUCCESS);
				taskVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(task.getModuleItemId(), Constants.ZERO, Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
				List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
				if (taskIds != null && !taskIds.isEmpty()) {
					for (Integer taskId : taskIds) {
						List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
						if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
							taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
						}
					}
				}
			} else if (task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS) && taskVO.getActionType().equals("R")) {
				task.setTaskStatusCode(Constants.TASK_STATUS_CODE_RETURNED);
				task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_RETURNED));
				taskVO.setMessage(APPROVAL_REJECTED);
				taskVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(task.getModuleItemId(), Constants.ZERO, Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
				List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
				if (taskIds != null && !taskIds.isEmpty()) {
					for (Integer taskId : taskIds) {
						List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
						if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
							taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
						}
					}
				}
				InboxVO inboxVO =new InboxVO();
				inboxVO.setModuleCode(taskVO.getModuleCode());
				inboxVO.setModuleItemKey(taskVO.getModuleItemId().toString());
				inboxVO.setMessageTypeCode(Constants.MESSAGE_TYPE_TASK_REJECT);
				inboxVO.setSubModuleCode(taskVO.getSubModuleCode());
				inboxVO.setSubModuleItemKey(taskVO.getTaskId().toString());
				inboxVO.setToPersonId(taskVO.getPersonId());
				inboxDao.markReadMessage(inboxVO);
			} else {
				taskVO.setMessage(APPROVAL_FAILED);
				taskVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(task.getModuleItemId(), Constants.ZERO, Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_SUBMODULE_CODE));
				List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
				if (taskIds != null && !taskIds.isEmpty()) {
					for (Integer taskId : taskIds) {
						List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, logginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
						if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
							taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
						}
					}
				}
				return taskVO;
			}
		}
		taskVO.setTask(task);
		workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, workFlowPersonId, updatedUser,
				actionType, approverComment, Integer.parseInt(subModuleCode), task.getTaskId().toString(),
				taskVO.getMapId(), taskVO.getMapNumber(), taskVO.getApproverStopNumber(), taskVO.getApproverNumber());
		if (files != null && workflowDetailId != null) {
			Integer id = Integer.parseInt(workflowDetailId);
			uploadAttachment(files, updatedUser, id);
		}
		if (workflowDetailId != null) {
			String taskType = task.getTaskType().getDescription();
			if (isFinalApprover.equals("1")) {
				if (taskVO.getActionType().equals("A") || taskVO.getActionType().equals("B")) {
					task.setTaskStatusCode(Constants.TASK_STATUS_CODE_COMPLETED);
					task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_COMPLETED));
					task.setEndModuleSubItemKey(taskVO.getEndModuleSubItemKey());
					taskVO.setMessage(FINAL_APPROVAL_SUCCESS);
					if (taskVO.getActionType().equals("A")) {
						String approveComment = taskType + " Task";
						taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_APPROVED, approveComment, taskVO.getUpdateUser());
					} else if (taskVO.getActionType().equals("B")) {
						String bypassComment = taskType + " Task";
						taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_BYPASSED, bypassComment, taskVO.getUpdateUser());
					}
					String completeComment = taskType + " Task";
					String assigneeUserName = personDao.getPersonDetailById(task.getAssigneePersonId()).getPrincipalName();
					taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_COMPLETED, completeComment, assigneeUserName);
				} else if (taskVO.getActionType().equals("R")) {
					task.setTaskStatusCode(Constants.TASK_STATUS_CODE_RETURNED);
					task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_RETURNED));
					taskVO.setMessage(FINAL_APPROVAL_REJECTED);
				}
				taskDao.saveOrUpdateTask(task);
			}
			taskVO.setWorkFlowDetailId(Integer.parseInt(workflowDetailId));
		}
		taskVO.setTaskStatusCode(taskDao.fetchTaskByTaskId(task.getTaskId()).getTaskStatusCode());
		return taskVO;
	}

	private void sendApproveOrDisApproveNotificationForTask(TaskVO vo, String approvalStatus) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkFlowDetailId() != null && !vo.getWorkFlowDetailId().equals(0)) {
		Task task = taskDao.fetchTaskByTaskId(vo.getTaskId());
		String taskType = task.getTaskType().getDescription();
		if ("A".equals(approvalStatus) || "B".equals(approvalStatus)) {
			if (vo.getWorkflow() != null) {
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkFlowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
					vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
					vo.setMapId(workflowDetail.getMapId());
				}
			}
			taskService.sendTaskNotification(vo, Constants.TASK_APPROVE_NOTIFICATION_CODE, dynamicEmailRecipients);
			if (vo.getActionType().equals("A")) {
				String approveComment = taskType + " Task";
				taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_APPROVED, approveComment, vo.getUpdateUser());
			} else if (vo.getActionType().equals("B")) {
				String bypassComment = taskType + " Task";
				taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_BYPASSED, bypassComment, vo.getUpdateUser());
				sendBypassNotificationForTask(vo);
			}
		} else if ("R".equals(approvalStatus)) {
			commonService.setNotificationRecipients(task.getAssigneePersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			taskService.sendTaskNotification(vo, Constants.TASK_RETURNED_NOTIFICATION_CODE, dynamicEmailRecipients);
			String systemComment = taskType + " Task";
			taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_RETURNED, systemComment, vo.getUpdateUser());
		}
		}
	}

	private void sendCompleteReviewNotificationForTask(TaskVO taskVO) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String assigneePersonId = taskVO.getTask().getAssigneePersonId();
		commonService.setNotificationRecipients(assigneePersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		taskService.sendTaskNotification(taskVO, Constants.TASK_REVIEW_COMPLETE_NOTIFICATION_CODE, dynamicEmailRecipients);
	}

	private void sendBypassNotificationForTask(TaskVO taskVO) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (taskVO.getWorkFlowDetailId() != null && !taskVO.getWorkFlowDetailId().equals(0) && taskVO.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(taskVO.getWorkflow().getWorkflowDetails(), taskVO.getMapNumber(), taskVO.getApproverStopNumber(), taskVO.getWorkFlowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				taskVO.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				taskVO.setMapId(workflowDetail.getMapId());
			}
			taskService.sendTaskNotification(taskVO, Constants.TASK_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
	}

	@Override
	public void evaluateAndSentNotification(Integer moduleCode, Integer subModuleCode, String moduleItemKey, String subModuleItemKey, String personId, String updateUser, 
			Map<String, String> placeHolder) {
			EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
			evaluateValidationRuleVO.setModuleCode(moduleCode);
			evaluateValidationRuleVO.setSubModuleCode(subModuleCode);
			evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
			evaluateValidationRuleVO.setSubModuleItemKey(subModuleItemKey);
			evaluateValidationRuleVO.setLogginPersonId(personId);
			evaluateValidationRuleVO.setUpdateUser(updateUser);
			List<String> notifications = businessRuleDao.evaluateNotificationRule(evaluateValidationRuleVO);
			if (notifications != null && !notifications.isEmpty()) {
				for (String notficationId : notifications) {
					EmailServiceVO emailServiceVO = new EmailServiceVO();
					emailServiceVO.setModuleCode(moduleCode);
					emailServiceVO.setModuleItemKey(moduleItemKey);
					emailServiceVO.setSubModuleCode(subModuleCode.toString());
					emailServiceVO.setSubModuleItemKey(subModuleItemKey);
					emailServiceVO.setPlaceHolder(placeHolder);
					emailServiceVO.setNotificationTypeId(Integer.parseInt(notficationId));
					emailService.sendEmail(emailServiceVO);
				}
			}
	}

	@Override
	public String approveOrRejectWorkflowForWaf(MultipartFile file, String formDataJson, String moduleCode) {
		if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_NEGOTIATIONS))) {
			return negotiationWorkflowApprovalForWaf(file, formDataJson, moduleCode);
		} else if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL))) {
			return proposalWorkflowApprovalForWaf(file, formDataJson, moduleCode);
		} else if (moduleCode.equals(String.valueOf(Constants.MODULE_CODE_AWARD))) {
			return awardWorkflowApprovalForWaf(formDataJson, moduleCode);
		}
		return commonDao.convertObjectToJSON("");
	}

	private String awardWorkflowApprovalForWaf(String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		AwardVO awardVO = new AwardVO();
		try {
			awardVO = mapper.readValue(formDataJson, AwardVO.class);
			awardVO.setModuleCode(Integer.parseInt(moduleCode));
			approveOrRejectAwardWorkflow(awardVO);
		} catch (Exception e) {
			logger.error("Error occured in awardWorkflowApprovalForWaf : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(awardVO);
	}

	private String proposalWorkflowApprovalForWaf(MultipartFile file, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		ProposalVO proposalVO = new ProposalVO();
		try {
			proposalVO = mapper.readValue(formDataJson, ProposalVO.class);
			MultipartFile multipartFile = null;
			Integer proposalId = proposalVO.getProposalId();
			String updatedUser = proposalVO.getUpdateUser();
			String moduleItemKey = proposalId.toString();
			String logginPersonId = proposalVO.getPersonId();
			String workFlowPersonId = proposalVO.getWorkFlowPersonId();
			MultipartFile[] files = new MultipartFile[0]; 
			String name = file.getName();
			Integer remaining = proposalVO.getRemaining();
			Integer length = proposalVO.getLength();
			String userId = proposalVO.getPersonId();
			String contentType = file.getContentType();
			String splicedFile = proposalVO.getFileContent();
			String timestamp = proposalVO.getFileTimestamp();
			if (splicedFile != null) {
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if(multipartFile != null && !multipartFile.isEmpty()) {
				files[0] = multipartFile;
			}
			Integer subModuleCode = Constants.DEV_PROPOSAL_SUBMODULE_CODE;
			Proposal proposal = proposalDao.fetchProposalById(proposalId);
			proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId));
			proposal.setUpdateUser(updatedUser);
			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposalVO.setProposal(proposal);
			String actionType = proposalVO.getActionType();
			String approverComment = proposalVO.getApproveComment();

			logger.info("proposalId : " + proposalId);
			logger.info("updatedUser : " + updatedUser);
			logger.info("moduleItemKey : " + moduleItemKey);
			logger.info("loginPersonId : " + logginPersonId);
			logger.info("subModuleCode : " + subModuleCode);
			logger.info("actionType : " + actionType);
			logger.info("approverComment : " + approverComment);

			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info("isFinalApprover : " + isFinalApprover);
			Integer canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info("canApproveRouting : " + canApproveRouting);
			proposalVO = approveOrRejectProposal(proposalVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, files);
			proposal = proposalDao.saveOrUpdateProposal(proposal);

			if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING) 
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_RETURNED) 
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)
					|| proposal.getStatusCode().equals(Constants.COMPLETED)
					|| proposal.getStatusCode().equals(Constants.ADMIN_CHECK_COMPLETED)
					|| proposal.getStatusCode().equals(Constants.REVIEW_IN_PROGRESS)) {
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				workflowService.prepareWorkflowDetails(workflow);
				proposalVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					proposalVO.setWorkflowList(workFlows);
				}
			}
			proposalVO.setProposal(proposal);
			proposalVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
			proposalVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
			//proposalVO.setIsFinalApprover(isFinalApprover);
			if (!isFinalApprover.equals("1")) {
				sendApproveOrDisApproveNotificationForProposal(proposalVO, actionType);
			}
			proposalService.loadInitialData(proposalVO);
			proposalService.loadProposalHomeData(proposalVO);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private String negotiationWorkflowApprovalForWaf(MultipartFile file, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		NegotiationVO negotiationVO = new NegotiationVO();
		try {
			negotiationVO = mapper.readValue(formDataJson, NegotiationVO.class);
			MultipartFile multipartFile = null;
			Negotiations negotiations = negotiationAgreementDao.fetchNegotiationById(negotiationVO.getNegotiationId());
			String updatedUser = negotiations.getUpdateUser();
			String moduleItemKey = negotiationVO.getNegotiationId().toString();
			String logginPersonId = negotiationVO.getPersonId();
			Integer subModuleCode = 0;
			MultipartFile[] files = new MultipartFile[0]; 
			String name = file.getName();
			Integer remaining = negotiationVO.getRemaining();
			Integer length = negotiationVO.getLength();
			String contentType = file.getContentType();
			String splicedFile = negotiationVO.getFileContent();
			String timestamp = negotiationVO.getFileTimestamp();
			if (splicedFile != null) {
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, logginPersonId, contentType);
			}
			if(multipartFile != null && !multipartFile.isEmpty()) {
				files[0] = multipartFile;
			}
			String actionType = negotiationVO.getActionType();
			String approverComment = negotiationVO.getApproveComment();
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			Integer canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId,
					Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			negotiationVO = approveOrRejectedNegotiation(negotiationVO, moduleItemKey, moduleCode, logginPersonId,
					subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files);
			if (negotiationVO.getMessage() == null) {
				if ("approval_failed".equals(negotiationVO.getMessage())) {
					negotiationVO.setMessage("action failed");
					return commonDao.convertObjectToJSON(negotiationVO);
				}
				negotiationVO.setMessage("action failed");
				return commonDao.convertObjectToJSON(negotiationVO);
			}
			if (negotiationVO.getActionType().equalsIgnoreCase("R")) {
				negotiations.setNegotiationStatusCode("3");
				negotiations.setNegotiationsStatus(negotiationAgreementDao.fetchStatusByStatusCode("3"));
				negotiationVO.setIsSubmit("1");
			} else if ((negotiationVO.getActionType().equalsIgnoreCase("A"))
					&& (negotiationVO.getIsFinalApprover().equalsIgnoreCase("1"))) {
				negotiations.setNegotiationStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVED.toString());
				negotiations.setNegotiationsStatus(
						negotiationAgreementDao.fetchStatusByStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVED.toString()));
			} else if ((negotiationVO.getActionType().equalsIgnoreCase("A"))
					&& (negotiationVO.getIsFinalApprover().equalsIgnoreCase("0"))) {
				negotiations.setNegotiationStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString());
				negotiations.setNegotiationsStatus(negotiationAgreementDao
						.fetchStatusByStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString()));
			}
			isFinalApprover = businessRuleDao.workflowfinalApproval(negotiationVO.getNegotiations().getNegotiationId().toString(), negotiationVO.getNegotiations().getNegotiatorPersonId(), Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			canApproveRouting = businessRuleDao.canApproveRouting(
					negotiationVO.getNegotiations().getNegotiationId().toString(),
					negotiationVO.getNegotiations().getNegotiatorPersonId(), Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			negotiationVO.setCanApproveRouting(canApproveRouting);
			negotiationVO.setIsFinalApprover(isFinalApprover);
			negotiations = negotiationAgreementDao.saveNegotiationInfo(negotiations);
			negotiationVO.setNegotiations(negotiations);
            Negotiations negotiation = negotiationAgreementDao.fetchNegotiationById(negotiationVO.getNegotiationId());			
			negotiationService.loadNegotiationUserFullNames(negotiation);
			negotiationVO.setNegotiations(negotiation);
			NegotiationMode negotiationsMode = negotiationService.getNegotiationMode(negotiation);
			if (negotiationsMode.getMode().equalsIgnoreCase(Constants.NEGOTIATION_EDIT_MODE)) {
				negotiationVO.setIsSubmit(Constants.NEGOTIATION_SHOW_SUBMIT_BUTTON);
			     }
				negotiationVO.setNegotiationMode(negotiationsMode);
				negotiationVO.setSubAwardOrganizationName(negotiationDao.getSubAwardOrganizationName(negotiationVO.getNegotiationId()));
			if (negotiationVO.getNegotiations().getNegotiationStatusCode()
					.equals(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString())) {
				negotiationVO.getNegotiationMode().setMode("EDIT");
				negotiationVO.getNegotiationMode().setStatus("1");
			}
			if (negotiationVO.getNegotiations().getNegotiationStatusCode()
					.equals(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString())
					|| negotiationVO.getNegotiations().getNegotiationStatusCode()
							.equals(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString())
					|| negotiationVO.getNegotiations().getNegotiationStatusCode()
							.equals(Constants.NEGOTIATION_STATUS_CODE_APPROVED.toString())) {
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
				workflowService.prepareWorkflowDetails(workflow);
				negotiationVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					negotiationVO.setWorkflowList(workFlows);
				}
			}
			sendNegotiationApproveOrDisapproveNotification(negotiationVO, actionType, isFinalApprover);
			if (negotiationVO.getNegotiationMode().getStatus().equals("1"))
				negotiationVO.setIsSubmit("1");
	
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(negotiationVO);
	}

	@Override
	public AwardVO approveOrRejectAwardWorkflow(AwardVO awardVO) {
		MultipartFile multipartFile = null;
		Integer awardId = awardVO.getAwardId();
		String awardNumber = awardVO.getAwardNumber();
		String updatedUser = awardVO.getUpdateUser();
		String moduleItemKey = awardId.toString();
		String logginPersonId = awardVO.getPersonId();
		String workFlowPersonId = awardVO.getWorkFlowPersonId();
		Integer subModuleCode = Constants.AWARD_SUBMODULE_CODE;
		MultipartFile[] files = new MultipartFile[1]; 
		String name = awardVO.getFileName();
		Integer remaining = awardVO.getRemaining();
		Integer length = awardVO.getLength();
		String userId = awardVO.getPersonId();
		String contentType = awardVO.getContentType();
		String splicedFile = awardVO.getFileContent();
		String timestamp = awardVO.getFileTimestamp();
		String isFinalApprover = awardVO.getIsFinalApprover();
		String moduleCode = awardVO.getModuleCode().toString();
		if (splicedFile != null) {
			multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
		}
		if (multipartFile != null && !multipartFile.isEmpty() || splicedFile == null) {
			if (splicedFile == null) {
				files = null;
			} else {
				files[0] = multipartFile;
			}
			Award award = awardDao.getAwardDetailsById(awardId);
			if (award.getSubmitUser() != null) {
				award.setSubmitUserFullName(personDao.getUserFullNameByUserName(award.getSubmitUser()));
			}
			award.setUpdateUser(updatedUser);
			award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardVO.setAward(award);
			String actionType = awardVO.getActionType();
			String approverComment = awardVO.getApproveComment();
			if (award.getUpdateUser() != null) {
				award.setUpdateUserFullName(personDao.getUserFullNameByUserName(award.getUpdateUser()));
			}
			if (award.getCreateUser() != null) {
				award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
			}
			if (award.getGrantHeaderId() != null) {
				award.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(award.getGrantHeaderId()));
			}
			logger.info("awardId : {}", awardId);
			logger.info("awardNumber : {}", awardNumber);
			logger.info("updatedUser : {}", updatedUser);
			logger.info("moduleItemKey : {}", moduleItemKey);
			logger.info("loginPersonId : {}", logginPersonId);
			logger.info("workFlowPersonId : {}", workFlowPersonId);
			logger.info("subModuleCode : {}", subModuleCode);
			logger.info("actionType : {}", actionType);
			logger.info("approverComment : {}", approverComment);

			if (awardVO.getWorkflowDetailId() == null) {
				isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
				awardVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
			}
			logger.info("isFinalApprover : {}", isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B") || actionType.equals("C")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			//Integer canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info("canApproveRouting : {}", canApproveRouting);
			awardVO = approveOrRejectAward(awardVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files, logginPersonId);
			award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award.setDocumentUpdateUser(updatedUser);
			award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award = awardDao.saveOrUpdateAwardDetails(award);

			if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)
					|| award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED)
					|| award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_ACTIVE)) {
				awardService.canTakeRoutingAction(awardVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					awardVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						awardVO.setWorkflowList(workFlows);
					}
				}
				if (workflow != null && workflow.getCurrentStopName() != null && award.getWorkflowAwardStatusCode()
						.equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
					award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription() + " : "
							+ workflow.getCurrentStopName());
				} else {
					award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription());
				}
			}
			awardVO.setAward(award);
			awardVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
			if ((!isFinalApprover.equals("1")) || (isFinalApprover.equals("1") && actionType.equals("R"))) {
				sendApproveOrDisApproveNotificationForAwardAndServiceRequest(awardVO, actionType);
			} else {
				sendFinalApprovalNotification(awardVO);
			}
			if (Boolean.TRUE.equals(awardVO.getIsMasterAwardCreation())) {
				awardVO.setAward(awardDao.fetchActiveAwardByAwardNumber(awardNumber));
			}
		}
		if (awardVO != null && awardVO.getAward() != null) {
			awardService.getPendingAwardDetails(awardVO, awardVO.getAward().getAwardNumber(), awardVO.getAward().getAwardId());
		}
		return awardVO;
	}

	@Override
	public String approveOrRejectAwardWorkflowForWaf(AwardVO awardVO) {
		return commonDao.convertObjectToJSON(approveOrRejectAwardWorkflow(awardVO));
	}

	private void sendNotificationForNoRoleBasedPersonAssigned(List<HashMap<String, Object>> details, EvaluateValidationRuleVO evaluateValidationRuleVO) {
		if (details != null && !details.isEmpty()) {
			for (HashMap<String, Object> detail : details) {
				EmailServiceVO emailServiceVO = new EmailServiceVO();
				if (evaluateValidationRuleVO.getModuleCode().equals(Constants.AWARD_MODULE_CODE)) {
					emailServiceVO.setNotificationTypeId(Constants.AWARD_MAP_ROLE_MISSING_NOTIFICATION);
				} else if (evaluateValidationRuleVO.getModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
					emailServiceVO.setNotificationTypeId(Constants.PROPOSAL_MAP_ROLE_MISSING_NOTIFICATION);
				} else if (evaluateValidationRuleVO.getModuleCode().equals(Constants.CLAIM_MODULE_CODE)) {
					emailServiceVO.setNotificationTypeId(Constants.CLAIM_MAP_ROLE_MISSING_NOTIFICATION);
				} else if (evaluateValidationRuleVO.getModuleCode().equals(Constants.PROGRESS_REPORT_MODULE_CODE)) {
					emailServiceVO.setNotificationTypeId(Constants.PROGRESS_REPORT_MAP_ROLE_MISSING_NOTIFICATION);
				}
				emailServiceVO.setModuleCode(evaluateValidationRuleVO.getModuleCode());
				emailServiceVO.setModuleItemKey(evaluateValidationRuleVO.getModuleItemKey());
				emailServiceVO.setPlaceHolder(getDynamicPlaceholders(detail));
				emailServiceVO.setSubModuleCode(evaluateValidationRuleVO.getSubModuleCode().toString());
				emailServiceVO.setSubModuleItemKey(evaluateValidationRuleVO.getSubModuleItemKey());
				emailServiceVO.setRecipients(new HashSet<>());
				emailService.sendEmail(emailServiceVO);
			}
		}
	}

	private Map<String, String> getDynamicPlaceholders(HashMap<String, Object> detail) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{MAP_NAME}", detail.get("MAP_NAME").toString() == null ? "" : detail.get("MAP_NAME").toString());
		placeHolder.put("{ROLE_NAME}", detail.get("ROLE_TYPE").toString() == null ? "" : detail.get("ROLE_TYPE").toString());
		return placeHolder;
	}

	private String claimWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		ClaimsVO claimVO = new ClaimsVO();
		try {
			claimVO = mapper.readValue(formDataJson, ClaimsVO.class);
			Integer claimId = claimVO.getClaimId();
			String updatedUser = claimVO.getUpdateUser();
			String moduleItemKey = claimVO.getClaimId().toString();
			String logginPersonId = claimVO.getPersonId();
			String workFlowPersonId = claimVO.getWorkFlowPersonId();
			Integer subModuleCode = Constants.CLAIM_SUBMODULE_CODE;
			Claim claim = claimsDao.getClaim(claimId);
			claim.setHostUnitDescription(commonDao.getUnitByUnitNumber(claim.getHostUnitNumber()).getUnitName());
			claimVO.setClaim(claim);
			String actionType = claimVO.getActionType();
			String approverComment = claimVO.getApproveComment();
			logger.info("claimId : {}", claimId);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(WORKFLOW_PERSON_ID, workFlowPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B") || actionType.equals("C")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			claimVO = approveOrRejectClaim(claimVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files);
			claimsDao.updateClaimDetailByParams(claimId, claim.getClaimStatusCode(), false, updatedUser, null);
			if (claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_APPROVAL_IN_PROGRESS) || claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_REVISION) || claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_APPROVED)) {
				claimsService.canClaimTakeRoutingAction(claimVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey, Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					claimVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey, Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						claimVO.setWorkflowList(workFlows);
					}
					claimVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
					claimVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
					if ((!isFinalApprover.equals("1")) || (isFinalApprover.equals("1") && actionType.equals("R"))) {
						sendApproveOrDisApproveNotificationForClaims(claimVO, actionType);
					} else if (actionType.equals("B")) {
						sendClaimBypassNotification(claimVO);
					}
				}
			}
			claim.setSubmitUserFullName(personDao.getPersonFullNameByPersonId(claimVO.getWorkflow().getWorkflowStartPerson()));
			claimVO.setClaim(claim);
			claimsService.loadClaimDetails(claimVO);
		} catch (Exception e) {
			logger.error("Exception in claimWorkflowApproval: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(claimVO);
	}


	private ClaimsVO approveOrRejectClaim(ClaimsVO claimVO, String moduleItemKey, String moduleCode,
			String workFlowPersonId, Integer subModuleCode, String updatedUser, String actionType,
			String approverComment, String isFinalApprover, Integer canApproveRouting, MultipartFile[] files) {
		Claim claim = claimVO.getClaim();
		String workflowDetailId = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Integer.parseInt(moduleCode));
		evaluateValidationRuleVO.setLogginPersonId(workFlowPersonId);
		evaluateValidationRuleVO.setSubModuleCode(subModuleCode);
		evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
		evaluateValidationRuleVO.setUpdateUser(updatedUser);
		evaluateValidationRuleVO.setAcType(claimVO.getActionType());
		evaluateValidationRuleVO.setComments(claimVO.getApproveComment());
		if (canApproveRouting.toString().equals("0")) {
			claimVO.setMessage(APPROVAL_FAILED);
			return claimVO;
		} else {
			if (claimVO.getActionType().equals("A") || claimVO.getActionType().equals("B")) {
				claimVO.setMessage(APPROVAL_SUCCESS);
			} else if (claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_APPROVAL_IN_PROGRESS) && claimVO.getActionType().equals("R")) {
				claim.setClaimStatusCode(Constants.CLAIM_STATUS_CODE_REVISION);
				claim.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_REVISION));
				claimVO.setMessage(APPROVAL_REJECTED);
				InboxVO inboxVO =new InboxVO();
				inboxVO.setModuleCode(Integer.parseInt(moduleCode));
				inboxVO.setModuleItemKey(claimVO.getClaimId().toString());
				inboxVO.setMessageTypeCode(Constants.MESSAGE_TYPE_CLAIM_REJECT);
				inboxVO.setSubModuleCode(subModuleCode);
				inboxVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				inboxVO.setToPersonId(claimVO.getPersonId());
				inboxDao.markReadMessage(inboxVO);
			} else {
				claimVO.setMessage(APPROVAL_FAILED);
				return claimVO;
			}
		}
		workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, workFlowPersonId, updatedUser,
				actionType, approverComment, subModuleCode, Constants.SUBMODULE_ITEM_KEY,
				claimVO.getMapId(), claimVO.getMapNumber(), claimVO.getApproverStopNumber(), claimVO.getApproverNumber());
		if (files != null && workflowDetailId != null) {
			Integer id = Integer.parseInt(workflowDetailId);
			uploadAttachment(files, updatedUser, id);
		}
		if (workflowDetailId != null) {
			if (isFinalApprover.equals("1")) {
				if (claimVO.getActionType().equals("A") || claimVO.getActionType().equals("B")) {
					claim.setClaimStatusCode(Constants.CLAIM_STATUS_CODE_APPROVED);
					claim.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_APPROVED));
					claimVO.setMessage(FINAL_APPROVAL_SUCCESS);
					String createUserId = personDao.getPersonIdByUserName(claim.getCreateUser());
					Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
					commonService.setNotificationRecipients(createUserId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
					claimsService.sendClaimNotification(claimVO, Constants.CLAIM_COMPLETE_NOTIFICATION_CODE, dynamicEmailRecipients);
				} else if (claimVO.getActionType().equals("R")) {
					claim.setClaimStatusCode(Constants.CLAIM_STATUS_CODE_REVISION);
					claim.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_REVISION));
					claimVO.setMessage(FINAL_APPROVAL_REJECTED);
				}
			}
			claimVO.setWorkFlowDetailId(Integer.parseInt(workflowDetailId));
		}
		claimVO.setClaim(claim);
		return claimVO;
	}

	private void sendApproveOrDisApproveNotificationForClaims(ClaimsVO vo, String approvalStatus) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		Workflow workFlow = vo.getWorkflow();
		if (workFlow != null && vo.getWorkFlowDetailId() != null && !vo.getWorkFlowDetailId().equals(0)) {
			if ("A".equals(approvalStatus) || "B".equals(approvalStatus)) {
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(workFlow.getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkFlowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
					vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
					vo.setMapId(workflowDetail.getMapId());
				}
				claimsService.sendClaimNotification(vo, Constants.CLAIM_APPROVAL_NOTIFICATION_CODE,	dynamicEmailRecipients);
				if ("B".equals(approvalStatus)) {
					sendClaimBypassNotification(vo);
				}
			} else if ("R".equals(approvalStatus)) {
				String createUserId = personDao.getPersonIdByUserName(vo.getClaim().getCreateUser());
				commonService.setNotificationRecipients(createUserId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				claimsService.sendClaimNotification(vo, Constants.CLAIM_REVISION_NOTIFICATION_CODE,	dynamicEmailRecipients);
			}
		}
	}

	private void sendClaimBypassNotification(ClaimsVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkFlowDetailId() != null && !vo.getWorkFlowDetailId().equals(0) && vo.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkFlowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				vo.setMapId(workflowDetail.getMapId());
			}
			claimsService.sendClaimNotification(vo, Constants.CLAIM_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
	}
	
	private String progressReportWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		ProgressReportVO progressReportVO = new ProgressReportVO();
		try {
			progressReportVO = mapper.readValue(formDataJson, ProgressReportVO.class);
			Integer progressReportId = progressReportVO.getProgressReportId();
			String updatedUser = AuthenticatedUser.getLoginUserName();
			String moduleItemKey = progressReportId.toString();
			String logginPersonId =  AuthenticatedUser.getLoginPersonId();
			String workFlowPersonId = progressReportVO.getWorkFlowPersonId();
			Integer subModuleCode = Constants.PROGRESS_REPORT_SUBMODULE_CODE;
			AwardProgressReport awardProgressReport = progressReportService.loadAwardProgressReportDetails(progressReportId);
			progressReportVO.setAwardProgressReport(awardProgressReport);
			String actionType = progressReportVO.getActionType();
			String approverComment = progressReportVO.getApproveComment();
			logger.info("progressReportId : {}", progressReportId);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(WORKFLOW_PERSON_ID, workFlowPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B") || actionType.equals("C")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			progressReportVO = approveOrRejectProgressReport(progressReportVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files);
			progressReportDao.updateProgressReportStatus(progressReportId,awardProgressReport.getProgressReportStatusCode(), null);
			if (awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS) || awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_REVISION) || awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED)) {
				progressReportService.canProgressReportTakeRoutingAction(progressReportVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey, Constants.PROGRESS_REPORT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					progressReportVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey, Constants.PROGRESS_REPORT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						progressReportVO.setWorkflowList(workFlows);
					}
					progressReportVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
					progressReportVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
					if ((!isFinalApprover.equals("1")) || (isFinalApprover.equals("1") && actionType.equals("R"))) {
						sendApproveOrDisApproveNotificationForProgressReport(progressReportVO, actionType);
					} else if (actionType.equals("B")) {
						sendProgressReportBypassNotification(progressReportVO);
					}
				}
			}
			AwardProgressReport finalAwardProgressReport = new AwardProgressReport();
			BeanUtils.copyProperties(awardProgressReport, finalAwardProgressReport);
			if(finalAwardProgressReport.getAwardProgressReportKPISummarys() != null)
				finalAwardProgressReport.setAwardProgressReportKPISummarys(finalAwardProgressReport.getAwardProgressReportKPISummarys().stream()
						.filter(summary -> summary.getOriginatingProgressReportId().equals(progressReportId))
						.sorted(Comparator.comparing(AwardProgressReportKPISummary::getKpiCategoryTypeCode)
						.thenComparing(summary -> summary.getKpiCriteriaType().getDescription())).collect(Collectors.toList()));
			progressReportVO.setAwardProgressReport(finalAwardProgressReport);
		} catch (Exception e) {
			logger.error("Exception in progressReportWorkflowApproval: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(progressReportVO);
	}


	private void saveWorkFlowDetailExt(ProgressReportVO progressReportVO, Integer workflowDetailId) {
		WorkflowDetailExt workflowDetailExt = progressReportVO.getWorkflowDetailExt();
		if (workflowDetailExt != null) {
			workflowDetailExt.setWorkflowDetailId(workflowDetailId);
			workflowDao.saveOrUpdateWorkflowDetailExt(workflowDetailExt);
		}
	}

	private ProgressReportVO approveOrRejectProgressReport(ProgressReportVO progressReportVO, String moduleItemKey, String moduleCode,
			String workFlowPersonId, Integer subModuleCode, String updatedUser, String actionType,
			String approverComment, String isFinalApprover, Integer canApproveRouting, MultipartFile[] files) {
		AwardProgressReport awardProgressReport = progressReportVO.getAwardProgressReport();
		String workflowDetailId = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Integer.parseInt(moduleCode));
		evaluateValidationRuleVO.setLogginPersonId(workFlowPersonId);
		evaluateValidationRuleVO.setSubModuleCode(subModuleCode);
		evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
		evaluateValidationRuleVO.setUpdateUser(updatedUser);
		evaluateValidationRuleVO.setAcType(progressReportVO.getActionType());
		evaluateValidationRuleVO.setComments(progressReportVO.getApproveComment());
		if (canApproveRouting.toString().equals("0")) {
			progressReportVO.setMessage(APPROVAL_FAILED);
			return progressReportVO;
		} else {
			if ((progressReportVO.getActionType().equals("A") || progressReportVO.getActionType().equals("B")) && !awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED)) {
				awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS);
				awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS));
				progressReportVO.setMessage(APPROVAL_SUCCESS);
			} else if ((!awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED)) && progressReportVO.getActionType().equals("R")) {
				awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_REVISION);
				awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_REVISION));
				progressReportVO.setMessage(APPROVAL_REJECTED);
				InboxVO inboxVO = new InboxVO();
				inboxVO.setModuleCode(Integer.parseInt(moduleCode));
				inboxVO.setModuleItemKey(progressReportVO.getProgressReportId().toString());
				inboxVO.setMessageTypeCode(Constants.MESSAGE_TYPE_PROGRESS_REPORT_REJECT);
				inboxVO.setSubModuleCode(subModuleCode);
				inboxVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				inboxVO.setToPersonId(progressReportVO.getPersonId());
				inboxDao.markReadMessage(inboxVO);
			} else {
				progressReportVO.setMessage(APPROVAL_FAILED);
				return progressReportVO;
			}
		}
		workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, workFlowPersonId, updatedUser,
				actionType, approverComment, subModuleCode, Constants.SUBMODULE_ITEM_KEY,
				progressReportVO.getMapId(), progressReportVO.getMapNumber(), progressReportVO.getApproverStopNumber(), progressReportVO.getApproverNumber());
		if (files != null && workflowDetailId != null) {
			Integer id = Integer.parseInt(workflowDetailId);
			uploadAttachment(files, updatedUser, id);
		}
		if (workflowDetailId != null) {
			if (actionType.equals("A")) {
				saveWorkFlowDetailExt(progressReportVO, Integer.valueOf(workflowDetailId));
			}
			if (isFinalApprover.equals("1")) {
				if (progressReportVO.getActionType().equals("A") || progressReportVO.getActionType().equals("B")) {
					awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED);
					awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED));
					progressReportVO.setMessage(FINAL_APPROVAL_SUCCESS);
					String createUserId = personDao.getPersonIdByUserName(awardProgressReport.getCreateUser());
					Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
					commonService.setNotificationRecipients(createUserId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
					progressReportService.sendProgressReportNotification(progressReportVO, Constants.PROGRESS_REPORT_COMPLETE_NOTIFICATION_CODE, dynamicEmailRecipients);
				} else if (progressReportVO.getActionType().equals("R")) {
					awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_REVISION);
					awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_REVISION));
					progressReportVO.setMessage(FINAL_APPROVAL_REJECTED);
				}
			}
			progressReportVO.setWorkFlowDetailId(Integer.parseInt(workflowDetailId));
		}
		progressReportVO.setAwardProgressReport(awardProgressReport);
		return progressReportVO;
	}

	private void sendApproveOrDisApproveNotificationForProgressReport(ProgressReportVO vo, String approvalStatus) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		Workflow workFlow = vo.getWorkflow();
		if (workFlow != null && vo.getWorkFlowDetailId() != null && !vo.getWorkFlowDetailId().equals(0)) {
			if ("A".equals(approvalStatus) || "B".equals(approvalStatus)) {
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(workFlow.getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkFlowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
					vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
					vo.setMapId(workflowDetail.getMapId());
				}
				progressReportService.sendProgressReportNotification(vo, Constants.PROGRESS_REPORT_APPROVAL_NOTIFICATION_CODE, dynamicEmailRecipients);
				if ("B".equals(approvalStatus)) {
					sendProgressReportBypassNotification(vo);
				}
			} else if ("R".equals(approvalStatus)) {
				String createUserId = personDao.getPersonIdByUserName(vo.getAwardProgressReport().getCreateUser());
				commonService.setNotificationRecipients(createUserId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				progressReportService.sendProgressReportNotification(vo, Constants.PROGRESS_REPORT_REVISION_NOTIFICATION_CODE, dynamicEmailRecipients);
			}
		}		
	}

	private void sendProgressReportBypassNotification(ProgressReportVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkFlowDetailId() != null && !vo.getWorkFlowDetailId().equals(0) && vo.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkFlowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				vo.setMapId(workflowDetail.getMapId());
			}
			progressReportService.sendProgressReportNotification(vo, Constants.PROGRESS_REPORT_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients);
		}		
	}

	private String agreementWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		AgreementVO agreementVO = new AgreementVO();
		try {
			agreementVO = mapper.readValue(formDataJson, AgreementVO.class);
			Integer agreementRequestId = agreementVO.getAgreementRequestId();
			String updatedUser = agreementVO.getUpdateUser();
			String moduleItemKey = agreementRequestId.toString();
			String logginPersonId = agreementVO.getPersonId();
			String workFlowPersonId = agreementVO.getWorkFlowPersonId();
			Integer subModuleCode = Constants.AWARD_SUBMODULE_CODE;
			AgreementHeader agreement = agreementDao.getAgreementById(agreementRequestId);
			agreement.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementVO.setAgreementHeader(agreement);
			String actionType = agreementVO.getActionType();
			String approverComment = agreementVO.getApproveComment();

			if (agreement.getUpdateUser() != null) {
				agreement.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreement.getUpdateUser()));
			}
			if (agreement.getCreateUser() != null) {
				agreement.setCreateUserFullName(personDao.getUserFullNameByUserName(agreement.getCreateUser()));
			}
			if (agreement.getSubmitUser() != null) {
				agreement.setSubmitUserFullName(personDao.getUserFullNameByUserName(agreement.getSubmitUser()));
			}
			logger.info("agreementId : {}", agreementRequestId);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(WORKFLOW_PERSON_ID, workFlowPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);

			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId,
					Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId,
						Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			agreementVO = approveOrRejectAgreement(agreementVO, moduleItemKey, moduleCode, workFlowPersonId, subModuleCode,
					updatedUser, actionType, approverComment, isFinalApprover, canApproveRouting, files,
					logginPersonId);
			agreement = agreementDao.saveOrUpdateAgreement(agreement);

			if (agreement.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_ROUTING) || agreement.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_SUBMITTED) || agreement.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_RETURNED)) {
				agreementWorkflowService.canTakeRoutingAction(agreementVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey, Constants.MODULE_CODE_AGREEMENT, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					agreementVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey,
							Constants.MODULE_CODE_AGREEMENT, Constants.SUBMODULE_ITEM_KEY,
							Constants.AGREEMENT_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						agreementVO.setWorkflowList(workFlows);
					}
					agreementVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey, logginPersonId,
							Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode).toString());
					agreementVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey, logginPersonId,
							Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
					if ((!isFinalApprover.equals("1")) || (isFinalApprover.equals("1") && actionType.equals("R"))) {
						sendApproveOrDisApproveNotificationForAgreement(agreementVO,actionType);
					} else {
						sendFinalApprovalNotificationForAgreement(agreementVO);
					}
				}
			}
			agreementVO.setAgreementHeader(agreement);
			agreementService.loadAgreementById(agreementVO);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("exception in agreementWorkflowApproval: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(agreementVO);
	}
	
	private void sendFinalApprovalNotificationForAgreement(AgreementVO vo) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		agreementService.sendNotificationForAgreement(vo, Constants.AGREEMENT_FINAL_APPROVAL_NOTIFICATION_CODE, dynamicEmailrecipients);
	}

	private void sendApproveOrDisApproveNotificationForAgreement(AgreementVO vo, String approvalStatus) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0)) {
			if ("A".equals(approvalStatus) || "B".equals(approvalStatus) && vo.getWorkflow() != null) {
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(vo.getWorkflow().getWorkflowDetails(),vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(),Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
					vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
					vo.setMapId(workflowDetail.getMapId());
				}
				if ("B".equals(approvalStatus)) {
					sendAgreementBypassNotification(vo);
				}
				if ("A".equals(approvalStatus)) {
					agreementService.sendNotificationForAgreement(vo,Constants.AGREEMENT_APPROVAL_NOTIFICATION_CODE, dynamicEmailrecipients);
				}
			} else if ("R".equals(approvalStatus)) {
				agreementService.sendNotificationForAgreement(vo, Constants.NOTIFICATION_AGREEMENT_REJECTED,dynamicEmailrecipients);
			}
		}
	}
	
	private void sendAgreementBypassNotification(AgreementVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0) && vo.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				vo.setMapId(workflowDetail.getMapId());
			}
			agreementService.sendNotificationForAgreement(vo, Constants.AGREEMENT_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients);
		}		
	}

	private AgreementVO approveOrRejectAgreement(AgreementVO agreementVO, String moduleItemKey, String moduleCode,
			String workFlowPersonId, Integer subModuleCode, String updatedUser, String actionType,
			String approverComment, String isFinalApprover, Integer canApproveRouting, MultipartFile[] files,
			String logginPersonId) {
		AgreementHeader agreement = agreementVO.getAgreementHeader();
		String workflowDetailId = null;
		InboxVO inboxVO = new InboxVO();
		inboxVO.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
		inboxVO.setModuleItemKey(moduleItemKey);
		inboxVO.setToPersonId(workFlowPersonId);
		inboxVO.setMessageTypeCode(Constants.MESSAGE_TYPE_ROUTING_IN_PROGRESS);
		inboxDao.markReadMessage(inboxVO);
		if (agreementVO.getWorkflowDetailId() == null) {
			if (canApproveRouting.toString().equals("0")) {
				agreementVO.setMessage(APPROVAL_FAILED);
				return agreementVO;
			} else {
				if (agreementVO.getActionType().equals("A") || agreementVO.getActionType().equals("B")) {
					agreementService.updateAgreementStatus(agreement, Constants.AGREEMENT_STATUS_ROUTING);
					agreementVO.setMessage(APPROVAL_SUCCESS);
				} else if (agreementVO.getActionType().equals("R")) {
					agreementService.updateAgreementStatus(agreement, Constants.AGREEMENT_STATUS_RETURNED);
					agreement.setWorkflowStatusCode(null);
					agreementVO.setMessage(APPROVAL_REJECTED);
					agreementService.addActionLogEntry(Integer.parseInt(moduleItemKey), Constants.ACTION_LOG_AGREEMENT_ROUTING_RETURNED, updatedUser, null);
				} else {
					agreementVO.setMessage(APPROVAL_FAILED);
					return agreementVO;
				}
			}
		}
		agreementVO.setAgreementHeader(agreement);
		workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, workFlowPersonId, updatedUser,
				actionType, approverComment, subModuleCode, Constants.SUBMODULE_ITEM_KEY, agreementVO.getMapId(),
				agreementVO.getMapNumber(), agreementVO.getApproverStopNumber(), agreementVO.getApproverNumber());
		if (agreementVO.getWorkflowDetailId() == null && agreementVO.getActionType().equals("R") && !canApproveRouting.toString().equals("0")) {
			inboxService.addAgreementMessageToInbox(agreement.getAgreementRequestId().toString(), agreement.getRequestorPersonId(), updatedUser, Constants.MESSAGE_TYPE_RETURNED, 
					"R", 0, 0, "#"+agreement.getAgreementRequestId() +" - " + agreement.getTitle()+ " - " +agreement.getAgreementType().getDescription());
		}
		if (files != null && workflowDetailId != null) {
			agreementVO.setWorkflowDetailId(Integer.parseInt(workflowDetailId));
			uploadAttachment(files, updatedUser, agreementVO.getWorkflowDetailId());
		}
		if (workflowDetailId != null && isFinalApprover.equals("1")) {
			if (agreementVO.getActionType().equals("A") || agreementVO.getActionType().equals("B")) {
				agreementService.addActionLogEntry(Integer.parseInt(moduleItemKey), Constants.ACTION_LOG_AGREEMENT_ROUTING_COMPLETED, updatedUser, null);
				if (agreement.getAdminPersonId() != null) {
					agreementService.updateAgreementStatus(agreement, Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS);
				} else {
					agreementService.updateAgreementStatus(agreement, Constants.AGREEMENT_STATUS_SUBMITTED);
				}
				agreementDao.saveOrUpdateAgreement(agreement);
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				String submitToEmail = commonDao.getParameterValueAsString(Constants.SUBMIT_AGREEMENT_EMAIL);
				commonService.setNotificationRecipientsforNonEmployees(submitToEmail, "TO", dynamicEmailrecipients);
				agreementService.sendNotificationForAgreement(agreementVO, Constants.SUBMIT_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
				String userMessage = "#" + agreement.getAgreementRequestId().toString() + " - " + agreement.getTitle()
						+ " - " + agreement.getAgreementType().getDescription();
				List<PersonRoleRT> personroles = new ArrayList<>();
				Set<String> personIds = new HashSet<>();
				if (agreement.getUnitNumber() != null) {
					personroles = personDao.fetchPersonRoleRTByRightNameAndUnitNumber(agreement.getUnitNumber(),
							"AGREEMENT_ADMINISTRATOR");
				} else {
					personroles = personDao.fetchPersonRoleRTByRightNameAndUnitNumber(Constants.ROOT_UNIT,
							"AGREEMENT_ADMINISTRATOR");
				}
				if (personroles != null && !personroles.isEmpty()) {
					for (PersonRoleRT personRole : personroles) {
						personIds.add(personRole.getPersonRoleRTAttributes().getPersonId());
					}
					for (String personId : personIds) {
						inboxService.addAgreementMessageToInbox(agreement.getAgreementRequestId().toString(), personId,
								agreementVO.getUpdateUser(), Constants.MESSAGE_TYPE_SUBMITTED, "P", 0,
								Constants.AGREEMENT_SUBMODULE_CODE, userMessage);
					}
				}
				agreementVO.setMessage(FINAL_APPROVAL_SUCCESS);
				// agreementService.addActionLogEntry(agreement.getAgreementRequestId(),
				// Constants.ACTION_LOG_AGREEMENT_SUBMITTED, agreement.getUpdateUser(),
				// "Agreement Submitted for Review");
			} else if (agreementVO.getActionType().equals("R")) {
				agreementService.addActionLogEntry(Integer.parseInt(moduleItemKey),
						Constants.ACTION_LOG_AGREEMENT_ROUTING_RETURNED, updatedUser, null);
				agreementService.updateAgreementStatus(agreement, Constants.AGREEMENT_STATUS_RETURNED);
				agreementDao.saveOrUpdateAgreement(agreement);
				agreementVO.setMessage(FINAL_APPROVAL_REJECTED);
			}
		}
		return agreementVO;
	}

	private String serviceRequestWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode) {
		ObjectMapper mapper = new ObjectMapper();
		ServiceRequestVO serviceRequestVO = new ServiceRequestVO();
		try {
			serviceRequestVO = mapper.readValue(formDataJson, ServiceRequestVO.class);
			Integer serviceRequestId = serviceRequestVO.getServiceRequestId();
			String updatedUser = AuthenticatedUser.getLoginUserName();
			String moduleItemKey = serviceRequestId.toString();
			String logginPersonId = AuthenticatedUser.getLoginPersonId();
			String workFlowPersonId = serviceRequestVO.getWorkFlowPersonId();
			Integer subModuleCode = Constants.PROGRESS_REPORT_SUBMODULE_CODE;
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(serviceRequestId);
			serviceRequestVO.setServiceRequest(serviceRequest);
			String actionType = serviceRequestVO.getActionType();
			String approverComment = serviceRequestVO.getApproveComment();
			logger.info("serviceRequestId : {}", serviceRequestId);
			logger.info(UPDATE_USER, updatedUser);
			logger.info(MODULE_ITEM_KEY, moduleItemKey);
			logger.info(LOGIN_PERSON_ID, logginPersonId);
			logger.info(WORKFLOW_PERSON_ID, workFlowPersonId);
			logger.info(SUB_MODULE_CODE, subModuleCode);
			logger.info(ACTION_TYPE, actionType);
			logger.info(APPROVER_COMMENT, approverComment);
			String isFinalApprover = businessRuleDao.workflowfinalApproval(moduleItemKey, workFlowPersonId,
					Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			logger.info(IS_FINAL_APPROVER, isFinalApprover);
			Integer canApproveRouting = 0;
			if (actionType.equals("B") || actionType.equals("C")) {
				canApproveRouting = 1;
			} else {
				canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey, workFlowPersonId,
						Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode);
			}
			logger.info(CAN_APPROVE_ROUTING, canApproveRouting);
			serviceRequestVO = approveOrRejectServiceRequest(serviceRequestVO, moduleItemKey, moduleCode,
					workFlowPersonId, subModuleCode, updatedUser, actionType, approverComment, isFinalApprover,
					canApproveRouting, files);
			if (serviceRequest.getStatusCode()
					.equals(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVAL_IN_PROGRESS)
					|| serviceRequest.getStatusCode()
							.equals(Constants.SERVICE_REQUEST_STATUS_CODE_RETURNED)
					|| serviceRequest.getStatusCode()
							.equals(Constants.SERVICE_REQUEST_STATUS_CODE_APPROVED)) {
				serviceRequestService.canServiceRequestTakeRoutingAction(serviceRequestVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey,
						Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
						Constants.SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					serviceRequestVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey,
							Constants.SERVICE_REQUEST_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
							Constants.SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						serviceRequestVO.setWorkflowList(workFlows);
					}
					serviceRequestVO.setCanApproveRouting(businessRuleDao.canApproveRouting(moduleItemKey,
							logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode)
							.toString());
					serviceRequestVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey,
							logginPersonId, Integer.parseInt(moduleCode), Constants.SUBMODULE_ITEM_KEY, subModuleCode));
					if ((!isFinalApprover.equals("1")) || (isFinalApprover.equals("1") && actionType.equals("R"))) {
						sendApproveOrDisApproveNotificationForSR(serviceRequestVO, actionType);
					}
					/*else if (actionType.equals("B")) {
						sendSRBypassNotification(serviceRequestVO);
					}*/
				}
			}
			serviceRequestService.loadServiceRequestInDetail(serviceRequestVO);
		} catch (Exception e) {
			logger.error("Exception in serviceRequestWorkflowApproval: {} ", e.getMessage());
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(serviceRequestVO);
	}

	private void sendSRBypassNotification(ServiceRequestVO vo) {
		Set<String> dynamicEmailRecipients = new HashSet<>();
		if (vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0) && vo.getWorkflow() != null) {
			List<WorkflowDetail> workFlowDetails = bypassedWorkflowDetails(vo.getWorkflow().getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
			for (WorkflowDetail workflowDetail : workFlowDetails) {
				dynamicEmailRecipients.add(workflowDetail.getApproverPersonId());
				vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
				vo.setMapId(workflowDetail.getMapId());
				vo.setPersonName(AuthenticatedUser.getLoginUserName());
			}
			serviceRequestService.sendMailForServiceRequestActions(Constants.SERVICE_REQUEST_BYPASS_NOTIFICATION_CODE, dynamicEmailRecipients, vo);
		}
		
	}

	private void sendApproveOrDisApproveNotificationForSR(ServiceRequestVO vo, String approvalStatus) {
		Set<String> dynamicEmailRecipients = new HashSet<>();
		Workflow workFlow = vo.getWorkflow();
		if (vo.getServiceRequest().getServiceRequestId() != null) {
			vo.setModuleItemKey(vo.getServiceRequest().getServiceRequestId().toString());
		}
		if (workFlow != null && vo.getWorkflowDetailId() != null && !vo.getWorkflowDetailId().equals(0)) {
			if ("A".equals(approvalStatus) || "B".equals(approvalStatus)) {
				List<WorkflowDetail> workFlowDetails = waitingWorkflowDetails(workFlow.getWorkflowDetails(), vo.getMapNumber(), vo.getApproverStopNumber(), vo.getWorkflowDetailId());
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					dynamicEmailRecipients.add(workflowDetail.getApproverPersonId());
					vo.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
					vo.setMapId(workflowDetail.getMapId());
				}
				vo.setPersonName(AuthenticatedUser.getLoginUserName());
				serviceRequestService.sendMailForServiceRequestActions(Constants.SERVICE_REQUEST_APPROVAL_NOTIFICATION_CODE, dynamicEmailRecipients, vo);
				if ("B".equals(approvalStatus)) {
					sendSRBypassNotification(vo);
				}
			} else if ("R".equals(approvalStatus)) {
				serviceRequestService.sendStatusUpdateMailForServiceRequest(dynamicEmailRecipients, vo.getServiceRequest());
			}
		}
	
	}

	private ServiceRequestVO approveOrRejectServiceRequest(ServiceRequestVO serviceRequestVO, String moduleItemKey,
			String moduleCode, String workFlowPersonId, Integer subModuleCode, String updatedUser, String actionType,
			String approverComment, String isFinalApprover, Integer canApproveRouting, MultipartFile[] files) {
		ServiceRequest serviceRequest = serviceRequestVO.getServiceRequest();
		String workflowDetailId = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Integer.parseInt(moduleCode));
		evaluateValidationRuleVO.setLogginPersonId(workFlowPersonId);
		evaluateValidationRuleVO.setSubModuleCode(subModuleCode);
		evaluateValidationRuleVO.setModuleItemKey(moduleItemKey);
		evaluateValidationRuleVO.setUpdateUser(updatedUser);
		evaluateValidationRuleVO.setAcType(serviceRequestVO.getActionType());
		evaluateValidationRuleVO.setComments(serviceRequestVO.getApproveComment());
		if (canApproveRouting.toString().equals("0")) {
			serviceRequestVO.setMessage(APPROVAL_FAILED);
			return serviceRequestVO;
		} else {
			if (serviceRequestVO.getActionType().equals("A") || serviceRequestVO.getActionType().equals("B")) {
				serviceRequestVO.setMessage(APPROVAL_SUCCESS);
			} else if (serviceRequestVO.getActionType().equals("R")) {
				serviceRequestVO.setMessage(APPROVAL_REJECTED);
				InboxVO inboxVO = new InboxVO();
				inboxVO.setModuleCode(Integer.parseInt(moduleCode));
				inboxVO.setModuleItemKey(serviceRequestVO.getServiceRequestId().toString());
				inboxVO.setMessageTypeCode(Constants.MESSAGE_TYPE_SR_REJECT);
				inboxVO.setSubModuleCode(subModuleCode);
				inboxVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				inboxVO.setToPersonId(serviceRequestVO.getPersonId());
				inboxDao.markReadMessage(inboxVO);
				serviceRequest = serviceRequestService.updateSRStatusAndActionLog(serviceRequest,Constants.RETURN_IN_ROUTE_LOG_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_RETURNED);
			} else {
				serviceRequestVO.setMessage(APPROVAL_FAILED);
				return serviceRequestVO;
			}
		}
		workflowDetailId = businessRuleDao.workflowApprove(moduleItemKey, moduleCode, workFlowPersonId, updatedUser,
				actionType, approverComment, subModuleCode, Constants.SUBMODULE_ITEM_KEY,
				serviceRequestVO.getMapId(), serviceRequestVO.getMapNumber(), serviceRequestVO.getApproverStopNumber(), serviceRequestVO.getApproverNumber());
		if (files != null && workflowDetailId != null) {
			Integer id = Integer.parseInt(workflowDetailId);
			uploadAttachment(files, updatedUser, id);
		}
		if (workflowDetailId != null) {
			if (isFinalApprover.equals("1")) {
				serviceRequest = serviceRequestService.updateSRStatusAndGroupId(serviceRequest);
				if (serviceRequestVO.getActionType().equals("A") || serviceRequestVO.getActionType().equals("B")) {
					serviceRequest = serviceRequestService.updateSRStatusAndActionLog(serviceRequest,Constants.APPROVED_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_APPROVED);
					serviceRequestVO.setMessage(FINAL_APPROVAL_SUCCESS);
					serviceRequestService.sendStatusUpdateMailForServiceRequest(new HashSet<>(), serviceRequestVO.getServiceRequest());
				} else if (serviceRequestVO.getActionType().equals("R")) {
					serviceRequestVO.setMessage(FINAL_APPROVAL_REJECTED);
				}
			}
			serviceRequestVO.setWorkflowDetailId(Integer.parseInt(workflowDetailId));
		}
		serviceRequestVO.setServiceRequest(serviceRequest);
		return serviceRequestVO;
	}
}
