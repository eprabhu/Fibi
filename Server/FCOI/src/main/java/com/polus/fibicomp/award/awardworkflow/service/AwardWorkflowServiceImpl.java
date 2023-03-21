package com.polus.fibicomp.award.awardworkflow.service;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.wbs.dao.WBSDaoImpl;
import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.service.AgreementService;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.awardworkflow.dao.AwardWorkflowDao;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.service.DatesAndAmountService;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardHistoryLog;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.version.service.AwardVersionService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.claims.dao.ClaimsDao;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.claims.service.ClaimsService;
import com.polus.fibicomp.claims.vo.ClaimsVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.integration.service.IntegrationService;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.manpowerintegration.scheduler.ManpowerIntegrationSchedulerService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.service.ProgressReportService;
import com.polus.fibicomp.progressreport.vo.ProgressReportVO;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
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
import com.polus.fibicomp.task.service.TaskService;
import com.polus.fibicomp.task.vo.TaskVO;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Service(value = "awardWorkflowService")
public class AwardWorkflowServiceImpl implements AwardWorkflowService {

	protected static Logger logger = LogManager.getLogger(AwardWorkflowServiceImpl.class.getName());

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	public BusinessRuleDao businessRuleDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private AwardWorkflowDao awardWorkflowDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	public PersonDao personDao;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private ServiceRequestService serviceRequestService;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private IntegrationService integrationService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private ClaimsDao claimDao;

	@Autowired
	private ClaimsService claimService;

	@Autowired
	ManpowerService manpowerService;

	@Autowired
	private AwardVersionService awardVersionService;

	@Autowired
	private DatesAndAmountService datesAndAmountService;

	@Autowired
	private ManpowerIntegrationSchedulerService manpowerIntegrationSchedulerService;

	@Autowired
	private ProgressReportService progressReportService;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;
	
	@Autowired
	private AgreementDao agreementDao;
	
	@Autowired
	private AgreementService agreementService;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private WBSDaoImpl wbsDao;

	@Override
	public AwardVO submitAward(AwardVO awardVO) {
		try {
			Award award = awardDao.getAwardDetailsById(awardVO.getAwardId());
			Award activeAward = awardDao.fetchActiveAwardByAwardNumber(award.getAwardNumber());
			Award currentActiveAward = null;
			String awardId = award.getAwardId().toString();
			String activeAwardId = "";
			AwardPerson newAwardPIPerson = manpowerIntegrationDao.getAwardPIPersonByAwardId(award.getAwardId());
			AwardPerson activeAwardPIPerson = null;
			String activeAwardSuperiorSupOrg = null;
			if (activeAward != null) {
				awardVO.setPreviousActiveAwardId(activeAward.getAwardId());
				activeAwardId = activeAward.getAwardId().toString();
				activeAwardPIPerson = manpowerIntegrationDao.getAwardPIPersonByAwardId(activeAward.getAwardId());
				currentActiveAward = activeAward;
				try {
					activeAwardSuperiorSupOrg = manpowerService.findSuperiorSupOrgForAward(activeAward);
				} catch (Exception e) {
					logger.error("Error in findSuperiorSupOrgForAward {} ", e.getMessage());
					throw new ApplicationException("error occured while find superior sup org", e, Constants.JAVA_ERROR);
				}
			}
			if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED)) {
				if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_VARIATION)) {
					inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, awardId, awardVO.getPersonId(), Constants.MESSAGE_TYPE_VARIATION_RETURN, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
				} else {
					inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, awardId, awardVO.getPersonId(), Constants.MESSAGE_TYPE_AWARD_REJECT, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
				}
			}
			boolean isEnableIOCodeGeneration = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_IO_CODE_GENERATION);
			if (Constants.AWARD_SETUP.equals(award.getAwardDocumentTypeCode()) && isEnableIOCodeGeneration && !Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED.equals(award.getAwardWorkflowStatus().getWorkflowAwardStatusCode())) {
				AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardVO.getAwardId());
				if (awardBudgetHeader != null && awardBudgetHeader.getBudgetId() != null) {
					String success = wbsDao.generateIOCode(awardVO.getAwardId(), awardBudgetHeader.getBudgetId(),"GEN_ALL", null, null);
				}
			}
			award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS);
			award.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS));
			award.setSubmitUser(awardVO.getUserName());
			award.setSubmissionDate(commonDao.getCurrentTimestamp());
			award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award.setDocumentUpdateUser(awardVO.getUserName());
			award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award = awardDao.saveOrUpdateAwardDetails(award);
			awardVO = buildAwardWorkflow(awardVO);
			awardVO = fetchPreviousWorkFlowsList(awardVO);
			Workflow workflow = workflowDao.fetchActiveWorkflowByParams(awardId, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
			workflowService.prepareWorkflowDetails(workflow);
			awardVO.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(awardId, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				awardVO.setWorkflowList(workFlows);
			}
			awardService.updateAwardBudgetStatus(award, Constants.AWARD_BUDGET_STATUS_CODE_SUBMITTED);
			if (award.getSubmitUser() != null) {
				award.setSubmitUserFullName(personDao.getUserFullNameByUserName(award.getSubmitUser()));
			}
			if (award.getUpdateUser() != null) {
				award.setUpdateUserFullName(personDao.getUserFullNameByUserName(award.getUpdateUser()));
			}
			if (award.getCreateUser() != null) {
				award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
			}
			if (activeAward != null) {
				ServiceRequest serviceRequest = awardDao.getServiceRequestBasedOnAwardId(activeAwardId, awardId);
				if (serviceRequest != null) {
					serviceRequest = serviceRequestService.updateSRStatusAndActionLog
							(serviceRequest,Constants.SUBMIT_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_APPROVAL_IN_PROGRESS);
					awardVO.setServiceRequest(serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest));
				}
			}
			if (workflow.getCurrentStopName() != null && award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
				award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription() + " : " +workflow.getCurrentStopName());
			} else {
				award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription());
			}
			if (award.getGrantHeaderId() != null) {
				award.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(award.getGrantHeaderId()));
			}
			awardVO.setAward(award);
			if (awardVO.getWorkflow() != null && awardVO.getWorkflow().getWorkflowDetails().isEmpty()
					|| awardVO.getWorkflow() == null) {
				//String awardStatusCode = commonDao.getParameterValueAsString(Constants.AWARD_STATUS_ON_APPROVAL);
				String awardVariationTypeCode = award.getAwardVariationTypeCode();
				String awardDocumentTypeCode = award.getAwardDocumentTypeCode();
				if (awardDocumentTypeCode.equals(Constants.AWARD_VARIATION)) {
					if (awardVariationTypeCode.equals(Constants.PROJECT_CLOSURE_TYPE_CODE)) {
						award.setStatusCode(Constants.AWARD_STATUS_CODE_CLOSED);
						award.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_CLOSED));
					} 
//						else {
//						award.setStatusCode(Constants.AWARD_STATUS_CODE_AWARDED);
//						award.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_AWARDED));
//					}
				}
				award.setSubmitUser(awardVO.getUserName());
				sapBasedWorkflowStatusAndSequenceStatus(award, awardVO.getUpdateUser());
				if (activeAward != null) {
					if (awardVariationTypeCode.equals(Constants.PROJECT_CLOSURE_TYPE_CODE)) {
						awardDao.updateAwardStatusByStatusCode(activeAward.getAwardId(), Constants.AWARD_STATUS_CODE_CLOSED);
					}
					ServiceRequest serviceRequest = awardDao.getServiceRequestBasedOnAwardId(activeAwardId, awardId);
					if (serviceRequest != null) {
						serviceRequest = serviceRequestService.updateSRStatusAndActionLog(serviceRequest,Constants.RESOLVED_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_RESOLVED);
						awardVO.setServiceRequest(serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest));
					}
				}
				if (commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED)) {
					manpowerIntegrationSchedulerService.checkForManpowerIntegration(award, currentActiveAward, newAwardPIPerson, activeAwardPIPerson, activeAwardSuperiorSupOrg);
				}
				awardVO.setIsMasterAwardCreation(true);
			}
			List<Integer> taskIds = taskDao.getPersonTaskIds(awardId, Constants.AWARD_MODULE_CODE, awardVO.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
			if (taskIds != null && !taskIds.isEmpty()) {
				for (Integer taskId : taskIds) {
					List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(awardId, taskId.toString(), Constants.AWARD_MODULE_CODE, awardVO.getPersonId(), Constants.AWARD_TASK_SUBMODULE_CODE);
					if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
						awardVO.getSectionTypeCodes().addAll(moduleVariableSections);
					}
				}
			}
			awardVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO, Constants.AWARD_MODULE_CODE, awardVO.getPersonId(), Constants.AWARD_SUBMODULE_CODE));
			awardService.setSapFeedStatus(awardVO);
			if (awardVO != null && awardVO.getAward() != null) {
				awardService.getPendingAwardDetails(awardVO, awardVO.getAward().getAwardNumber(), awardVO.getAward().getAwardId());
			}
			if (Boolean.TRUE.equals(awardVO.getIsMasterAwardCreation())) {
				checkAndFeedSapAwardDetail(awardVO.getAward(), awardVO.getUpdateUser());
			}
			commonDao.doflush();
			if (Boolean.TRUE.equals(awardVO.getIsMasterAwardCreation())) {
				businessRuleService.sendFinalApprovalNotification(awardVO);
				if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
					awardService.sendAwardNotification(awardVO, Constants.LIVE_AWARD_NOTIFICATION_FOR_CLAIM_PREPARER_CODE, new HashSet<>());
				}
			}
			sendServiceRequestAndAwardSubmitNotification(awardVO);
			businessRuleService.evaluateAndSentNotification(Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE, awardVO.getAwardId().toString(), Constants.SUBMODULE_ITEM_KEY, awardVO.getPersonId(), awardVO.getUserName(),awardService.getAwardPlaceholders(awardVO));
		} 
		catch (Exception e) {
			throw e;
		}
		return awardVO;
	}

	private AwardVO buildAwardWorkflow(AwardVO awardVO) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		evaluateValidationRuleVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE);
		evaluateValidationRuleVO.setModuleItemKey(awardVO.getAwardId().toString());
		evaluateValidationRuleVO.setLogginPersonId(awardVO.getPersonId());
		evaluateValidationRuleVO.setUpdateUser(awardVO.getUserName());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			awardVO.setWorkflow(workflowDao.fetchActiveWorkflowByParams(awardVO.getAwardId().toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		awardVO.setCanApproveRouting(canApproveRouting.toString());
		awardVO.setIsFinalApprover(isFinalApprover);
		return awardVO;
	}

	private AwardVO fetchPreviousWorkFlowsList(AwardVO awardVO) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(awardVO.getAwardId().toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			awardVO.setWorkflowList(workFlows);
		}
		return awardVO;
	}

	@Override
	public String addAlternativeApprover(AwardVO vo) {
		Integer moduleCode = vo.getModuleCode();
		Integer moduleItemKey = vo.getModuleItemKey();
		Integer subModuleCode = vo.getSubModuleCode();
		Integer subModuleItemKey = vo.getSubModuleItemKey();
		vo.setApproverFlag("N");
		if(vo.isPrimaryApprover()) {
			vo.setApproverNumber(workflowDao.getMaxApproverNumber(vo.getWorkFlowId(), vo.getMapId(), vo.getMapNumber()));
			vo.setApproverFlag("Y");
		}
		Integer workflowDetailId = awardWorkflowDao.addAlternativeApprover(vo);
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(moduleItemKey.toString(), moduleCode, subModuleItemKey.toString(), subModuleCode);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			vo.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(moduleItemKey.toString(),
					moduleCode, subModuleItemKey.toString(), subModuleCode);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				vo.setWorkflowList(workFlows);
			}
		}
		Integer canApproveRouting = businessRuleDao.canApproveRouting(moduleItemKey.toString(), vo.getPersonId(),
				moduleCode, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		vo.setCanApproveRouting(canApproveRouting.toString());
		vo.setIsFinalApprover(businessRuleDao.workflowfinalApproval(moduleItemKey.toString(), vo.getPersonId(), moduleCode, subModuleItemKey.toString(), subModuleCode));
		if (moduleCode.equals(Constants.AWARD_MODULE_CODE)) {
// need to implement the the code for award setup
			if (subModuleCode.equals(Constants.AWARD_TASK_SUBMODULE_CODE)) {
				vo.setTask(taskDao.fetchTaskByTaskId(subModuleItemKey));
				if (workflow != null && workflow.getCurrentStopName() != null
						&& vo.getTask().getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS)) {
					vo.getTask().setWorkFlowStatusName(
							vo.getTask().getTaskStatus().getDescription() + " : " + workflow.getCurrentStopName());
				} else {
					vo.getTask().setWorkFlowStatusName(vo.getTask().getTaskStatus().getDescription());
				}
				TaskVO taskVO = new TaskVO();
				taskVO.setTaskId(subModuleItemKey);
				taskVO.setModuleCode(moduleCode);
				taskVO.setModuleItemId(moduleItemKey);
				taskVO.setSubModuleCode(subModuleCode);
				taskVO.setPersonId(vo.getPersonId());
				taskVO.setWorkflow(vo.getWorkflow());
				if (vo.getApprovalStopNumber() != null) {
					taskVO.setApproverStopNumber(vo.getApprovalStopNumber());
				}
				if (vo.getMapId() != null) {
					taskVO.setMapId(vo.getMapId());
				}
				taskService.canTaskTakeRoutingAction(taskVO);
				commonDao.doflush();
				if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING) && (workflowDetailId != null && !workflowDetailId.equals(0))) {
					WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
					Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
					commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(),Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
					taskService.sendTaskNotification(taskVO, Constants.TASK_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
				}
			} else {
				Award award = awardDao.getAwardDetailsById(moduleItemKey);
				vo.setAward(award);
				if (workflow.getCurrentStopName() != null && vo.getAward().getWorkflowAwardStatusCode()
						.equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
					vo.getAward().setWorkFlowStatusName(vo.getAward().getAwardWorkflowStatus().getDescription() + " : "
							+ workflow.getCurrentStopName());
				} else {
					vo.getAward().setWorkFlowStatusName(vo.getAward().getAwardWorkflowStatus().getDescription());
				}
				if (award.getSubmitUser() != null) {
					award.setSubmitUserFullName(personDao.getUserFullNameByUserName(award.getSubmitUser()));
				}
				if (vo.getApprovalStopNumber() != null) {
				 vo.setApproverStopNumber(vo.getApprovalStopNumber());
				}
				awardService.canTakeRoutingAction(vo);
				commonDao.doflush();
				if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING) && (workflowDetailId != null && !workflowDetailId.equals(0))) {
					sendAlternateApproverNotificationForAwardOrServiceRequest(vo, workflowDetailId);
				}
			}
		} else if (moduleCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
			Proposal proposal = proposalDao.fetchProposalById(moduleItemKey);
			List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId());
			proposal.setProposalPersons(proposalPersons);
			vo.setProposal(proposal);
			ProposalVO proposalVo= new ProposalVO();
			if (proposal.getSubmitUser() != null) {
				proposal.setSubmitUserFullName(personDao.getUserFullNameByUserName(proposal.getSubmitUser()));
			}
			proposalVo.setProposal(proposal);
			proposalVo.setWorkflow(vo.getWorkflow());
			proposalVo.setPersonId(vo.getPersonId());
			vo.setIsPersonCanScore(proposalDao.fetchPersonCanScore(moduleItemKey, AuthenticatedUser.getLoginPersonId(), workflowDetailId));
			if (vo.getApprovalStopNumber() != null) {
				proposalVo.setApproverStopNumber(vo.getApprovalStopNumber());
			}
			if (vo.getMapId() != null) {
				proposalVo.setMapId(vo.getMapId());
			}
			proposalService.canTakeRoutingAction(proposalVo);
			commonDao.doflush();
			if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING) && (workflowDetailId != null && !workflowDetailId.equals(0))) {
				sendAlternateApproverNotificationForProposal(proposalVo, workflowDetailId);
			}
		} else if (moduleCode.equals(Constants.CLAIM_MODULE_CODE)) {
			Claim claim = claimDao.getClaim(moduleItemKey);
			claim.setHostUnitDescription(commonDao.getUnitByUnitNumber(claim.getHostUnitNumber()).getUnitName());
			claim.setUpdateUserName(personDao.getUserFullNameByUserName(claim.getUpdateUser()));
			claim.setCreateUserName(personDao.getUserFullNameByUserName(claim.getCreateUser()));
			vo.setClaim(claim);
			ClaimsVO claimVO= new ClaimsVO();
			claimVO.setWorkflow(vo.getWorkflow());
			claimVO.setPersonId(vo.getPersonId());
			claimVO.setClaimId(moduleItemKey);
			claimVO.setLastClaimEndDate(claimDao.getLastClaimEndDate(claim.getAwardNumber(), claim.getClaimId()));
			if (vo.getApprovalStopNumber() != null) {
				claimVO.setApproverStopNumber(vo.getApprovalStopNumber());
			}
			if (vo.getMapId() != null) {
				claimVO.setMapId(vo.getMapId());
			}
			claimService.canClaimTakeRoutingAction(claimVO);
			commonDao.doflush();
			if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING) && (workflowDetailId != null && !workflowDetailId.equals(0))) {
				sendAlternateApproverNotificationForClaim(claimVO, workflowDetailId);
			}
		} else if (moduleCode.equals(Constants.PROGRESS_REPORT_MODULE_CODE)) {
			AwardProgressReport awardProgressReport = progressReportService.loadAwardProgressReportDetails(moduleItemKey);
			vo.setAwardProgressReport(awardProgressReport);
			ProgressReportVO progressReportVO = new ProgressReportVO();
			progressReportVO.setAwardProgressReport(awardProgressReport);
			progressReportVO.setWorkflow(vo.getWorkflow());
			progressReportVO.setPersonId(vo.getPersonId());
			progressReportVO.setProgressReportId(moduleItemKey);
			if (vo.getApprovalStopNumber() != null) {
				progressReportVO.setApproverStopNumber(vo.getApprovalStopNumber());
			}
			if (vo.getMapId() != null) {
				progressReportVO.setMapId(vo.getMapId());
			}
			progressReportService.canProgressReportTakeRoutingAction(progressReportVO);
			commonDao.doflush();
			if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
				sendAlternateApproverNotificationForProgressReport(progressReportVO, workflowDetailId);
			}
		} else if (moduleCode.equals(Constants.SERVICE_REQUEST_MODULE_CODE)) {
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(moduleItemKey);
			vo.setServiceRequest(serviceRequest);
			ServiceRequestVO serviceRequestVO = new ServiceRequestVO();
			serviceRequestVO.setServiceRequest(serviceRequest);
			serviceRequestVO.setWorkflow(vo.getWorkflow());
			serviceRequestVO.setPersonId(vo.getPersonId());
			serviceRequestVO.setServiceRequestId(moduleItemKey);
			if (vo.getApprovalStopNumber() != null) {
				serviceRequestVO.setApproverStopNumber(vo.getApprovalStopNumber());
			}
			if (vo.getMapId() != null) {
				serviceRequestVO.setMapId(vo.getMapId());
			}
			serviceRequestService.canServiceRequestTakeRoutingAction(serviceRequestVO);
			commonDao.doflush();
			if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
				sendAlternateApproverNotificationForServiceRequest(serviceRequestVO, workflowDetailId);
			}
		} else if (moduleCode.equals(Constants.AGREEMENT_MODULE_CODE)) {
			AgreementHeader agreementHeader = agreementDao.getAgreementById(moduleItemKey);
			vo.setAgreementHeader(agreementHeader); 
			AgreementVO agreementvo = new AgreementVO();
			agreementvo.setAgreementHeader(agreementHeader);
			agreementvo.setWorkflow(vo.getWorkflow());
			agreementvo.setPersonId(vo.getPersonId());
			agreementvo.setAgreementRequestId(moduleItemKey);  
			agreementService.canAgreementTakeRoutingAction(agreementvo);
			if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
				if (vo.getApprovalStopNumber()!= null) {
					agreementvo.setApproverStopNumber(vo.getApprovalStopNumber());
				}
				if (vo.getMapId() != null) {
					agreementvo.setMapId(vo.getMapId());
				}
				commonDao.doflush();
				sendAlternateApproverNotificationForAgreement(agreementvo, workflowDetailId);
			}

		}
		return commonDao.convertObjectToJSON(vo);
	}
	
	private void sendAlternateApproverNotificationForProgressReport(ProgressReportVO progressReportVO,
			Integer workflowDetailId) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
		commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		progressReportService.sendProgressReportNotification(progressReportVO, Constants.PROGRESS_REPORT_APPROVAL_NOTIFICATION_CODE, dynamicEmailrecipients);		
	}

	@Override
	public AwardPerson getAwardPrincipalInvestigator(List<AwardPerson> awardPersons) {
		AwardPerson piAwardPerson = null;
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson person : awardPersons) {
				if (person != null){
					if (StringUtils.equals(person.getProposalPersonRole().getCode(), Constants.PRINCIPAL_INVESTIGATOR)) {
						piAwardPerson = person;
						break;
					}
				}
			}
		}
		return piAwardPerson;
	}

	private void sendServiceRequestAndAwardSubmitNotification(AwardVO awardVO) {
		Award award = awardVO.getAward();
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		Workflow workFlow = awardVO.getWorkflow();
		Integer personRoleTypeCode = null;
		if (workFlow.getWorkflowId() != null) {
			List<WorkflowDetail> workFlowDetails = workFlow.getWorkflowDetails();
			if (workFlowDetails != null && !workFlowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workFlowDetails) {
					if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
						if (workflowDetail.getPersonRoleType() != null) {
							personRoleTypeCode = workflowDetail.getPersonRoleType().getRoleTypeCode();
						}
						commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
						awardVO.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
						awardVO.setMapId(workflowDetail.getMapId());
					}
				}
			}
		}
		if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
			if (Constants.FINANCE_ROLE_TYPE_CODE.equals(personRoleTypeCode)) {
				awardService.sendAwardNotification(awardVO, Constants.FINANCE_AWARD_ASSIGN_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else if (Constants.GRANT_ADMINISTRATOR_ROLE_TYPE_CODE.equals(personRoleTypeCode)) {
				awardService.sendAwardNotification(awardVO, Constants.GRANT_ADMIN_AWARD_ASSIGN_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else {
				awardService.sendAwardNotification(awardVO, Constants.AWARD_SUBMIT_NOTIFICATION_CODE, dynamicEmailrecipients);
			}
		} else {
			sendNotification(awardVO, Constants.AWARD_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
		}
		if (award.getAwardVariationTypeCode() != null && award.getAwardVariationTypeCode().equals(Constants.OTHER_SERVICE_REQUEST_TYPE_CODE)) {
			sendNotification(awardVO, Constants.SERVICE_REQUEST_FOR_DMP_NOTIFICATION_CODE, new HashSet<>());
		}
	}

	@Override
	public void sendNotification(AwardVO awardVO, Integer createVariationRequestNotification, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(createVariationRequestNotification);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(awardVO.getAwardId().toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO.setPlaceHolder(getServiceRequestPlaceholders(awardVO));
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);	
	}

	private Map<String, String> getServiceRequestPlaceholders(AwardVO vo) {
		Map<String, String> placeHolder = new HashMap< >();
		placeHolder.put("{SERVICE_REQUEST_TYPE}", vo.getAward().getServiceRequestType() == null ? "" : vo.getAward().getServiceRequestType().getDescription());
		placeHolder.put("{WORKFLOW_COMMENT}", vo.getApproveComment() != null ? vo.getApproveComment() : "No Comments");
		String stopName = commonService.getPlaceHolderDataForRouting(vo.getApproverStopNumber(),vo.getMapId(),vo.getWorkflowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	private void sendAlternateApproverNotificationForAwardOrServiceRequest(AwardVO vo, Integer workflowDetailId) {
		if (vo.getApprovalStatus().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
			WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
			vo.setActionType("A");
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(),Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
			awardService.sendAwardNotification(vo, Constants.AWARD_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
		}
	}

	private void sendAlternateApproverNotificationForProposal(ProposalVO proposalVO, Integer workflowDetailId) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
		commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		proposalService.sendProposalNotification(proposalVO, Constants.PROPOSAL_APPROVE_NOTIFICATION_CODE, dynamicEmailrecipients);
	}

	@Override
	public String addSequentialStop(AwardVO vo) {
		vo.setApprovalStopNumber(workflowDao.getNextStopNumberBasedOnMap(vo.getMapId(), vo.getWorkFlowId()));
		return addAlternativeApprover(vo);
	}

	@Override
	public void sapBasedWorkflowStatusAndSequenceStatus(Award award, String updateUser) {
		String budgetStatusCode = null;
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SAP_AWARD_FEED) && award != null) {
			if (Boolean.TRUE.equals(sectionWiseEditDao.isChangeBudgetStatus(award.getAwardId().toString(), Constants.SUBMODULE_ITEM_KEY,Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE, Constants.BUDGET_EDITABLE_SECTION_TYPE_CODE)) ||
					award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
				budgetStatusCode = Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE;
			} else {
				budgetStatusCode = getActiveAwardBudgetStatusCode(award.getAwardId());
			}
		} else {
			budgetStatusCode = Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE;
		}
		updateAwardWorkflowStatusAndSequenceStatus(award, Constants.AWARD_WORKFLOW_STATUS_ACTIVE, Constants.AWARD_FINAL_STATUS_ACTIVE, updateUser, budgetStatusCode);
		awardService.updateAwardBudgetStatusByStatusCode(award.getAwardId(), budgetStatusCode);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(award.getAwardId(), updateUser);
	}

	private String getActiveAwardBudgetStatusCode(Integer awardId) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardId);
		if (awardBudgetHeader != null) {
			return awardBudgetHeader.getBudgetStatusCode();
		} else {
			return null;
		}
	}

	private void updateAwardWorkflowStatusAndSequenceStatus(Award award, String workflowStatus, String sequenceStatus, String updateUser, String budgetStatusCode) {
		award.setWorkflowAwardStatusCode(workflowStatus);
		award.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(workflowStatus));
		awardDao.saveOrUpdateAwardDetails(award);
		updateAwardHistoryLog(award);
		datesAndAmountService.updateDatesAndAmounts(award.getAwardId(), award.getAwardNumber(), award.getSequenceNumber(), Constants.ACTIVE_TRANSACTION);
		if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
			AwardVO vo = new AwardVO();
			vo.setIsMasterAwardCreation(true);
			vo.setUpdateUser(updateUser);
			vo.setCreateUser(award.getCreateUser());
			vo.setAwardId(award.getAwardId());
			vo.setAward(award);
			vo.setBudgetStatusCode(budgetStatusCode);
			awardVersionService.copyAward(vo);
			Award activeAward = vo.getAward();
			if (activeAward.getAwardDocumentTypeCode().equals(Constants.MASTER_AWARD)) {
				if (activeAward.getBeginDate().after(commonDao.getCurrentTimestamp())) {
					String awardStatusCode = commonDao.getParameterValueAsString(Constants.AWARD_STATUS_ON_APPROVAL);
					award.setStatusCode(awardStatusCode);
					award.setAwardStatus(awardDao.fetchAwardStatusByCode(awardStatusCode));
					activeAward.setStatusCode(awardStatusCode);
					activeAward.setAwardStatus(awardDao.fetchAwardStatusByCode(awardStatusCode));
				}
				awardDao.deleteAwardReportTrackingAfterEndDate(activeAward.getAwardId());
			}
		} else {
			award.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_ARCHIVE);
			awardDao.saveOrUpdateAwardDetails(award);
			awardDao.createOrUpdateMasterAward(award.getAwardId(), updateUser);
		}
	}

	@Override
	public void updateAwardHistoryLog(Award award) {
		AwardHistoryLog awardHistoryLog = awardDao.getAwardHistoryLogBasedOnAwardId(award.getAwardId());
		if (awardHistoryLog != null) {
			awardVersionService.addToAwardHistoryLog(award, awardHistoryLog);
		}
	}

	@Override
	public void checkAndFeedSapAwardDetail(Award award, String userName) {
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SAP_AWARD_FEED) && award != null) {
//			String canInsertSAP = integrationService.checkForTheSAPFeedResponse(award.getAwardNumber());
//			logger.info("canInsertSap for saveSAPAwardDetails : {}", canInsertSAP);
//			if (canInsertSAP.equals("TRUE")) {				
				integrationService.saveSAPAwardDetails(award.getAwardId(), award.getAwardNumber(), award.getSequenceNumber(), userName);
//			}
		}
		
	}

	private void sendAlternateApproverNotificationForClaim(ClaimsVO claimVO, Integer workflowDetailId) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
		commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		claimService.sendClaimNotification(claimVO, Constants.CLAIM_APPROVAL_NOTIFICATION_CODE, dynamicEmailrecipients);
	}

	private void sendAlternateApproverNotificationForServiceRequest(ServiceRequestVO vo, Integer workflowDetailId) {
		Set<String> dynamicEmailrecipients = new HashSet<>();
		WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
		dynamicEmailrecipients.add(workflowDetail.getApproverPersonId());
		vo.setApproverNumber(workflowDetail.getApprovalStopNumber());
		vo.setMapId(workflowDetail.getMapId());
		serviceRequestService.sendMailForServiceRequestActions(Constants.SERVICE_REQUEST_APPROVAL_NOTIFICATION_CODE,dynamicEmailrecipients,vo);		
	}
	
	private void sendAlternateApproverNotificationForAgreement(AgreementVO vo, Integer workflowDetailId) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		WorkflowDetail workflowDetail = workflowDao.fetchWorkflowDetailById(workflowDetailId);
		commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		agreementService.sendNotificationForAgreement(vo, Constants.AGREEMENT_APPROVAL_NOTIFICATION_CODE, dynamicEmailrecipients);    
	}
}
