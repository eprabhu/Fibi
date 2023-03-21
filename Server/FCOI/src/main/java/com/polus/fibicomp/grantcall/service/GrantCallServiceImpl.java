package com.polus.fibicomp.grantcall.service;

import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dao.GrantCallEvaluationPanelDao;
import com.polus.fibicomp.grantcall.dao.GrantCallIOIDao;
import com.polus.fibicomp.grantcall.dao.GrantCallKPIDao;
import com.polus.fibicomp.grantcall.dao.GrantCallScoringDao;
import com.polus.fibicomp.grantcall.dto.ScoredPersonDTO;
import com.polus.fibicomp.grantcall.dto.ScoringReportDto;
import com.polus.fibicomp.grantcall.module.GrantCallModuleDao;
import com.polus.fibicomp.grantcall.pojo.FundingSchemeAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallContact;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibleDepartment;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIQuestionnaire;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPICriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallKeyword;
import com.polus.fibicomp.grantcall.pojo.GrantCallRelevant;
import com.polus.fibicomp.grantcall.pojo.GrantCallResearchArea;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallStatus;
import com.polus.fibicomp.grantcall.pojo.GrantEligibilityTarget;
import com.polus.fibicomp.grantcall.vo.EvaluationMainPanelVO;
import com.polus.fibicomp.grantcall.vo.GrantCallVO;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerComment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerScore;
import com.polus.fibicomp.utils.QueryBuilder;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Transactional
@Service(value = "grantCallService")
public class GrantCallServiceImpl implements GrantCallService {

	protected static Logger logger = LogManager.getLogger(GrantCallServiceImpl.class.getName());

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private PrintService printService;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private GrantCallIOIDao grantCallIOIDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AuthorizationService authorizationService;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private GrantCallScoringDao grantCallScoringDao;

	@Autowired
	private GrantCallKPIDao grantCallKPIDao;

	@Autowired
	private GrantCallEvaluationPanelDao grantCallEvaluationPanelDao;

	@Autowired
	private RolesManagementDao rolesManagementDao;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private GrantCallModuleDao grantCallModuleDao;

	@Autowired
	private BudgetDao proposalBudgetDao;

	@Autowired
	private InboxDao inboxDao;

	@Override
	public String createGrantCall(GrantCallVO grantCallVO) {
		String sponsorCode = grantCallVO.getGrantCall().getSponsorCode();
		if (sponsorCode != null && !sponsorCode.isEmpty()) {
			grantCallVO.setSponsorFundingSchemes(grantCallDao.fetchFundingSchemeBySponsor(sponsorCode));
		}
		if (grantCallVO.getGrantCall().getGrantCallId() == null) {
			GrantCallStatus grantCallStatus = grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_DRAFT);
			GrantCall grantCall = grantCallVO.getGrantCall();
			grantCall.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_DRAFT);
			grantCall.setGrantCallStatus(grantCallStatus);
		}
		grantCallVO.setGrantCallTypes(grantCallDao.fetchAllGrantCallTypes());
		grantCallVO.setSponsorTypes(grantCallDao.fetchAllSponsorTypes());
		grantCallVO.setGrantCallCriterias(grantCallDao.fetchAllGrantCallCriteria());
		grantCallVO.setGrantCallEligibilityTypes(grantCallDao.fetchAllEligibilityTypes());
		grantCallVO.setGrantCallAttachTypes(grantCallDao.fetchAllGrantCallAttachTypes());
		grantCallVO.setHomeUnits(committeeDao.fetchAllHomeUnits());
		grantCallVO.setGrantCallStatus(grantCallDao.fetchAllGrantCallStatus());
		grantCallVO.setGrantCallPersonRoles(proposalLookUpDao.fetchAllProposalPersonRoles());
		grantCallVO.setResearchTypes(commonDao.fetchAllResearchTypes());
		grantCallVO.setCurrencyDetail(commonDao.fetchCurrencyDetails());
		grantCallVO.setRelevantFields(grantCallDao.fetchAllRelevantFields());
		grantCallVO.setGrantEligibiltyTargetTypes(grantCallDao.fetchAllGrantEligibiltyTargetTypes());
		grantCallVO.setAvailableRights(authorizationService.allDepartmentPermission(Constants.GRANTCALL_MODULE_CODE,
				grantCallVO.getLoginPersonId(), grantCallVO.getHomeUnitNumber(), grantCallVO.getGrantCallId()));
		grantCallVO.setTimeZone(Constants.CRON_JOB_TIMEZONE);
		grantCallVO.setRateTypes(proposalBudgetDao.fetchRateTypeByParams("1", proposalBudgetDao.fetchRateClassCodesByType(commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE))));
		grantCallVO.setEnableUserDefinedFundingAgency(commonDao.getParameterValueAsBoolean(Constants.ENABLE_USER_DEFINED_FUNDING_AGENCY));
		grantCallVO.setIsReplaceAttachmentEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_REPLACE_ATTACHMENTS_GRANTCALL));
		grantCallVO.setGrantCallPocDefaultMail(commonDao.getParameterValueAsString(Constants.GRANT_CALL_POC_DEFAULT_EMAIL));
		return committeeDao.convertObjectToJSON(grantCallVO);
	}

	@Override
	public String loadGrantCallById(GrantCallVO vo) {
		GrantCallVO grantCallVO = new GrantCallVO();
		GrantCall grantCall = grantCallDao.fetchGrantCallById(vo.getGrantCallId());
		String homeUnitNumber = null;
		List<FundingSchemeAttachment> fundingSchemeAttachments = new ArrayList<>();
		if (grantCall.getFundingSchemeId() != null) {
			fundingSchemeAttachments = grantCallDao.fetchFundingSchemeAttachmentBasedOnScheme(grantCall.getFundingSchemeId());
		}
		loadGrantCallHomeData(grantCallVO, vo.getGrantCallId());
		Integer grantCallStatusCode = grantCall.getGrantStatusCode();
		Boolean viewGrantCallApplicationRight = rolesManagementDao.isPersonHasRightInAnyDepartment(vo.getLoginPersonId(), Constants.RIGHT_VIEW_GRANT_CALL_APPLICATIONS);
		Boolean isPersonHasRole = rolesManagementDao.isPersonHasRole(vo.getLoginPersonId(), Constants.GRANT_ADMINISTRATOR_ROLE_ID);
		if ((grantCallStatusCode.equals(Constants.GRANT_CALL_STATUS_CODE_OPEN) || grantCallStatusCode.equals(Constants.GRANT_CALL_STATUS_CODE_CLOSED)
				|| grantCallStatusCode.equals(Constants.GRANT_CALL_STATUS_CODE_ARCHIVED))
				&& Boolean.TRUE.equals(viewGrantCallApplicationRight)) {
			List<Integer> proposalStatus = new ArrayList<>();
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_GRANT_MANAGER_RETURNED);
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_GRANT_ADMIN_RETURNED);
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_RETURNED);
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_REVISION_REQUESTED);
			proposalStatus.add(Constants.PROPOSAL_STATUS_CODE_HOD_RETURNED);
			if (Boolean.TRUE.equals(isPersonHasRole)) {
				homeUnitNumber = vo.getHomeUnitNumber();
			}
			List<Proposal> proposals = proposalDao.fetchProposalsByGrantCallIdAndStatus(vo.getGrantCallId(), proposalStatus, homeUnitNumber);
			if (proposals != null && !proposals.isEmpty()) {
				List<Proposal> proposalDatas = new ArrayList<>();
				for (Proposal proposalObject : proposals) {
					Proposal propObj = new Proposal();
					propObj.setProposalId(proposalObject.getProposalId());
					propObj.setApplicationId(proposalObject.getApplicationId());
					propObj.setTitle(proposalObject.getTitle());
					if (proposalObject.getActivityType() != null)
						propObj.setApplicationActivityType(proposalObject.getActivityType().getDescription());
					propObj.setApplicationStatus(proposalObject.getProposalStatus().getDescription());
					propObj.setSubmissionDate(proposalObject.getSubmissionDate());
					propObj.setProposalPersons(proposalDao.fetchProposalPersonBasedOnProposalId(proposalObject.getProposalId()));
					Sponsor sponsor = proposalObject.getSponsor();
					if (sponsor != null) {
						propObj.setSponsorName(commonService.getSponsorFormatBySponsorDetail(sponsor.getSponsorCode(), sponsor.getSponsorName(), sponsor.getAcronym()));
					}
					propObj.setHomeUnitName(proposalObject.getUnit().getUnitName());
					propObj.setSubmitUser(proposalObject.getSubmitUser());
					propObj.setCreateUser(proposalObject.getCreateUser());
					propObj.setSponsorDeadlineDate(proposalObject.getSponsorDeadlineDate());
					propObj.setScore(getEvaluationScore(vo.getGrantCallId(), proposalObject.getProposalId()));
					proposalDatas.add(propObj);
				}
				grantCall.setProposals(proposalDatas);
			}
		}
		loadGrantCallUserFullNames(grantCall);
		grantCall.setClosingTime(setGrantCallClosingDate(grantCall.getClosingDate()));
		grantCallVO.setGrantCall(grantCall);
		grantCallVO.setLoginPersonId(vo.getLoginPersonId());
		grantCallVO.setGrantCallId(grantCall.getGrantCallId());
		grantCallVO.setHomeUnitNumber(grantCall.getHomeUnitNumber());
		grantCallVO.setFundingSchemeAttachment(fundingSchemeAttachments);
		grantCallVO.setProposalEvaluationScores(grantCallDao.fetchProposalEvaluationById(vo.getGrantCallId()));
		createGrantCall(grantCallVO);
		if (grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_OPEN) || grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE)) {
			grantCallVO.setCanCreateIOI(true);
		}
		grantCallVO.setTimeZone(Constants.CRON_JOB_TIMEZONE);
		grantCallVO.setRateTypes(proposalBudgetDao.fetchRateTypeByParams("1", proposalBudgetDao.fetchRateClassCodesByType(commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE))));
		grantCallVO.setIsReplaceAttachmentEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_REPLACE_ATTACHMENTS_GRANTCALL));
		grantCallVO.setGrantCallPocDefaultMail(commonDao.getParameterValueAsString(Constants.GRANT_CALL_POC_DEFAULT_EMAIL));
		grantCallVO.setEnableUserDefinedFundingAgency(commonDao.getParameterValueAsBoolean(Constants.ENABLE_USER_DEFINED_FUNDING_AGENCY));
		return committeeDao.convertObjectToJSON(grantCallVO);
	}

	private String setGrantCallClosingDate(Timestamp grantCallClosingDate) {
		DateFormat timeFormat = new SimpleDateFormat(Constants.TIME_FORMAT);
		return (timeFormat.format(grantCallClosingDate)).toString();
	}

	private void loadGrantCallHomeData(GrantCallVO grantCallVO, Integer grantCallId) {
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
			List<GrantCallEvaluationPanel> grantCallEvaluationPanels = grantCallEvaluationPanelDao.fetchEvaluationPanelByGrantCallId(grantCallId);
			if (grantCallEvaluationPanels != null && !grantCallEvaluationPanels.isEmpty()) {
				grantCallVO.setGrantCallEvaluationPanels(grantCallEvaluationPanels);
			}		
		}		
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			List<GrantCallKPI> grantCallKPIs = grantCallKPIDao.fetchKPIByGrantCallId(grantCallId);
			if (grantCallKPIs != null && !grantCallKPIs.isEmpty()) {
				grantCallVO.setGrantCallKPIs(grantCallKPIs);
			}		
		}		
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SCORING_CRITERIA)) {
			List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(grantCallId);
			if (grantCallScoringCriterias != null && !grantCallScoringCriterias.isEmpty()) {
				grantCallVO.setGrantCallScoringCriterias(grantCallScoringCriterias);		
			}
		}
		List <GrantCallAttachment> grantCallAttachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCallId);
		if (grantCallAttachments != null && !grantCallAttachments.isEmpty()) {
			for (GrantCallAttachment grantCallAttachment : grantCallAttachments) {
				if (grantCallAttachment.getUpdateUser() != null) {
					grantCallAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(grantCallAttachment.getUpdateUser()));
				}
			}
			grantCallVO.setGrantCallAttachments(grantCallAttachments);
		}
		loadGrantCallEligibility(grantCallVO, grantCallId);
		List<GrantCallContact> grantCallContacts = grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(grantCallId);
		if (grantCallContacts != null && !grantCallContacts.isEmpty()) {
			grantCallVO.setGrantCallContacts(grantCallContacts);
		}
		List<GrantCallResearchArea> grantCallResearchAreas = grantCallModuleDao.fetchGrantCallResearchAreaBasedOnGrantCallId(grantCallId);
		if (grantCallResearchAreas != null && !grantCallResearchAreas.isEmpty()) {
			grantCallVO.setGrantCallResearchAreas(grantCallResearchAreas);
		}
		List<GrantCallEligibleDepartment> grantCallEligibleDepartments = grantCallModuleDao.fetchGrantCallEligibleDepartmentBasedOnGrantCallId(grantCallId);
		if (grantCallEligibleDepartments != null && !grantCallEligibleDepartments.isEmpty()) {
			grantCallVO.setGrantEligibleDepartments(grantCallEligibleDepartments);
		}
		List<ProposalEvaluationScore> proposalEvaluationScore = grantCallDao.fetchProposalEvaluationById(grantCallId);
		if (proposalEvaluationScore != null && !proposalEvaluationScore.isEmpty()) {
			grantCallVO.setProposalEvaluationScores(proposalEvaluationScore);
		}
		List<GrantCallIOIHeader> grantCallIOIHeaders = grantCallModuleDao.fetchGrantCallIOIHeaderBasedOnGrantCallId(grantCallId);
		if (grantCallIOIHeaders != null && !grantCallIOIHeaders.isEmpty()) {
			grantCallVO.setGrantCallIOIHeaders(grantCallIOIHeaders);
		}
	}

	private void loadGrantCallEligibility(GrantCallVO grantCallVO, Integer grantCallId) {
		List<GrantCallEligibility> grantCallEligibilities = grantCallModuleDao.fetchGrantCallEligibilityBasedOnGrantCallId(grantCallId);
		if (grantCallEligibilities != null && !grantCallEligibilities.isEmpty()) {
			for (GrantCallEligibility grantCallEligibility : grantCallEligibilities) {
				GrantEligibilityTarget grantEligibilityTarget = grantCallEligibility.getGrantEligibilityTarget();
				if (grantEligibilityTarget != null) {
					String target = grantEligibilityTarget.getTargetValue();
					if (target != null) {
						String targetCategoryTypeCode = grantEligibilityTarget.getTargetCategoryTypeCode();
						if (targetCategoryTypeCode != null && targetCategoryTypeCode.equals("U")) {
							Unit unit = commonDao.getUnitByUnitNumber(target);
							grantEligibilityTarget.setTargetValueDescription(unit.getUnitName());
						} else if (targetCategoryTypeCode != null && targetCategoryTypeCode.equals("P")) {
							Person person = personDao.getPersonDetailById(target);
							if (person != null) {
								grantEligibilityTarget.setTargetValueDescription(person.getFullName());
							}
						}
					}
				}
			}
		}
		grantCallVO.setGrantCallEligibilities(grantCallEligibilities);
	}

	private BigDecimal getEvaluationScore(Integer grantCallID, Integer proposalID) {
		BigDecimal score = BigDecimal.ZERO;
		try {
			String query = QueryBuilder.selectScoringCriteriaCalculationsForMAinPanel(grantCallID);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
			if (!dataList.isEmpty()) {
				for (HashMap<String, Object> data : dataList) {
					if (proposalID.equals(Integer.parseInt(data.get("PROPOSAL_ID").toString()))) {
						score = new BigDecimal(data.get("AVERAGE_SCORE").toString()).setScale(2, RoundingMode.HALF_UP);
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in getEvaluationScore {}", e.getMessage());
		}
		return score;
	}

	@Override
	public String saveUpdateGrantCall(GrantCallVO vo) {
		GrantCall grantCall = vo.getGrantCall();
		if (grantCall.getClosingDate() != null && grantCall.getClosingTime() != null) {
			grantCall.setClosingDate(Timestamp.valueOf((grantCall.getClosingDate().toString()).substring(0, 11).concat(grantCall.getClosingTime())));
		}
		List<FundingSchemeAttachment> fundingSchemeAttachments = new ArrayList<>();
		grantCall = grantCallDao.saveOrUpdateGrantCall(grantCall);
		vo.setStatus(true);
		String updateType = vo.getUpdateType();
		if (updateType != null && updateType.equals("SAVE")) {
			vo.setMessage("Grant Call saved successfully");
			saveGrantCallActionLogDetails(grantCall.getGrantCallId(), Constants.GRANT_CALL_CREATED, vo.getGrantCall().getUpdateUser());
		} else {
			vo.setMessage("Grant Call updated successfully");
			saveGrantCallActionLogDetails(grantCall.getGrantCallId(), Constants.GRANT_CALL_UPDATED, vo.getGrantCall().getUpdateUser());
		}
		if (grantCall.getFundingSchemeId() != null) {
			fundingSchemeAttachments = grantCallDao.fetchFundingSchemeAttachmentBasedOnScheme(grantCall.getFundingSchemeId());
		}
		vo.setFundingSchemeAttachment(fundingSchemeAttachments);
		loadGrantCallUserFullNames(grantCall);
		if (grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_OPEN) || grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE)) {
			vo.setCanCreateIOI(true);
		}
		vo.setGrantCall(grantCall);
		Integer moduleCode = Constants.GRANTCALL_MODULE_CODE;
		String personId = vo.getLoginPersonId();
		vo.setAvailableRights(authorizationService.allDepartmentPermission(moduleCode, personId, grantCall.getHomeUnitNumber(), vo.getGrantCallId()));
		sendGrantCallNotification(vo, Constants.GRANTCALL_UPDATE_NOTIFICATION_CODE, new HashSet<>());
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String publishGrantCall(GrantCallVO vo) {
		GrantCall grantCall = vo.getGrantCall();
		grantCall.setIsPublished(true);
		grantCall.setPublishTimeStamp(commonDao.getCurrentTimestamp());
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		List<FundingSchemeAttachment> fundingSchemeAttachments = new ArrayList<>();
		if (commonDao.getParameterValueAsBoolean(Constants.AUTOMATE_GRANTCALL_INTERNAL_STATUS) && grantCall.getGrantCallType().getCategoryCode().equals(Constants.GRANT_CALL_TYPE_INTERNAL)) {
			Timestamp currentDate = committeeDao.getCurrentTimestamp();
			if (grantCall.getOpeningDate().before(currentDate)) {
				grantCall.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_OPEN);
				GrantCallStatus grantCallStatus = grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_OPEN);
				grantCall.setGrantCallStatus(grantCallStatus);
			} else {
				grantCall.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE);
				GrantCallStatus grantCallStatus = grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE);
				grantCall.setGrantCallStatus(grantCallStatus);
			}
		}
		if (grantCall.getClosingDate() != null && grantCall.getClosingTime() != null) {
			grantCall.setClosingDate(Timestamp.valueOf((grantCall.getClosingDate().toString()).substring(0, 11).concat(grantCall.getClosingTime())));
		}
		grantCall = grantCallDao.saveOrUpdateGrantCall(grantCall);
		saveGrantCallActionLogDetails(grantCall.getGrantCallId(), Constants.GRANT_CALL_PUBLISHED, vo.getGrantCall().getUpdateUser());
		if (grantCall.getFundingSchemeId() != null) {
			fundingSchemeAttachments = grantCallDao.fetchFundingSchemeAttachmentBasedOnScheme(grantCall.getFundingSchemeId());
		}
		if (grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_OPEN) || grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE)) {
			vo.setCanCreateIOI(true);
		}
		loadGrantCallHomeData(vo, grantCall.getGrantCallId());
		vo.setFundingSchemeAttachment(fundingSchemeAttachments);
		loadGrantCallUserFullNames(grantCall);
		vo.setGrantCall(grantCall);
		getEligibilityTargetPersons(dynamicEmailRecipients, grantCall.getGrantCallId());
		sendGrantCallNotification(vo, Constants.GRANTCALL_PUBLISH_NOTIFICATION_CODE, dynamicEmailRecipients);
		List<GrantCallContact> grantCallContacts = grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(grantCall.getGrantCallId());
		if (grantCallContacts != null && !grantCallContacts.isEmpty()) {
			sendGrantCallNotificationForPointOfContacts(vo);
		}
		if (((grantCall.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_INTERNAL_SIMPLE_EVALUATION))
				|| (grantCall.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_EXTERNAL_SIMPLE_EVALUATION)))
				&& commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
			List<GrantCallEvaluationPanel> grantCallEvaluationPanels = grantCallEvaluationPanelDao.fetchEvaluationPanelByGrantCallId(grantCall.getGrantCallId());
			if (grantCallEvaluationPanels != null && !grantCallEvaluationPanels.isEmpty()) {
				for (GrantCallEvaluationPanel grantCallEvaluationPanel : grantCallEvaluationPanels) {
					grantCallEvaluationPanelDao.deleteGrantCallEvaluationPanel(grantCallEvaluationPanel.getGrantCallEvaluationPanelId());
				}
			}
			GrantCallEvaluationPanel grantCallEvaluationPanel = new GrantCallEvaluationPanel();
			grantCallEvaluationPanel.setGrantCallId(grantCall.getGrantCallId());
			grantCallEvaluationPanel.setMapId(Constants.SIMPLE_EVALUATION_PANEL_MAP_ID);
			grantCallEvaluationPanel.setUpdateUser(grantCall.getUpdateUser());
			grantCallEvaluationPanel.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			grantCallEvaluationPanel.setIsMainPanel(Constants.MAIN_PANEL_FOR_SIMPLE_EVALUATION_PANEL);
			grantCallEvaluationPanelDao.saveOrUpdateGrantCallEvaluationPanel(grantCallEvaluationPanel);
		}
		sendGrantCallModificationNotification(vo);
		return committeeDao.convertObjectToJSON(vo);
	}

	private void getEligibilityTargetPersons(Set<NotificationRecipient> dynamicEmailRecipients, Integer moduleItemKey) {
		if (Boolean.TRUE.equals(grantCallDao.checkGrantEligibilityExternalExist(moduleItemKey))) {
			String toEmailAddress = commonDao.getParameterValueAsString(Constants.GRANTCALL_ELIGIBLE_TARGET_TYPE_MAILING_LIST);
			String[] emailIds = toEmailAddress.split(",");
			if (StringUtils.isNotBlank(toEmailAddress) && emailIds.length > 0) {
				for (String recipientMailAddress : emailIds) {
	                 commonService.setNotificationRecipientsforNonEmployees(recipientMailAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
	            }
			}
		}
		List<HashMap<String, Object>> targetPersons = grantCallDao.fetchEligibilityTargetPersons(moduleItemKey);
		if (targetPersons != null && !targetPersons.isEmpty()) {
			for (HashMap<String, Object> targetPerson : targetPersons) {
				if (targetPerson.get("TARGET_VALUE") != null) {
					commonService.setNotificationRecipients(targetPerson.get("TARGET_VALUE").toString(), "TO", dynamicEmailRecipients);
				}
			}
		}
	}

	@Override
	public String fetchFundingSchemeBySponsor(GrantCallVO vo) {
		vo.setSponsorFundingSchemes(grantCallDao.fetchFundingSchemeBySponsor(vo.getSponsorCode()));
		return committeeDao.convertObjectToJSON(vo);
	}

	 @Override
	public String deleteGrantCallKeyword(GrantCallVO vo) {
		try {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(vo.getGrantCallId());
			List<GrantCallKeyword> grantCallKeywords = grantCall.getGrantCallKeywords();
			List<GrantCallKeyword> updatedGrantCallKeywords = new ArrayList<>(grantCallKeywords);
			Collections.copy(updatedGrantCallKeywords, grantCallKeywords);
			for (GrantCallKeyword grantCallKeyword : grantCallKeywords) {
				if (grantCallKeyword.getGrantKeywordId().equals(vo.getGrantKeywordId())) {
					updatedGrantCallKeywords.remove(grantCallKeyword);
				}
			}
			grantCall.getGrantCallKeywords().clear();
			grantCall.getGrantCallKeywords().addAll(updatedGrantCallKeywords);
			grantCallDao.saveOrUpdateGrantCall(grantCall);
			loadGrantCallUserFullNames(grantCall);
			vo.setGrantCall(grantCall);
			vo.setStatus(true);
			vo.setMessage("Grant call keyword deleted successfully");
		} catch (Exception e) {
			vo.setStatus(true);
			vo.setMessage("Problem occurred in deleting grant call keyword");
			logger.error("Exception while saving deleteGrantCallKeyword {}", e.getMessage());
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteGrantCallContact(GrantCallVO vo) {
		grantCallModuleDao.deleteGrantCallContact(grantCallModuleDao.fetchGrantCallContactById(vo.getGrantContactId()));
		vo.setGrantCallContacts(grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(vo.getGrantCallId()));
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteGrantCallAreaOfResearch(GrantCallVO vo) {
		grantCallModuleDao.deleteGrantCallResearchArea(grantCallModuleDao.fetchGrantCallResearchAreaById(vo.getGrantResearchAreaId()));
		vo.setGrantCallResearchAreas(grantCallModuleDao.fetchGrantCallResearchAreaBasedOnGrantCallId(vo.getGrantCallId()));
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteGrantCallEligibility(GrantCallVO vo) {
		grantCallModuleDao.deleteGrantCallEligibility(grantCallModuleDao.fetchGrantCallEligibilityById(vo.getGrantEligibilityId()));
		vo.setGrantCallEligibilities(grantCallModuleDao.fetchGrantCallEligibilityBasedOnGrantCallId(vo.getGrantCallId()));
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteGrantCallAttachment(GrantCallVO vo) {
		List<GrantCallAttachment> grantCallAttachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallIdAndDocumentId(vo.getGrantCallId(), vo.getDocumentId());
		if (grantCallAttachments != null && !grantCallAttachments.isEmpty()) {
			for (GrantCallAttachment grantCallAttachment : grantCallAttachments) {
				commonDao.deleteFileData(commonDao.getFileDataById(grantCallAttachment.getFileDataId()));
				grantCallModuleDao.deleteGrantCallAttachment(grantCallAttachment);
			}
		}
		vo.setGrantCallAttachments(grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(vo.getGrantCallId()));
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String addGrantCallAttachment(MultipartFile[] files, String formDataJSON) {
		GrantCallVO grantCallVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			grantCallVO = mapper.readValue(formDataJSON, GrantCallVO.class);
			GrantCall grantCall = grantCallVO.getGrantCall();
			List<GrantCallAttachment> attachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCall.getGrantCallId());
			Integer documentId = 0;
			if (attachments != null && !attachments.isEmpty()) {
				Collections.sort(attachments,
						(attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
								: attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
				documentId = attachments.get(0).getDocumentId();
			}
			List<GrantCallAttachment> newAttachments = grantCallVO.getNewAttachments();
			List<GrantCallAttachment> grantCallAttachments = new ArrayList<>();
			Integer versionNumber = 0;
			Boolean isReplaced = false;
			for (int i = 0; i < files.length; i++) {
				for (GrantCallAttachment newAttachment : newAttachments) {
					File file = new File(files[i].getOriginalFilename());
					String fileName = file.getName();
					String replaceFileName = newAttachment.getFileName();
					boolean isRenameRequired = false;
					int count = 1;
					isRenameRequired = checkForDuplication(newAttachment.getFileName(), attachments);
					while(isRenameRequired) {
						 replaceFileName = newAttachment.getFileName();
						 replaceFileName = generateFileName(replaceFileName, count);
						 count = count +1;
						 isRenameRequired = checkForDuplication(replaceFileName, attachments);
					}
					if (newAttachment.getAttachmentId() != null) {
						for (GrantCallAttachment attachment : attachments) {
							if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
								GrantCallAttachment grantCallAttachment = null;
								isReplaced = true;
								attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
								attachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
								versionNumber = attachment.getVersionNumber();
								documentId = attachment.getDocumentId();
								grantCallAttachment = addNewGrantCallAttachment(newAttachment, files[i], fileName, versionNumber, documentId, replaceFileName, grantCall.getGrantCallId());
								grantCallAttachment.setGrantCallId(grantCall.getGrantCallId());
								grantCallAttachments.add(grantCallAttachment);
							}
						}
					} else {
						if (newAttachment.getFileName().equals(fileName)) {
							documentId = documentId + 1;
							GrantCallAttachment grantCallAttachment = null;
							grantCallAttachment = addNewGrantCallAttachment(newAttachment, files[i], fileName, versionNumber, documentId, replaceFileName, grantCall.getGrantCallId());
							grantCallAttachments.add(grantCallAttachment);
						}
						i++;
					}
				}
			}
			if (Boolean.TRUE.equals(isReplaced)) {
				grantCall = grantCallDao.saveOrUpdateGrantCall(grantCall);
			}
			loadGrantCallUserFullNames(grantCall);
			grantCallVO.setGrantCallAttachments(getGrantCallAttachmentsByGrantCallId(grantCall.getGrantCallId()));
		} catch (Exception e) {
			logger.error("Exception while saving addGrantCallAttachment {}", e.getMessage());
		}
		return committeeDao.convertObjectToJSON(grantCallVO);
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
	}

	private boolean checkForDuplication(String fileName, List<GrantCallAttachment> attachments) {
		for(GrantCallAttachment attachment : attachments) {
			if(fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	public GrantCallAttachment addNewGrantCallAttachment(GrantCallAttachment attachment, MultipartFile file, String fileName, Integer versionNumber, Integer documentId, String replacedFileName, Integer grantCallId) {
		GrantCallAttachment grantCallAttachment = new GrantCallAttachment();
		try {
			if (attachment.getFileName().equals(fileName)) {
				grantCallAttachment.setGrantCallAttachType(attachment.getGrantCallAttachType());
				grantCallAttachment.setGrantAttachmentTypeCode(attachment.getGrantAttachmentTypeCode());
				grantCallAttachment.setDescription(attachment.getDescription());
				grantCallAttachment.setUpdateTimeStamp(attachment.getUpdateTimeStamp());
				grantCallAttachment.setUpdateUser(attachment.getUpdateUser());
				if (attachment.getUpdateUser() != null) {
					grantCallAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
				}
				grantCallAttachment.setMimeType(file.getContentType());
				grantCallAttachment.setVersionNumber(versionNumber + 1);
				grantCallAttachment.setFileName(replacedFileName);
				grantCallAttachment.setMimeType(file.getContentType());
				FileData fileData = new FileData();
				fileData.setAttachment(file.getBytes());
				fileData = commonDao.saveFileData(fileData);
				grantCallAttachment.setFileDataId(fileData.getFileDataId());
				grantCallAttachment.setVersionNumber(versionNumber + 1);
				grantCallAttachment.setAttachment(file.getBytes());
				grantCallAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
				grantCallAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
				grantCallAttachment.setDocumentId(documentId);
				grantCallAttachment.setGrantCallId(grantCallId);
				grantCallModuleDao.saveOrUpdateGrantCallAttachment(grantCallAttachment);
			}
		} catch (Exception e) {
			logger.error(e);
		}
		return grantCallAttachment;
	}

	@Override
	public ResponseEntity<byte[]> downloadGrantCallAttachment(Integer attachmentId) {
		GrantCallAttachment attachment = grantCallDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error(e);
		}
		return attachmentData;
	}

	@Override
	public String copyGrantCall(GrantCallVO vo) {
		GrantCall originalGrantCall = null;
		if (vo.getGrantCallId() != null) {
			originalGrantCall = grantCallDao.fetchGrantCallById(vo.getGrantCallId());
		}
		GrantCall copyGrantCall = new GrantCall();
		copyGrantCallMandatoryFields(copyGrantCall, originalGrantCall);
		copyGrantCallNonMandatoryFields(vo, copyGrantCall, originalGrantCall, vo.getUpdateUser());
		copyGrantCall = grantCallDao.saveOrUpdateGrantCall(copyGrantCall);
		saveGrantCallActionLogDetails(copyGrantCall.getGrantCallId(), Constants.GRANT_CALL_CREATED, vo.getUpdateUser());
		loadGrantCallUserFullNames(copyGrantCall);
		if (copyGrantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_OPEN) || copyGrantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE)) {
			vo.setCanCreateIOI(true);
		}
		copyGrantCall.setClosingTime(setGrantCallClosingDate(copyGrantCall.getClosingDate()));
		vo.setGrantCall(copyGrantCall);
		vo.setGrantCallId(copyGrantCall.getGrantCallId());
		vo.setStatus(true);
		vo.setMessage("Grant call copied successfully");
		return commonDao.convertObjectToJSON(vo);
	}

	private GrantCall copyGrantCallMandatoryFields(GrantCall copyGrantCall, GrantCall originalGrantCall) {
		try {
			copyGrantCall.setGrantCallName(originalGrantCall.getGrantCallName());
			copyGrantCall.setGrantTypeCode(originalGrantCall.getGrantTypeCode());
			copyGrantCall.setOpeningDate(originalGrantCall.getOpeningDate());
			copyGrantCall.setClosingDate(originalGrantCall.getClosingDate());
			copyGrantCall.setDescription(originalGrantCall.getDescription());
			copyGrantCall.setGrantTheme(originalGrantCall.getGrantTheme());
			copyGrantCall.setMaximumBudget(originalGrantCall.getMaximumBudget());
			copyGrantCall.setFundingSchemeId(originalGrantCall.getFundingSchemeId());
			copyGrantCall.setSponsorFundingScheme(originalGrantCall.getSponsorFundingScheme());
			if (originalGrantCall.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_EXTERNAL)) {
				copyGrantCall.setExternalUrl(originalGrantCall.getExternalUrl());
			}
			copyGrantCall.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_DRAFT);
			copyGrantCall.setGrantCallStatus(grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_DRAFT));
			copyGrantCall.setHomeUnitName(originalGrantCall.getHomeUnitName());		
			copyGrantCall.setHomeUnitNumber(originalGrantCall.getHomeUnitNumber());		
			copyGrantCall.setAbbrevation(originalGrantCall.getAbbrevation());
			return grantCallDao.saveOrUpdateGrantCall(copyGrantCall);
		} catch (Exception e) {
			logger.error("Exception in copyGrantCallMandatoryFields {}", e.getMessage());
			return copyGrantCall;
		}
	}

	private void copyGrantCallNonMandatoryFields(GrantCallVO vo, GrantCall copyGrantCall, GrantCall originalGrantCall, String updateUser) {
		Integer grantCallId = originalGrantCall.getGrantCallId();
		copyGrantCall.setQuantum(originalGrantCall.getQuantum());
		copyGrantCall.setApplicationProcedure(originalGrantCall.getApplicationProcedure());
		copyGrantCall.setOtherInformation(originalGrantCall.getOtherInformation());
		copyGrantCall.setGrantCallType(originalGrantCall.getGrantCallType());
		copyGrantCall.setGrantCallType(originalGrantCall.getGrantCallType());
		copyGrantCall.setSponsorCode(originalGrantCall.getSponsorCode());
		copyGrantCall.setSponsor(originalGrantCall.getSponsor());
		copyGrantCall.setInternalSubmissionDeadLineDate(originalGrantCall.getInternalSubmissionDeadLineDate());
		copyGrantCall.setSponsorTypeCode(originalGrantCall.getSponsorTypeCode());
		copyGrantCall.setSponsorType(originalGrantCall.getSponsorType());
		copyGrantCall.setFundingSchemeId(originalGrantCall.getFundingSchemeId());
		copyGrantCall.setSponsorFundingScheme(originalGrantCall.getSponsorFundingScheme());
		copyGrantCall.setOtherInformation(originalGrantCall.getOtherInformation());
		copyGrantCall.setCreateTimestamp(committeeDao.getCurrentTimestamp());
		copyGrantCall.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
		copyGrantCall.setCreateUser(updateUser);
		copyGrantCall.setUpdateUser(updateUser);
		copyGrantCall.setOverHeadComment(originalGrantCall.getOverHeadComment());
		copyGrantCall.setRateTypeCode(originalGrantCall.getRateTypeCode());
		copyGrantCall.setRateClassCode(originalGrantCall.getRateClassCode());
		copyGrantCall.setExternalUrl(originalGrantCall.getExternalUrl());
		if (originalGrantCall.getPrimeSponsorCode() != null) {
			copyGrantCall.setPrimeSponsorCode(originalGrantCall.getPrimeSponsorCode());
			copyGrantCall.setPrimeSponsor(commonDao.getSponsorById(originalGrantCall.getPrimeSponsorCode()));
		}
		List<GrantCallContact> grantCallContacts = grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(grantCallId);
		if (grantCallContacts != null && !grantCallContacts.isEmpty()) {
			vo.setGrantCallContacts(copyGrantCallContacts(copyGrantCall, grantCallContacts, updateUser));
		}
		List<GrantCallResearchArea> grantCallResearchAreas = grantCallModuleDao.fetchGrantCallResearchAreaBasedOnGrantCallId(grantCallId);
		if (grantCallResearchAreas != null && !grantCallResearchAreas.isEmpty()) {
			vo.setGrantCallResearchAreas(copyGrantCallResearchAreas(copyGrantCall, updateUser, grantCallResearchAreas));
		}
		List<GrantCallAttachment> grantCallAttachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCallId);
		if (grantCallAttachments != null && !grantCallAttachments.isEmpty()) {
			vo.setGrantCallAttachments(copyGrantCallAttachments(copyGrantCall, updateUser, grantCallAttachments));
		}
		List<GrantCallEligibility> grantCallEligibilities = grantCallModuleDao.fetchGrantCallEligibilityBasedOnGrantCallId(grantCallId);
		if (grantCallEligibilities != null && !grantCallEligibilities.isEmpty()) {
			vo.setGrantCallEligibilities(copyGrantCallEligibility(copyGrantCall, updateUser, grantCallEligibilities));
		}
		List<GrantCallEligibleDepartment> grantCallEligibleDepartments = grantCallModuleDao.fetchGrantCallEligibleDepartmentBasedOnGrantCallId(grantCallId);
		if (grantCallEligibleDepartments != null && !grantCallEligibleDepartments.isEmpty()) {
			vo.setGrantEligibleDepartments(copyGrantCallEligibleDepartments(copyGrantCall, updateUser, grantCallEligibleDepartments));
		}
		if (originalGrantCall.getGrantCallKeywords() != null && !originalGrantCall.getGrantCallKeywords().isEmpty()) {
			copyGrantCall.setGrantCallKeywords(copyGrantCallKeywords(copyGrantCall, originalGrantCall, updateUser));
		}
		if (originalGrantCall.getGrantCallRelevants() != null && !originalGrantCall.getGrantCallRelevants().isEmpty()) {
			copyGrantCall.setGrantCallRelevants(copyGrantCallRelevants(copyGrantCall, originalGrantCall, updateUser));
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {		
			List<GrantCallKPI> grantCallKPIs = grantCallKPIDao.fetchKPIByGrantCallId(originalGrantCall.getGrantCallId());		
			if (grantCallKPIs != null && !grantCallKPIs.isEmpty()) {		
				vo.setGrantCallKPIs(copyGrantCallKPI(copyGrantCall.getGrantCallId(), grantCallKPIs, updateUser));		
			}		
		}		
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SCORING_CRITERIA)) {		
			List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(originalGrantCall.getGrantCallId());		
			if (grantCallScoringCriterias != null && !grantCallScoringCriterias.isEmpty()) {		
				vo.setGrantCallScoringCriterias(copyGrantCallScoringCriteria(copyGrantCall.getGrantCallId(), grantCallScoringCriterias, updateUser));		
			}		
		}				
		GrantCallIOIQuestionnaire grantQuestionnaire = grantCallDao.fetchGrantCallIOIQuestionnaireByGrantId(originalGrantCall.getGrantCallId());		
		if (grantQuestionnaire != null) {		
			copyGrantCall.setGrantCallIOIQuestionnaire(copygrantCallIOIQuestionnaire(copyGrantCall, originalGrantCall, updateUser));		
		}
		vo.setGrantCallEvaluationPanels(copyGrantCallEvaluationPanels(copyGrantCall.getGrantCallId(), updateUser, originalGrantCall.getGrantCallId()));
	}

	private List<GrantCallEvaluationPanel> copyGrantCallEvaluationPanels(Integer copyGrantCallId, String updateUser, Integer grantCallId) {
		List<GrantCallEvaluationPanel> grantCallEvaluationPanels = grantCallEvaluationPanelDao.fetchEvaluationPanelByGrantCallId(grantCallId);
		List<GrantCallEvaluationPanel> newGrantCallEvaluationPanel = new ArrayList<>();
		if (grantCallEvaluationPanels != null && !grantCallEvaluationPanels.isEmpty()) {
			List<GrantCallEvaluationPanel> copiedGrantCallEvaluationPanel = new ArrayList<>(grantCallEvaluationPanels);
			Collections.copy(copiedGrantCallEvaluationPanel, grantCallEvaluationPanels);
			for (GrantCallEvaluationPanel copiedEvaluationPanelDetails : copiedGrantCallEvaluationPanel) {
				GrantCallEvaluationPanel evaluationPanelDetails = new GrantCallEvaluationPanel();
				evaluationPanelDetails.setGrantCallId(copyGrantCallId);
				evaluationPanelDetails.setIsMainPanel(copiedEvaluationPanelDetails.getIsMainPanel());
				evaluationPanelDetails.setMapId(copiedEvaluationPanelDetails.getMapId());
				evaluationPanelDetails.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
				evaluationPanelDetails.setUpdateUser(updateUser);
				newGrantCallEvaluationPanel.add(grantCallEvaluationPanelDao.saveOrUpdateGrantCallEvaluationPanel(evaluationPanelDetails));
			}
		}
		return newGrantCallEvaluationPanel;
	}

	private List<GrantCallRelevant> copyGrantCallRelevants(GrantCall copyGrantCall, GrantCall grantCall, String updateUser) {
		List<GrantCallRelevant> grantCallRelevants = grantCall.getGrantCallRelevants();
		List<GrantCallRelevant> copiedGrantCallRelevants = new ArrayList<>(grantCallRelevants);
		Collections.copy(copiedGrantCallRelevants, grantCallRelevants);
		List<GrantCallRelevant> newGrantCallRelevants = new ArrayList<>();
		for (GrantCallRelevant copiedGrantCallRelevant : copiedGrantCallRelevants) {
			GrantCallRelevant relevantDetail = new GrantCallRelevant();
			relevantDetail.setGrantCall(copyGrantCall);
			relevantDetail.setRelevantFieldCode(copiedGrantCallRelevant.getRelevantFieldCode());
			relevantDetail.setRelevantField(copiedGrantCallRelevant.getRelevantField());
			relevantDetail.setUpdateUser(updateUser);
			relevantDetail.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			newGrantCallRelevants.add(relevantDetail);
		}
		return newGrantCallRelevants;
	}

	private List<GrantCallKeyword> copyGrantCallKeywords(GrantCall copyGrantCall, GrantCall grantCall, String updateUser) {
		List<GrantCallKeyword> grantCallKeywords = grantCall.getGrantCallKeywords();
		List<GrantCallKeyword> copiedGrantCallKeywords = new ArrayList<>(grantCallKeywords);
		Collections.copy(copiedGrantCallKeywords, grantCallKeywords);
		List<GrantCallKeyword> newKeywords = new ArrayList<>();
		for (GrantCallKeyword copiedKeywordDetail : copiedGrantCallKeywords) {
			GrantCallKeyword keywordtDetail = new GrantCallKeyword();
			keywordtDetail.setGrantCall(copyGrantCall);
			keywordtDetail.setScienceKeywordCode(copiedKeywordDetail.getScienceKeywordCode());
			keywordtDetail.setScienceKeyword(copiedKeywordDetail.getScienceKeyword());
			keywordtDetail.setUpdateUser(updateUser);
			keywordtDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			keywordtDetail.setKeyword(copiedKeywordDetail.getKeyword());
			newKeywords.add(keywordtDetail);
		}
		return newKeywords;
	}

	private List<GrantCallEligibleDepartment> copyGrantCallEligibleDepartments(GrantCall copyGrantCall, String updateUser, List<GrantCallEligibleDepartment> grantCallEligibleDepartment) {
		List<GrantCallEligibleDepartment> copiedgrantCallEligibleDepartment = new ArrayList<>(grantCallEligibleDepartment);
		Collections.copy(copiedgrantCallEligibleDepartment, grantCallEligibleDepartment);
		List<GrantCallEligibleDepartment> newGrantCallEligibleDepartment = new ArrayList<>();
		for (GrantCallEligibleDepartment copiedDepartmentDetails : copiedgrantCallEligibleDepartment) {
			GrantCallEligibleDepartment eligibleDepartmentDetails = new GrantCallEligibleDepartment();
			eligibleDepartmentDetails.setGrantCallId(copyGrantCall.getGrantCallId());
			eligibleDepartmentDetails.setUnitName(copiedDepartmentDetails.getUnitName());
			eligibleDepartmentDetails.setUnitNumber(copiedDepartmentDetails.getUnitNumber());
			eligibleDepartmentDetails.setUpdateUser(updateUser);
			eligibleDepartmentDetails.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			newGrantCallEligibleDepartment.add(eligibleDepartmentDetails);
		}
		return newGrantCallEligibleDepartment;
	}

	private List<GrantCallContact> copyGrantCallContacts(GrantCall copyGrantCall, List<GrantCallContact> grantCallContacts, String updateUser) {
		List<GrantCallContact> copiedGrantCallContacts = new ArrayList<>(grantCallContacts);
		Collections.copy(copiedGrantCallContacts, grantCallContacts);
		List<GrantCallContact> newGrantCallContacts = new ArrayList<>();
		for (GrantCallContact copiedKeywordDetail : copiedGrantCallContacts) {
			GrantCallContact grantCallContactDetail = new GrantCallContact();
			grantCallContactDetail.setGrantCallId(copyGrantCall.getGrantCallId());
			grantCallContactDetail.setPersonId(copiedKeywordDetail.getPersonId());
			grantCallContactDetail.setFullName(copiedKeywordDetail.getFullName());
			grantCallContactDetail.setDesignation(copiedKeywordDetail.getDesignation());
			grantCallContactDetail.setEmail(copiedKeywordDetail.getEmail());
			grantCallContactDetail.setMobile(copiedKeywordDetail.getMobile());
			grantCallContactDetail.setIsEmployee(copiedKeywordDetail.getIsEmployee());
			grantCallContactDetail.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			grantCallContactDetail.setUpdateUser(updateUser);
			newGrantCallContacts.add(grantCallModuleDao.saveOrUpdateGrantCallContact(grantCallContactDetail));
		}
		return newGrantCallContacts;
	}

	private List<GrantCallResearchArea> copyGrantCallResearchAreas(GrantCall copyGrantCall, String updateUser, List<GrantCallResearchArea> grantCallResearchAreas) {
		List<GrantCallResearchArea> copiedGrantCallResearchAreas = new ArrayList<>(grantCallResearchAreas);
		Collections.copy(copiedGrantCallResearchAreas, grantCallResearchAreas);
		List<GrantCallResearchArea> newGrantCallResearchAreas = new ArrayList<>();
		for (GrantCallResearchArea copiedGrantCallResearchAreaDetail : copiedGrantCallResearchAreas) {
			GrantCallResearchArea grantCallResearchAreaDetail = new GrantCallResearchArea();
			grantCallResearchAreaDetail.setGrantCallId(copyGrantCall.getGrantCallId());
			grantCallResearchAreaDetail.setResearchTypeCode(copiedGrantCallResearchAreaDetail.getResearchTypeCode());
			grantCallResearchAreaDetail.setResearchType(copiedGrantCallResearchAreaDetail.getResearchType());
			grantCallResearchAreaDetail.setResearchTypeAreaCode(copiedGrantCallResearchAreaDetail.getResearchTypeAreaCode());
			grantCallResearchAreaDetail.setResearchTypeArea(copiedGrantCallResearchAreaDetail.getResearchTypeArea());
			grantCallResearchAreaDetail.setResearchTypeSubAreaCode(copiedGrantCallResearchAreaDetail.getResearchTypeSubAreaCode());
			grantCallResearchAreaDetail.setResearchTypeSubArea(copiedGrantCallResearchAreaDetail.getResearchTypeSubArea());
			grantCallResearchAreaDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			grantCallResearchAreaDetail.setUpdateUser(updateUser);
			newGrantCallResearchAreas.add(grantCallModuleDao.saveOrUpdateGrantCallResearchArea(grantCallResearchAreaDetail));
		}
		return newGrantCallResearchAreas;
	}

	private List<GrantCallAttachment> copyGrantCallAttachments(GrantCall copyGrantCall, String updateUser, List<GrantCallAttachment> grantCallAttachments) {
		List<GrantCallAttachment> copyGrantCallAttachments = new ArrayList<>(grantCallAttachments);
		Collections.copy(copyGrantCallAttachments, grantCallAttachments);
		List<GrantCallAttachment> newGrantcallAttachment = new ArrayList<>();
		for (GrantCallAttachment copiedGrantCallDetails : copyGrantCallAttachments) {
			GrantCallAttachment attachmentDetails = new GrantCallAttachment();
			attachmentDetails.setGrantCallId(copyGrantCall.getGrantCallId());
			attachmentDetails.setDescription(copiedGrantCallDetails.getDescription());
			attachmentDetails.setFileName(copiedGrantCallDetails.getFileName());
			attachmentDetails.setGrantAttachmentTypeCode(copiedGrantCallDetails.getGrantAttachmentTypeCode());
			attachmentDetails.setGrantCallAttachType(copiedGrantCallDetails.getGrantCallAttachType());
			attachmentDetails.setMimeType(copiedGrantCallDetails.getMimeType());
			attachmentDetails.setUpdateUser(updateUser);
			if (updateUser != null) {
				attachmentDetails.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(updateUser));
			}
			attachmentDetails.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			attachmentDetails.setVersionNumber(copiedGrantCallDetails.getVersionNumber());
			attachmentDetails.setDocumentStatusCode(copiedGrantCallDetails.getDocumentStatusCode());
			attachmentDetails.setDocumentId(copiedGrantCallDetails.getDocumentId());
			FileData fileData = commonDao.getFileDataById(copiedGrantCallDetails.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			fileData.getFileDataId();
			attachmentDetails.setFileDataId(file.getFileDataId());
			newGrantcallAttachment.add(grantCallModuleDao.saveOrUpdateGrantCallAttachment(attachmentDetails));
		}
		return newGrantcallAttachment;
	}

	private List<GrantCallEligibility> copyGrantCallEligibility(GrantCall copyGrantCall, String updateUser, List<GrantCallEligibility> grantCallEligibilities) {
		List<GrantCallEligibility> copyGrantCallEligibility = new ArrayList<>(grantCallEligibilities);
		Collections.copy(copyGrantCallEligibility, grantCallEligibilities);
		List<GrantCallEligibility> newGrantCallEligibility = new ArrayList<>();
		for (GrantCallEligibility copiedGrantCallEligibilityDetails : copyGrantCallEligibility) {
			GrantCallEligibility eligibilityDetails = new GrantCallEligibility();
			eligibilityDetails.setGrantCallId(copyGrantCall.getGrantCallId());
			eligibilityDetails.setProposalPersonRole(copiedGrantCallEligibilityDetails.getProposalPersonRole());
			eligibilityDetails.setGrantCallEligibilityType(copiedGrantCallEligibilityDetails.getGrantCallEligibilityType());
			eligibilityDetails.setPersonId(copiedGrantCallEligibilityDetails.getPersonId());
			eligibilityDetails.setGrantEligibilityTypeCode(copiedGrantCallEligibilityDetails.getGrantEligibilityTypeCode());
			eligibilityDetails.setUpdateUser(updateUser);
			eligibilityDetails.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			GrantEligibilityTarget grantEligibilityTarget = copiedGrantCallEligibilityDetails.getGrantEligibilityTarget();		
			if (grantEligibilityTarget != null) {		
				GrantEligibilityTarget eligibilityTarget = new GrantEligibilityTarget();		
				eligibilityTarget.setGrantCallEligibility(eligibilityDetails);		
				eligibilityTarget.setEligibilityTargetTypeCode(grantEligibilityTarget.getEligibilityTargetTypeCode());		
				eligibilityTarget.setTargetCategoryTypeCode(grantEligibilityTarget.getTargetCategoryTypeCode());		
				eligibilityTarget.setTargetValue(grantEligibilityTarget.getTargetValue());		
				eligibilityTarget.setUpdateTimestamp(committeeDao.getCurrentTimestamp());		
				eligibilityTarget.setUpdateUser(updateUser);		
				eligibilityDetails.setGrantEligibilityTarget(eligibilityTarget);		
			}
			newGrantCallEligibility.add(grantCallModuleDao.saveOrUpdateGrantCallEligibility(eligibilityDetails));
		}
		return newGrantCallEligibility;
	}

	@Override
	public String deleteGrantEligibleDepartments(GrantCallVO vo) {
		grantCallModuleDao.deleteGrantCallEligibleDepartment(grantCallModuleDao.fetchGrantCallEligibleDepartmentById(vo.getGrantEligibilityDepartmentId()));
		vo.setGrantEligibleDepartments(grantCallModuleDao.fetchGrantCallEligibleDepartmentBasedOnGrantCallId(vo.getGrantCallId()));
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteGrantCall(GrantCallVO vo) {
		Integer grantCallId = vo.getGrantCallId();
		deleteAllGrantCallKPI(grantCallId);
		deleteAllGrantCallScoringCriteria(grantCallId);
		deleteAllGrantCallEvaluationPanel(grantCallId);
		deleteAllGrantCallAttachments(grantCallId);
		deleteAllGrantCallContacts(grantCallId);
		deleteAllGrantCallResearchAreas(grantCallId);
		deleteAllGrantCallEligibilities(grantCallId);
		deleteAllGrantCallEligibleDepartment(grantCallId);
		deleteAllProposalEvaluationScores(grantCallId);
		deleteAllGrantCallIOIHeader(grantCallId);
		deleteAllGrantCallActionLog(grantCallId);
		vo.setMessage(grantCallDao.deleteGrantCall(grantCallId));
		return committeeDao.convertObjectToJSON(vo);
	}

	private void deleteAllGrantCallIOIHeader(Integer grantCallId) {
		List<GrantCallIOIHeader> grantCallIOIHeaders = grantCallModuleDao.fetchGrantCallIOIHeaderBasedOnGrantCallId(grantCallId);
		if (grantCallIOIHeaders != null && !grantCallIOIHeaders.isEmpty()) {
			for (GrantCallIOIHeader grantCallIOIHeader : grantCallIOIHeaders) {
				grantCallModuleDao.deleteGrantCallIOIHeader(grantCallIOIHeader);
			}
		}
	}

	private void deleteAllProposalEvaluationScores(Integer grantCallId) {
		List<ProposalEvaluationScore> proposalEvaluationScores = grantCallModuleDao.fetchProposalEvaluationScoreBasedOnGrantCallId(grantCallId);
		if (proposalEvaluationScores != null && !proposalEvaluationScores.isEmpty()) {
			for (ProposalEvaluationScore proposalEvaluationScore : proposalEvaluationScores) {
				grantCallModuleDao.deleteProposalEvaluationScore(proposalEvaluationScore);
			}
		}
	}

	private void deleteAllGrantCallEligibleDepartment(Integer grantCallId) {
		List<GrantCallEligibleDepartment> grantCallEligibleDepartments = grantCallModuleDao.fetchGrantCallEligibleDepartmentBasedOnGrantCallId(grantCallId);
		if (grantCallEligibleDepartments != null && !grantCallEligibleDepartments.isEmpty()) {
			for (GrantCallEligibleDepartment grantCallEligibleDepartment : grantCallEligibleDepartments) {
				grantCallModuleDao.deleteGrantCallEligibleDepartment(grantCallEligibleDepartment);
			}
		}
	}

	private void deleteAllGrantCallEligibilities(Integer grantCallId) {
		List<GrantCallEligibility> grantCallEligibilities = grantCallModuleDao.fetchGrantCallEligibilityBasedOnGrantCallId(grantCallId);
		if (grantCallEligibilities != null && !grantCallEligibilities.isEmpty()) {
			for (GrantCallEligibility grantCallEligibility : grantCallEligibilities) {
				grantCallModuleDao.deleteGrantCallEligibility(grantCallEligibility);
			}
		}
	}

	private void deleteAllGrantCallResearchAreas(Integer grantCallId) {
		List<GrantCallResearchArea> grantCallResearchAreas = grantCallModuleDao.fetchGrantCallResearchAreaBasedOnGrantCallId(grantCallId);
		if (grantCallResearchAreas != null && !grantCallResearchAreas.isEmpty()) {
			for (GrantCallResearchArea grantCallResearchArea : grantCallResearchAreas) {
				grantCallModuleDao.deleteGrantCallResearchArea(grantCallResearchArea);
			}
		}
	}

	private void deleteAllGrantCallContacts(Integer grantCallId) {
		List<GrantCallContact> grantCallContacts = grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(grantCallId);
		if (grantCallContacts != null && !grantCallContacts.isEmpty()) {
			for (GrantCallContact grantCallContact : grantCallContacts) {
				grantCallModuleDao.deleteGrantCallContact(grantCallContact);
			}
		}
	}

	private void deleteAllGrantCallAttachments(Integer grantCallId) {
		List<GrantCallAttachment> grantCallAttachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCallId);
		if (grantCallAttachments != null && !grantCallAttachments.isEmpty()) {
			for (GrantCallAttachment grantCallAttachment : grantCallAttachments) {
				grantCallModuleDao.deleteGrantCallAttachment(grantCallAttachment);
			}
		}
	}

	@Override
	public List<Sponsor> fetchSponsorsBySponsorType(String searchString, String sponsorTypeCode) {
		return grantCallDao.fetchSponsorsBySponsorType(searchString, sponsorTypeCode);
	}

	@Override
	public String archiveGrantCall(GrantCallVO vo) {
		GrantCall grantCall = grantCallDao.fetchGrantCallById(vo.getGrantCallId());
		Integer grantStatus = grantCall.getGrantStatusCode();
		if (grantStatus.equals((Constants.GRANT_CALL_STATUS_CODE_CLOSED))) {
			grantCall.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_ARCHIVED);
			grantCall.setGrantCallStatus(grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_ARCHIVED));
		} else if (grantStatus.equals((Constants.GRANT_CALL_STATUS_CODE_ARCHIVED))) {
			grantCall.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_CLOSED);
			grantCall.setGrantCallStatus(grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_CLOSED));
		}
		grantCall.setUpdateUser(vo.getUpdateUser());
		grantCall.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
		grantCall = grantCallDao.saveOrUpdateGrantCall(grantCall);
		loadGrantCallUserFullNames(grantCall);
		if (grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_OPEN) || grantCall.getGrantStatusCode().equals(Constants.GRANT_CALL_STATUS_CODE_TENTATIVE)) {
			vo.setCanCreateIOI(true);
		}
		vo.setGrantCall(grantCall);
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String grantInvitation(EmailServiceVO emailServiceVO) {
		try {
			int limit = 0;
			int mailGroupRecipiantLimit = emailServiceVO.getRecipients().size() / 50;
			mailGroupRecipiantLimit = (int) Math.ceil(mailGroupRecipiantLimit / 100.0);
			while (limit <= mailGroupRecipiantLimit) {
				emailServiceVO.setModuleCode(Constants.GRANTCALL_MODULE_CODE);
				emailServiceVO.setSubModuleCode(Constants.GRANTCALL_SUBMODULE_CODE.toString());
				emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				emailServiceVO = emailService.sendEmail(emailServiceVO);
				limit++;
			}
		} catch (Exception e) {
			logger.error("Exception in grantInvitation : {}", e.getMessage());
		}
		return committeeDao.convertObjectToJSON("success");
	}

	@Override
	public ResponseEntity<byte[]> downloadFundingSchemeAttachment(Integer attachmentId) {
		FundingSchemeAttachment attachment = grantCallDao.fetchFundingSchemeAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			byte[] data = fileData.getAttachment();
			HttpHeaders headers = new HttpHeaders();
			if (attachment.getMimeType() != null && !attachment.getMimeType().isEmpty()) {
				headers.setContentType(MediaType.parseMediaType(attachment.getMimeType()));
			}
			String filename = attachment.getFileName();
			headers.setContentDispositionFormData(filename, filename);
			headers.setContentLength(data.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<byte[]>(data, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Exception in downloadFundingSchemeAttachment : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String addGrantCallKeyword(GrantCallVO vo) {
		ScienceKeyword scienceKeyword = new ScienceKeyword();
		scienceKeyword.setCode(proposalDao.getMaxScienceKeyword());
		scienceKeyword.setDescription(vo.getScienceKeyword());
		scienceKeyword.setUpdateUser(vo.getUserFullName());
		scienceKeyword.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
		return committeeDao.convertObjectToJSON(proposalDao.saveOrUpdateScienceKeyword(scienceKeyword));	
	}

	@Override
	public String deleteGrantCallRelevantField(GrantCallVO vo) {
		grantCallModuleDao.deleteGrantCallRelevant(grantCallModuleDao.fetchGrantCallRelevantById(vo.getGrantCallRelevantId()));
		vo.setGrantEligibleDepartments(grantCallModuleDao.fetchGrantCallEligibleDepartmentBasedOnGrantCallId(vo.getGrantCallId()));
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void loadGrantCallUserFullNames(GrantCall grantCall) {
		if (grantCall.getCreateUser() != null) {
			grantCall.setCreateUserFullName(personDao.getUserFullNameByUserName(grantCall.getCreateUser()));
		}
		if (grantCall.getUpdateUser() != null) {
			grantCall.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(grantCall.getUpdateUser()));
		}
		if (grantCall.getSponsor() != null) {
			grantCall.setSponsorName(commonService.getSponsorFormatBySponsorDetail(grantCall.getSponsor().getSponsorCode(), grantCall.getSponsor().getSponsorName(), grantCall.getSponsor().getAcronym()));
		}
		if (grantCall.getPrimeSponsor() != null) {
			grantCall.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(grantCall.getPrimeSponsor().getSponsorCode(), grantCall.getPrimeSponsor().getSponsorName(), grantCall.getPrimeSponsor().getAcronym()));
		}
	}

	@Override
	public GrantCallVO sendGrantCallNotification(GrantCallVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.GRANTCALL_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.getGrantCall().getGrantCallId().toString());
		emailServiceVO.setPlaceHolder(getGrantCallPlaceholders(vo));
		emailServiceVO.setSubModuleCode(Constants.GRANTCALL_SUBMODULE_CODE.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
		return vo;
	}

	private Map<String, String> getGrantCallPlaceholders(GrantCallVO vo) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		placeHolder.put("{USER_NAME}", vo.getUserFullName());
		return placeHolder;
	}

	@Override
	public String saveOrUpdateGrantCallIOIQuestionnaire(GrantCallVO vo) {
		GrantCallIOIQuestionnaire grantQuestionnaire = vo.getGrantQuestionnaire();
		grantQuestionnaire.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		grantQuestionnaire = grantCallDao.saveOrUpdateGrantCallIOIQuestionnaire(grantQuestionnaire);
		return commonDao.convertObjectToJSON(grantQuestionnaire);
	}

	@Override
	public String deleteGrantIOIQuestionnaire(GrantCallVO vo) {
		grantCallDao.deleteGrantIOIQuestionnaire(vo.getGrantIOIQuestionnaireId());
		vo.setMessage("Grant Questionnaire deleted");
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchGrantCallIOIQuestionnaireByGrantId(GrantCallVO vo) {
		GrantCallIOIQuestionnaire grantQuestionnaire = grantCallDao.fetchGrantCallIOIQuestionnaireByGrantId(vo.getGrantCallId());
		if (grantQuestionnaire != null) {
			vo.setGrantQuestionnaire(grantQuestionnaire);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void setGrantCallUpdateUser(GrantCallVO grantCallVO) {
		GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallVO.getGrantCallId());
		grantCall.setUpdateUser(grantCallVO.getUpdateUser());
		grantCall.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		grantCallDao.saveOrUpdateGrantCall(grantCall);
	}

	private void deleteAllGrantCallKPI(Integer grantCallId) {
		List<GrantCallKPI> grantCallKPIs = grantCallKPIDao.fetchKPIByGrantCallId(grantCallId);
		if (grantCallKPIs != null && !grantCallKPIs.isEmpty()) {
			for (GrantCallKPI grantCallKPI : grantCallKPIs) {
				grantCallDao.deleteGrantCallKPI(grantCallKPI);
			}
		}
	}

	private void deleteAllGrantCallScoringCriteria(Integer grantCallId) {
		List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(grantCallId);
		if (grantCallScoringCriterias != null && !grantCallScoringCriterias.isEmpty()) {
			for (GrantCallScoringCriteria grantCallScoringCriteria : grantCallScoringCriterias) {
				grantCallDao.deleteGrantCallScoringCriteria(grantCallScoringCriteria);
			}
		}
	}

	@Override
	public String saveOrUpdateProposalEvalautionScore(EvaluationMainPanelVO vo) {
		List<Proposal> submittedProposals = vo.getSubmittedProposals();
		List<Proposal> submittedProposalDatas = proposalDao.fetchSubmittedProposalList(vo.getGrantCallId());
		for (Proposal submittedProposal : submittedProposalDatas) {
			for (Proposal proposal : submittedProposals) {
				if (proposal.getProposalId() != null && proposal.getProposalId().equals(submittedProposal.getProposalId())) {
					ProposalVO proposalVO = new ProposalVO();
					Proposal proposalDetails = proposalDao.fetchProposalById(submittedProposal.getProposalId());
					Proposal updatedProposal = proposalDao.fetchProposalById(proposal.getProposalId());
					proposalVO.setProposal(proposalDetails);
					proposalVO.setProposalId(submittedProposal.getProposalId());
					proposalVO.setUpdateUser(vo.getUpdateUser());
					setProposalDetails(proposalVO);
					ProposalEvaluationScore proposalEvaluationScore = proposal.getProposalEvaluationScore();
					submittedProposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					submittedProposal.setUpdateUser(vo.getUpdateUser());
					if (vo.getStatusType().equalsIgnoreCase("U")) {
						submittedProposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL);
						submittedProposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL));

						// Send notification for Research Administrators
						sendNotificationForResearchAdministrators(proposalVO, vo.getStatusType());

					} else if (vo.getStatusType().equalsIgnoreCase("A")) {
						submittedProposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
						submittedProposal.setProposalStatus(proposalLookUpDao
								.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
						if (submittedProposal.getIpNumber() == null || (submittedProposal.getIpNumber() != null && submittedProposal.getIpNumber().equals(""))) {
							proposalVO.setIpGenerationOnly(Boolean.TRUE);
							Proposal ipProposal = businessRuleService.generateInstitutionalProposal(proposalVO);
							submittedProposal.setIpNumber(ipProposal.getIpNumber());
							submittedProposal.setStatusCode(ipProposal.getStatusCode());
							submittedProposal.setProposalStatus(proposalLookUpDao
								.fetchProposalStatusByStatusCode(ipProposal.getStatusCode()));
						}

						// Send notification for PI
						proposalService.sendProposalNotification(proposalVO, Constants.PROPOSAL_AWARDED_NOTIFICATION_CODE, new HashSet<>());

						// Send notification for Research Administrators
						sendNotificationForResearchAdministrators(proposalVO, vo.getStatusType());
					} else if (vo.getStatusType().equalsIgnoreCase("S")) {
						submittedProposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_SHORTLISTED);
						submittedProposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_SHORTLISTED));
						// Send notification for PI
						proposalService.sendProposalNotification(proposalVO, Constants.SHORTLIST_PROPOSAL_NOTIFICATION_CODE, new HashSet<>());

						// Send notification for Research Administrators
						sendNotificationForResearchAdministrators(proposalVO, vo.getStatusType());
					}
					updatedProposal.setGrantCallId(vo.getGrantCallId());
					updatedProposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					updatedProposal.setUpdateUser(submittedProposal.getUpdateUser());
					updatedProposal.setStatusCode(submittedProposal.getStatusCode());
					updatedProposal.setProposalStatus(submittedProposal.getProposalStatus());
					proposalDao.saveOrUpdateProposal(updatedProposal);
					if (proposalEvaluationScore == null) {
						proposalEvaluationScore = new ProposalEvaluationScore();
					}
					if (vo.getStatusType().equalsIgnoreCase("A") || vo.getStatusType().equalsIgnoreCase("S")) {
						proposalEvaluationScore.setIsShortListed("Y");
					} else {
						proposalEvaluationScore.setIsShortListed("N");
					}
					proposalEvaluationScore.setGrantHeaderId(vo.getGrantCallId());
					proposalEvaluationScore.setProposalId(submittedProposal.getProposalId());
					proposalEvaluationScore.setUpdateUser(vo.getUpdateUser());
					proposalEvaluationScore.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					submittedProposal.setProposalEvaluationScore(grantCallDao.saveOrUpdateProposalEvalautionScore(proposalEvaluationScore));
					inboxDao.markReadMessage(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId().toString(), null, Constants.MESSAGE_TYPE_MAIN_PANEL_REVIEW, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				}
			}
		}
		vo.setSubmittedProposals(submittedProposalDatas);
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteAllGrantCallEvaluationPanel(Integer grantCallId) {
		List<GrantCallEvaluationPanel> grantCallEvaluationPanels = grantCallEvaluationPanelDao.fetchEvaluationPanelByGrantCallId(grantCallId);
		if (grantCallEvaluationPanels != null && !grantCallEvaluationPanels.isEmpty()) {
			for (GrantCallEvaluationPanel grantCallEvaluationPanel : grantCallEvaluationPanels) {
				grantCallDao.deleteGrantCallEvaluationPanel(grantCallEvaluationPanel);
			}
		}
	}

	@Override
	public String getProposalByGrantCallId(EvaluationMainPanelVO vo) {
		vo.setSubmittedProposals(proposalDao.fetchSubmittedProposalList(vo.getGrantCallId()));
		List<Integer> statusCodes = new ArrayList<>();
		statusCodes.add(38);
		statusCodes.add(40);
		statusCodes.add(29);		
		statusCodes.add(11);
		statusCodes.add(8);
		statusCodes.add(12);
		statusCodes.add(35);
		statusCodes.add(2);
		statusCodes.add(37);
		vo.setProposalStatuses(proposalLookUpDao.fetAllProposalStatus(statusCodes));
		return commonDao.convertObjectToJSON(vo);
	}

	private List<GrantCallKPI> copyGrantCallKPI(Integer copyGrantCallId, List<GrantCallKPI> grantCallKPIs, String updateUser) {
		List<GrantCallKPI> newKPIs = new ArrayList<>();
		for (GrantCallKPI copiedKPI : grantCallKPIs) {
			GrantCallKPI copygrantCallKPI = new GrantCallKPI();
			List<GrantCallKPICriteria> grantCallKPICriterias = new ArrayList<>();
			for (GrantCallKPICriteria grantCallKPICriteria : copiedKPI.getGrantCallKpiCriterias()) {
				GrantCallKPICriteria copyCriteria = new GrantCallKPICriteria();
				copyCriteria.setGrantCallKpi(copygrantCallKPI);
				copyCriteria.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
				copyCriteria.setUpdateUser(updateUser);
				copyCriteria.setKpiCriteriaTypeCode(grantCallKPICriteria.getKpiCriteriaTypeCode());
				copyCriteria.setKpiTypeCode(grantCallKPICriteria.getKpiTypeCode());
				grantCallKPICriterias.add(copyCriteria);
			}
			copygrantCallKPI.setGrantCallId(copyGrantCallId);
			copygrantCallKPI.setKpiTypeCode(copiedKPI.getKpiTypeCode());
			copygrantCallKPI.setGrantCallKpiCriterias(grantCallKPICriterias);
			copygrantCallKPI.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			copygrantCallKPI.setUpdateUser(copiedKPI.getUpdateUser());
			newKPIs.add(grantCallKPIDao.saveOrUpdateGrantCallKPI(copygrantCallKPI));
		}
		return newKPIs;
	}

	private List<GrantCallScoringCriteria> copyGrantCallScoringCriteria(Integer copyGrantCallId, List<GrantCallScoringCriteria> grantCallScoringCriterias, String updateUser) {
		List<GrantCallScoringCriteria> newScoringCriteria = new ArrayList<>();
		for (GrantCallScoringCriteria scoringCriteria : grantCallScoringCriterias) {
			GrantCallScoringCriteria copycriteria = new GrantCallScoringCriteria();
			copycriteria.setGrantCallId(copyGrantCallId);
			copycriteria.setScoringCriteriaTypeCode(scoringCriteria.getScoringCriteriaTypeCode());
			copycriteria.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			copycriteria.setUpdateUser(updateUser);
			newScoringCriteria.add(grantCallScoringDao.saveOrUpdateGrantCallScoringCriteria(copycriteria));
		}
		return newScoringCriteria;
	}

	private GrantCallIOIQuestionnaire copygrantCallIOIQuestionnaire(GrantCall copyGrantCall, GrantCall grantCall, String updateUser) {
		GrantCallIOIQuestionnaire grantCallIOIQuestionnaire = grantCall.getGrantCallIOIQuestionnaire();
		GrantCallIOIQuestionnaire copygrantCallIOIQuestionnaire = new GrantCallIOIQuestionnaire();
		copygrantCallIOIQuestionnaire.setQuestionnaireId(grantCallIOIQuestionnaire.getQuestionnaireId());
		copygrantCallIOIQuestionnaire.setUpdateUser(updateUser);
		copygrantCallIOIQuestionnaire.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copygrantCallIOIQuestionnaire.setGrantCallId(copyGrantCall.getGrantCallId());
		grantCallDao.saveOrUpdateGrantCallIOIQuestionnaire(copygrantCallIOIQuestionnaire);
		return copygrantCallIOIQuestionnaire;
	}

	public List<GrantCallAttachment> setGrantCallAttachmentObject(GrantCallAttachment newAttachment, List<GrantCallAttachment> attachments, MultipartFile file, Integer versionNumber, Integer documentId, Boolean isReplaced, GrantCall grantCall) {
		List<GrantCallAttachment> grantCallAttachments = new ArrayList<GrantCallAttachment>();
		String fileName = file.getName();
		String replaceFileName = newAttachment.getFileName();
		boolean isRenameRequired = false;
		int count = 1;
		isRenameRequired = checkForDuplication(newAttachment.getFileName(), attachments);
		while (isRenameRequired) {
			replaceFileName = newAttachment.getFileName(); 
			replaceFileName = generateFileName(replaceFileName, count);
			count = count + 1;
			isRenameRequired = checkForDuplication(replaceFileName, attachments);
		}
		if (newAttachment.getAttachmentId() != null) {
			for (GrantCallAttachment attachment : attachments) {
				if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
					GrantCallAttachment grantCallAttachment = new GrantCallAttachment();
					isReplaced = true;
					attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
					attachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
					versionNumber = attachment.getVersionNumber();
					documentId = attachment.getDocumentId();
					grantCallAttachment = addNewGrantCallAttachment(newAttachment, file, fileName, versionNumber, documentId, replaceFileName, grantCall.getGrantCallId());
					grantCallAttachment.setGrantCallId(grantCall.getGrantCallId());
					grantCallAttachments.add(grantCallAttachment);
				}
			}
		} else {
			if (newAttachment.getFileName().equals(fileName)) {
				documentId = documentId + 1;
				GrantCallAttachment grantCallAttachment = addNewGrantCallAttachment(newAttachment, file, fileName, versionNumber, documentId, replaceFileName, grantCall.getGrantCallId());
				grantCallAttachments.add(grantCallAttachment);
			}
			grantCallAttachments.addAll(grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCall.getGrantCallId())); 
		}
		return grantCallAttachments;
	}

	@Override
	public Map<String, List<ScoringReportDto>> getCriteriaScoreByProposalId(Integer proposalId, String loginPersonId, String loginPersonUnitNumber, String loginUserName, Boolean byCriteria) {
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(loginPersonId, Constants.VIEW_PRIVATE_COMMENTS_RIGHT, loginPersonUnitNumber);
		List<WorkflowDetail> workFlowDetails = workflowDao.getScoringWorkflowDetails(proposalId);
		List<ScoringReportDto> scoringReports = new ArrayList<>();
		Integer grantCallId = proposalDao.getGrantCallIdByProposalId(proposalId);
		List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(grantCallId);
		workFlowDetails.stream().forEach(workflowDetail -> {
			List<String> reviewedCriterias = new ArrayList<>();
			List<WorkflowReviewerScore> workflowReviewerScoreDatas = proposalDao.fetchAllWorkflowReviewerDetails(workflowDetail.getWorkflowDetailId());
			workflowReviewerScoreDatas.stream().forEach(reviewerScore -> {
				ScoringReportDto scoringReportDto = new ScoringReportDto();
				ScoredPersonDTO scoredPerson = new ScoredPersonDTO();
				scoredPerson.setScore(reviewerScore.getScore());
				List<WorkflowReviewerComment> workflowReviewerComments = new ArrayList<>();
				reviewerScore.getWorkflowReviewerComments().forEach(workflowReviewerComment -> {
					if (Boolean.TRUE.equals(isPersonHasPermission)
							&& Boolean.TRUE.equals(workflowReviewerComment.getIsPrivate())
							|| (Boolean.FALSE.equals(workflowReviewerComment.getIsPrivate())
									|| Boolean.TRUE.equals(workflowReviewerComment.getIsPrivate())
											&& loginUserName.equals(workflowReviewerComment.getUpdateUser()))) {
						workflowReviewerComments.add(workflowReviewerComment);
					}
				});
				scoredPerson.setWorkflowReviewerComments(workflowReviewerComments);
				scoredPerson.setPersonId(reviewerScore.getWorkflowDetail().getApproverPersonId());
				scoredPerson.setPersonName(reviewerScore.getWorkflowDetail().getApproverPersonName());
				scoredPerson.setIsPersonCanScore(workflowDetail.getIsReviewerCanScore());
				scoringReportDto.setReviewerScoreId(reviewerScore.getWorkflowReviewerScoreId());
				scoredPerson.setEvaluationMapName(workflowDetail.getWorkflowMap().getMapName());
				scoringReportDto.setScoringCriteriaCode(reviewerScore.getScoringCriteriaTypeCode());
				scoringReportDto.setScoringCriteriaDescription(reviewerScore.getScoringCriteria().getDescription());
				scoringReportDto.setUpdateTimeStamp(reviewerScore.getUpdateTimeStamp());
				scoringReportDto.setPerson(scoredPerson);
				scoringReports.add(scoringReportDto);
				reviewedCriterias.add(reviewerScore.getScoringCriteriaTypeCode());
			});
			List<GrantCallScoringCriteria> notReviwedCriterias = grantCallScoringCriterias.stream()
					.filter(grantCallScoringCriteria -> !reviewedCriterias
							.contains(grantCallScoringCriteria.getScoringCriteriaTypeCode())).collect(Collectors.toList());
			notReviwedCriterias.stream().forEach(notReviwedCriteria -> {
				ScoringReportDto scoringReportDto = new ScoringReportDto();
				ScoredPersonDTO scoredPerson = new ScoredPersonDTO();
				scoredPerson.setPersonId(workflowDetail.getApproverPersonId());
				scoredPerson.setPersonName(workflowDetail.getApproverPersonName());
				scoredPerson.setIsPersonCanScore(workflowDetail.getIsReviewerCanScore());
				scoredPerson.setEvaluationMapName(workflowDetail.getWorkflowMap().getMapName());
				scoringReportDto.setScoringCriteriaDescription(notReviwedCriteria.getScoringCriteria().getDescription());
				scoringReportDto.setScoringCriteriaCode(notReviwedCriteria.getScoringCriteriaTypeCode());
				scoredPerson.setWorkflowReviewerComments(new ArrayList<>());
				scoringReportDto.setPerson(scoredPerson);
				scoringReports.add(scoringReportDto);
			});
		});
		return Boolean.TRUE.equals(byCriteria) ? 
		scoringReports.stream().collect(Collectors.groupingBy(ScoringReportDto::getScoringCriteriaDescription)) :
		scoringReports.stream().collect(Collectors.groupingBy(scoringReport -> scoringReport.getPerson().getPersonName()));
	}

	private void sendGrantCallModificationNotification(GrantCallVO grantCallVO) {
		List<GrantCallIOIHeader> grantCallIOIHeaders = grantCallIOIDao.fetchSubmittedGrantCallIOIByGrantCallId(grantCallVO.getGrantCall().getGrantCallId(), Constants.GRANT_CALL_IOI_STATUS_CODE_SUBMITTED);
		if (grantCallIOIHeaders != null && !grantCallIOIHeaders.isEmpty()) {
			for (GrantCallIOIHeader grantCallIOIHeader : grantCallIOIHeaders) {
				grantCallVO.setUserFullName(grantCallIOIHeader.getPerson().getFullName());
				Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
				commonService.setNotificationRecipients(grantCallIOIHeader.getPrincipalInvestigatorId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				sendGrantCallNotification(grantCallVO, Constants.GRANT_CALL_MODIFICATION_NOTIFICATION_CODE, dynamicEmailRecipients);
			}
		}
		List<Proposal> proposals = proposalDao.fetchProposalsOfGrantCall(grantCallVO.getGrantCall().getGrantCallId());
		for (Proposal proposal : proposals) {
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
			grantCallVO.setUserFullName(proposal.getInvestigator().getFullName());
			commonService.setNotificationRecipients(proposal.getInvestigator().getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendGrantCallNotification(grantCallVO, Constants.GRANT_CALL_MODIFICATION_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
	}

	private void sendNotificationForResearchAdministrators(ProposalVO proposalVO, String statusType) {
		if (statusType.equalsIgnoreCase("S")) {
			proposalService.sendProposalNotification(proposalVO, Constants.RA_SHORTLIST_PROPOSAL_NOTIFICATION_CODE,	new HashSet<>());
		} else if (statusType.equalsIgnoreCase("A")) {
			proposalService.sendProposalNotification(proposalVO, Constants.RA_AWARDED_PROPOSAL_NOTIFICATION_CODE, new HashSet<>());
		} else if (statusType.equalsIgnoreCase("U")) {
			proposalService.sendProposalNotification(proposalVO, Constants.RA_REJECT_PROPOSAL_NOTIFICATION_CODE, new HashSet<>());
		}
	}

	private void setProposalDetails(ProposalVO proposalVO) {
		Proposal proposal = proposalVO.getProposal();
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
		if (proposal.getGrantCallId() != null) {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
			proposal.setGrantCallName(grantCall.getGrantCallName());
		}
		proposalVO.setUserFullName(proposal.getInvestigator().getFullName());
		proposalVO.setProposal(proposal);
	}

	@Override
    public String updateProposalEvalautionRank(EvaluationMainPanelVO vo) {
            return commonDao.convertObjectToJSON(grantCallDao.saveOrUpdateProposalEvalautionScore(vo.getProposalEvaluationScore()));
    }

	@Override
	public String saveOrUpdateAreaOfResearch(GrantCallVO vo) {
		grantCallModuleDao.saveOrUpdateGrantCallResearchArea(vo.getGrantCallResearchArea());
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(grantCallModuleDao.fetchGrantCallResearchAreaBasedOnGrantCallId(vo.getGrantCallId()));
	}

	@Override
	public String saveOrUpdateProposalEvalautionScores(EvaluationMainPanelVO vo) {
		List<ProposalEvaluationScore> proposalEvaluationScores = vo.getProposalEvaluationScores();
		if (proposalEvaluationScores != null && !proposalEvaluationScores.isEmpty()) {
			for (ProposalEvaluationScore proposalEvaluationScore : proposalEvaluationScores) {
				grantCallDao.saveOrUpdateProposalEvalautionScore(proposalEvaluationScore);
			}
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	private void sendGrantCallNotificationForPointOfContacts(GrantCallVO vo) {
		List<GrantCallContact> grantCallContacts = grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(vo.getGrantCall().getGrantCallId());
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		grantCallContacts.stream().forEach(grantCallContact -> {
			commonService.setNotificationRecipients(grantCallContact.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		});
		sendGrantCallNotification(vo, Constants.ENABLE_POC_NOTIFICATION_ON_PUBLISH_GRANT_CALL, dynamicEmailRecipients);
	}

	@Override
	public String saveGrantCallEligibilityCriteria(GrantCallVO vo) {
		GrantCallEligibility grantCallEligibility = vo.getGrantCallEligibility();
		grantCallDao.saveOrUpdateGrantCallEligibility(grantCallEligibility);
		setGrantCallUpdateUser(vo);
		loadGrantCallEligibility(vo, vo.getGrantCallId());
		return committeeDao.convertObjectToJSON(vo.getGrantCallEligibilities());
	}

	@Override
	public String saveOrUpdatePointOfContact(GrantCallVO vo) {
		grantCallModuleDao.saveOrUpdateGrantCallContact(vo.getGrantCallContact());
		vo.setGrantCallId(vo.getGrantCallContact().getGrantCallId());
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(grantCallModuleDao.fetchGrantCallContactBasedOnGrantCallId(vo.getGrantCallId()));
	}

	public String updateGrantCallAttachmentDetails(GrantCallVO vo) {
		GrantCallAttachment grantCallAttachment = grantCallDao.fetchAttachmentById(vo.getGrantCallAttachment().getAttachmentId());
		grantCallAttachment.setDescription(vo.getGrantCallAttachment().getDescription());
		grantCallAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		grantCallAttachment.setUpdateUser(vo.getGrantCallAttachment().getUpdateUser());
		grantCallModuleDao.saveOrUpdateGrantCallAttachment(grantCallAttachment);
		setGrantCallUpdateUser(vo);
		return commonDao.convertObjectToJSON(getGrantCallAttachmentsByGrantCallId(grantCallAttachment.getGrantCallId()));
	}

	private List<GrantCallAttachment> getGrantCallAttachmentsByGrantCallId(Integer grantCallId) {
		 List <GrantCallAttachment> grantCallAttachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCallId);
		if (grantCallAttachments != null && !grantCallAttachments.isEmpty()) {
			for(GrantCallAttachment grantCallAttachment : grantCallAttachments) {
				if (grantCallAttachment.getUpdateUser() != null) {
					grantCallAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(grantCallAttachment.getUpdateUser()));
				}
			}
		}
		return grantCallAttachments;
	}

	@Override
	public String addGrantCallAttachmentForWaf(GrantCallVO grantCallVO) {
		try {
			MultipartFile multipartFile = null;
			String contentType = null;
			GrantCallAttachment newAttachment = grantCallVO.getNewAttachment();
			String name = grantCallVO.getFileName();
			String splicedFile = grantCallVO.getFileContent();
			Integer remaining = grantCallVO.getRemaining();
			Integer length = grantCallVO.getLength();
			String userId = grantCallVO.getLoginPersonId();
			String timestamp = grantCallVO.getFileTimestamp();
			if (splicedFile != null) {
				contentType = grantCallVO.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if(multipartFile != null && !multipartFile.isEmpty()) {
			GrantCall grantCall = grantCallVO.getGrantCall();
			List<GrantCallAttachment> attachments = grantCallModuleDao.fetchGrantCallAttachmentBasedOnGrantCallId(grantCall.getGrantCallId());
			Integer documentId = 0;
			if (attachments != null && !attachments.isEmpty()) {
				Collections.sort(attachments,
						(attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
								: attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
				documentId = attachments.get(0).getDocumentId();
			}
			List<GrantCallAttachment> grantCallAttachments = new ArrayList<>();
			Integer versionNumber = 0;
			Boolean isReplaced = false;
					File file = new File(multipartFile.getOriginalFilename());
					String fileName = file.getName();
					String replaceFileName = newAttachment.getFileName();
					boolean isRenameRequired = false;
					int count = 1;
					isRenameRequired = checkForDuplication(newAttachment.getFileName(), attachments);
					while(isRenameRequired) {
						 replaceFileName = newAttachment.getFileName();
						 replaceFileName = generateFileName(replaceFileName, count);
						 count = count +1;
						 isRenameRequired = checkForDuplication(replaceFileName, attachments);
					}
					if (newAttachment.getAttachmentId() != null) {
						for (GrantCallAttachment attachment : attachments) {
							if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
								GrantCallAttachment grantCallAttachment = null;
								isReplaced = true;
								attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
								attachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
								versionNumber = attachment.getVersionNumber();
								documentId = attachment.getDocumentId();
								grantCallAttachment = addNewGrantCallAttachment(newAttachment, multipartFile, fileName, versionNumber, documentId, replaceFileName, grantCall.getGrantCallId());
								grantCallAttachment.setGrantCallId(grantCall.getGrantCallId());
								grantCallAttachments.add(grantCallAttachment);
							}
						}
					} else {
						if (newAttachment.getFileName().equals(fileName)) {
							documentId = documentId + 1;
							GrantCallAttachment grantCallAttachment = null;
							grantCallAttachment = addNewGrantCallAttachment(newAttachment, multipartFile, fileName, versionNumber, documentId, replaceFileName, grantCall.getGrantCallId());
							grantCallAttachments.add(grantCallAttachment);
						}
					}
			if (Boolean.TRUE.equals(isReplaced)) {
				grantCall = grantCallDao.saveOrUpdateGrantCall(grantCall);
			}
			loadGrantCallUserFullNames(grantCall);
			grantCallVO.setGrantCallAttachments(getGrantCallAttachmentsByGrantCallId(grantCall.getGrantCallId()));
		}
		} catch (Exception e) {
			logger.error("Exception while saving addGrantCallAttachment {}", e.getMessage());
		}
		return committeeDao.convertObjectToJSON(grantCallVO);
	}

	@Override
	public GrantCallActionLog saveGrantCallActionLogDetails(Integer grantHeaderId, String grantCallActionTypeCode, String updateUser) {
		GrantCallActionLog grantCallActionLog = new GrantCallActionLog();
		grantCallActionLog.setGrantHeaderId(grantHeaderId);
		grantCallActionLog.setActionTypeCode(grantCallActionTypeCode);
		grantCallActionLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		grantCallActionLog.setUpdateUser(updateUser);
		return grantCallDao.saveOrUpdateGrantCallActionLog(grantCallActionLog);
	}

	@Override
	public String getGrantCallHistory(GrantCallVO vo) {
		List<GrantCallActionLog> grantCallActionLogDetails = grantCallDao.fetchGrantCallActionLog(vo.getGrantCallId());
		getFullNameOfUpdateUser(grantCallActionLogDetails);
		vo.setGrantCallActionLogs(grantCallActionLogDetails);
		return commonDao.convertObjectToJSON(vo);
	}

	private void getFullNameOfUpdateUser(List<GrantCallActionLog> grantCallActionLogDetails) {
		Set<String> userName = grantCallActionLogDetails.stream().map(GrantCallActionLog::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			grantCallActionLogDetails.stream().filter(item -> item.getUpdateUser() != null).filter(grantCallActionLog -> collect.containsKey(grantCallActionLog.getUpdateUser().toUpperCase())).forEach(grantCallActionLog -> grantCallActionLog.setUpdateUserFullName(collect.get(grantCallActionLog.getUpdateUser().toUpperCase())));
		}	
	}

	private void deleteAllGrantCallActionLog(Integer grantCallId) {
		List<GrantCallActionLog> grantCallActionLogs =  grantCallDao.fetchGrantCallActionLog(grantCallId);
		if (grantCallActionLogs != null && !grantCallActionLogs.isEmpty()) {
			grantCallModuleDao.deleteGrantCallActionLog(grantCallActionLogs);
		}
	}

	@Override
	public String getCriteriaScoreByProposalId(Integer proposalId, String loginPersonId,
			String loginPersonUnitNumber, String loginUserName) {
		return commonDao.convertObjectToJSON(getCriteriaScoreByProposalId(proposalId, loginPersonId, loginPersonUnitNumber, loginUserName, Boolean.TRUE));
	}

}
