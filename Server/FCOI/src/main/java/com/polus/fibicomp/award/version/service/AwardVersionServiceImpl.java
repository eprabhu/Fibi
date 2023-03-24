package com.polus.fibicomp.award.version.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.awardreviewcomment.dao.AwardReviewCommentDao;
import com.polus.fibicomp.award.awardworkflow.dao.AwardWorkflowDao;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountFNADistribution;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardApprovedEquipment;
import com.polus.fibicomp.award.pojo.AwardAprovedForeignTravel;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationDetail;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardHistoryLog;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.award.pojo.AwardKeyword;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonAttachment;
import com.polus.fibicomp.award.pojo.AwardPersonRoles;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.award.pojo.AwardReportTermRecipient;
import com.polus.fibicomp.award.pojo.AwardReportTerms;
import com.polus.fibicomp.award.pojo.AwardReportTracking;
import com.polus.fibicomp.award.pojo.AwardReportTrackingFile;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSponsorTerm;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardLinkInstituteProposalVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.service.AwardBudgetCopyService;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.compilance.dao.ComplianceDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.service.CustomDataElementService;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dao.GrantCallKPIDao;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.timesheetdetail.dao.TimesheetDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.scopusintegration.dao.ScopusDao;
import com.polus.fibicomp.scopusintegration.pojo.AwardScopus;
import com.polus.fibicomp.sectionwiseedit.dao.SectionWiseEditDao;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatusHistory;
import com.polus.fibicomp.servicerequest.service.ServiceRequestService;
import com.polus.fibicomp.task.dao.TaskDao;

@Transactional
@Service(value = "awardVersionService")
public class AwardVersionServiceImpl implements AwardVersionService {

	protected static Logger logger = LogManager.getLogger(AwardVersionServiceImpl.class.getName());

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardWorkflowDao awardWorkflowDao;

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;

	@Autowired
	private AuthorizationService authorizationService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private GrantCallKPIDao grantCallKPIDao;

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	public AwardReviewCommentDao awardReviewCommentDao;

	@Autowired
	public ServiceRequestService serviceRequestService;

	@Autowired
	private ManpowerDao manpowerDao;

	@Autowired
	ManpowerService manpowerService;

	@Autowired
	private TimesheetDao timesheetDao;

	@Autowired
	private AwardBudgetCopyService awardBudgetCopyService;

	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	@Qualifier(value = "rolesManagement")
	private RolesManagementDao rolesManagementDao;

	@Autowired
	private ComplianceDao complianceDao;

	@Autowired
	private ScopusDao scopusDao;

	@Override
	public String copyAward(AwardVO vo) {
		Award orginalAward = null;
		if (Boolean.TRUE.equals(vo.getIsMasterAwardCreation())) {
			orginalAward = vo.getAward();
		} else {
			orginalAward = awardDao.getAwardDetailsById(vo.getAwardId());
		}
		Award copyAward = new Award();
		copyAwardMandatoryFields(vo, copyAward, orginalAward);
		copyAwardNonMandatoryFields(vo, copyAward, orginalAward);
		if (Boolean.FALSE.equals(vo.getIsAwardHierarchy()) && Boolean.FALSE.equals(vo.getIsCopyAward())) {
			copyAward = copyAwardBudgetDetails(orginalAward, copyAward, vo);
		}
		if (copyAward.getCreateUser() != null) {
			copyAward.setCreateUserFullName(personDao.getUserFullNameByUserName(copyAward.getCreateUser()));
		}
		if (copyAward.getUpdateUser() != null) {
			copyAward.setUpdateUserFullName(personDao.getUserFullNameByUserName(copyAward.getUpdateUser()));
		}
		copyAward.setWorkFlowStatusName(copyAward.getAwardWorkflowStatus().getDescription());
		vo.setAward(copyAward);
		if (Boolean.TRUE.equals(vo.getIsCopyAward())) {
			awardService.moveDataToAwardHierarchy(copyAward);
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			vo.setKpiTypes(grantCallKPIDao.fetchAllKPIs());
			vo.setAwardKpis(awardDao.fetchAllAwardKPI(copyAward.getAwardId()));
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {
			List<AwardMileStone> awardMilestones = awardDao.fetchAwardMileStonesBasedOnAwardId(copyAward.getAwardId());
			getFullNameOfUser(awardMilestones);
			vo.setAwardMileStones(awardMilestones);
		}
		if (vo.getIsFeededAwardId().equals(Boolean.TRUE)) {
			return copyAward.getAwardId().toString();
		}
		return commonDao.convertObjectToJSON(copyAward);
	}

	private void getFullNameOfUser(List<AwardMileStone> awardMilestones) {
		Set<String> userName = awardMilestones.stream().map(AwardMileStone::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors
					.toMap(person -> person.getPrincipalName().toUpperCase(), Person::getFullName));
			awardMilestones.stream().filter(item -> item.getUpdateUser() != null)
					.filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase()))
					.forEach(item -> item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	private void copyAwardMandatoryFields(AwardVO vo, Award copyAward, Award orginalAward) {
		copyAward.setActivityTypeCode(orginalAward.getActivityTypeCode());
		copyAward.setBeginDate(orginalAward.getBeginDate());
		copyAward.setAccountTypeCode(orginalAward.getAccountTypeCode());
		copyAward.setAccountType(orginalAward.getAccountType());
		copyAward.setFinalExpirationDate(orginalAward.getFinalExpirationDate());
		copyAward.setLeadUnitNumber(orginalAward.getLeadUnitNumber());
		copyAward.setLeadUnit(commonDao.getLeadUnitByUnitNumber(orginalAward.getLeadUnitNumber()));
		copyAward.setSponsorCode(orginalAward.getSponsorCode());
		copyAward.setSponsor(commonDao.getSponsorById(orginalAward.getSponsorCode()));
		copyAward.setSponsorName(commonService.getSponsorFormatBySponsorDetail(copyAward.getSponsorCode(), copyAward.getSponsor().getSponsorName(), copyAward.getSponsor().getAcronym()));
		copyAward.setTitle(orginalAward.getTitle());
		copyAward.setResearchDescription(orginalAward.getResearchDescription());
		copyAward.setMultiDisciplinaryDescription(orginalAward.getMultiDisciplinaryDescription());
		copyAward.setCfdaNumber(orginalAward.getCfdaNumber());	
		copyAward.setDfafsNumber(orginalAward.getDfafsNumber());	
		copyAward.setSponsorAwardNumber(orginalAward.getSponsorAwardNumber());	
		copyAward.setFunderApprovalDate(orginalAward.getFunderApprovalDate());
		if (orginalAward.getPrimeSponsorCode() != null) {	
			copyAward.setPrimeSponsorCode(orginalAward.getPrimeSponsorCode());	
			copyAward.setPrimeSponsor(commonDao.getSponsorById(orginalAward.getPrimeSponsorCode()));
			copyAward.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(copyAward.getPrimeSponsorCode(), copyAward.getPrimeSponsor().getSponsorName(), copyAward.getPrimeSponsor().getAcronym()));
		}	
		copyAward.setInvoiceInstructions(orginalAward.getInvoiceInstructions());	
		copyAward.setFinalInvoiceDue(orginalAward.getFinalInvoiceDue());	
		copyAward.setInvoiceNoOfCopies(orginalAward.getInvoiceNoOfCopies());	
		copyAward.setPaymentInvoiceFrequencyCode(orginalAward.getPaymentInvoiceFrequencyCode());	
		copyAward.setMethodOfPaymentCode(orginalAward.getMethodOfPaymentCode());	
		copyAward.setBasisOfPaymentCode(orginalAward.getBasisOfPaymentCode());
		copyAward.setUpdateUser(vo.getUpdateUser());
		copyAward.setCreateUser(vo.getUpdateUser());
		copyAward.setCreateTimestamp(commonDao.getCurrentTimestamp());
		copyAward.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copyAward.setDocumentUpdateUser(vo.getUpdateUser());
		copyAward.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copyAward.setIsLatest(true);
		copyAward.setActivityTypeCode(orginalAward.getActivityTypeCode());
		String awardTypeCode = orginalAward.getAwardTypeCode();
		if (awardTypeCode != null && !awardTypeCode.isEmpty()) {
			copyAward.setAwardTypeCode(orginalAward.getAwardTypeCode());
			copyAward.setAwardType(awardDao.fetchAwardTypeByAwardTypeCode(orginalAward.getAwardTypeCode()));
		}
		copyAward.setDuration(orginalAward.getDuration());
		copyAward.setActivityType(orginalAward.getActivityType());
		if (vo.getIsCopyAward().equals(Boolean.TRUE) || vo.getIsAwardHierarchy().equals(Boolean.TRUE)) {
			copyAward.setSequenceNumber(1);
			if (vo.getIsCopyAward().equals(Boolean.TRUE)) {
				copyAward.setAwardNumber(awardService.generateNextAwardNumber());
			} else if (vo.getIsAwardHierarchy().equals(Boolean.TRUE)) {
				copyAward.setAwardNumber(awardService.getBaseAwardNumber(orginalAward.getAwardNumber()));
			}
			copyAward.setAwardDocumentTypeCode(Constants.AWARD_SETUP);
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_SETUP));
			copyAward.setCreateUser(AuthenticatedUser.getLoginUserName());
			copyAward.setUpdateUser(AuthenticatedUser.getLoginUserName());
		} else if (vo.getIsMasterAwardCreation().equals(Boolean.TRUE)) {
			copyAward.setSequenceNumber(0);
			copyAward.setAwardNumber(orginalAward.getAwardNumber());
			copyAward.setUpdateUser(orginalAward.getUpdateUser());
		} else {
			copyAward.setSequenceNumber(awardDao.getMaxSequenceNumberBasedOnAwardNumber(orginalAward.getAwardNumber()));
			copyAward.setAwardNumber(orginalAward.getAwardNumber());
		}
		copyAward.setAwardExecutionDate(orginalAward.getAwardExecutionDate());
		copyAward.setAwardEffectiveDate(orginalAward.getAwardEffectiveDate());
		copyAward.setFundCenter(orginalAward.getFundCenter());
		if (Boolean.FALSE.equals(vo.getIsAwardHierarchy()) && Boolean.FALSE.equals(vo.getIsCopyAward())) {
			copyAward.setAccountNumber(orginalAward.getAccountNumber());
			orginalAward.setIsLatest(false);
		}
		copyAward.setGrantHeaderId(orginalAward.getGrantHeaderId());
		if (orginalAward.getGrantHeaderId() != null) {
			copyAward.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(orginalAward.getGrantHeaderId()));	
		}
		if (Boolean.TRUE.equals(vo.getIsMasterAwardCreation())) {
			orginalAward.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_ARCHIVE);
			copyAward.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_ACTIVE);
			copyAward.setCreateUser(orginalAward.getCreateUser());
			copyAward.setCreateTimestamp(orginalAward.getCreateTimestamp());
			copyAward.setStatusCode(orginalAward.getStatusCode());
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(orginalAward.getStatusCode()));
			copyAward.setWorkflowAwardStatusCode(orginalAward.getWorkflowAwardStatusCode());
			copyAward.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(orginalAward.getWorkflowAwardStatusCode()));
			copyAward.setAwardDocumentTypeCode(Constants.MASTER_AWARD);
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.MASTER_AWARD));
		} else {
			copyAward.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_PENDING);
			copyAward.setStatusCode(Constants.AWARD_STATUS_CODE_PENDING);
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_PENDING));
			copyAward.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT);
			copyAward.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT));
		}
		awardDao.saveOrUpdateAwardDetails(orginalAward);
		copyAward.setAwardVariationTypeCode(vo.getServiceRequestTypeCode());
		copyAward.setServiceRequestType(vo.getServiceRequestType());
		if (Boolean.TRUE.equals(vo.getIsAwardModification())) {
			copyAward.setAwardDocumentTypeCode(Constants.AWARD_MODIFICATION);
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_MODIFICATION));
			copyAward.setAwardVariationTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
			copyAward.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.ALL_SERVICE_REQUEST_TYPE_CODE));
			copyAward.setStatusCode(orginalAward.getStatusCode());
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(orginalAward.getStatusCode()));
		} else if (Boolean.TRUE.equals(vo.getIsVariationRequest())) {
			copyAward.setAwardDocumentTypeCode(Constants.AWARD_VARIATION);
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_VARIATION));
			copyAward.setAwardVariationTypeCode(vo.getServiceRequestTypeCode());
			copyAward.setServiceRequestType(vo.getServiceRequestType());
			copyAward.setStatusCode(orginalAward.getStatusCode());
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(orginalAward.getStatusCode()));
		} else if (Boolean.TRUE.equals(vo.getIsProjectClosure())) {
			copyAward.setAwardDocumentTypeCode(Constants.AWARD_VARIATION);
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_VARIATION));
			copyAward.setAwardVariationTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
			copyAward.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.PROJECT_CLOSURE_TYPE_CODE));
			copyAward.setStatusCode(orginalAward.getStatusCode());
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(orginalAward.getStatusCode()));
		} else if (Boolean.TRUE.equals(vo.getIsAwardOutcome())) {
			copyAward.setAwardDocumentTypeCode(Constants.AWARD_VARIATION);
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_VARIATION));
			copyAward.setAwardVariationTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
			copyAward.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.AWARD_OUTCOME_TYPE_CODE));
			copyAward.setStatusCode(orginalAward.getStatusCode());
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(orginalAward.getStatusCode()));
		}
		copyAward = awardDao.saveOrUpdateAwardDetails(copyAward);
		Integer awardId = copyAward.getAwardId();
		vo.setAward(copyAward);
		List<AwardFundingProposal> newFundingProposals = new ArrayList<>(); 
		List<AwardFundingProposal> fundingProposals = awardDao.getAwardFundingProposals(orginalAward.getAwardId());
		try {
			fundingProposals.stream().forEach(proposal -> {
				AwardFundingProposal awardFundingProposal = new AwardFundingProposal();
				awardFundingProposal.setAwardId(awardId);
				awardFundingProposal.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardFundingProposal.setIsActive(true);
				awardFundingProposal.setProposal(proposal.getProposal());
				awardFundingProposal.setProposalId(proposal.getProposalId());
				awardFundingProposal = awardDao.saveOrUpdateFundingProposal(awardFundingProposal);
				newFundingProposals.add(awardFundingProposal);
			});
		} catch (Exception e) {
			logger.info("Error occured in copyAwardMandatoryFields : {}", e.getMessage());
			throw e;
		}
		vo.setAwardFundingProposals(newFundingProposals);
		if (((Boolean.FALSE.equals(vo.getIsAwardHierarchy()) && Boolean.FALSE.equals(vo.getIsCopyAward())) 
				|| ((Boolean.TRUE.equals(vo.getIsCopyAward()) || Boolean.TRUE.equals(vo.getIsAwardHierarchy())) && (Boolean.TRUE.equals(vo.getCopyOtherInformation()) && Boolean.TRUE.equals(vo.getCopyQuestionnaire())))
				|| Boolean.TRUE.equals(vo.getIsProjectClosure()))) {
			customDataElementService.copyCustomDataBasedOnModule(orginalAward.getAwardId(), copyAward.getAwardId(), Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE , Constants.SUBMODULE_ITEM_KEY);
			vo.setAwardId(copyAward.getAwardId());
			copyQuestionnaireData(orginalAward.getAwardId(), vo);
		} else if ((Boolean.TRUE.equals(vo.getIsAwardHierarchy()) || Boolean.TRUE.equals(vo.getIsCopyAward())) && Boolean.TRUE.equals(vo.getCopyOtherInformation())) {
			customDataElementService.copyCustomDataBasedOnModule(orginalAward.getAwardId(), copyAward.getAwardId(), Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE , Constants.SUBMODULE_ITEM_KEY);
		} else if ((Boolean.TRUE.equals(vo.getIsAwardHierarchy()) || Boolean.TRUE.equals(vo.getIsCopyAward())) && Boolean.TRUE.equals(vo.getCopyQuestionnaire())) {
			vo.setAwardId(copyAward.getAwardId());
			copyQuestionnaireData(orginalAward.getAwardId(), vo);
		}
//		if (Boolean.TRUE.equals(vo.getIsAwardWithdrawal()) && commonDao.getParameterValueAsBoolean(Constants.ENABLE_SAP_AWARD_FEED) && copyAward != null && copyAward.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_ACTIVE)) {
//			integrationService.saveSAPAwardDetails(copyAward.getAwardId(), copyAward.getAwardNumber(), copyAward.getSequenceNumber(), vo.getUpdateUser());
//		}
	}

	private void copyAwardNonMandatoryFields(AwardVO vo, Award copyAward, Award orginalAward) {
		List<AwardPerson> awardPersons = awardDao.getAwardPersons(orginalAward.getAwardId());
		if (!awardPersons.isEmpty() && awardPersons != null) {
			vo.setAwardPersons(copyAwardPersons(copyAward, vo.getUpdateUser(), awardPersons));
			copyAward.setAwardPersons(vo.getAwardPersons());
		}
		List<AwardProjectTeam> awardProjectTeams = awardDao.getAwardProjectTeamList(orginalAward.getAwardId());
		if (awardProjectTeams != null && !awardProjectTeams.isEmpty()) {
			vo.setAwardProjectTeams(copyAwardProjectTeams(copyAward, awardProjectTeams, vo.getUpdateUser()));
		}
		if (orginalAward.getAwardKeywords() != null && !orginalAward.getAwardKeywords().isEmpty()) {
			copyAward.getAwardKeywords().addAll(copyAwardKeywords(copyAward, orginalAward, vo.getUpdateUser()));
		}
		List<AwardContact> awardContacts = awardDao.getAwardContactList(orginalAward.getAwardId());
		if (awardContacts != null && !awardContacts.isEmpty()) {
			vo.setAwardContacts(copyAwardContacts(copyAward, awardContacts, vo.getUpdateUser()));
		}
		List<AwardSpecialReview> awardSpecialReview = awardDao.getAwardSpecialReviewsByAwardId(orginalAward.getAwardId());
		if (awardSpecialReview != null && !awardSpecialReview.isEmpty()) {
			vo.setAwardSpecialReviews(copyAwardSpecialReview(copyAward, awardSpecialReview, vo.getUpdateUser()));
		}
		List<AwardSubContract> awardSubContracts = awardDao.getSubContractsByAwardId(orginalAward.getAwardId());
		if (awardSubContracts != null && !awardSubContracts.isEmpty()) {
			vo.setAwardSubContracts(copyAwardSubContract(copyAward, awardSubContracts, vo.getUpdateUser()));
		}
		List<AwardResearchArea> awardResearchAreas = awardDao.fetchAwardResearchAreaBasedOnAwardId(orginalAward.getAwardId());
		if (!awardResearchAreas.isEmpty() && awardResearchAreas != null) {
			vo.setAwardResearchAreas(copyAwardResearchAreas(copyAward, vo.getUpdateUser(), awardResearchAreas));
			copyAward.setAwardPersons(vo.getAwardPersons());
		}
		if (Boolean.FALSE.equals(vo.getIsAwardHierarchy()) && Boolean.FALSE.equals(vo.getIsCopyAward())) {
			List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(orginalAward.getAwardId());
			if (awardCostShares != null && !awardCostShares.isEmpty()) {
				vo.setAwardCostShares(copyAwardCostShare(copyAward, awardCostShares, vo.getUpdateUser()));
			}
		}
		if (Boolean.FALSE.equals(vo.getIsCopyAward())) {
			List<AwardReportTerms> awardReportTerms = awardDao.getAwardReportTermsByAwardId(orginalAward.getAwardId());
			if (awardReportTerms != null && !awardReportTerms.isEmpty()) {
				copyAwardReportTerms(copyAward, awardReportTerms, vo.getUpdateUser(),vo.getIsAwardHierarchy());
			}
		}
		List<AwardSponsorTerm> awardSponsorTerms = awardDao.getAwardSponsorTermByAwardId(orginalAward.getAwardId());
		if (awardSponsorTerms != null && !awardSponsorTerms.isEmpty()) {
			copyAwardSponsorTerm(copyAward, awardSponsorTerms, vo.getUpdateUser());
		}
		List<AwardPublications> awardPublications = awardProjectOutcomeDao.fetchAllAwardPublications(orginalAward.getAwardId());
		if (awardPublications != null && !awardPublications.isEmpty()) {
			copyAwardPublications(copyAward, awardPublications, vo.getUpdateUser());
		}
		List<AwardAssociation> awardAssociations = awardProjectOutcomeDao.fetchAllAwardAssociation(orginalAward.getAwardId());
		if (awardAssociations != null && !awardAssociations.isEmpty()) {
			copyAwardAssociation(copyAward, awardAssociations, vo.getUpdateUser());
		}
		List<AwardAcheivements> awardAcheivements = awardProjectOutcomeDao.fetchAllAwardAcheivements(orginalAward.getAwardId());
		if (awardAcheivements != null && !awardAcheivements.isEmpty()) {
			copyAwardAcheivements(copyAward, awardAcheivements, vo.getUpdateUser());
		}
		List<AwardApprovedEquipment> awardApprovedEquipments = awardDao.getAwardApprovedEquipmentByAwardId(orginalAward.getAwardId());
		if (awardApprovedEquipments != null && !awardApprovedEquipments.isEmpty()) {
			copyAwardApprovedEquipment(copyAward, awardApprovedEquipments, vo.getUpdateUser());
		}
		List<AwardAprovedForeignTravel> awardAprovedForeignTravel = awardDao.getAwardAprovedForeignTravelByAwardId(orginalAward.getAwardId());
		if (awardAprovedForeignTravel != null && !awardAprovedForeignTravel.isEmpty()) {
			copyAwardAprovedForeignTravel(copyAward, awardAprovedForeignTravel, vo.getUpdateUser());
		}
		List<AwardScopus> awardScopuses = scopusDao.fetchAllAwardScopus(orginalAward.getAwardId());
		if (awardScopuses != null && !awardScopuses.isEmpty()) {
			copyAwardScopuses(copyAward.getAwardId(), awardScopuses, vo.getUpdateUser());
		}
		if (Boolean.TRUE.equals(vo.getIsCopyAward()) || Boolean.TRUE.equals(vo.getIsAwardHierarchy())) {
			List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.AWARD_MODULE_CODE);
			if (derivedRoles != null && !derivedRoles.isEmpty()) {
				awardService.assignDerivedRolesForCreator(copyAward, personDao.getPersonIdByUserName(copyAward.getCreateUser()), derivedRoles);
			}
			Optional<AwardPerson> awardPerson = copyAward.getAwardPersons().stream().filter(person -> person.getPersonRoleId().equals(Constants.PI_ROLE_CODE)).findAny();
			if (awardPerson.isPresent()) {
				awardService.assignDerivedRolesForPI(awardPerson.get(),copyAward.getAwardId());
			}
		} else {
			List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRoles(orginalAward.getAwardId());
			if (awardPersonRoles != null && !awardPersonRoles.isEmpty()) {
				copyAwardPersonRoles(copyAward, awardPersonRoles, vo.getUpdateUser());
			}
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			List<AwardKPI> awardKPIs = awardDao.fetchAllAwardKPI(orginalAward.getAwardId());
			if (awardKPIs != null && !awardKPIs.isEmpty()) {
				copyAwardKPIs(copyAward, awardKPIs, vo.getUpdateUser());
			}
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {
			List<AwardMileStone> awardMileStones = awardDao.fetchAwardMileStonesBasedOnAwardId(orginalAward.getAwardId());
			if (awardMileStones != null && !awardMileStones.isEmpty()) {
				copyAwardMileStones(copyAward, awardMileStones,vo.getIsCopyAward(),vo.getUpdateUser());
			}
		}
		if (Boolean.TRUE.equals(vo.getIsMasterAwardCreation()) || (Boolean.FALSE.equals(vo.getIsAwardHierarchy()) && Boolean.FALSE.equals(vo.getIsCopyAward()) &&
				sectionWiseEditDao.getSectionTypeCodeBasedOnTypeCode(vo.getServiceRequestTypeCode()).contains(Constants.DATES_AND_AMOUNT_EDITABLE_SECTION_TYPE_CODE))) {
			copyAwardFNADistributionDetails(orginalAward, copyAward);
		}
	}

	private void copyAwardScopuses(Integer awardId, List<AwardScopus> awardScopuses, String updateUser) {
		awardScopuses.forEach(awardScopus -> {
			AwardScopus newAwardScopus = new AwardScopus();
			BeanUtils.copyProperties(awardScopus, newAwardScopus);
			newAwardScopus.setAwardScopusId(null);
			newAwardScopus.setUpdateUser(updateUser);
			newAwardScopus.setAwardId(awardId);
			newAwardScopus.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			scopusDao.saveOrUpdateAwardScopus(newAwardScopus);
		});
	}

	private List<AwardResearchArea> copyAwardResearchAreas(Award award, String updateUser, List<AwardResearchArea> awardResearchAreas) {
		List<AwardResearchArea> newAwardResearchAreas = new ArrayList<>();
		awardResearchAreas.stream().forEach(copiedAwardResearchArea -> {
			AwardResearchArea awardResearchArea = new AwardResearchArea();
			awardResearchArea.setAwardId(award.getAwardId());
			awardResearchArea.setAwardNumber(award.getAwardNumber());
			awardResearchArea.setSequenceNumber(award.getSequenceNumber());
			awardResearchArea.setResearchTypeCode(copiedAwardResearchArea.getResearchTypeCode());
			awardResearchArea.setResearchTypeAreaCode(copiedAwardResearchArea.getResearchTypeAreaCode());
			awardResearchArea.setResearchTypeArea(copiedAwardResearchArea.getResearchTypeArea());
			awardResearchArea.setResearchType(copiedAwardResearchArea.getResearchType());
			awardResearchArea.setResearchTypeSubAreaCode(copiedAwardResearchArea.getResearchTypeSubAreaCode());
			awardResearchArea.setResearchTypeSubArea(copiedAwardResearchArea.getResearchTypeSubArea());
			awardResearchArea.setUpdateUser(updateUser);
			awardResearchArea.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardResearchArea = awardDao.saveOrUpdateAwardResearchArea(awardResearchArea);
			newAwardResearchAreas.add(awardResearchArea);
		});
		return newAwardResearchAreas;
	}

	private void copyAwardPersonRoles(Award copyAward, List<AwardPersonRoles> awardPersonRoles, String updateUser) {
		List<AwardPersonRoles> newAwardPersonRoles = new ArrayList<>();
		awardPersonRoles.stream().forEach(copiedAwardPersonRoles -> {
			AwardPersonRoles awardPersonRole = new AwardPersonRoles();
			awardPersonRole.setAwardId(copyAward.getAwardId());
			awardPersonRole.setAwardNumber(copyAward.getAwardNumber());
			awardPersonRole.setSequenceNumber(copyAward.getSequenceNumber());
			awardPersonRole.setPersonId(copiedAwardPersonRoles.getPersonId());
			awardPersonRole.setRoleId(copiedAwardPersonRoles.getRoleId());
			awardPersonRole.setIsSystemGenerated(copiedAwardPersonRoles.getIsSystemGenerated());
			awardPersonRole.setUpdateUser(updateUser);
			awardPersonRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardPersonRole = awardDao.saveOrUpdateAwardPersonRoles(awardPersonRole);
			newAwardPersonRoles.add(awardPersonRole);
		});
	}

	private void copyAwardAprovedForeignTravel(Award copyAward, List<AwardAprovedForeignTravel> awardAprovedForeignTravels, String updateUser) {
		List<AwardAprovedForeignTravel> newAwardAprovedForeignTravels = new ArrayList<>();
		try {
			awardAprovedForeignTravels.stream().forEach(copiedAwardAprovedForeignTravel -> {
				AwardAprovedForeignTravel awardAprovedForeignTravel = new AwardAprovedForeignTravel();
				awardAprovedForeignTravel.setAwardId(copyAward.getAwardId());
				awardAprovedForeignTravel.setAwardNumber(copyAward.getAwardNumber());
				awardAprovedForeignTravel.setSequenceNumber(copyAward.getSequenceNumber());
				awardAprovedForeignTravel.setPersonId(copiedAwardAprovedForeignTravel.getPersonId());
				awardAprovedForeignTravel.setRolodexId(copiedAwardAprovedForeignTravel.getRolodexId());
				awardAprovedForeignTravel.setTravellerName(copiedAwardAprovedForeignTravel.getTravellerName());
				awardAprovedForeignTravel.setDestination(copiedAwardAprovedForeignTravel.getDestination());
				awardAprovedForeignTravel.setStartDate(copiedAwardAprovedForeignTravel.getStartDate());
				awardAprovedForeignTravel.setEndDate(copiedAwardAprovedForeignTravel.getEndDate());
				awardAprovedForeignTravel.setAmount(copiedAwardAprovedForeignTravel.getAmount());
				awardAprovedForeignTravel.setUpdateUser(updateUser);
				awardAprovedForeignTravel.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardAprovedForeignTravel = awardDao.saveOrUpdateAwardAprovedForeignTravel(awardAprovedForeignTravel);
				newAwardAprovedForeignTravels.add(awardAprovedForeignTravel);
			});
		} catch (Exception e) {
			logger.info("Error occured in copyAwardAprovedForeignTravel : {}", e.getMessage());
			throw e;
		}
	}

	private void copyAwardApprovedEquipment(Award copyAward, List<AwardApprovedEquipment> awardApprovedEquipments, String updateUser) {
		List<AwardApprovedEquipment> newAwardApprovedEquipment = new ArrayList<>();
		try {
			awardApprovedEquipments.stream().forEach(copiedAwardApprovedEquipment -> {
				AwardApprovedEquipment awardApprovedEquipment = new AwardApprovedEquipment();
				awardApprovedEquipment.setAwardId(copyAward.getAwardId());
				awardApprovedEquipment.setAwardNumber(copyAward.getAwardNumber());
				awardApprovedEquipment.setSequenceNumber(copyAward.getSequenceNumber());
				awardApprovedEquipment.setItem(copiedAwardApprovedEquipment.getItem());
				awardApprovedEquipment.setModel(copiedAwardApprovedEquipment.getModel());
				awardApprovedEquipment.setVendor(copiedAwardApprovedEquipment.getVendor());
				awardApprovedEquipment.setAmount(copiedAwardApprovedEquipment.getAmount());
				awardApprovedEquipment.setUpdateUser(updateUser);
				awardApprovedEquipment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardApprovedEquipment = awardDao.saveOrUpdateAwardApprovedEquipment(awardApprovedEquipment);
				newAwardApprovedEquipment.add(awardApprovedEquipment);
			});
		} catch (Exception e) {
			logger.info("Error occured in copyAwardApprovedEquipment : {}", e.getMessage());
			throw e;
		}
	}

	private void copyAwardAcheivements(Award copyAward, List<AwardAcheivements> awardAcheivements, String updateUser) {
		List<AwardAcheivements> newAwardAcheivements = new ArrayList<>();
		awardAcheivements.stream().forEach(copiedAwardAcheivements -> {
			AwardAcheivements awardAcheivement = new AwardAcheivements();
			awardAcheivement.setAwardId(copyAward.getAwardId());
			awardAcheivement.setAwardNumber(copyAward.getAwardNumber());
			awardAcheivement.setSequenceNumber(copyAward.getSequenceNumber());
			awardAcheivement.setFileName(copiedAwardAcheivements.getFileName());
			awardAcheivement.setMimeType(copiedAwardAcheivements.getMimeType());
			awardAcheivement.setComment(copiedAwardAcheivements.getComment());
			if (copiedAwardAcheivements.getFileDataId() != null) {
				FileData fileData = commonDao.getFileDataById(copiedAwardAcheivements.getFileDataId());
				FileData dataCopy = commonDao.saveFileData(fileData);
				awardAcheivement.setFileDataId(dataCopy.getFileDataId());
			}
			awardAcheivement.setUpdateUser(updateUser);
			awardAcheivement.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardProjectOutcomeDao.saveOrUpdateAwardAcheivements(awardAcheivement);
			newAwardAcheivements.add(awardAcheivement);
		});
	}

	private void copyAwardAssociation(Award copyAward, List<AwardAssociation> awardAssociations, String updateUser) {	
		List<AwardAssociation> newAwardAssociations = new ArrayList<>();	
		awardAssociations.stream().forEach(copiedAwardAssociation -> {	
			AwardAssociation awardAssociation = new AwardAssociation();	
			awardAssociation.setAwardId(copyAward.getAwardId());	
			awardAssociation.setAwardNumber(copyAward.getAwardNumber());	
			awardAssociation.setSequenceNumber(copyAward.getSequenceNumber());	
			awardAssociation.setAssociationTypeCode(copiedAwardAssociation.getAssociationTypeCode());	
			awardAssociation.setAwardAssociationType(copiedAwardAssociation.getAwardAssociationType());	
			awardAssociation.setAssociatedProjectId(copiedAwardAssociation.getAssociatedProjectId());	
			awardAssociation.setUpdateUser(updateUser);	
			awardAssociation.setUpdateTimestamp(commonDao.getCurrentTimestamp());	
			awardProjectOutcomeDao.saveOrUpdateAwardAssociation(awardAssociation);	
			if (copiedAwardAssociation.getAwardAssociationDetail() != null) {	
				AwardAssociationDetail copiedAwardAssociationDetail = copiedAwardAssociation.getAwardAssociationDetail();	
				AwardAssociationDetail awardAssociationDetail = new AwardAssociationDetail();	
				awardAssociation.setAwardAssociationDetail(copiedAwardAssociation.getAwardAssociationDetail());	
				awardAssociationDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());	
				awardAssociationDetail.setAwardAssociation(awardAssociation);
				awardAssociationDetail.setTitle(copiedAwardAssociationDetail.getTitle());	
				awardAssociationDetail.setSponsorCode(copiedAwardAssociationDetail.getSponsorCode());	
				awardAssociationDetail.setContactAdminPersonId(copiedAwardAssociationDetail.getContactAdminPersonId());	
				awardAssociationDetail.setLeadUnit(copiedAwardAssociationDetail.getLeadUnit());	
				awardAssociationDetail.setPiName(copiedAwardAssociationDetail.getPiName());	
				awardAssociationDetail.setPrimeSponsorCode(copiedAwardAssociationDetail.getPrimeSponsorCode());	
				awardAssociationDetail.setRolodexId(copiedAwardAssociationDetail.getRolodexId());	
				awardAssociationDetail.setUpdateUser(updateUser);	
				awardAssociationDetail.setSponsorAwardNumber(copiedAwardAssociationDetail.getSponsorAwardNumber());
				awardAssociationDetail.setStatusDescription(copiedAwardAssociationDetail.getStatusDescription());
				awardAssociationDetail.setFundingSchemeCode(copiedAwardAssociationDetail.getFundingSchemeCode());	
				awardAssociationDetail.setFundingScheme(copiedAwardAssociationDetail.getFundingScheme());	
				awardAssociationDetail.setTotalProjectCost(copiedAwardAssociationDetail.getTotalProjectCost());	
				awardProjectOutcomeDao.saveAwardAssociationDetail(awardAssociationDetail);	
				newAwardAssociations.add(awardAssociation);	
			}	
		});	
	}

	private void copyAwardPublications(Award copyAward, List<AwardPublications> awardPublications, String updateUser) {
		List<AwardPublications> newAwardPublications = new ArrayList<>();
		awardPublications.stream().forEach(copiedAwardPublication -> {
			AwardPublications awardPublication = new AwardPublications();
			awardPublication.setAwardId(copyAward.getAwardId());
			awardPublication.setAwardNumber(copyAward.getAwardNumber());
			awardPublication.setSequenceNumber(copyAward.getSequenceNumber());
			awardPublication.setPublicationId(copiedAwardPublication.getPublicationId());
			awardPublication.setPublication(copiedAwardPublication.getPublication());
			awardPublication.setUpdateUser(updateUser);
			awardPublication.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardProjectOutcomeDao.saveOrUpdateAwardPublications(awardPublication);
			newAwardPublications.add(awardPublication);
		});
	}

	private void copyAwardSponsorTerm(Award copyAward, List<AwardSponsorTerm> awardSponsorTerms, String updateUser) {
		List<AwardSponsorTerm> newAwardSponsorTerm = new ArrayList<>();
		awardSponsorTerms.stream().forEach(copiedAwardSponsorTerm -> {
			AwardSponsorTerm awardSponsorTerm = new AwardSponsorTerm();
			awardSponsorTerm.setAwardId(copyAward.getAwardId());
			awardSponsorTerm.setAward(copyAward);
			awardSponsorTerm.setAwardNumber(copyAward.getAwardNumber());
			awardSponsorTerm.setSequenceNumber(copyAward.getSequenceNumber());
			awardSponsorTerm.setSponsorTermTypeCode(copiedAwardSponsorTerm.getSponsorTermTypeCode());
			awardSponsorTerm.setSponsorTermCode(copiedAwardSponsorTerm.getSponsorTermCode());
			awardSponsorTerm.setUpdateUser(updateUser);
			awardSponsorTerm.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardSponsorTerms(awardSponsorTerm);
			newAwardSponsorTerm.add(awardSponsorTerm);
		});
	}

	private void copyAwardReportTerms(Award copyAward, List<AwardReportTerms> awardReportTerms, String updateUser, Boolean isAwardHierarchy) {
		for (AwardReportTerms copiedAwardReportTerms : awardReportTerms) {
			AwardReportTerms awardReportTerm = new AwardReportTerms();
			awardReportTerm.setAwardId(copyAward.getAwardId());
			awardReportTerm.setAward(copyAward);
			awardReportTerm.setAwardNumber(copyAward.getAwardNumber());
			awardReportTerm.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTerm.setReportClassCode(copiedAwardReportTerms.getReportClassCode());
			awardReportTerm.setReportCode(copiedAwardReportTerms.getReportCode());
			awardReportTerm.setFrequencyCode(copiedAwardReportTerms.getFrequencyCode());
			awardReportTerm.setFrequencyBaseCode(copiedAwardReportTerms.getFrequencyBaseCode());
			awardReportTerm.setOspDistributionCode(copiedAwardReportTerms.getOspDistributionCode());
			awardReportTerm.setDueDate(copiedAwardReportTerms.getDueDate());
			awardReportTerm.setBaseDate(copiedAwardReportTerms.getBaseDate());
			awardReportTerm.setUpdateUser(updateUser);
			awardReportTerm.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			List<AwardReportTermRecipient> awardReportTermRecipient = copiedAwardReportTerms.getAwardReportTermRecipient();
			if (awardReportTermRecipient != null && !awardReportTermRecipient.isEmpty()) {
				awardReportTerm.getAwardReportTermRecipient().addAll(copyAwardReportTermRecipient(copyAward, awardReportTerm, awardReportTermRecipient, updateUser));
			}
			awardDao.saveOrUpdateAwardReports(awardReportTerm);
			List<AwardReportTracking> awardReportTracking = copiedAwardReportTerms.getAwardReportTracking();
			if (awardReportTracking != null && !awardReportTracking.isEmpty()) {
				copyAwardReportTracking(copyAward, awardReportTerm, awardReportTracking, updateUser, isAwardHierarchy);
			}
		}
	}

	private List<AwardReportTracking> copyAwardReportTracking(Award copyAward, AwardReportTerms copiedAwardReportTerms, List<AwardReportTracking> awardReportTrackings, String updateUser, Boolean isAwardHierarchy) {
		List<AwardReportTracking> newAwardReportTrackings = new ArrayList<>();
		awardReportTrackings.stream().forEach(copiedAwardReportTracking ->  {
			AwardReportTracking awardReportTracking = new AwardReportTracking();
			awardReportTracking.setAwardId(copyAward.getAwardId());
			awardReportTracking.setAwardNumber(copyAward.getAwardNumber());
			awardReportTracking.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTracking.setAwardReportTerms(copiedAwardReportTerms);
			awardReportTracking.setStatusCode(copiedAwardReportTracking.getStatusCode());
			awardReportTracking.setActivityDate(copiedAwardReportTracking.getActivityDate());
			awardReportTracking.setComments(copiedAwardReportTracking.getComments());
			awardReportTracking.setPreparerId(copiedAwardReportTracking.getPreparerId());
			awardReportTracking.setCreateUser(copiedAwardReportTracking.getCreateUser());
			awardReportTracking.setCreateDate(copiedAwardReportTracking.getCreateDate());
			awardReportTracking.setDueDate(copiedAwardReportTracking.getDueDate());
			awardReportTracking.setPreparerName(copiedAwardReportTracking.getPreparerName());
			awardReportTracking.setUpdateUser(updateUser);
			if(Boolean.FALSE.equals(isAwardHierarchy))
				awardReportTracking.setProgressReportId(copiedAwardReportTracking.getProgressReportId());
			awardReportTracking.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardReportTracking = awardDao.saveOrUpdateReportTracking(awardReportTracking);
			if(Boolean.FALSE.equals(isAwardHierarchy)) {
				List<AwardReportTrackingFile> awardReportTrackingFile = awardDao.fetchAwardReportTrackingFileBasedOnAwardReportTrackingId(copiedAwardReportTracking.getAwardReportTrackingId());
				if (awardReportTrackingFile != null && !awardReportTrackingFile.isEmpty()) {
					copyAwardReportTrackingFiles(copyAward, awardReportTracking, awardReportTrackingFile, updateUser);
				}
			}		
			newAwardReportTrackings.add(awardReportTracking);
		});
		return newAwardReportTrackings;
	}

	private void copyAwardReportTrackingFiles(Award copyAward, AwardReportTracking awardReportTracking, List<AwardReportTrackingFile> awardReportTrackingFiles, String updateUser) {
		for (AwardReportTrackingFile copiedAwardReportTracking : awardReportTrackingFiles) {
			AwardReportTrackingFile awardReportTrackingFile = new AwardReportTrackingFile();
			awardReportTrackingFile.setAwardId(copyAward.getAwardId());
			awardReportTrackingFile.setAwardNumber(copyAward.getAwardNumber());
			awardReportTrackingFile.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTrackingFile.setAwardReportTermsId(awardReportTracking.getAwardReportTerms().getAwardReportTermsId());
			awardReportTrackingFile.setAwardReportTrackingId(awardReportTracking.getAwardReportTrackingId());
			awardReportTrackingFile.setFileId(copiedAwardReportTracking.getFileId());
			awardReportTrackingFile.setFileName(copiedAwardReportTracking.getFileName());
			awardReportTrackingFile.setUpdateUser(updateUser);
			awardReportTrackingFile.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardReportTrackingFile.setContentType(copiedAwardReportTracking.getContentType());
			awardReportTrackingFile.setDocumentStatusCode(copiedAwardReportTracking.getDocumentStatusCode());
			awardReportTrackingFile.setVersionNumber(copiedAwardReportTracking.getVersionNumber());
			awardDao.saveOrUpdateAwardReportTrackingFile(awardReportTrackingFile);
		}
	}

	private List<AwardReportTermRecipient> copyAwardReportTermRecipient(Award copyAward, AwardReportTerms copiedAwardReportTerms, List<AwardReportTermRecipient> copiedawardReportTermRecipient, String updateUser) {
		List<AwardReportTermRecipient> newAwardReportTermRecipients = new ArrayList<>();
		copiedawardReportTermRecipient.stream().forEach(copiedAwardReportTermRecipient -> {
			AwardReportTermRecipient awardReportTermRecipient = new AwardReportTermRecipient();
			awardReportTermRecipient.setAwardId(copyAward.getAwardId());
			awardReportTermRecipient.setAwardNumber(copyAward.getAwardNumber());
			awardReportTermRecipient.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTermRecipient.setAwardReportTerms(copiedAwardReportTerms);
			awardReportTermRecipient.setRecipientId(copiedAwardReportTermRecipient.getRecipientId());
			awardReportTermRecipient.setUpdateUser(updateUser);
			awardReportTermRecipient.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAwardReportTermRecipients.add(awardReportTermRecipient);
		});
		return newAwardReportTermRecipients;
	}

	private List<AwardSpecialReview> copyAwardSpecialReview(Award copyAward, List<AwardSpecialReview> awardSpecialReview, String updateUser) {
		List<AwardSpecialReview> awardSpecialReviews = new ArrayList<>();
		Map<String, String> nonEmployees = new HashMap<>();
		Map<String, String> persons = new HashMap<>();
		awardSpecialReview.stream().forEach(copiedAwardSpecialReviewDetail ->  {
			AwardSpecialReview awardSpecialReviewDetail = new AwardSpecialReview();
			awardSpecialReviewDetail.setAwardId(copyAward.getAwardId());
			awardSpecialReviewDetail.setAwardNumber(copyAward.getAwardNumber());
			awardSpecialReviewDetail.setSpecialReviewCode(copiedAwardSpecialReviewDetail.getSpecialReviewCode());
			awardSpecialReviewDetail.setSpecialReview(copiedAwardSpecialReviewDetail.getSpecialReview());
			awardSpecialReviewDetail.setApprovalTypeCode(copiedAwardSpecialReviewDetail.getApprovalTypeCode());
			awardSpecialReviewDetail.setSpecialReviewApprovalType(copiedAwardSpecialReviewDetail.getSpecialReviewApprovalType());
			awardSpecialReviewDetail.setExpirationDate(copiedAwardSpecialReviewDetail.getExpirationDate());
			awardSpecialReviewDetail.setProtocolNumber(copiedAwardSpecialReviewDetail.getProtocolNumber());
			awardSpecialReviewDetail.setApplicationDate(copiedAwardSpecialReviewDetail.getApplicationDate());
			awardSpecialReviewDetail.setApprovalDate(copiedAwardSpecialReviewDetail.getApprovalDate());
			awardSpecialReviewDetail.setComments(copiedAwardSpecialReviewDetail.getComments());
			awardSpecialReviewDetail.setUpdateUser(updateUser);
			awardSpecialReviewDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardSpecialReviewDetail.setIsProtocolIntegrated(copiedAwardSpecialReviewDetail.getIsProtocolIntegrated());
			awardDao.saveOrUpdateAwardSpecialReview(awardSpecialReviewDetail);
			awardSpecialReviews.add(awardService.setIntegratedAwardSpecialReviews(persons, nonEmployees, awardSpecialReviewDetail));
		});
		return awardSpecialReviews;
	}

	private List<AwardCostShare> copyAwardCostShare(Award copyAward, List<AwardCostShare> awardCostShares, String updateUser) {
		List<AwardCostShare> newAwardCostShares = new ArrayList<>();
		awardCostShares.stream().forEach(copiedAwardCostShareDetail -> {
			AwardCostShare awardCostShareDetail = new AwardCostShare();
			awardCostShareDetail.setAwardId(copyAward.getAwardId());
			awardCostShareDetail.setAwardNumber(copyAward.getAwardNumber());
			awardCostShareDetail.setSequenceNumber(copyAward.getSequenceNumber());
			awardCostShareDetail.setProjectPeriod(copiedAwardCostShareDetail.getProjectPeriod());
			awardCostShareDetail.setCostSharePercentage(copiedAwardCostShareDetail.getCostSharePercentage());
			awardCostShareDetail.setCostShareTypeCode(copiedAwardCostShareDetail.getCostShareTypeCode());
			awardCostShareDetail.setSource(copiedAwardCostShareDetail.getSource());
			awardCostShareDetail.setDestination(copiedAwardCostShareDetail.getDestination());
			awardCostShareDetail.setCommitmentAmount(copiedAwardCostShareDetail.getCommitmentAmount());
			awardCostShareDetail.setVerificationDate(copiedAwardCostShareDetail.getVerificationDate());
			awardCostShareDetail.setCostShareMet(copiedAwardCostShareDetail.getCostShareMet());
			awardCostShareDetail.setUnitNumber(copiedAwardCostShareDetail.getUnitNumber());
			awardCostShareDetail.setUpdateUser(updateUser);
			awardCostShareDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardCostShare(awardCostShareDetail);
			newAwardCostShares.add(awardCostShareDetail);
		});
		return newAwardCostShares;
	}

	private List<AwardSubContract> copyAwardSubContract(Award copyAward, List<AwardSubContract> awardSubContracts, String updateUser) {
		List<AwardSubContract> newAwardSubContracts = new ArrayList<>();
		awardSubContracts.stream().forEach(copiedAwardSubContractDetail ->  {
			AwardSubContract awardSubContractDetail = new AwardSubContract();
			awardSubContractDetail.setAwardId(copyAward.getAwardId());
			awardSubContractDetail.setAwardNumber(copyAward.getAwardNumber());
			awardSubContractDetail.setSequenceNumber(copyAward.getSequenceNumber());
			awardSubContractDetail.setOrganizationId(copiedAwardSubContractDetail.getOrganizationId());
			awardSubContractDetail.setOrganization(copiedAwardSubContractDetail.getOrganization());
			awardSubContractDetail.setAmount(copiedAwardSubContractDetail.getAmount());
			awardSubContractDetail.setUpdateUser(updateUser);
			awardSubContractDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardSubContract(awardSubContractDetail);
			newAwardSubContracts.add(awardSubContractDetail);
		});
		return newAwardSubContracts;
	}

	private List<AwardContact> copyAwardContacts(Award copyAward, List<AwardContact> awardContacts, String updateUser) {
		List<AwardContact> newAwardContacts = new ArrayList<>();
		awardContacts.stream().forEach(copiedAwardContactDetail -> {
			AwardContact awardContactDetail = new AwardContact();
			awardContactDetail.setAwardId(copyAward.getAwardId());
			awardContactDetail.setAwardNumber(copyAward.getAwardNumber());
			awardContactDetail.setSequenceNumber(copyAward.getSequenceNumber());
			awardContactDetail.setRolodexId(copiedAwardContactDetail.getRolodexId());
			awardContactDetail.setRolodex(copiedAwardContactDetail.getRolodex());
			awardContactDetail.setPersonId(copiedAwardContactDetail.getPersonId());
			awardContactDetail.setFullName(copiedAwardContactDetail.getFullName());
			awardContactDetail.setAwardContactType(copiedAwardContactDetail.getAwardContactType());
			awardContactDetail.setContactTypeCode(copiedAwardContactDetail.getContactTypeCode());
			awardContactDetail.setUpdateUser(updateUser);
			awardContactDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardContact(awardContactDetail);
			newAwardContacts.add(awardContactDetail);
		});
		return newAwardContacts;
	}

	private List<AwardKeyword> copyAwardKeywords(Award copyAward, Award orginalAward, String updateUser) {
		List<AwardKeyword> awardKeywords = orginalAward.getAwardKeywords();
		List<AwardKeyword> copiedAwardKeywords = new ArrayList<>(awardKeywords);
		Collections.copy(copiedAwardKeywords, awardKeywords);
		List<AwardKeyword> newKeywords = new ArrayList<>();
		copiedAwardKeywords.stream().forEach(copiedKeywordDetail -> {
			AwardKeyword keywordDetail = new AwardKeyword();
			keywordDetail.setAward(copyAward);
			keywordDetail.setScienceKeywordCode(copiedKeywordDetail.getScienceKeywordCode());
			keywordDetail.setScienceKeyword(copiedKeywordDetail.getScienceKeyword());
			keywordDetail.setUpdateUser(updateUser);
			keywordDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newKeywords.add(keywordDetail);
		});
		return newKeywords;
	}

	private List<AwardProjectTeam> copyAwardProjectTeams(Award copyAward, List<AwardProjectTeam> awardProjectTeams, String updateUser) {
		List<AwardProjectTeam> newAwardProjectTeams = new ArrayList<>();
		awardProjectTeams.stream().forEach(copiedProjectTeamDetail ->  {
			AwardProjectTeam projectTeamDetail = new AwardProjectTeam();
			projectTeamDetail.setAwardId(copyAward.getAwardId());
			projectTeamDetail.setAwardNumber(copyAward.getAwardNumber());
			projectTeamDetail.setSequenceNumber(copyAward.getSequenceNumber());
			projectTeamDetail.setPersonId(copiedProjectTeamDetail.getPersonId());
			projectTeamDetail.setFullName(copiedProjectTeamDetail.getFullName());
			projectTeamDetail.setProjectRole(copiedProjectTeamDetail.getProjectRole());
			projectTeamDetail.setNonEmployeeFlag(copiedProjectTeamDetail.getNonEmployeeFlag());
			projectTeamDetail.setPercentageCharged(copiedProjectTeamDetail.getPercentageCharged());
			projectTeamDetail.setStartDate(copiedProjectTeamDetail.getStartDate());
			projectTeamDetail.setEndDate(copiedProjectTeamDetail.getEndDate());
			projectTeamDetail.setIsActive(copiedProjectTeamDetail.getIsActive());
			projectTeamDetail.setUpdateUser(updateUser);
			projectTeamDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			projectTeamDetail.setDesignation(copiedProjectTeamDetail.getDesignation());
			awardDao.saveOrUpdateAwardProjectTeam(projectTeamDetail);
			newAwardProjectTeams.add(projectTeamDetail);
		});
		return newAwardProjectTeams;
	}

	private List<AwardPerson> copyAwardPersons(Award copyAward, String updateUser, List<AwardPerson> awardPersons) {
		List<AwardPerson> newAwardPersons = new ArrayList<>();
		awardPersons.stream().forEach(copiedPersonDetail -> {
			AwardPerson personDetail = new AwardPerson();
			personDetail.setAwardId(copyAward.getAwardId());
			personDetail.setAwardNumber(copyAward.getAwardNumber());
			personDetail.setSequenceNumber(copyAward.getSequenceNumber());
			personDetail.setPersonId(copiedPersonDetail.getPersonId());
			personDetail.setRolodexId(copiedPersonDetail.getRolodexId());
			personDetail.setFullName(copiedPersonDetail.getFullName());
			personDetail.setPercentageEffort(copiedPersonDetail.getPercentageEffort());
			personDetail.setUnitNumber(copiedPersonDetail.getUnitNumber());
			personDetail.setUnitName(copiedPersonDetail.getUnitName());
			personDetail.setPersonRoleId(copiedPersonDetail.getPersonRoleId());
			personDetail.setDepartment(copiedPersonDetail.getDepartment());
			personDetail.setProjectRole(copiedPersonDetail.getProjectRole());
			personDetail.setUpdateUser(updateUser);
			personDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			personDetail.setEmailAddress(copiedPersonDetail.getEmailAddress());
			personDetail.setProposalPersonRole(copiedPersonDetail.getProposalPersonRole());
			personDetail.setIsPi(copiedPersonDetail.getIsPi());
			personDetail.setDesignation(copiedPersonDetail.getDesignation());
			List<AwardPersonUnit> units = copiedPersonDetail.getAwardPersonUnits();
			if (units != null && !units.isEmpty()) {
				personDetail.getAwardPersonUnits().addAll(copyAwardPersonUnits(copyAward, copiedPersonDetail, personDetail, updateUser));
			}
			List<AwardPersonAttachment> personAttachments = copiedPersonDetail.getAwardPersonAttachment();
			if (personAttachments != null && !personAttachments.isEmpty()) {
				personDetail.getAwardPersonAttachment().addAll(copyAwardPersonAttachment(copiedPersonDetail, personDetail, updateUser));
			}
			personDetail.setIsMultiPi(copiedPersonDetail.getIsMultiPi());
			awardDao.saveOrUpdateAwardPersons(personDetail);
			newAwardPersons.add(personDetail);
			copyAwardKeyPersonTimesheets(personDetail.getAwardPersonId(), copyAward, copiedPersonDetail.getAwardPersonId(), copiedPersonDetail.getAwardId());
		});
		return newAwardPersons;
	}

	private void copyAwardKeyPersonTimesheets(Integer newAwardPersonId, Award copyAward, Integer awardPersonId, Integer originalAwardId) {
		List<AwardKeyPersonTimesheet> awardKeyPersonTimesheets = timesheetDao.getAwardKeyPersonTimesheetByParams(awardPersonId, originalAwardId);
			awardKeyPersonTimesheets.forEach(awardKeyPersonTimesheet -> {
				AwardKeyPersonTimesheet timesheetData = new AwardKeyPersonTimesheet();
				timesheetData.setYear(awardKeyPersonTimesheet.getYear());
				timesheetData.setTimesheetType(awardKeyPersonTimesheet.getTimesheetType());
				timesheetData.setValue(awardKeyPersonTimesheet.getValue());
				timesheetData.setOrderNumber(awardKeyPersonTimesheet.getOrderNumber());
				timesheetData.setAwardId(copyAward.getAwardId());
				timesheetData.setAwardPersonId(newAwardPersonId);
				timesheetData.setAwardNumber(copyAward.getAwardNumber());
				timesheetDao.saveOrUpdateAwardKeyPersonTimesheet(timesheetData);
			});
	}

	private List<AwardPersonAttachment> copyAwardPersonAttachment(AwardPerson copiedPersonDetail, AwardPerson personDetail, String updateUser) {
		List<AwardPersonAttachment> awardPersonAttachments = copiedPersonDetail.getAwardPersonAttachment();
		List<AwardPersonAttachment> newproposalPersonAttachments = new ArrayList<>();
		awardPersonAttachments.stream().forEach(copiedAwardPersonAttachment -> {
			AwardPersonAttachment attachment = new AwardPersonAttachment();
			attachment.setAwardPerson(personDetail);
			attachment.setDescription(copiedAwardPersonAttachment.getDescription());
			attachment.setFileName(copiedAwardPersonAttachment.getFileName());
			attachment.setMimeType(copiedAwardPersonAttachment.getMimeType());
			FileData fileData = commonDao.getFileDataById(copiedAwardPersonAttachment.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			attachment.setFileDataId(file.getFileDataId());
			attachment.setUpdateUser(updateUser);
			attachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newproposalPersonAttachments.add(attachment);
		});
		return newproposalPersonAttachments;
	}

	private List<AwardPersonUnit> copyAwardPersonUnits(Award copyAward, AwardPerson copiedPersonDetail,
			AwardPerson personDetail, String updateUser) {
		List<AwardPersonUnit> proposalPersonUnits = copiedPersonDetail.getAwardPersonUnits();
		List<AwardPersonUnit> newAwardPersonUnits = new ArrayList<>();
		proposalPersonUnits.stream().forEach(copiedPersonPersonUnit -> {
			AwardPersonUnit personUnit = new AwardPersonUnit();
			personUnit.setAwardId(copyAward.getAwardId());
			personUnit.setAwardNumber(copyAward.getAwardNumber());
			personUnit.setSequenceNumber(copyAward.getSequenceNumber());
			personUnit.setAwardPerson(personDetail);
			personUnit.setUnitNumber(copiedPersonPersonUnit.getUnitNumber());
			personUnit.setLeadUnitFlag(copiedPersonPersonUnit.getLeadUnitFlag());
			personUnit.setUnit(copiedPersonPersonUnit.getUnit());
			personUnit.setUpdateUser(updateUser);
			personUnit.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAwardPersonUnits.add(personUnit);
		});
		return newAwardPersonUnits;
	}

	@Override
	public String createAwardVariationRequest(MultipartFile[] files, String formDataJson, HttpServletRequest request) {
		AwardVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJson, AwardVO.class);
			Award award = awardDao.getAwardDetailsById(vo.getAwardId());
			awardService.canCreateVariationRequest(vo, vo.getAwardNumber(), vo.getServiceRequestTypeCode());
			Boolean canCreateVariationRequest = vo.getCanCreateVariationRequest();
			vo.setUserFullName(AuthenticatedUser.getLoginUserFullName());
			vo.setUserName(AuthenticatedUser.getLoginUserName());
			vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			logger.info("login personId : {}", vo.getPersonId());
			logger.info("userFullName : {}", vo.getUserFullName());
			logger.info("update User : {}", vo.getUpdateUser());
			logger.info("userName : {}", vo.getUserName());
			if (Boolean.TRUE.equals(canCreateVariationRequest) && award != null && award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_ACTIVE)) {
				ServiceRequest serviceRequest = new ServiceRequest();
				serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT);
				serviceRequest.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT));
				serviceRequest.setModuleCode(Constants.AWARD_MODULE_CODE);
				if (Boolean.TRUE.equals(vo.getIsAwardModification())) {
					serviceRequest.setTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
					serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.ALL_SERVICE_REQUEST_TYPE_CODE));
					vo.setServiceRequestTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
				} else if (Boolean.TRUE.equals(vo.getIsVariationRequest())) {
					serviceRequest.setTypeCode(vo.getServiceRequestTypeCode());
					serviceRequest.setServiceRequestType(vo.getServiceRequestType());
				} else if (Boolean.TRUE.equals(vo.getIsProjectClosure())) {
					serviceRequest.setTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
					serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.PROJECT_CLOSURE_TYPE_CODE));
					vo.setServiceRequestTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
				} else if (Boolean.TRUE.equals(vo.getIsAwardOutcome())) {
					serviceRequest.setTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
					serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.AWARD_OUTCOME_TYPE_CODE));
					vo.setServiceRequestTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
				}
				serviceRequest.setModuleItemKey(vo.getAwardId().toString());
				serviceRequest.setSubject(vo.getSubject());
				serviceRequest.setDescription(vo.getDescription());
				serviceRequest.setReporterPersonId(vo.getPersonId());
				serviceRequest.setIsSystemGenerated(Boolean.TRUE);
				serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
				serviceRequest.setCreateUserFullName(personDao.getUserFullNameByUserName(serviceRequest.getCreateUser()));
				vo.setServiceRequest(serviceRequest);
				Integer actionLogId = serviceRequestService.saveServiceRequestActionLog(Constants.NEW_REQUEST_ACTION_CODE, serviceRequest);				
				ServiceRequestStatusHistory serviceRequestStatusHistory = new ServiceRequestStatusHistory();
				serviceRequestStatusHistory.setActionLogId(actionLogId);
				serviceRequestStatusHistory.setActionStartTime(commonDao.getCurrentTimestamp());
				serviceRequestStatusHistory.setStatusCode(serviceRequest.getStatusCode());
				serviceRequestStatusHistory.setServiceRequestId(serviceRequest.getServiceRequestId());
				serviceRequestDao.saveOrUpdateServiceRequestStatusHistory(serviceRequestStatusHistory);
				List<ServiceRequestAttachment> attachments = vo.getNewAttachments();
				if (vo.getNewAttachments() != null && !vo.getNewAttachments().isEmpty()) {
					serviceRequestService.addServiceRequestAttachments(files, serviceRequest, attachments, actionLogId);
				}
				copyAward(vo);
				vo.setPreviousExpirationDate(vo.getAward().getFinalExpirationDate());
				Integer copiedAwardId = vo.getAward().getAwardId();
				addToAwardHistoryLog(vo.getAward(), prepareAwardHistoryLog(new AwardHistoryLog(), vo.getAward()));
				serviceRequest.setOriginatingModuleItemKey(copiedAwardId.toString());
				serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
				vo.setServiceRequest(serviceRequest);
				if (vo.getServiceRequestTypeCode() != null) {
					addAwardEditableFields(vo);
				}
				if (vo.getAward().getLeadUnitNumber() != null && copiedAwardId != null && vo.getPersonId() != null) {
					vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AWARD_MODULE_CODE, vo.getPersonId(), vo.getAward().getLeadUnitNumber(), copiedAwardId));
				}
				vo.setIsGenerateWBSNumber(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION));
				awardService.setSapFeedStatus(vo);
				if (vo.getAward().getAwardNumber() != null) {
					List<String> statusCodes = new ArrayList<>();
					statusCodes.add(Constants.TASK_STATUS_CODE_OPEN);
					statusCodes.add(Constants.TASK_STATUS_CODE_IN_PROGRESS);
					statusCodes.add(Constants.TASK_STATUS_CODE_RETURNED);
					vo.setTaskCount(taskDao.fetchTaskCountBasedOnModuleItemKeyAndTaskStatus(vo.getAward().getAwardNumber(), statusCodes));
				}
			} else {
				vo.setCanCreateVariationRequest(false);
			}
			if (vo != null && vo.getAward() != null) {
				awardService.getPendingAwardDetails(vo, vo.getAward().getAwardNumber(), vo.getAward().getAwardId());
			}
			vo.setCanEnableMilestoneStatus(commonDao.getParameterValueAsBoolean(Constants.SHOW_AWARD_MILESTONE_STATUS));
			vo.setAcProtocolStatuses(complianceDao.getAcProtocolStatus());
			vo.setIrbProtocolStatuses(complianceDao.getIrbProtocolStatus());
			commonDao.doflush();
			sendNotification(vo, Constants.CREATE_VARIATION_REQUEST_NOTIFICATION, new HashSet<>());
		} catch (Exception e) {
			throw new ApplicationException("error occured in createAwardVariationRequest", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void addToAwardHistoryLog(Award award, AwardHistoryLog awardHistoryLog) {
		Integer awardId = awardDao.getLatestArchiveAwardId(award.getAwardNumber());
		if (awardHistoryLog.getLastMergedawardIdCreation() != null) {
			awardHistoryLog.setLastMergedawardIdApproval(awardId);
		} else {
			awardHistoryLog.setLastMergedawardIdCreation(awardId);
		}
		awardHistoryLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardHistoryLog.setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardDao.saveOrUpdateAwardHistoryLog(awardHistoryLog);
	}

	private AwardHistoryLog prepareAwardHistoryLog(AwardHistoryLog awardHistoryLog, Award award) {
		awardHistoryLog.setAwardId(award.getAwardId());
		awardHistoryLog.setAwardNumber(award.getAwardNumber());
		awardHistoryLog.setSequenceNumber(award.getSequenceNumber());
		return awardHistoryLog;
	}

	private void sendNotification(AwardVO vo, Integer createVariationRequestNotification, Set<NotificationRecipient> dynamicEmailRecipients) {	
		EmailServiceVO emailServiceVO = new EmailServiceVO();	
		emailServiceVO.setNotificationTypeId(createVariationRequestNotification);	
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);	
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());	
		emailServiceVO.setModuleItemKey(vo.getAwardId().toString());	
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO.setPlaceHolder(getServiceRequestPlaceholders(vo));	
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {	
			emailServiceVO.setRecipients(dynamicEmailRecipients);	
		}	
		emailService.sendEmail(emailServiceVO);	
	}

	private Map<String, String> getServiceRequestPlaceholders(AwardVO vo) {	
		Map<String, String> placeHolder = new HashMap<>();		
		placeHolder.put("{SERVICE_REQUEST_TYPE}", vo.getAward().getServiceRequestType() == null ? "" : vo.getAward().getServiceRequestType().getDescription());
		return placeHolder;	
	}

	private void copyAwardBudgetHeader(Award orginalAward, Award copyAward, AwardVO vo) {
		List<AwardBudgetHeader> awardBudgetHeaderDetail = new ArrayList<>();
		List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.getAwardBudgetVersionsByAwardId(orginalAward.getAwardId());
		vo.setOriginalAwardId(orginalAward.getAwardId());
		if (awardBudgetHeaders != null && !awardBudgetHeaders.isEmpty()) {
			awardBudgetHeaders.stream().forEach(awardBudgetHeader  -> {
				awardBudgetService.fetchAwardBudgetPeriods(awardBudgetHeader);
				AwardBudgetHeader awardBudgetHeaderData = awardBudgetCopyService.createAwardBudgetHeader(vo, copyAward, awardBudgetHeader, Constants.OTHER_SERVICE_REQUEST_TYPE_CODE, null);
				awardBudgetHeaderData.setFundCode(orginalAward.getAccountNumber());
				awardBudgetHeaderData.setFundCenter(orginalAward.getFundCenter());
				awardBudgetHeaderDetail.add(awardBudgetDao.saveBudgetHeader(awardBudgetHeaderData));
			});
			if (vo.getServiceRequestTypeCode() != null) {
				List<String> sectionTypeCodes = sectionWiseEditDao.getSectionTypeCodeBasedOnTypeCode(vo.getServiceRequestTypeCode());
				if (sectionTypeCodes.contains(Constants.BUDGET_EDITABLE_SECTION_TYPE_CODE)) {
					AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderWithOutErrorInPosting(orginalAward.getAwardId());
					AwardBudgetHeader awardBudgetHeaderData = awardBudgetCopyService.createAwardBudgetHeader(vo, copyAward, awardBudgetHeader, Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE, awardBudgetHeaderDetail);
					awardBudgetHeaderData.setFundCode(orginalAward.getAccountNumber());
					awardBudgetHeaderData.setFundCenter(orginalAward.getFundCenter());
					awardBudgetDao.saveBudgetHeader(awardBudgetHeaderData);
				}
			}
			if (commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED)) {
				copyAwardManpowerDetail(orginalAward.getAwardId(), copyAward);
			}
		}
	}

	private void copyAwardManpowerDetail(Integer originalAwardId, Award copyAward) {
		List<AwardManpower> awardManpowers = manpowerDao.getAwardManpowerDetails(originalAwardId, null);
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(copyAward.getAwardId());
		if (awardBudgetHeader != null) {
			List<AwardBudgetPeriod> awardBudgetPeriods = awardBudgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId());
			awardBudgetPeriods.stream().forEach(awardBudgetPeriod -> {
				List<AwardBudgetDetail> awardBudgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodId(awardBudgetPeriod.getBudgetPeriodId());
				setAwardManpowerDetails(copyAward, awardManpowers, awardBudgetDetails, awardBudgetHeader.getVersionNumber());
			});
			saveAwardManpowerOtherDetail(copyAward, originalAwardId, awardBudgetHeader.getVersionNumber());
	   }
	}

	private void setAwardManpowerDetails(Award copyAward, List<AwardManpower> awardManpowers, List<AwardBudgetDetail> awardBudgetDetails, Integer budgetVersionNumber) {
		awardBudgetDetails.stream().forEach(awardBudgetDetail -> {
			awardManpowers.stream().forEach(awardManpower -> {
			AwardManpower awardManpowerDetail = new AwardManpower();
				if (((awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_CODE_EOM)
						&& awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STAFF))
						|| awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_CODE_RSS)
								&& awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STUDENT))
						&& (awardBudgetDetail.getInternalOrderCode() != null && awardBudgetDetail.getInternalOrderCode().equals(awardManpower.getBudgetReferenceNumber()))
						|| (awardBudgetDetail.getPreviousAwardBudgetDetailId() != null && awardBudgetDetail.getPreviousAwardBudgetDetailId().toString().equals(awardManpower.getBudgetReferenceNumber()))) {
					String budgetReferenceNumber = awardBudgetDetail.getInternalOrderCode() != null ? awardBudgetDetail.getInternalOrderCode() : awardBudgetDetail.getBudgetDetailId().toString();
				setAwardManpower(awardManpowerDetail, budgetReferenceNumber , awardManpower, budgetVersionNumber, copyAward);
				}
			});
		});
	}

	private void saveAwardManpowerOtherDetail(Award award, Integer originalAwardId, Integer budgetVersionNumber) {
		List<AwardManpower> awardManPowers = manpowerDao.getAwardManpowerDetails(originalAwardId, Constants.MANPOWER_TYPE_OTHER);
		AwardManpower awardManpower = null;
		if (awardManPowers != null && !awardManPowers.isEmpty()) {
			awardManpower =awardManPowers.get(0);
		}
		if (awardManpower != null) {
			AwardManpower awardManpowerDetail = new AwardManpower();
			setAwardManpower(awardManpowerDetail, null, awardManpower, budgetVersionNumber, award);
		}
	}

	private void setAwardManpower(AwardManpower awardManpowerDetail, String budgetReferenceNumber, AwardManpower awardManpower, Integer budgetVersionNumber, Award award) {
		awardManpowerDetail.setBudgetReferenceNumber(budgetReferenceNumber);
		awardManpowerDetail.setActualHeadCount(awardManpower.getActualHeadCount());
		awardManpowerDetail.setAwardId(award.getAwardId());
		awardManpowerDetail.setAwardNumber(award.getAwardNumber());
		awardManpowerDetail.setBudgetVersionNumber(budgetVersionNumber);
		awardManpowerDetail.setCreateTimestamp(commonDao.getCurrentTimestamp());
		awardManpowerDetail.setCreateUser(award.getCreateUser());
		awardManpowerDetail.setManpowerTypeCode(awardManpower.getManpowerTypeCode());
		awardManpowerDetail.setSequenceNumber(award.getSequenceNumber());
		awardManpowerDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardManpowerDetail.setUpdateUser(awardManpower.getCreateUser());
		awardManpowerDetail.setBudgetReferenceTypeCode(awardManpower.getBudgetReferenceTypeCode());
		awardManpowerDetail.setModuleCode(awardManpower.getModuleCode());
		awardManpowerDetail.setSubModuleCode(awardManpower.getSubModuleCode());
		awardManpowerDetail = manpowerDao.saveOrUpdateAwardManpower(awardManpowerDetail);
		copyAwardManpowerResource(awardManpowerDetail.getAwardManpowerId(), award, awardManpower.getAwardManpowerId());
	}

	private void copyAwardManpowerResource(Integer awardManpowerId, Award copyAward, Integer oldAwardManpowerId) {
		List<AwardManpowerResource> awardManpowerResourceDetail = manpowerDao.getAwardManpowerResources(oldAwardManpowerId);
		if (awardManpowerResourceDetail != null && !awardManpowerResourceDetail.isEmpty()) {
			awardManpowerResourceDetail.stream().forEach(awardManpowerResource -> {
				AwardManpowerResource awardManpowerResourceData = new AwardManpowerResource();
				awardManpowerResourceData.setAwardManpowerId(awardManpowerId);
				awardManpowerResourceData.setCandidateTitleTypeCode(awardManpowerResource.getCandidateTitleTypeCode());
				awardManpowerResourceData.setChargeDuration(awardManpowerResource.getChargeDuration());
				awardManpowerResourceData.setChargeEndDate(awardManpowerResource.getChargeEndDate());
				awardManpowerResourceData.setChargeStartDate(awardManpowerResource.getChargeStartDate());
				awardManpowerResourceData.setCommittedCost(awardManpowerResource.getCommittedCost());
				awardManpowerResourceData.setCompensationGradeTypeCode(awardManpowerResource.getCompensationGradeTypeCode());
				awardManpowerResourceData.setCostAllocation(awardManpowerResource.getCostAllocation());
				awardManpowerResourceData.setCreateTimestamp(commonDao.getCurrentTimestamp());
				awardManpowerResourceData.setCreateUser(copyAward.getCreateUser());
				awardManpowerResourceData.setDescription(awardManpowerResource.getDescription());
				awardManpowerResourceData.setFullName(awardManpowerResource.getFullName());
				awardManpowerResourceData.setJobProfileTypeCode(awardManpowerResource.getJobProfileTypeCode());
				awardManpowerResourceData.setPersonId(awardManpowerResource.getPersonId());
				awardManpowerResourceData.setPlanCompensationTypeCode(awardManpowerResource.getPlanCompensationTypeCode());
				awardManpowerResourceData.setPlanDuration(awardManpowerResource.getPlanDuration());
				awardManpowerResourceData.setPlanEndDate(awardManpowerResource.getPlanEndDate());
				awardManpowerResourceData.setPlanJobProfileTypeCode(awardManpowerResource.getPlanJobProfileTypeCode());
				awardManpowerResourceData.setPlanStartDate(awardManpowerResource.getPlanStartDate());
				awardManpowerResourceData.setPositionId(awardManpowerResource.getPositionId());
				awardManpowerResourceData.setPositionOwnedByAward(awardManpowerResource.getPositionOwnedByAward());
				awardManpowerResourceData.setPositionStatusCode(awardManpowerResource.getPositionStatusCode());
				awardManpowerResourceData.setPositionTriggerDate(awardManpowerResource.getPositionTriggerDate());
				awardManpowerResourceData.setResourceTypeCode(awardManpowerResource.getResourceTypeCode());
				awardManpowerResourceData.setRolodexId(awardManpowerResource.getRolodexId());
				awardManpowerResourceData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardManpowerResourceData.setUpdateUser(copyAward.getUpdateUser());
				awardManpowerResourceData.setPlannedBaseSalary(awardManpowerResource.getPlannedBaseSalary());
				awardManpowerResourceData.setPlannedSalary(awardManpowerResource.getPlannedSalary());
				awardManpowerResourceData.setResourceUniqueId(awardManpowerResource.getResourceUniqueId());
				awardManpowerResourceData.setAwardNumber(awardManpowerResource.getAwardNumber());
				awardManpowerResourceData.setModuleCode(awardManpowerResource.getModuleCode());
				awardManpowerResourceData.setSubModuleCode(awardManpowerResource.getSubModuleCode());
				awardManpowerResourceData.setFreezeDate(awardManpowerResource.getFreezeDate());
				awardManpowerResourceData.setUpgradeTypeCode(awardManpowerResource.getUpgradeTypeCode());
				awardManpowerResourceData.setMultiplierValueUsed(awardManpowerResource.getMultiplierValueUsed());
				awardManpowerResourceData.setPreviousChargeEndDate(awardManpowerResource.getChargeEndDate());
				awardManpowerResourceData.setPreviousChargeStartDate(awardManpowerResource.getChargeStartDate());
				awardManpowerResourceData.setIsResourceCreatedOrUpdated(Boolean.FALSE);
				awardManpowerResourceData.setBaseSalaryUsed(awardManpowerResource.getBaseSalaryUsed());
				awardManpowerResourceData.setIsRemainingCAFromWBS(awardManpowerResource.getIsRemainingCAFromWBS());
				manpowerDao.saveOrUpdateAwardManpowerResources(awardManpowerResourceData);
			});
		}
	}

	@Override
	public void createAwardFromProposal(AwardLinkInstituteProposalVO vo) {
		if (vo.getIpNumbers() != null) {
			vo.getIpNumbers().stream().forEach(ipNumber -> {
				Integer proposalId = awardDao.getProposalId(ipNumber);
				AwardFundingProposal awardFundingProposal = new AwardFundingProposal();
				awardFundingProposal.setProposalId(proposalId);
				awardFundingProposal.setUpdateUser(vo.getUpdateUser());
				vo.setAwardFundingProposal(awardFundingProposal);
				vo.setDateofAwardRequired(true);
				try {
					awardService.createAwardByFundingProposal(vo);
				} catch (Exception e) {
					logger.info("Error occured in createAwardFromProposal : {}", e.getMessage());
				}
			});
		}
	}

	private Award copyAwardBudgetDetails(Award orginalAward, Award copyAward, AwardVO vo) {
		copyAwardBudgetHeader(orginalAward, copyAward, vo);
		copyAward = awardDao.saveOrUpdateAwardDetails(copyAward);
		AwardBudgetHeader header = awardBudgetDao.getAwardBudgetHeaderByAwardId(copyAward.getAwardId());
		if (header != null && Boolean.TRUE.equals(vo.getIsMasterAwardCreation()) && vo.getBudgetStatusCode() != null) {
			header.setBudgetStatusCode(vo.getBudgetStatusCode());
			header.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(vo.getBudgetStatusCode()));
			awardBudgetDao.saveOrUpdateAwardBudgetOverView(header);
		}
		if (header != null) {
			vo.setIsBudgetCreated(true);
		} else {
			vo.setIsBudgetCreated(false);
		}
		return copyAward;
	}

	@Override
	public void addAwardEditableFields(AwardVO vo) {
		List<String> sectionTypeCodes = sectionWiseEditDao.getSectionTypeCodeBasedOnTypeCode(vo.getServiceRequestTypeCode());
		List<ModuleVariableSection> moduleVariableSections = new ArrayList<>();
		sectionTypeCodes.stream().forEach(sectionCode -> {
			if (sectionWiseEditDao.checkSectionInModule(Constants.AWARD_MODULE_CODE, sectionCode)
				&& (!commonDao.getParameterValueAsBoolean(Constants.ENABLE_SPECIAL_REVIEW) && !sectionCode.equals(Constants.SPECIAL_REVIEW_SECTION_TYPE_CODE)
					|| commonDao.getParameterValueAsBoolean(Constants.ENABLE_SPECIAL_REVIEW))) {
				ModuleVariableSection moduleVariableSection = new ModuleVariableSection();
				moduleVariableSection.setModuleCode(Constants.AWARD_MODULE_CODE);
				moduleVariableSection.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE);
				moduleVariableSection.setModuleItemKey(vo.getAward().getAwardId().toString());
				moduleVariableSection.setSectionCode(sectionCode);
				moduleVariableSection.setSectionType(sectionWiseEditDao.getSectionTypebySectionTypeId(sectionCode));
				moduleVariableSection.setTypeCode(Integer.parseInt(vo.getServiceRequestTypeCode()));
				moduleVariableSection.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				moduleVariableSection.setPersonId(vo.getPersonId());
				moduleVariableSection.setSubModuleItemKey(Constants.ZERO);
				moduleVariableSection.setUpdateUser(vo.getUpdateUser());
				moduleVariableSection.setVariableType(Constants.VARIATION_REQUEST_VARIABLE_TYPE);
				moduleVariableSection = sectionWiseEditDao.saveorUpdateModuleVariableSection(moduleVariableSection);
				moduleVariableSections.add(moduleVariableSection);
			}
		});
		vo.setSectionTypeCodes(moduleVariableSections);
	}

	private void copyAwardKPIs(Award award, List<AwardKPI> awardKPIs, String updateUser) {
		for (AwardKPI newAwardKPI : awardKPIs) {
			List<AwardKPICriteria> awardKPICriterias = new ArrayList<>();
			AwardKPI awardKPI = new AwardKPI();
			 newAwardKPI.getAwardKPICriterias().stream().forEach(newAwardKPICriteria -> {
				AwardKPICriteria awardKPICriteria = new AwardKPICriteria();
				awardKPICriteria.setAwardKPI(awardKPI);
				awardKPICriteria.setKpiCriteriaType(newAwardKPICriteria.getKpiCriteriaType());
				awardKPICriteria.setKpiCriteriaTypeCode(newAwardKPICriteria.getKpiCriteriaTypeCode());
				awardKPICriteria.setTarget(newAwardKPICriteria.getTarget());
				awardKPICriteria.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardKPICriteria.setUpdateUser(updateUser);
				awardKPICriterias.add(awardKPICriteria);
			});
			awardKPI.setKpiType(newAwardKPI.getKpiType());
			awardKPI.setAwardId(award.getAwardId());
			awardKPI.setKpiTypeCode(newAwardKPI.getKpiTypeCode());
			awardKPI.setAwardNumber(award.getAwardNumber());
			awardKPI.setSequenceNumber(award.getSequenceNumber());
			awardKPI.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardKPI.setUpdateUser(updateUser);
			awardKPI.setAwardKPICriterias(awardKPICriterias);
			awardDao.saveOrUpdateAwardKPI(awardKPI);
		}
	}

	private void copyAwardMileStones(Award award, List<AwardMileStone> originalAwardMileStones,Boolean isCopyAward,String updateUser) {
		List<AwardMileStone> copyMileStones = new ArrayList<>();
		originalAwardMileStones.stream().forEach(originalAwardMileStone -> {
			AwardMileStone copyAwardMileStone = new AwardMileStone();
			copyAwardMileStone.setDuration(originalAwardMileStone.getDuration());
			copyAwardMileStone.setMilestone(originalAwardMileStone.getMilestone());
			copyAwardMileStone.setAwardId(award.getAwardId());
			copyAwardMileStone.setStartDate(originalAwardMileStone.getStartDate());
			copyAwardMileStone.setEndDate(originalAwardMileStone.getEndDate());
			copyAwardMileStone.setSequenceNumber(award.getSequenceNumber());
			copyAwardMileStone.setAwardNumber(award.getAwardNumber());
			copyAwardMileStone.setMilestoneNumber(originalAwardMileStone.getMilestoneNumber());
			copyAwardMileStone.setComment(originalAwardMileStone.getComment());
			copyAwardMileStone.setMilestoneStatusCode(originalAwardMileStone.getMilestoneStatusCode());
			copyAwardMileStone.setMilestoneStatus(originalAwardMileStone.getMilestoneStatus());
			if(Boolean.TRUE.equals(isCopyAward)) {
				copyAwardMileStone.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				copyAwardMileStone.setUpdateUser(updateUser);
			} else {
				copyAwardMileStone.setUpdateUser(originalAwardMileStone.getUpdateUser());
				copyAwardMileStone.setUpdateTimeStamp(originalAwardMileStone.getUpdateTimeStamp());
			}
			copyMileStones.add(awardDao.saveOrUpdateAwardMileStone(copyAwardMileStone));
		});
	}

	private void copyQuestionnaireData(Integer orginalAwardId, AwardVO vo) {
		List<Integer> submoduleCodes = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setActionPersonId(vo.getPersonId());
		questionnaireDataBus.setActionUserId(vo.getUserName());
		questionnaireDataBus.setModuleItemCode(Constants.AWARD_MODULE_CODE);
		questionnaireDataBus.setModuleItemKey(orginalAwardId.toString());
		submoduleCodes.add(Constants.AWARD_SUBMODULE_CODE);
		submoduleCodes.add(Constants.AWARD_DMP_SUBMODULE_CODE);
		submoduleCodes.add(Constants.AWARD_PROJECT_CLOSURE_SUBMODULE_CODE);
		submoduleCodes.add(Constants.AWARD_SUMBIT_CLOSURE_SUBMODULE_CODE);
		questionnaireDataBus.getModuleSubItemCodes().addAll(submoduleCodes);
		questionnaireDataBus.setModuleSubItemKey("0");
		questionnaireDataBus.setCopyModuleItemKey(vo.getAwardId().toString());
		questionnaireService.copyQuestionnaireForVersion(questionnaireDataBus);
	}

	@Override
	public String createAwardVariationRequestForWaf(AwardVO vo) {
		MultipartFile multipartFile = null;
		Boolean IsLastUploadedFile = true;
		ServiceRequestAttachment attachment = vo.getServiceRequestAttachment();
		String splicedFile = vo.getFileContent();
		if (vo.getServiceRequestAttachment() != null && splicedFile != null) {
			multipartFile = commonService.uploadMedia(splicedFile, attachment.getFileName(), vo.getRemaining(), vo.getLength(), vo.getFileTimestamp(), vo.getPersonId(), attachment.getContentType());
			IsLastUploadedFile = vo.getIsLastUploadedFile();
		}
		if (multipartFile != null || splicedFile == null) {
			if (Boolean.TRUE.equals(vo.getIsFirstTimeCreation())) {
				awardService.canCreateVariationRequest(vo, vo.getAwardNumber(), vo.getServiceRequestTypeCode());
			}
			Boolean canCreateVariationRequest = vo.getCanCreateVariationRequest();
			if (Boolean.TRUE.equals(canCreateVariationRequest) && Boolean.TRUE.equals(vo.getIsFirstTimeCreation())) {
				if (vo.getServiceRequestId() == null && IsLastUploadedFile) {
					vo.setServiceRequest(createVariationRequestForAward(vo));
					vo.setServiceRequestId(vo.getServiceRequest().getServiceRequestId());
					copyAward(vo);
					vo.setPreviousExpirationDate(vo.getAward().getFinalExpirationDate());
					ServiceRequest serviceRequest = vo.getServiceRequest();
					Integer copiedAwardId = vo.getAward().getAwardId();
					serviceRequest.setOriginatingModuleItemKey(copiedAwardId.toString());
					serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
					addToAwardHistoryLog(vo.getAward(), prepareAwardHistoryLog(new AwardHistoryLog(), vo.getAward()));
				}
				if (vo.getServiceRequestTypeCode() != null) {
					addAwardEditableFields(vo);
				}
			} else if (Boolean.TRUE.equals(vo.getIsFirstTimeCreation()) && Boolean.FALSE.equals(canCreateVariationRequest)) {
				vo.setCanCreateVariationRequest(false);
			}
		}
		if (Boolean.FALSE.equals(vo.getIsFirstTimeCreation()) && Boolean.TRUE.equals(vo.getCanCreateVariationRequest())) {
			vo.setServiceRequest(serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId()));
			vo.setAward(awardDao.getAwardDetailsById(vo.getLatestAwardId()));
		}
		if (Boolean.TRUE.equals(vo.getCanCreateVariationRequest()) && multipartFile != null) {
			addVariationAttachmentsForWaf(multipartFile, vo.getServiceRequest(), vo.getServiceRequestAttachment());
		}
		if (vo.getAward() != null && vo.getAward().getAwardId() != null && Boolean.TRUE.equals(vo.getIsLastRequest()) && Boolean.TRUE.equals(vo.getCanCreateVariationRequest())) {
			awardService.getPendingAwardDetails(vo, vo.getAward().getAwardNumber(), vo.getAward().getAwardId());
			vo.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(vo.getAward().getAwardId().toString(), Constants.ZERO, Constants.AWARD_MODULE_CODE, vo.getPersonId(), Constants.AWARD_SUBMODULE_CODE));
			if (vo.getAward().getLeadUnitNumber() != null && vo.getAward().getAwardId() != null && vo.getPersonId() != null) {
				vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AWARD_MODULE_CODE, vo.getPersonId(), vo.getAward().getLeadUnitNumber(), vo.getAward().getAwardId()));
			}
		}
		vo.setCanEnableMilestoneStatus(commonDao.getParameterValueAsBoolean(Constants.SHOW_AWARD_MILESTONE_STATUS));
		return commonDao.convertObjectToJSON(vo);
	}

	private void addVariationAttachmentsForWaf(MultipartFile multipartFile, ServiceRequest serviceRequest, ServiceRequestAttachment newAttachment) {
		List<ServiceRequestAttachment> attachments = serviceRequestDao.fetchServiceRequestAttachmentBasedOnSRId(serviceRequest.getServiceRequestId());
		Integer documentId = 0;
		if (attachments != null && !attachments.isEmpty()) {
			Collections.sort(attachments,
					(attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1 : attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
			documentId = attachments.get(0).getDocumentId();
		}
		Integer versionNumber = 0;
		File file = new File(multipartFile.getOriginalFilename());
		String fileName = file.getName();
		if (newAttachment.getFileName().equals(fileName)) {
			documentId = documentId + 1;
			ServiceRequestAttachment serviceRequestAttachment = serviceRequestService.addNewServiceRequestAttachment(newAttachment, multipartFile, fileName,
					versionNumber, documentId);
			serviceRequestAttachment.setServiceRequestId(serviceRequest.getServiceRequestId());
			serviceRequestDao.saveOrUpdateServiceRequestAttachment(serviceRequestAttachment);
		}
	}

	private ServiceRequest createVariationRequestForAward(AwardVO vo) {
		ServiceRequest serviceRequest = new ServiceRequest();
		serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT);
		serviceRequest.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT));
		serviceRequest.setModuleCode(Constants.AWARD_MODULE_CODE);
		if (Boolean.TRUE.equals(vo.getIsAwardModification())) {
			serviceRequest.setTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
			serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.ALL_SERVICE_REQUEST_TYPE_CODE));
			vo.setServiceRequestTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
		} else if (Boolean.TRUE.equals(vo.getIsVariationRequest())) {
			serviceRequest.setTypeCode(vo.getServiceRequestTypeCode());
			serviceRequest.setServiceRequestType(vo.getServiceRequestType());
		} else if (Boolean.TRUE.equals(vo.getIsProjectClosure())) {
			serviceRequest.setTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
			serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.PROJECT_CLOSURE_TYPE_CODE));
			vo.setServiceRequestTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
		} else if (Boolean.TRUE.equals(vo.getIsAwardOutcome())) {
			serviceRequest.setTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
			serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.AWARD_OUTCOME_TYPE_CODE));
			vo.setServiceRequestTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
		}
		serviceRequest.setModuleItemKey(vo.getAwardId().toString());
		serviceRequest.setSubject(vo.getSubject());
		serviceRequest.setDescription(vo.getDescription());
		serviceRequest.setReporterPersonId(vo.getPersonId());
		serviceRequest.setIsSystemGenerated(Boolean.TRUE);
		serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
		return serviceRequest;
	}

	private void copyAwardFNADistributionDetails(Award orginalAward, Award copyAward) {
		datesAndAmountDao.getAwardAmountFNADistributionBasedOnAwardId(orginalAward.getAwardId()).stream().
		forEach(copiedAwardFNADistributions ->  {
			AwardAmountFNADistribution awardFNADistribution = new AwardAmountFNADistribution();
			awardFNADistribution.setAwardId(copyAward.getAwardId());
			awardFNADistribution.setAwardNumber(copyAward.getAwardNumber());
			awardFNADistribution.setSequenceNumber(copyAward.getSequenceNumber());
			awardFNADistribution.setBudgetPeriod(copiedAwardFNADistributions.getBudgetPeriod());;
			awardFNADistribution.setStartDate(copiedAwardFNADistributions.getStartDate());
			awardFNADistribution.setEndDate(copiedAwardFNADistributions.getEndDate());
			awardFNADistribution.setTotalDirectCost(copiedAwardFNADistributions.getTotalDirectCost());
			awardFNADistribution.setTotalIndirectCost(copiedAwardFNADistributions.getTotalIndirectCost());
			awardFNADistribution.setCreateTimeStamp(copiedAwardFNADistributions.getCreateTimeStamp());
			awardFNADistribution.setCreateUser(copiedAwardFNADistributions.getCreateUser());
			awardFNADistribution.setComment(copiedAwardFNADistributions.getComment());
			awardFNADistribution.setUpdateUser(AuthenticatedUser.getLoginUserName());
			awardFNADistribution.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			datesAndAmountDao.saveOrUpdateAwardAmountFNADistribution(awardFNADistribution);
		});
	}
}
