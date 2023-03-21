package com.polus.fibicomp.award.service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.Period;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.awardreporttracking.scheduling.sequence.DefaultScheduleSequence;
import com.polus.fibicomp.award.awardreporttracking.scheduling.sequence.ScheduleSequence;
import com.polus.fibicomp.award.awardreporttracking.scheduling.sequence.TrimDatesScheduleSequenceDecorator;
import com.polus.fibicomp.award.awardreporttracking.scheduling.sequence.WeekScheduleSequenceDecorator;
import com.polus.fibicomp.award.awardreporttracking.scheduling.sequence.XMonthlyScheduleSequenceDecorator;
import com.polus.fibicomp.award.awardreporttracking.scheduling.service.ScheduleService;
import com.polus.fibicomp.award.awardreporttracking.scheduling.util.Time24HrFmt;
import com.polus.fibicomp.award.awardworkflow.dao.AwardWorkflowDao;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.datesandamounts.service.DatesAndAmountService;
import com.polus.fibicomp.award.dto.SponsorTermCodeData;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardApprovedEquipment;
import com.polus.fibicomp.award.pojo.AwardAprovedForeignTravel;
import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardComment;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardHierarchy;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardKeyword;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonAttachment;
import com.polus.fibicomp.award.pojo.AwardPersonRoles;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardReportReminder;
import com.polus.fibicomp.award.pojo.AwardReportTermRecipient;
import com.polus.fibicomp.award.pojo.AwardReportTerms;
import com.polus.fibicomp.award.pojo.AwardReportTracking;
import com.polus.fibicomp.award.pojo.AwardReportTrackingFile;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSponsorTerm;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.pojo.Frequency;
import com.polus.fibicomp.award.pojo.Report;
import com.polus.fibicomp.award.pojo.ReportStatus;
import com.polus.fibicomp.award.pojo.SponsorReport;
import com.polus.fibicomp.award.pojo.SponsorTermReport;
import com.polus.fibicomp.award.pojo.ValidReportClass;
import com.polus.fibicomp.award.version.service.AwardVersionService;
import com.polus.fibicomp.award.vo.AwardAttachmentsVO;
import com.polus.fibicomp.award.vo.AwardCostShareVO;
import com.polus.fibicomp.award.vo.AwardDatesandAmountVO;
import com.polus.fibicomp.award.vo.AwardDetailsVO;
import com.polus.fibicomp.award.vo.AwardGeneralDetailsVO;
import com.polus.fibicomp.award.vo.AwardHierarchyVO;
import com.polus.fibicomp.award.vo.AwardKPIVO;
import com.polus.fibicomp.award.vo.AwardLinkInstituteProposalVO;
import com.polus.fibicomp.award.vo.AwardMilestoneVO;
import com.polus.fibicomp.award.vo.AwardPaymentAndInvoicesVO;
import com.polus.fibicomp.award.vo.AwardSpecialReviewVO;
import com.polus.fibicomp.award.vo.AwardSubContractVO;
import com.polus.fibicomp.award.vo.AwardSummaryDetailsVO;
import com.polus.fibicomp.award.vo.AwardSummaryVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.award.vo.MaintainAwardHierarchyVO;
import com.polus.fibicomp.award.vo.PersonnelVO;
import com.polus.fibicomp.award.vo.ReportTermsVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.compilance.dao.ComplianceDao;
import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.service.CustomDataElementService;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dao.GrantCallKPIDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.ip.dao.InstitutionalProposalDao;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalKeywords;
import com.polus.fibicomp.ip.pojo.InstituteProposalPerson;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonAttachment;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonUnit;
import com.polus.fibicomp.ip.pojo.InstituteProposalProjectTeam;
import com.polus.fibicomp.ip.pojo.InstituteProposalResearchArea;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.timesheetdetail.dao.TimesheetDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.service.ProgressReportService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.ProposalKPI;
import com.polus.fibicomp.proposal.pojo.ProposalKPICriteria;
import com.polus.fibicomp.proposal.pojo.ProposalMileStone;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.roles.service.RoleManagementService;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.sectionwiseedit.dao.SectionWiseEditDao;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatusHistory;
import com.polus.fibicomp.servicerequest.service.ServiceRequestService;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;
import com.polus.fibicomp.task.dao.TaskDao;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskActionLog;
import com.polus.fibicomp.task.pojo.TaskType;
import com.polus.fibicomp.task.service.TaskService;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Service(value = "awardService")
public class AwardServiceImpl implements AwardService {

	protected static Logger logger = LogManager.getLogger(AwardServiceImpl.class.getName());

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ScheduleService scheduleService;

	@Autowired
	private AuthorizationService authorizationService;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	BusinessRuleDao businessRuleDao;

	@Autowired
	private AwardWorkflowDao awardWorkflowDao;

	@Autowired
	private EmailService emailService;
	
	@Autowired
	private AwardVersionService awardVersionService;

	@Autowired
	private InstitutionalProposalDao institutionalProposalDao;

	@Autowired
	private PrintService printService;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;

	@Autowired
	private RolodexDao rolodexDao;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private DatesAndAmountService datesAndAmountService;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private RoleManagementService roleManagementService;

	@Value("${system.timezone}")
	private String timezone;

	@Autowired
	private ProposalDao proposalDao;
	
	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private GrantCallKPIDao grantCallKPIDao;

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private TaskService taskService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	@Qualifier(value = "rolesManagement")
	private RolesManagementDao rolesManagementDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	public ServiceRequestService serviceRequestService;

	@Autowired
	private CustomDataElementService customDataElementService;

	private static final String MIMETYPE = "application/pdf";

	@Autowired
	private ProgressReportService progressReportService;

	@Autowired
	private TimesheetDao timesheetDao;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private ComplianceDao complianceDao;

	private static final String DELETE_AWARD_RIGHT_NAME = "DELETE_AWARD";
	
	private static final String MESSAGE = "message";

	@Override
	public String getAwardSummaryData(String awardId) throws Exception {
		AwardDetailsVO awardDetailsVO = awardDao.fetchAwardSummaryData(awardId);
		return commonDao.convertObjectToJSON(awardDetailsVO);
	}

	@Override
	public String getAwardHierarchyData(String awardNumber, String selectedAwardNumber) throws Exception {
		AwardHierarchyVO awardHierarchyVO = awardDao.fetchAwardHierarchyData(awardNumber, selectedAwardNumber);
		return commonDao.convertObjectToJSON(awardHierarchyVO);
	}

	@Override
	public String saveAwardDetails(AwardVO awardVo) throws Exception {
		Award award = awardVo.getAward();
		Integer awardId = award.getAwardId();
		Boolean isFirstTimeCreation = false;
		if (award.getAwardId() == null) {
			isFirstTimeCreation = true;
		}
		if (award.getSequenceNumber() == null || award.getSequenceNumber() == 0) {
			award.setSequenceNumber(1);
			award.setAwardNumber(generateNextAwardNumber());
			award.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_PENDING);
			award.setAwardDocumentTypeCode(Constants.AWARD_SETUP);
			award.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_SETUP));
			award.setIsLatest(true);
		}
		award.setLeadUnit(commonDao.getLeadUnitByUnitNumber(award.getLeadUnitNumber()));
		award.setDocumentUpdateUser(award.getUpdateUser());
		award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		award = awardDao.saveOrUpdateAwardDetails(award);
		awardVo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardVo.setAward(award);
		if(awardVo.getIsGrantCallChanged()) {
			Boolean flag = awardDao.checkAwardReportTermsExist(award.getAwardId());
			if(flag.equals(Boolean.FALSE))
				flag = awardDao.checkAwardSponsorTermsExist(award.getAwardId());
			awardVo.setIsReportTermsExist(flag);
		}
		if(award.getCreateUser() != null && isFirstTimeCreation) {
			award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
			List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.AWARD_MODULE_CODE);
			if (derivedRoles != null && !derivedRoles.isEmpty()) {
				assignDerivedRolesForCreator(award, personDao.getPersonIdByUserName(award.getCreateUser()), derivedRoles);
			}
		}
		if(award.getUpdateUser() != null) {
			award.setUpdateUserFullName(personDao.getUserFullNameByUserName(award.getUpdateUser()));
		}
		updateAwardBudgetFundCodeOnAwardSave(award);
		if (Boolean.TRUE.equals(isFirstTimeCreation) && award.getPrincipalInvestigatorId() != null) {
			addPrincipalInvestigatorToAward(awardVo);
		}
		String updateType = awardVo.getUpdateType();
		if (updateType != null && updateType.equals("SAVE")) {
			moveDataToAwardHierarchy(award);
			awardVo.setMessage("Award saved successfully");
		} else {
			if(awardDao.checkAwardHierarchyExisted(award).isEmpty()) {
				moveDataToAwardHierarchy(award);
			}
			awardVo.setMessage("Award updated successfully");
		}
		awardVo.setAward(award);
		if (Boolean.FALSE.equals(isFirstTimeCreation)) {
			award.setAwardPersons(awardDao.getAwardPersonList(award.getAwardId()));
		} else  {
			awardVo.setAwardPersons(award.getAwardPersons());
		}
		String leadUnit = award.getLeadUnitNumber();
		if (leadUnit != null) {
			String loginPersonId = awardVo.getPersonId();
			awardVo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AWARD_MODULE_CODE, loginPersonId, leadUnit, awardId));
		}
		if (awardId != null) {
			awardVo.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId.toString(), Constants.ZERO, Constants.AWARD_MODULE_CODE, awardVo.getPersonId(), Constants.AWARD_SUBMODULE_CODE));
			List<Integer> taskIds = taskDao.getPersonTaskIds(awardId.toString(), Constants.AWARD_MODULE_CODE, awardVo.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
			if (taskIds != null && !taskIds.isEmpty()) {
				for (Integer taskId : taskIds) {
					List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(awardId.toString(), taskId.toString(), Constants.AWARD_MODULE_CODE, awardVo.getPersonId(), Constants.AWARD_TASK_SUBMODULE_CODE);
					if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
						awardVo.getSectionTypeCodes().addAll(moduleVariableSections);
					}
				}
			}
		}
		awardVo.setIsGenerateWBSNumber(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION));
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			awardVo.setKpiTypes(grantCallKPIDao.fetchAllKPIs());
			awardVo.setAwardKpis(awardDao.fetchAllAwardKPI(awardId));
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {
			awardVo.setAwardMileStones(awardDao.fetchAwardMileStonesBasedOnAwardId(awardId));
		}
		setAwardSponsorDetail(award);
		awardVo.setCanEnableMilestoneStatus(commonDao.getParameterValueAsBoolean(Constants.SHOW_AWARD_MILESTONE_STATUS));
		return commonDao.convertObjectToJSON(awardVo);
	}

	private void addPrincipalInvestigatorToAward(AwardVO awardVo) {
		Award award = awardVo.getAward();
		String personId = award.getPrincipalInvestigatorId();
		AwardPerson awardPerson = new AwardPerson();
		awardPerson.setAwardId(award.getAwardId());
		awardPerson.setAwardNumber(award.getAwardNumber());
		awardPerson.setSequenceNumber(award.getSequenceNumber());
		if (Boolean.TRUE.equals(awardVo.getIsNonEmployee())) {
			Rolodex rolodex = rolodexDao.getRolodexDetailById(Integer.parseInt(personId));
			awardPerson.setRolodexId(rolodex.getRolodexId());
			awardPerson.setFullName(rolodex.getFullName());
			awardPerson.setEmailAddress(rolodex.getEmailAddress());
			awardPerson.setDesignation(rolodex.getDesignation());
		} else {
			Person personDetails = personDao.getPersonDetailById(personId);
			awardPerson.setPersonId(personId);
			awardPerson.setFullName(personDetails.getFullName());
			awardPerson.setEmailAddress(personDetails.getEmailAddress());
			awardPerson.setDesignation(personDetails.getPrimaryTitle());
		}
		awardPerson.setIsPi(true);
		awardPerson.setPercentageEffort(Constants.PERCENTAGE_OF_EFFORT);
		awardPerson.setPersonRoleId(Constants.PI_ROLE_CODE);
		awardPerson.setProposalPersonRole(proposalDao.fetchProposalPersonRoles(Constants.PI_ROLE_CODE));
		awardPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardPerson.setUpdateUser(awardVo.getUpdateUser());
		AwardPersonUnit awardPersonUnit = new AwardPersonUnit();
		awardPersonUnit.setAwardPerson(awardPerson);
		awardPersonUnit.setAwardId(award.getAwardId());
		awardPersonUnit.setAwardNumber(award.getAwardNumber());
		awardPersonUnit.setSequenceNumber(award.getSequenceNumber());
		awardPersonUnit.setUnitNumber(award.getLeadUnitNumber());
		awardPersonUnit.setUnit(commonDao.getUnitByUnitNumber(award.getLeadUnitNumber()));
		awardPersonUnit.setLeadUnitFlag(true);
		awardPersonUnit.setUpdateUser(awardVo.getUpdateUser());
		awardPersonUnit.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardPerson.getAwardPersonUnits().add(awardPersonUnit);
		awardPerson = awardDao.saveAwardKeyPersonnel(awardPerson);
		award.getAwardPersons().add(awardPerson);
		assignDerivedRolesForPI(awardPerson, award.getAwardId());
	}

	@Override
	public String getAwardLookupData(AwardVO awardVO) throws Exception {
		AwardGeneralDetailsVO detailsVO = getAwardGeneralLookupData();
		return commonDao.convertObjectToJSON(detailsVO);
	}

	private AwardGeneralDetailsVO getAwardGeneralLookupData() {
		AwardGeneralDetailsVO detailsVO = new AwardGeneralDetailsVO();
		detailsVO.setAccountTypes(commonDao.fetchAllAccountTypes());
		detailsVO.setAwardTypes(awardDao.fetchAllAwardTypes());
		detailsVO.setActivitytypes(commonDao.fetchAllActivityTypes());
		detailsVO.setAwardStatus(awardDao.fetchAllAwardStatus());
		detailsVO.setProposalPersonRoles(proposalLookUpDao.fetchAllProposalPersonRoles());
		detailsVO.setAwardContactTypeList(awardDao.getAwardContactTypeList());
		detailsVO.setReviewTypes(commonDao.fetchAllReviewTypes());
		detailsVO.setApprovalStatusTypes(commonDao.fetchAllApprovalStatusTypes());
		detailsVO.setCostShareTypes(commonDao.fetchAllCostShareTypes());
		detailsVO.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT));
		detailsVO.setResearchTypes(commonDao.fetchAllResearchTypes());
		detailsVO.setIsGenerateWBSNumber(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION));
		detailsVO.setMilestoneStatus(awardDao.getAwardMilestoneStatus());
		detailsVO.setAcProtocolStatuses(complianceDao.getAcProtocolStatus());
		detailsVO.setIrbProtocolStatuses(complianceDao.getIrbProtocolStatus());
		return detailsVO;
	}

	@Override
	public List<Organization> findOrganizationList(String searchString) {
		List<Organization> organizations = commonDao.fetchOrganizationList(searchString);
		List<Integer> rolodexId = organizations.stream().map(organization -> organization.getContactAddressId()).collect(Collectors.toList());
		if(!rolodexId.isEmpty()){
			List<Rolodex> rolodex = commonDao.getRolodexDetailByRolodexId(rolodexId);
			Map<Integer, String> collect = rolodex.stream().filter(item -> item.getFullName() != null).collect(Collectors.toMap(Rolodex :: getRolodexId, Rolodex :: getFullName));
			organizations.stream()
					.filter(item -> collect.containsKey(item.getContactAddressId()))
					.forEach(item -> item.setContactPersonName(collect.get(item.getContactAddressId())));
		}
		return organizations;
	}

	@Override
	public String getAwardDetails(AwardVO awardVo) throws Exception {
		Integer awardId = awardVo.getAwardId();
		String loginPersonId = awardVo.getPersonId();
		Award award = new Award();
		if (awardId != null) {
			award = awardDao.getAwardDetailsById(awardId);
		} else {
			award = awardDao.fetchActiveAwardByAwardNumber(awardVo.getAwardNumber());
			awardId = award.getAwardId();
			awardVo.setAwardId(awardId);
		}
		String awardNumber = award.getAwardNumber();
		Integer awardSequenceNumber = award.getSequenceNumber();
		if (award != null && award.getLeadUnitNumber() != null) {
			awardVo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AWARD_MODULE_CODE, loginPersonId, award.getLeadUnitNumber(), awardId));
		}
		if (award.getGrantHeaderId() != null) {
			award.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(award.getGrantHeaderId()));
		}
		List<AwardFundingProposal> awardFundingProposals = awardDao.getAwardFundingProposals(awardId);
		awardFundingProposals.forEach(awardFundingProposal -> {
			InstituteProposal instituteProposal = awardFundingProposal.getProposal();
			if (instituteProposal.getSponsor() != null) {
				instituteProposal.setSponsorName(commonService.getSponsorFormatBySponsorDetail(instituteProposal.getSponsor().getSponsorCode(), instituteProposal.getSponsor().getSponsorName(), instituteProposal.getSponsor().getAcronym()));
			}
			if (instituteProposal.getPrimeSponsor() != null) {
				instituteProposal.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(instituteProposal.getPrimeSponsor().getSponsorCode(), instituteProposal.getPrimeSponsor().getSponsorName(), instituteProposal.getPrimeSponsor().getAcronym()));
			}
		});
		awardVo.setAwardFundingProposals(awardFundingProposals);
		awardVo.setAward(awardDao.getAwardDetailsById(awardId));
		awardVo.setAwardPersons(awardDao.getAwardPersonList(awardId));
		awardVo.setAwardProjectTeams(awardDao.getAwardProjectTeamList(awardId));
		awardVo.setAwardContacts(prepareAwardContactList(awardId));
		awardVo.setAwardSpecialReviews(prepareAwardSpecialReviewDetail(awardId));
		awardVo.setAwardSubContracts(awardDao.getSubContractsByAwardId(awardId));
		awardVo.setAwardCostShares(datesAndAmountDao.getCostShareTypesByAwardId(awardId));
		awardVo.setAwardResearchAreas(awardDao.fetchAwardResearchAreaBasedOnAwardId(awardId));
		if(award.getSubmitUser() != null) {
			award.setSubmitUserFullName(personDao.getUserFullNameByUserName(award.getSubmitUser()));
		}
		if(award.getCreateUser() != null) {
			award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
		}
		if(award.getUpdateUser() != null) {
			award.setUpdateUserFullName(personDao.getUserFullNameByUserName(award.getUpdateUser()));
		}
		if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_VARIATION) || award.getAwardDocumentTypeCode().equals(Constants.AWARD_MODIFICATION)) {
			Award awardData = awardDao.fetchActiveAwardByAwardNumber(award.getAwardNumber());
			if (awardData != null && award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_PENDING)) {
				awardVo.setServiceRequest(awardDao.getServiceRequestBasedOnAwardId(awardData.getAwardId().toString(), award.getAwardId().toString()));
				if(awardVo.getServiceRequest() != null)
					awardVo.getServiceRequest().setCreateUserFullName(personDao.getUserFullNameByUserName(awardVo.getServiceRequest().getCreateUser()));
			}
			awardVo.setPreviousExpirationDate(awardData.getFinalExpirationDate());
		}
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(awardId.toString(),Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			awardVo.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(awardId.toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				awardVo.setWorkflowList(workFlows);
			}
			if (workflow.getCurrentStopName() != null && award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
				award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription() + " : " +workflow.getCurrentStopName());
			} else {
				award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription());
			}
		}
		canTakeRoutingAction(awardVo);
		if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_WITHDRAWN)) {
			awardVo.setCanApproveRouting("0");
		} else {
			Integer canApproveRouting = businessRuleDao.canApproveRouting(awardId.toString(), awardVo.getPersonId(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
			awardVo.setCanApproveRouting(canApproveRouting.toString());
		}
		awardVo.setIsFinalApprover(businessRuleDao.workflowfinalApproval(awardId.toString(), awardVo.getPersonId(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE));
		awardVo.setIsSubmit("1");
		getEditableSectionForAward(awardVo, award);
		awardVo.setAwardAmountInfos(datesAndAmountService.prepareAwardAmountInfoForAward(awardId, awardNumber, awardSequenceNumber));
		List<String> statusCodes = new ArrayList<>();
		statusCodes.add(Constants.TASK_STATUS_CODE_OPEN);
		statusCodes.add(Constants.TASK_STATUS_CODE_IN_PROGRESS);
		statusCodes.add(Constants.TASK_STATUS_CODE_RETURNED);
		awardVo.setTaskCount(taskDao.fetchTaskCountBasedOnModuleItemKeyAndTaskStatus(award.getAwardNumber(), statusCodes));
		AwardBudgetHeader header = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardId);
		if (header != null) {
			awardVo.setIsBudgetCreated(true);
		} else {
			awardVo.setIsBudgetCreated(false);
		}
		awardVo.setIsGenerateWBSNumber(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION));
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			awardVo.setKpiTypes(grantCallKPIDao.fetchAllKPIs());
			List<AwardKPI> awardKPIs = awardDao.fetchAllAwardKPI(awardVo.getAwardId());
			List<AwardKPI> sortedAwardKPIs = new ArrayList<>();
			awardKPIs.forEach(kpi -> {
				AwardKPI newAwardKpi = new AwardKPI();
				BeanUtils.copyProperties(kpi, newAwardKpi);
				Comparator<AwardKPICriteria> comparator = Comparator.comparing(criteria -> criteria.getKpiCriteriaType().getDescription());
				newAwardKpi.setAwardKPICriterias(kpi.getAwardKPICriterias().stream().sorted(comparator).collect(Collectors.toList()));	
				sortedAwardKPIs.add(newAwardKpi);
			});		
			awardVo.setAwardKpis(sortedAwardKPIs);
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {
			List<AwardMileStone> awardMilestones = awardDao.fetchAwardMileStonesBasedOnAwardId(awardVo.getAwardId());
			getFullNameOfUser(awardMilestones);
			awardVo.setAwardMileStones(awardMilestones.stream().sorted(Comparator.comparing(AwardMileStone::getStartDate).reversed().thenComparing(AwardMileStone::getMilestone)).collect(Collectors.toList()));
		}
		setSapFeedStatus(awardVo);
		getPendingAwardDetails(awardVo, award.getAwardNumber(), award.getAwardId());
		setAwardSponsorDetail(award);
		awardVo.setCanEnableMilestoneStatus(commonDao.getParameterValueAsBoolean(Constants.SHOW_AWARD_MILESTONE_STATUS));
		return commonDao.convertObjectToJSON(awardVo);
	}

	private List<AwardSpecialReview> prepareAwardSpecialReviewDetail(Integer awardId) {
		List<AwardSpecialReview> awardSpecialReviews = awardDao.getAwardSpecialReviewsByAwardId(awardId);
		List<AwardSpecialReview> specialReviewDetail = new ArrayList<>();
		Map<String, String> nonEmployees = new HashMap<>();
		Map<String, String> persons = new HashMap<>();
		awardSpecialReviews.forEach(awardSpecialReview -> {
			specialReviewDetail.add(setIntegratedAwardSpecialReviews(persons, nonEmployees, awardSpecialReview));
		});
		return specialReviewDetail;
	}

	@Override
	public AwardSpecialReview setIntegratedAwardSpecialReviews(Map<String, String> persons, Map<String, String> nonEmployees, AwardSpecialReview awardSpecialReview) {
		AwardSpecialReview specialReview = new AwardSpecialReview();
		BeanUtils.copyProperties(awardSpecialReview, specialReview);
		if (Boolean.TRUE.equals(specialReview.getIsProtocolIntegrated()) && Constants.SPECIAL_REVIEW_IRB_TYPE_CODE.equals(specialReview.getSpecialReviewCode())
				&& specialReview.getProtocolNumber() != null) {
			IrbProtocol irbProtocol = proposalService.getIrbProtocols(specialReview.getProtocolNumber());
				if (irbProtocol != null) {
					specialReview.setApplicationDate(irbProtocol.getInitialSubmissionDate());
					specialReview.setApprovalDate(irbProtocol.getApprovalDate());
					specialReview.setExpirationDate(irbProtocol.getExpirationDate());
					specialReview.setApprovalTypeCode(irbProtocol.getProtocolStatusCode());
					specialReview.setSpecialReviewApprovalType(proposalService.setApprovalTypeForIrbProtocol(irbProtocol));
					irbProtocol.setFullName(proposalService.setFullNameForProtocol(persons, irbProtocol.getPersonId(), irbProtocol.getNonEmployeeFlag()));
					specialReview.setIrbProtocol(irbProtocol);
				}
			} else if (Boolean.TRUE.equals(specialReview.getIsProtocolIntegrated()) && Constants.SPECIAL_REVIEW_AC_TYPE_CODE.equals(specialReview.getSpecialReviewCode())
					&& specialReview.getProtocolNumber() != null) {
				AcProtocol acProtocol = proposalService.getAcProtocols(specialReview.getProtocolNumber());
				if (acProtocol != null) {
					specialReview.setApplicationDate(acProtocol.getInitialSubmissionDate());
					specialReview.setApprovalDate(acProtocol.getApprovalDate());
					specialReview.setExpirationDate(acProtocol.getExpirationDate());
					specialReview.setApprovalTypeCode(acProtocol.getProtocolStatusCode());
					specialReview.setSpecialReviewApprovalType(proposalService.setApprovalTypeForAcProtocol(acProtocol));
					acProtocol.setFullName(proposalService.setFullNameForProtocol(nonEmployees, acProtocol.getPersonId(), acProtocol.getNonEmployeeFlag()));
					specialReview.setAcProtocol(acProtocol);
				}
			}
		return specialReview;
	}

	public void setAwardSponsorDetail(Award award) {
		if (award.getSponsor() != null) {
			award.setSponsorName(commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
		}
		if (award.getPrimeSponsor() != null) {
			award.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(award.getPrimeSponsor().getSponsorCode(), award.getPrimeSponsor().getSponsorName(), award.getPrimeSponsor().getAcronym()));
		}
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

	@Override
	public void getPendingAwardDetails(AwardVO vo, String awardNumber, Integer awardId) {
		AwardSummaryVO awardSummaryVo = new AwardSummaryVO();
		List<String> sequenceStatuses = new ArrayList<>();
		sequenceStatuses.add(Constants.AWARD_FINAL_STATUS_PENDING);
		List<AwardSummaryDetailsVO> awards = awardDao.fetchAwardByAwardNumbersAndAwardSequenceStatus(awardNumber,
				sequenceStatuses);
		awards.stream().filter(award -> award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_PENDING)
				&& (!awardId.equals(award.getAwardId()))).forEach(award -> {
					if (award.getCreateUser() != null) {
						award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
					}
					awardSummaryVo.getPendingAwards().add(award);
				});
		vo.setPendingAwardsSummary(awardSummaryVo);
	}

	private void getEditableSectionForAward(AwardVO awardVo, Award award) {
		String loginPersonId = awardVo.getPersonId();
		String awardId = award.getAwardId().toString();
		if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_DRAFT)
				|| award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED)
				|| (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_WITHDRAWN)
						&& !award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_CANCELLED))) {
			awardVo.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO, Constants.AWARD_MODULE_CODE, loginPersonId, Constants.AWARD_SUBMODULE_CODE));
		} else if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS) ||
				award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_HOLD)){
			awardVo.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(awardId, Constants.ZERO, Constants.AWARD_MODULE_CODE, null, Constants.AWARD_SUBMODULE_CODE));
		}		
		List<Integer> taskIds = taskDao.getPersonTaskIds(awardId, Constants.AWARD_MODULE_CODE, loginPersonId, Constants.TASK_STATUS_CODE_IN_PROGRESS);
		if (taskIds != null && !taskIds.isEmpty()) {
			for (Integer taskId : taskIds) {
				List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(awardId, taskId.toString(), Constants.AWARD_MODULE_CODE, loginPersonId, Constants.AWARD_TASK_SUBMODULE_CODE);
				if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
					awardVo.getSectionTypeCodes().addAll(moduleVariableSections);
				}
			}
		}	
	}

	@Override
	public void canCreateVariationRequest(AwardVO vo, String awardNumber, String variationTypeCode) {
		Boolean canCreateVariationRequest = true;
		List<String> editableSectionCodes = sectionWiseEditDao.getAwardEditingSectionsBasedOnParams(awardNumber);
		List<String> newEditableSections= sectionWiseEditDao.getSectionTypeCodeBasedOnTypeCode(variationTypeCode);
		if (editableSectionCodes.stream().anyMatch(editableSectionCode -> newEditableSections.contains(editableSectionCode))) {
			canCreateVariationRequest =  false;
			vo.setMessage("The new Variation Request conflicts with another variation that is currently In Progress. Please complete the conflicting Variation Request before creating a new one.");
		}
		if (Boolean.FALSE.equals(canCreateVariationRequest)) {
			editableSectionCodes.parallelStream().forEach(editableSectionCode -> {
				if (newEditableSections.contains(editableSectionCode)) {
					vo.getEditableSectionCodes().add(editableSectionCode);
				}
			});
		}
		if (Boolean.TRUE.equals(canCreateVariationRequest)) {
			String response = awardDao.canCreateVariationRequest(vo.getAwardId(), variationTypeCode);
			if (response.equalsIgnoreCase("TRUE")) {
				canCreateVariationRequest =  true;
			} else {
				canCreateVariationRequest = false;
				vo.setMessage(response);
			}
		}
		vo.setCanCreateVariationRequest(canCreateVariationRequest);
	}

	@Override
	public String saveAwardSpecialReview(AwardSpecialReviewVO vo) throws Exception {
		AwardSpecialReview specialReview = vo.getSpecialReview();
		Integer awardId = vo.getAwardId();
		specialReview = awardDao.saveOrUpdateAwardSpecialReview(specialReview);
		setAwardUpdateUser(awardId, specialReview.getUpdateUser());
		Map<String, String> nonEmployees = new HashMap<>();
		Map<String, String> persons = new HashMap<>();
		vo.setSpecialReview(setIntegratedAwardSpecialReviews(persons,nonEmployees, specialReview));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveAwardCostShare(AwardCostShareVO costShareVO) throws Exception {
		AwardCostShare costShare = costShareVO.getAwardCostShare();
		Integer awardId = costShareVO.getAwardId();
		costShare = awardDao.saveOrUpdateAwardCostShare(costShare);
		setAwardUpdateUser(awardId, costShare.getUpdateUser());
		costShareVO.setAwardCostShare(costShare);
		return commonDao.convertObjectToJSON(costShareVO);
	}

	@Override
	public String saveAwardSubContracts(AwardSubContractVO subContractVO) throws Exception {
		AwardSubContract subContract = subContractVO.getSubContract();
		if (subContract.getOrganization() != null) {
			Organization organization = subContract.getOrganization();
			if (subContract.getOrganizationId() == null) {
				Integer organizationId = commonDao.fetchMaxOrganizationId();
				if (organizationId != null) {
					organization.setOrganizationId(organizationId.toString());
					subContract.setOrganizationId(organization.getOrganizationId());
				}
			}
			if (organization.getOrganizationId() != null) {
				organization = rolodexDao.saveOrUpdateOrganization(organization);
			}
			subContract.setOrganization(organization);
		}
		String updateType = subContractVO.getUpdateType();
		if (updateType != null && updateType.equals("SAVE")) {
			subContractVO.setMessage("Award Sub Contract saved successfully");
		} else {
			subContractVO.setMessage("Award Sub Contract updated successfully");
		}
		subContractVO.setSubContract(awardDao.saveOrUpdateAwardSubContract(subContract));
		updateAwardDocumentUpdateUserAndTimestamp(subContract.getAwardId(), subContract.getUpdateUser());
		return commonDao.convertObjectToJSON(subContractVO);
	}

	@Override
	public String deleteAwardSpecialReview(AwardSpecialReviewVO vo) throws Exception {
		awardDao.deleteAwardSpecialReview(vo.getAwardSpecailReviewId());
		vo.setSpecialReview(null);
		vo.setMessage("Special Review Deleted Successfully");
		updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAwardSubContract(AwardSubContractVO vo) throws Exception {
		awardDao.deleteAwardSubContract(vo.getAwardSubawardId());
		vo.setMessage("Sub Contract Deleted Successfully");
		vo.setSubContract(null);
		updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAwardCostShare(AwardCostShareVO vo) throws Exception {
		awardDao.deleteAwardCostShare(vo.getAwardCostShareId());
		vo.setAwardCostShare(null);
		vo.setMessage("Cost Share Deleted Successfully");
		updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String maintainSpecialApproval(ReportTermsVO reportTermsVO) throws Exception {
		AwardApprovedEquipment awardApprovedEquipment = reportTermsVO.getAwardApprovedEquipment();
		AwardAprovedForeignTravel awardAprovedForeignTravel = reportTermsVO.getAwardAprovedForeignTravel();
		String acType = reportTermsVO.getAcType();
		Timestamp newTimeStamp = commonDao.getCurrentTimestamp();
		if (awardApprovedEquipment != null) {
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeDelete)) {
				awardDao.deleteAwardApprovedEquipment(awardApprovedEquipment);
				reportTermsVO.setMessage("Award Approved Equipment deleted successfully");
			} else {
				awardApprovedEquipment.setUpdateTimestamp(newTimeStamp);
				awardApprovedEquipment = awardDao.saveOrUpdateAwardApprovedEquipment(awardApprovedEquipment);
			}
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeInsert)) {
				reportTermsVO.setMessage("Award Approved Equipment saved successfully");
			} else {
				reportTermsVO.setMessage("Award Approved Equipment updated successfully");
			}
			reportTermsVO.setAwardApprovedEquipment(awardApprovedEquipment);
			updateAwardDocumentUpdateUserAndTimestamp(awardApprovedEquipment.getAwardId(), awardApprovedEquipment.getUpdateUser());
			return commonDao.convertObjectToJSON(reportTermsVO);
		} else if (awardAprovedForeignTravel != null) {
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeDelete)) {
				awardDao.deleteAwardAprovedForeignTravel(awardAprovedForeignTravel);
				reportTermsVO.setMessage("Award Approved Foreign Travel deleted successfully");
			} else {
				awardAprovedForeignTravel.setUpdateTimestamp(newTimeStamp);
				awardAprovedForeignTravel = awardDao.saveOrUpdateAwardAprovedForeignTravel(awardAprovedForeignTravel);
			}
			reportTermsVO.setAwardAprovedForeignTravel(awardAprovedForeignTravel);
			updateAwardDocumentUpdateUserAndTimestamp(awardAprovedForeignTravel.getAwardId(), awardAprovedForeignTravel.getUpdateUser());
			return commonDao.convertObjectToJSON(reportTermsVO);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}

	}

	@Override
	public String getReportLookupData(ReportTermsVO reportTermsVO) throws Exception {
		Map<String, Timestamp> mapOfDates = new HashMap<>();
		Integer awardId = reportTermsVO.getAwardId();
		reportTermsVO.setReportClassList(awardDao.getReportClassList());
		reportTermsVO.setFrequencyList(awardDao.getFrequencyList());
		reportTermsVO.setFrequencyBaseList(awardDao.getFrequencyBaseList());
		reportTermsVO.setReportStatusList(awardDao.getReportStatusList());
		reportTermsVO.setDistributionList(awardDao.getDistributionList());
		reportTermsVO.setMapOfDates(getAwardDates(mapOfDates, awardId));
		return commonDao.convertObjectToJSON(reportTermsVO);
	}

	@Override
	public String getTermsLookupData(ReportTermsVO reportTermsVO) throws Exception {
		Integer awardId = reportTermsVO.getAwardId();
		reportTermsVO.setSponsorTermTypeList(awardDao.getSponsorTermTypeList());
		reportTermsVO.setSponsorTermList(awardDao.getSponsorTermList());
		List<AwardApprovedEquipment> awardApprovedEquipments = awardDao.getAwardApprovedEquipmentByAwardId(awardId);
		if (awardApprovedEquipments != null && !awardApprovedEquipments.isEmpty()) {
			reportTermsVO.setAwardApprovedEquipmentList(awardApprovedEquipments);
		}
		List<AwardAprovedForeignTravel> awardAprovedForeignTravels = awardDao.getAwardAprovedForeignTravelByAwardId(awardId);
		if (awardAprovedForeignTravels != null && !awardAprovedForeignTravels.isEmpty()) {
			reportTermsVO.setAwardAprovedForeignTravelList(awardAprovedForeignTravels);
		}
		reportTermsVO.setAwardForeignTravellerList(awardDao.getAwardPersonListForTerms(awardId));
		return commonDao.convertObjectToJSON(reportTermsVO);
	}

	@Override
	public String maintainReports(ReportTermsVO reportTermsVO) throws Exception {
		AwardReportTerms awardReport = reportTermsVO.getAwardReport();
		if (awardReport != null) {
			List<AwardReportTermRecipient> awardReportTermRecipients = awardReport.getAwardReportTermRecipient();
			List<AwardReportTermRecipient> awardReportTermRecipientToDelete = new ArrayList<>();
			String acType = reportTermsVO.getAcType();
			Timestamp newTimeStamp = commonDao.getCurrentTimestamp();
			awardReport.setUpdateTimestamp(newTimeStamp);
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeDelete)) {
				awardDao.deleteReportTrackingAttachment(null, null, awardReport.getAwardReportTermsId());
				awardDao.deleteAwardReport(awardReport.getAwardReportTermsId());
				reportTermsVO.setMessage("AwardReport deleted successfully");
			} else if (acType != null && acType.equalsIgnoreCase(Constants.acTypeInsert)) {
				List<Date> dates = generateSchedules(awardReport, reportTermsVO.getMapOfDates());
				awardReport = buildReportTracking(awardReport, dates);
				awardReport = awardDao.saveOrUpdateAwardReports(awardReport);
				if (awardReportTermRecipients != null && !awardReportTermRecipients.isEmpty()) {
					for (AwardReportTermRecipient awardReportTermRecipient : awardReportTermRecipients) {
						if (awardReportTermRecipient.getAcType() != null) {
							if (awardReportTermRecipient.getAcType().equalsIgnoreCase(Constants.acTypeInsert)) {
								awardReportTermRecipient.setAwardReportTerms(awardReport);
								awardReportTermRecipient.setUpdateTimestamp(newTimeStamp);
								awardReportTermRecipient.setAwardId(awardReport.getAwardId());
								awardReportTermRecipient.setAwardNumber(awardReport.getAwardNumber());
								awardReportTermRecipient.setSequenceNumber(awardReport.getSequenceNumber());
								awardReportTermRecipient.setUpdateUser(awardReport.getUpdateUser());
							} else if (awardReportTermRecipient.getAcType().equalsIgnoreCase(Constants.acTypeDelete)) {
								awardDao.deleteAwardReportRecepientById(awardReportTermRecipient);
								awardReportTermRecipientToDelete.add(awardReportTermRecipient);
								reportTermsVO.setMessage("AwardreportRecepient deleted successfully");
							}
						}
					}
					awardReportTermRecipients.removeAll(awardReportTermRecipientToDelete);
				}
			} else if (acType != null && acType.equalsIgnoreCase(Constants.acTypeUpdate)) {
				if (Boolean.TRUE.equals(reportTermsVO.getFrequenciesChanged())) {
					List<Date> dates = generateSchedules(awardReport, reportTermsVO.getMapOfDates());
					List<AwardReportTracking> reportTrackings = awardReport.getAwardReportTracking();
					List<AwardReportTracking> updatedReportTrackings = new ArrayList<>(reportTrackings);
					Collections.copy(updatedReportTrackings, reportTrackings);
					List<Integer> awardReportTrackingIds = new ArrayList<>();
					boolean isProgressReportEnabled = commonDao.getParameterValueAsBoolean(Constants.AWARD_PROGRESS_REPORT_ENABLED);
					final Integer reportTermsId = awardReport.getAwardReportTermsId();
					final Integer awardId = awardReport.getAwardId();
					if (Boolean.TRUE.equals(isProgressReportEnabled)) {
						reportTrackings.stream().filter(reportTracking -> reportTracking.getAwardReportTerms().getAwardReportTermsId().equals(reportTermsId) 
								&& reportTracking.getAwardReportTerms().getAwardId().equals(awardId)
								&& reportTracking.getProgressReportId() == null).forEach(reportTracking -> {
									if (reportTracking.getAwardReportTrackingId() != null) {
										awardReportTrackingIds.add(reportTracking.getAwardReportTrackingId());
									}
									updatedReportTrackings.remove(reportTracking);	
								});	
					} else {
						reportTrackings.stream().filter(reportTracking -> reportTracking.getAwardReportTerms().getAwardReportTermsId().equals(reportTermsId) 
								&& reportTracking.getAwardReportTerms().getAwardId().equals(awardId)
								&& reportTracking.getAwardReportTrackingFile() == null).forEach(reportTracking -> {
									if (reportTracking.getAwardReportTrackingId() != null) {
										awardReportTrackingIds.add(reportTracking.getAwardReportTrackingId());
									}
									updatedReportTrackings.remove(reportTracking);	
								});		
					}
					if (!awardReportTrackingIds.isEmpty()) {
						if (Boolean.TRUE.equals(isProgressReportEnabled)) {
							awardDao.deleteAwardReportTrackingFileByTrackingId(awardReportTrackingIds);
						}
						awardDao.deleteAwardReportTrackingById(awardReport, awardReportTrackingIds);
					}
					if (updatedReportTrackings != null && !updatedReportTrackings.isEmpty()) {
						awardReport.getAwardReportTracking().clear();
						awardReport.getAwardReportTracking().addAll(updatedReportTrackings);
						awardReport = awardDao.saveOrUpdateAwardReports(awardReport);
						awardReport = generateNewReportTrackings(awardReport, dates);
					} else {
						awardReport.getAwardReportTracking().clear();
						awardReport = buildReportTracking(awardReport, dates);
					}
				}
				awardReport = awardDao.saveOrUpdateAwardReports(awardReport);
				if (awardReportTermRecipients != null && !awardReportTermRecipients.isEmpty()) {
					for (AwardReportTermRecipient awardReportTermRecipient : awardReportTermRecipients) {
						if (awardReportTermRecipient.getAcType() != null) {
							if (awardReportTermRecipient.getAcType().equalsIgnoreCase(Constants.acTypeInsert)) {
								awardReportTermRecipient.setAwardReportTerms(awardReport);
								awardReportTermRecipient.setUpdateTimestamp(newTimeStamp);
								awardReportTermRecipient.setAwardId(awardReport.getAwardId());
								awardReportTermRecipient.setAwardNumber(awardReport.getAwardNumber());
								awardReportTermRecipient.setSequenceNumber(awardReport.getSequenceNumber());
								awardReportTermRecipient.setUpdateUser(awardReport.getUpdateUser());
							} else if (awardReportTermRecipient.getAcType().equalsIgnoreCase(Constants.acTypeDelete)) {
								awardDao.deleteAwardReportRecepientById(awardReportTermRecipient);
								awardReportTermRecipientToDelete.add(awardReportTermRecipient);
								reportTermsVO.setMessage("AwardreportRecepient deleted successfully");
							} else if (awardReportTermRecipient.getAcType().equalsIgnoreCase(Constants.acTypeUpdate)) {
								awardReportTermRecipient.setAcType(Constants.acTypeUpdate);
							}
						}
					}
					awardReportTermRecipients.removeAll(awardReportTermRecipientToDelete);
				}
			}
			awardReport.setAwardReportTermRecipient(awardReportTermRecipients);
			reportTermsVO.setAwardReport(awardReport);
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeInsert)) {
				reportTermsVO.setMessage("AwardRecepient saved successfully");
			} else if (acType.equalsIgnoreCase(Constants.acTypeUpdate)) {
				reportTermsVO.setMessage("AwardReport updated successfully");
			}
			updateAwardDocumentUpdateUserAndTimestamp(awardReport.getAwardId(), awardReport.getUpdateUser());
			return commonDao.convertObjectToJSON(reportTermsVO);
		}
		return commonDao.convertObjectToJSON("Awardreport Issue with Service");
	}

	@Override
	public String maintainTerms(ReportTermsVO reportTermsVO) throws Exception {
		String response = "";
		Timestamp newTimeStamp = commonDao.getCurrentTimestamp();
		AwardSponsorTerm awardSponsorTerm = reportTermsVO.getAwardSponsorTerm();
		String acType = reportTermsVO.getAcType();
		List<SponsorTermCodeData> awardSponsorTermToDelete = new ArrayList<>();

		Map<String, List<HashMap<String, Object>>> awardTermsMaps = new HashMap<>();

		if (acType != null && (acType.equalsIgnoreCase(Constants.acTypeInsert)
				|| acType.equalsIgnoreCase(Constants.acTypeUpdate))) {
			if (awardSponsorTerm != null) {
				List<SponsorTermCodeData> sponsorTermCodeList = awardSponsorTerm.getSponsorTermCodeList();
				for (SponsorTermCodeData sponsorTermDetail : sponsorTermCodeList) {
					AwardSponsorTerm tempAwardSponsorTerm = new AwardSponsorTerm();
					tempAwardSponsorTerm.setAwardId(awardSponsorTerm.getAwardId());
					tempAwardSponsorTerm.setAwardNumber(awardSponsorTerm.getAwardNumber());
					tempAwardSponsorTerm.setSequenceNumber(awardSponsorTerm.getSequenceNumber());
					tempAwardSponsorTerm.setUpdateUser(awardSponsorTerm.getUpdateUser());
					tempAwardSponsorTerm.setSponsorTermTypeCode(awardSponsorTerm.getSponsorTermTypeCode());
					tempAwardSponsorTerm.setSponsorTermCode(sponsorTermDetail.getCode());
					tempAwardSponsorTerm.setUpdateTimestamp(newTimeStamp);

					if (sponsorTermDetail.getAcType() != null
							&& (sponsorTermDetail.getAcType().equalsIgnoreCase(Constants.acTypeInsert)
									|| sponsorTermDetail.getAcType().equalsIgnoreCase(Constants.acTypeUpdate))) {
						tempAwardSponsorTerm = awardDao.saveOrUpdateAwardSponsorTerms(tempAwardSponsorTerm);
					} else if (sponsorTermDetail.getAcType() != null
							&& (sponsorTermDetail.getAcType().equalsIgnoreCase(Constants.acTypeDelete))) {
						tempAwardSponsorTerm.setAwardSponsorTermId(sponsorTermDetail.getAwardSponsorTermId());
						awardDao.deleteAwardSponsorTerm(tempAwardSponsorTerm);
						awardSponsorTermToDelete.add(sponsorTermDetail);
					}

					HashMap<String, Object> sponsorTermDetailsField = new HashMap<>();
					sponsorTermDetailsField.put("awardId", tempAwardSponsorTerm.getAwardId());
					sponsorTermDetailsField.put("awardNumber", tempAwardSponsorTerm.getAwardNumber());
					sponsorTermDetailsField.put("sponsorTermTypeCode", tempAwardSponsorTerm.getSponsorTermTypeCode());
					sponsorTermDetailsField.put("awardSponsorTermId", tempAwardSponsorTerm.getAwardSponsorTermId());
					sponsorTermDetailsField.put("sponsorTermType", tempAwardSponsorTerm.getSponsorTermType());
					sponsorTermDetailsField.put("sponsorTermCode", sponsorTermDetail.getCode());
					sponsorTermDetailsField.put("sponsorTerm", sponsorTermDetail.getDescription());

					List<HashMap<String, Object>> sponsorTermsByType = new ArrayList<>();
					sponsorTermsByType.add(sponsorTermDetailsField);

					if (!awardTermsMaps.containsKey(awardSponsorTerm.getSponsorTermType()) && !sponsorTermDetail.getAcType().equalsIgnoreCase(Constants.acTypeDelete)) {
						awardTermsMaps.put(tempAwardSponsorTerm.getSponsorTermTypeCode(), sponsorTermsByType);
					} else if(!sponsorTermDetail.getAcType().equalsIgnoreCase(Constants.acTypeDelete)){
						awardTermsMaps.get(tempAwardSponsorTerm.getSponsorTermTypeCode()).add(sponsorTermDetailsField);
					}
				}
				sponsorTermCodeList.removeAll(awardSponsorTermToDelete);
				reportTermsVO.setAwardTermsList(awardTermsMaps);
			}
			reportTermsVO.setAcType(Constants.acTypeUpdate);
			reportTermsVO.setMessage("Award Sponsor Terms saved successfully");
			response = commonDao.convertObjectToJSON(reportTermsVO);
		} else if (acType != null && acType.equalsIgnoreCase(Constants.acTypeDelete)) {
			// response = awardDao.deleteAwardSponsorTerm(sponsorTermList.get(0));
		}
		updateAwardDocumentUpdateUserAndTimestamp(awardSponsorTerm.getAwardId(), awardSponsorTerm.getUpdateUser());
		return response;
	}

	@Override
	public String saveAwardFinal(AwardVO awardVo) throws Exception {
		logger.info("personId : {}", awardVo.getPersonId());
		logger.info("awardSequenceStatus : {}", awardVo.getAwardSequenceStatus());
		logger.info("awardId : {}", awardVo.getAwardId());
		try {
			Award award = awardDao.getAwardDetailsById(awardVo.getAwardId());
			award.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_ACTIVE);
			award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award.setUpdateUser(awardVo.getPersonId());
			award.setIsLatest(true);
			awardVo.setAward(award);
			saveAwardDetails(awardVo); // updating Award table in database
		} catch (Exception e) {
			logger.error("error in saveAwardFinal: {}", e.getMessage());
		}

		return null;
	}

	@Override
	public String getReportsData(ReportTermsVO reportTermsVo) throws Exception {
		Integer awardId = reportTermsVo.getAwardId();
		awardDao.getAwardSponsorReports(awardId, reportTermsVo);
		List<AwardReportTerms> awardReportTerms = reportTermsVo.getAwardReportTerms();
		List<AwardReportTracking> reportTracking = new ArrayList<>();
		awardReportTerms.forEach(reportTerms -> {
			reportTracking.addAll(reportTerms.getAwardReportTracking());
		});
		boolean isProgressReportEnabled = commonDao.getParameterValueAsBoolean(Constants.AWARD_PROGRESS_REPORT_ENABLED);
		reportTermsVo.setProgressReportEnabled(isProgressReportEnabled);
		List<Integer> trackingId = reportTracking.stream().map(AwardReportTracking::getAwardReportTrackingId).collect(Collectors.toList());
		List<Integer> progressReportIds = reportTracking.stream().filter(awardReportTracking -> awardReportTracking.getProgressReportId() != null).map(AwardReportTracking::getProgressReportId).collect(Collectors.toList());
		if (trackingId != null && !trackingId.isEmpty()) {
			if (isProgressReportEnabled && progressReportIds != null && !progressReportIds.isEmpty()) {
				List<AwardProgressReport> awardProgressReport = new ArrayList<>();
				progressReportIds.stream().forEach(progressReportId -> {
					AwardProgressReport progressReport = progressReportService.loadAwardProgressReportDetails(progressReportId);
					progressReport.setAward(null);
					awardProgressReport.add(progressReport);
				});
				Map<Integer, List<AwardProgressReport>> collect = awardProgressReport.stream().collect(Collectors.groupingBy(AwardProgressReport::getProgressReportId));
				awardReportTerms.forEach(awardReportTerm -> awardReportTerm.getAwardReportTracking().stream()
						.filter(item -> collect.containsKey(item.getProgressReportId()))
						.forEach(item -> item.setAwardProgressReport(collect.get(item.getProgressReportId()).get(0))));
			}
			if(awardReportTerms != null && !awardReportTerms.isEmpty()) {
				if(awardReportTerms.get(0).getAward().getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_ACTIVE)) {
					List<Integer> awardIds = awardDao.getListOfPendingAwardForAwardNumber(awardReportTerms.get(0).getAward().getAwardNumber());
					List<String> moduleItemKeys = new ArrayList<>();
					awardIds.forEach(moduleItemKey -> moduleItemKeys.add(moduleItemKey.toString()));
					if(!moduleItemKeys.isEmpty())
						reportTermsVo.setIsEditEnabledForSection(sectionWiseEditDao.isSectionEditableForActiveDocument(moduleItemKeys, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE, Constants.REPORT_REQ_EDITABLE_SECTION_TYPE_CODE));
				}
			}		
			List<AwardReportTrackingFile> trackingFile = awardDao.getAllDraftReportTrackingAttachments(trackingId);
			Map<Integer, List<AwardReportTrackingFile>> collect = trackingFile.stream()
					.collect(Collectors.groupingBy(AwardReportTrackingFile::getAwardReportTrackingId));
			awardReportTerms.forEach(awardReportTerm -> awardReportTerm.getAwardReportTracking()
					.stream().filter(item -> collect.containsKey(item.getAwardReportTrackingId())).forEach(item -> item.setAwardReportTrackingFile(collect.get(item.getAwardReportTrackingId()).get(0))));
		
		}
		Map<String, List<AwardReportTerms>> groupedAwardReportTerms = null;
		if(!awardReportTerms.isEmpty()){
			groupedAwardReportTerms = awardReportTerms.stream()
					.collect(Collectors.groupingBy(awardReportTerm -> awardReportTerm.getReportClass().getDescription()));
		}
		reportTermsVo.setAwardReportsList(groupedAwardReportTerms);
		reportTermsVo.setIsReplaceAttachmentEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_REPLACE_ATTACHMENTS_AWARD));
		return commonDao.convertObjectToJSON(reportTermsVo);
	}

	@Override
	public String getTermsData(ReportTermsVO reportTermsVo) throws Exception {
		Integer awardId = reportTermsVo.getAwardId();
		awardDao.getAwardSponsorTerms(awardId, reportTermsVo);
		return commonDao.convertObjectToJSON(reportTermsVo);
	}

	public String linkInstituteProposalToAward(AwardLinkInstituteProposalVO awardLinkInstituteProposalVO) throws Exception {
		AwardFundingProposal awardFundingProposal = awardLinkInstituteProposalVO.getAwardFundingProposal();
		if (awardFundingProposal != null) {					
			InstituteProposal instituteProposal = awardDao.fetchInstProposalById(awardFundingProposal.getProposalId());
			String acType = awardLinkInstituteProposalVO.getAcType();
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeDelete)) {
				awardDao.deleteFundingProposal(awardFundingProposal.getAwardFundingProposalId());
				if (Boolean.FALSE.equals(awardDao.checkIpLinkedInAwards(awardFundingProposal.getProposalId()))) {
					setInstituteProposalStatus(instituteProposal, Constants.IP_STATUS_PENDING);
				}
				awardLinkInstituteProposalVO.setMessage("Award Funding proposal deleted successfully");
				updateAwardDocumentUpdateUserAndTimestamp(awardLinkInstituteProposalVO.getAwardId(), awardLinkInstituteProposalVO.getUpdateUser());
			} else {
				if (awardFundingProposal.getAwardId() != null) {
					awardFundingProposal.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					awardFundingProposal = awardDao.saveOrUpdateFundingProposal(awardFundingProposal);
					setInstituteProposalStatus(instituteProposal, Constants.IP_STATUS_FUNDED);
					if (instituteProposal.getSponsor() != null) {
						instituteProposal.setSponsorName(commonService.getSponsorFormatBySponsorDetail(instituteProposal.getSponsor().getSponsorCode(), instituteProposal.getSponsor().getSponsorName(), instituteProposal.getSponsor().getAcronym()));
					}
					if (instituteProposal.getPrimeSponsor() != null) {
						instituteProposal.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(instituteProposal.getPrimeSponsor().getSponsorCode(), instituteProposal.getPrimeSponsor().getSponsorName(), instituteProposal.getPrimeSponsor().getAcronym()));
					}
					awardFundingProposal.setProposal(instituteProposal);
					awardLinkInstituteProposalVO.setProposalId(instituteProposal.getProposalId());
					Award award = awardDao.fetchAwardByAwardId(awardFundingProposal.getAwardId().toString());
					if (instituteProposal.getGrantCallId() != null && award.getGrantHeaderId() == null) {
						award.setGrantHeaderId(instituteProposal.getGrantCallId());
						award.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(instituteProposal.getGrantCallId()));
						award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						awardDao.saveOrUpdateAwardDetails(award);
					}
					copyIPKeywordsToAward (award, instituteProposal);
					awardLinkInstituteProposalVO.setAward(awardDao.fetchAwardByAwardId(award.getAwardId().toString()));
					if (commonDao.getParameterValueAsBoolean(Constants.IS_COPY_CUSTOM_DATA)) {
						Integer devProposalId = awardDao.fetchDevProposalByInstProposal(awardFundingProposal.getProposalId());
						customDataElementService.copyCustomData(awardFundingProposal.getAwardId().toString(), Constants.AWARD_MODULE_CODE, devProposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, awardLinkInstituteProposalVO.getUpdateUser());
					}
					awardLinkInstituteProposalVO.setAcType(Constants.acTypeUpdate);
					if (acType != null && acType.equalsIgnoreCase(Constants.acTypeInsert)) {
						awardLinkInstituteProposalVO.setMessage("AwardFunding proposal saved successfully");
					} else {
						awardLinkInstituteProposalVO.setMessage("AwardFunding proposal updated successfully");
					}
					awardLinkInstituteProposalVO.setAwardFundingProposal(awardFundingProposal);
					if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {
						copyMilestoneFromDevelopmentProposal(awardLinkInstituteProposalVO);
					}
					if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
						copyKPIFromDevelopmentProposal(awardLinkInstituteProposalVO);
					}
					updateAwardDocumentUpdateUserAndTimestamp(awardFundingProposal.getAwardId(), awardLinkInstituteProposalVO.getUpdateUser());
				} else {
					createAwardByFundingProposal(awardLinkInstituteProposalVO);
				}
			}
			return commonDao.convertObjectToJSON(awardLinkInstituteProposalVO);
		} else {
			return commonDao.convertObjectToJSON("Issue with linking proposal to award");
		}
	}
	
	private void setInstituteProposalStatus(InstituteProposal instituteProposal, Integer statusCode) {
		if (instituteProposal.getStatusCode() != null) {
			instituteProposal.setStatusCode(statusCode);
			instituteProposal.setInstProposalStatus(institutionalProposalDao.fetchInstituteProposalStatusById(statusCode));
			institutionalProposalDao.saveOrUpdateInstituteProposal(instituteProposal);
		}
	}


	private void copyIPKeywordsToAward (Award award, InstituteProposal instituteProposal) {
		List<AwardKeyword> awardKeywords = award.getAwardKeywords();
		List<InstituteProposalKeywords> instituteProposalKeywords = institutionalProposalDao.fetchAllIPKeywordByProposal(instituteProposal.getProposalId());
		List<String> awardKeywordCodes = awardKeywords.stream().map(awardKeyword -> awardKeyword.getScienceKeywordCode()).collect(Collectors.toList());
		if (instituteProposalKeywords != null && !instituteProposalKeywords.isEmpty()) {
			for (InstituteProposalKeywords keyword : instituteProposalKeywords) {
				if ( awardKeywordCodes == null || awardKeywordCodes.isEmpty() || !awardKeywordCodes.contains(keyword.getScienceKeywordCode())) {
					awardDao.saveOrUpdateAwardKeyword(createAwardScienceKeyword(award.getAwardId(), keyword, AuthenticatedUser.getLoginUserName()));
				}
			}
		}
	}

	@Override
	public void createAwardByFundingProposal(AwardLinkInstituteProposalVO awardLinkInstituteProposalVO) throws Exception {
		AwardFundingProposal awardFundingProposal = awardLinkInstituteProposalVO.getAwardFundingProposal();
		InstituteProposal instituteProposal = awardDao.fetchInstProposalById(awardFundingProposal.getProposalId());
		if (instituteProposal != null) {
			instituteProposal.setStatusCode(Constants.IP_STATUS_FUNDED);
			instituteProposal.setInstProposalStatus(institutionalProposalDao.fetchInstituteProposalStatusById(Constants.IP_STATUS_FUNDED));
			institutionalProposalDao.saveOrUpdateInstituteProposal(instituteProposal);
			String updateUser = awardFundingProposal.getUpdateUser();
			Award award = createAward(instituteProposal, updateUser);
			if (awardLinkInstituteProposalVO.isDateofAwardRequired()) {
				award.setAwardEffectiveDate(commonDao.getCurrentTimestamp());
				award = awardDao.saveOrUpdateAwardDetails(award);
			}
			if (awardDao.checkAwardHierarchyExisted(award).isEmpty()) {
				moveDataToAwardHierarchy(award);
			}
			String userFullName = personDao.getUserFullNameByUserName(updateUser);
			if (award.getCreateUser() != null) {
				award.setCreateUserFullName(userFullName);
			}
			if (award.getUpdateUser() != null) {
				award.setUpdateUserFullName(userFullName);
			}
			if (commonDao.getParameterValueAsBoolean(Constants.IMPORT_PD_BUDGET_TO_AWARD)) {
				BudgetHeader selectedBudgetHeader = budgetDao.fetchLinkedBudgetsByProposalNumber(instituteProposal.getProposalNumber());
				if (selectedBudgetHeader != null) {
					datesAndAmountService.createAwardAmountInfo(award, instituteProposal, selectedBudgetHeader, updateUser);
					AwardBudgetVO awardBudgetVO = new AwardBudgetVO();
					awardBudgetVO.setAwardId(award.getAwardId());
					awardBudgetVO.setActivityTypeCode(award.getActivityTypeCode());
					awardBudgetVO.setAcType("CREATE_NEW_BUDGET");
					awardBudgetVO.setUserName(updateUser);
					awardBudgetVO.setAwardNumber(award.getAwardNumber());
					awardBudgetVO.setUserFullName(userFullName);
					awardBudgetVO.setAvailableFundType(Constants.FUND_TYPE_CODE);
					AwardBudgetHeader awardBudgetHeader = awardBudgetService.createAwardBudgetHeader(awardBudgetVO, award);
					awardBudgetVO.setAwardBudgetHeader(awardBudgetHeader);
					awardBudgetVO.setBudgetHeader(selectedBudgetHeader);
					awardBudgetHeader = awardBudgetService.generateAwardBudgetFromProposalBudget(awardBudgetVO);
					awardBudgetHeader.setCostSharingTypeCode(selectedBudgetHeader.getCostSharingTypeCode());
					awardBudgetHeader.setOnCampusRates(selectedBudgetHeader.getOnCampusRates());
					awardBudgetHeader.setOffCampusRates(selectedBudgetHeader.getOffCampusRates());
					awardBudgetDao.saveBudgetHeader(awardBudgetHeader);
				}
			}
			List<InstituteProposalKeywords> instituteProposalKeywords = institutionalProposalDao.fetchAllIPKeywordByProposal(instituteProposal.getProposalId());
			if (instituteProposalKeywords != null && !instituteProposalKeywords.isEmpty()) {
				List<AwardKeyword> awardKeywords = new ArrayList<>();
				for (InstituteProposalKeywords keyword : instituteProposalKeywords) {
					awardKeywords.add(awardDao.saveOrUpdateAwardKeyword(createAwardScienceKeyword(award.getAwardId(), keyword, updateUser)));
				}
			}
			List<InstituteProposalPerson> instituteProposalPersons = institutionalProposalDao.loadInstProposalPersonsByProposalId(instituteProposal.getProposalId());
			if (instituteProposalPersons != null && !instituteProposalPersons.isEmpty()) {
				List<AwardPerson> awardPersons = new ArrayList<>();
				for (InstituteProposalPerson instProposalPerson : instituteProposalPersons) {
					awardPersons.add(awardDao.saveOrUpdateAwardPersonDetails(createAwardPerson(award, instProposalPerson, updateUser)));
				}
				award.setAwardPersons(awardPersons);
			}
			/*List<InstituteProposalProjectTeam> instProposalProjectTeams = instituteProposal.getInstProposalProjectTeam();
			if (instProposalProjectTeams != null && !instProposalProjectTeams.isEmpty()) {
				List<AwardProjectTeam> awardProjectTeams = new ArrayList<>();
				for (InstituteProposalProjectTeam instProposalProjectTeam : instProposalProjectTeams) {
					awardProjectTeams.add(awardDao.saveOrUpdateAwardProjectTeam(createAwardProjectTeam(award, instProposalProjectTeam, updateUser)));
				}
			}*/
			List<InstituteProposalResearchArea> instProposalResearchAreas = institutionalProposalDao.loadInstProposalResearchAreaByProposalId(instituteProposal.getProposalId());
			if (instProposalResearchAreas != null && !instProposalResearchAreas.isEmpty()) {
				List<AwardResearchArea> awardResearchAreas= new ArrayList<>();
				for (InstituteProposalResearchArea instProposalResearchArea : instProposalResearchAreas) {
					awardResearchAreas.add(awardDao.saveOrUpdateAwardResearchArea(createAwardResearchArea(award, instProposalResearchArea, updateUser)));
				}
			}
			List<InstituteProposalSpecialReview> awardSpecialReviews = institutionalProposalDao.loadInstProposalSpecialReviewByProposalId(instituteProposal.getProposalId());
			if (awardSpecialReviews != null && !awardSpecialReviews.isEmpty()) {
				List<AwardSpecialReview> awardSpecialReview = new ArrayList<>();
				for (InstituteProposalSpecialReview instawardSpecialReview : awardSpecialReviews) {
					awardSpecialReview.add(awardDao.saveOrUpdateAwardSpecialReview(createAwardSpecialReview(award, instawardSpecialReview, updateUser)));
				}
			}
			AwardFundingProposal fundingProposal = new AwardFundingProposal();
			fundingProposal.setAwardId(award.getAwardId());
			fundingProposal.setIsActive(true);
			fundingProposal.setProposal(instituteProposal);
			fundingProposal.setProposalId(instituteProposal.getProposalId());
			fundingProposal.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateFundingProposal(fundingProposal);
			awardLinkInstituteProposalVO.setInstituteProposal(instituteProposal);
			awardLinkInstituteProposalVO.setAcType(Constants.acTypeUpdate);
			awardLinkInstituteProposalVO.setMessage("Award updated successfully");
			awardLinkInstituteProposalVO.setAward(award);
			awardLinkInstituteProposalVO.setAwardFundingProposal(fundingProposal);
			awardLinkInstituteProposalVO.setProposalId(instituteProposal.getProposalId());
			if (instituteProposal.getGrantCall() != null) {
				GrantCall grantCall = instituteProposal.getGrantCall();
				Integer fundingSchemeId = grantCall.getFundingSchemeId();
				String sponsorCode = grantCall.getSponsorCode();
				logger.info("fundingSchemeId : {}", fundingSchemeId);
				logger.info("sponsorCode : {}", sponsorCode);
				addSponserReportIntoAward(fundingSchemeId, sponsorCode, award);
			}
			if (instituteProposal.getActivityTypeCode().equals(Constants.GRANT_CALL_CATEGORY_A_TYPE_CODE)
					|| instituteProposal.getActivityTypeCode().equals(Constants.GRANT_CALL_CATEGORY_B_TYPE_CODE)
					|| instituteProposal.getActivityTypeCode().equals(Constants.GRANT_CALL_CATEGORY_C_TYPE_CODE)
					||	instituteProposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE))) {
				addAwardNoticeLetter(award.getAwardId(), award.getAwardNumber(), award.getSequenceNumber(), updateUser);
			}
			if (commonDao.getParameterValueAsBoolean(Constants.IS_COPY_CUSTOM_DATA)) {
				Integer devProposalId = awardDao.fetchDevProposalByInstProposal(instituteProposal.getProposalId());
				customDataElementService.copyCustomData(award.getAwardId().toString(), Constants.AWARD_MODULE_CODE, devProposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, updateUser);
			}
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {	
			copyMilestoneFromDevelopmentProposal(awardLinkInstituteProposalVO);	
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			copyKPIFromDevelopmentProposal(awardLinkInstituteProposalVO);
		}
	}

	private void addSponserReportIntoAward(Integer fundingSchemeId, String sponsorCode, Award award) throws Exception {		
		List<SponsorReport> sponsorReports = null;
		List<SponsorTermReport> sponsorTermReports = null;
		if (fundingSchemeId != null) {
			sponsorReports = awardDao.fetchSponsorReportBasedOnFundingSourceType(fundingSchemeId, sponsorCode);
			sponsorTermReports = awardDao.fetchSponsorTermReportBasedOnFundingSourceType(fundingSchemeId, sponsorCode);
		} else if (sponsorCode != null && fundingSchemeId == null) {
			sponsorReports = awardDao.fetchSponsorReportBasedOnSponsor(sponsorCode);
			sponsorTermReports = awardDao.fetchSponsorTermReportBasedOnSponsor(sponsorCode);
		}
		if (sponsorReports != null && !sponsorReports.isEmpty()) {
			Map<String, Timestamp> mapOfDates = getAwardDates(new HashMap<>(), award.getAwardId());
			for (SponsorReport sponsorReport : sponsorReports) {
				AwardReportTerms awardReportTerm = new AwardReportTerms();
				awardReportTerm.setAward(award);
				awardReportTerm.setAwardId(award.getAwardId());
				awardReportTerm.setAwardNumber(award.getAwardNumber());
				awardReportTerm.setSequenceNumber(award.getSequenceNumber());
				awardReportTerm.setReportClassCode(sponsorReport.getReportClassCode());
				awardReportTerm.setReportCode(sponsorReport.getReportCode());
				awardReportTerm.setFrequencyBaseCode(sponsorReport.getFrequencyBaseCode());
				awardReportTerm.setFrequencyCode(sponsorReport.getFrequencyCode());
				awardReportTerm.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardReportTerm.setUpdateUser(AuthenticatedUser.getLoginUserName());
				awardReportTerm=awardDao.saveOrUpdateAwardReports(awardReportTerm);
				ReportTermsVO reportTermsVO = new ReportTermsVO();
				reportTermsVO.setAwardReport(awardReportTerm);
				reportTermsVO.setAcType(Constants.acTypeInsert);
                reportTermsVO.setMapOfDates(mapOfDates);
                maintainReports(reportTermsVO);
			}
		}
		if (sponsorTermReports != null && !sponsorTermReports.isEmpty()) {
			for (SponsorTermReport sponsorTermReport : sponsorTermReports) {
				AwardSponsorTerm awardSponsorTerm = new AwardSponsorTerm();
				awardSponsorTerm.setAward(award);
				awardSponsorTerm.setAwardId(award.getAwardId());
				awardSponsorTerm.setAwardNumber(award.getAwardNumber());
				awardSponsorTerm.setSequenceNumber(award.getSequenceNumber());
				awardSponsorTerm.setSponsorTermTypeCode(sponsorTermReport.getSponsorTerm().getSponsorTermTypeCode());
				awardSponsorTerm.setSponsorTermCode(sponsorTermReport.getSponsorTerm().getSponsorTermCode());
				awardDao.saveOrUpdateAwardSponsorTerms(awardSponsorTerm);
			}
		}
		
	}

	private AwardSpecialReview createAwardSpecialReview(Award award, InstituteProposalSpecialReview instawardSpecialReview, String updateUser) {
		AwardSpecialReview awardSpecialReview = new AwardSpecialReview();
		awardSpecialReview.setAwardId(award.getAwardId());
		awardSpecialReview.setAwardNumber(award.getAwardNumber());
		awardSpecialReview.setSpecialReviewCode(instawardSpecialReview.getSpecialReviewCode());
		awardSpecialReview.setSequenceNumber(instawardSpecialReview.getSequenceNumber());
		awardSpecialReview.setApprovalTypeCode(instawardSpecialReview.getApprovalTypeCode());
		awardSpecialReview.setExpirationDate(instawardSpecialReview.getExpirationDate());
		awardSpecialReview.setProtocolNumber(instawardSpecialReview.getProtocolNumber());
		awardSpecialReview.setApplicationDate(instawardSpecialReview.getApplicationDate());
		awardSpecialReview.setApprovalDate(instawardSpecialReview.getApplicationDate());
		awardSpecialReview.setComments(instawardSpecialReview.getComments());
		awardSpecialReview.setUpdateUser(updateUser);
		awardSpecialReview.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardSpecialReview.setIsProtocolIntegrated(instawardSpecialReview.getIsProtocolIntegrated());
		return awardSpecialReview;
	}

	private AwardResearchArea createAwardResearchArea(Award award, InstituteProposalResearchArea instProposalResearchArea, String updateUser) {
		AwardResearchArea awardResearchArea = new AwardResearchArea();
		awardResearchArea.setAwardId(award.getAwardId());
		awardResearchArea.setAwardNumber(award.getAwardNumber());
		awardResearchArea.setSequenceNumber(award.getSequenceNumber());
		awardResearchArea.setResearchTypeCode(instProposalResearchArea.getResearchTypeCode());
		awardResearchArea.setResearchTypeAreaCode(instProposalResearchArea.getResearchTypeAreaCode());
		awardResearchArea.setResearchTypeArea(instProposalResearchArea.getResearchTypeArea());
		awardResearchArea.setResearchTypeSubAreaCode(instProposalResearchArea.getResearchTypeSubAreaCode());
		awardResearchArea.setResearchTypeSubArea(instProposalResearchArea.getResearchTypeSubArea());
		awardResearchArea.setUpdateUser(updateUser);
		awardResearchArea.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		return awardResearchArea;
	}

	private void addAwardNoticeLetter(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser) {
		byte[] bFile = printService.getTemplateData(Constants.AWARD_NOTICE_LETTER_TEMPLATE_TYPE_CODE);
		byte[] mergedOutput = printService.mergePlaceHoldersOfNotifyAward(bFile, awardId);
		if(mergedOutput != null) {
			AwardAttachment awardAttachment = new AwardAttachment();
			awardAttachment.setAwardId(awardId);
			awardAttachment.setAwardNumber(awardNumber);
			awardAttachment.setTypeCode(Constants.AWARD_ATTACHMENT_TYPE_LETTER_OF_TEMPLATE);
			awardAttachment.setDescription("Award Notice Letter");
			awardAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardAttachment.setUpdateUser(updateUser);
			awardAttachment.setFileName("AwardNoticeLetter.pdf");
			awardAttachment.setNarrativeStatusCode(Constants.NARRATIVE_STATUS_CODE_COMPLETE);
			awardAttachment.setNarrativeStatus(commonDao.getNarrativeStatusByCode(Constants.NARRATIVE_STATUS_CODE_COMPLETE));
			awardAttachment.setMimeType(MIMETYPE);
			awardAttachment.setVersionNumber(1);
			awardAttachment.setSequenceNumber(sequenceNumber);
			FileData fileData = new FileData();
			fileData.setAttachment(mergedOutput);
			fileData = commonDao.saveFileData(fileData);
			awardAttachment.setFileDataId(fileData.getFileDataId());
			awardAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
			awardAttachment.setDocumentId(1);
			if (awardAttachment.getUpdateUser() != null) {
				awardAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(awardAttachment.getUpdateUser()));
			}
		    awardDao.saveAttachment(awardAttachment);
		}
	}

	private Award createAward(InstituteProposal instProposal, String updateUser) {
		try {
			Award award = new Award();
			award.setActivityTypeCode(instProposal.getActivityTypeCode());
			award.setBeginDate(instProposal.getStartDate());
			award.setFinalExpirationDate(instProposal.getEndDate());
			award.setDuration(getDurationValue(award.getBeginDate(), award.getFinalExpirationDate()));
			award.setLeadUnitNumber(instProposal.getHomeUnitNumber());
			award.setLeadUnit(commonDao.getLeadUnitByUnitNumber(instProposal.getHomeUnitNumber()));
			award.setSponsorAwardNumber(instProposal.getExternalFundingAgencyId());
			if (instProposal.getSponsorCode() != null) {
				award.setSponsorCode(instProposal.getSponsorCode());
				award.setSponsor(commonDao.getSponsorById(instProposal.getSponsorCode()));
			}
			if (instProposal.getPrimeSponsorCode() != null) {
				award.setPrimeSponsorCode(instProposal.getPrimeSponsorCode());
				award.setPrimeSponsor(commonDao.getSponsorById(instProposal.getPrimeSponsorCode()));
			}
			setAwardSponsorDetail(award);
			award.setTitle(instProposal.getTitle());
			award.setUpdateUser(updateUser);
			award.setCreateUser(updateUser);
			award.setIsLatest(true);
			award.setSequenceNumber(1);
			award.setAwardNumber(generateNextAwardNumber());
			award.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_PENDING);
			award.setStatusCode(Constants.AWARD_STATUS_CODE_PENDING);
			award.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_PENDING));
			award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT);
			award.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT));
			award.setAwardDocumentTypeCode(Constants.AWARD_SETUP);
			award.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_SETUP));
			award.setCreateTimestamp(commonDao.getCurrentTimestamp());
			award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			award.setGrantHeaderId(instProposal.getGrantCallId());
			award.setResearchDescription(instProposal.getResearchDescription());
			award.setMultiDisciplinaryDescription(instProposal.getMultiDisciplinaryDescription());
			if (instProposal.getGrantCallId() != null) {
				award.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(instProposal.getGrantCallId()));
			}
			award.setCfdaNumber(instProposal.getCfdaNumber());
			award.setAwardTypeCode(instProposal.getAwardTypecode());
			award.setDocumentUpdateUser(updateUser);
			award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
			return awardDao.saveOrUpdateAwardDetails(award);
		} catch (Exception e) {
			logger.error("error in createAward : {}", e.getMessage());
			return null;
		}
	}

	private AwardProjectTeam createAwardProjectTeam(Award award, InstituteProposalProjectTeam instProposalProjectTeam, String updateUser) {
		AwardProjectTeam awardProjectTeam = new AwardProjectTeam();
		awardProjectTeam.setAwardId(award.getAwardId());
		awardProjectTeam.setAwardNumber(award.getAwardNumber());
		awardProjectTeam.setEndDate(instProposalProjectTeam.getEndDate());
		awardProjectTeam.setFullName(instProposalProjectTeam.getFullName());
		awardProjectTeam.setIsActive(instProposalProjectTeam.getIsActive());
		awardProjectTeam.setNonEmployeeFlag(instProposalProjectTeam.getNonEmployeeFlag());
		awardProjectTeam.setPersonId(instProposalProjectTeam.getPersonId());
		awardProjectTeam.setProjectRole(instProposalProjectTeam.getProjectRole());
		awardProjectTeam.setSequenceNumber(award.getSequenceNumber());
		awardProjectTeam.setPercentageCharged(instProposalProjectTeam.getPercentageCharged() == null ? null : Double.valueOf(instProposalProjectTeam.getPercentageCharged()));
		awardProjectTeam.setStartDate(instProposalProjectTeam.getStartDate());
		awardProjectTeam.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardProjectTeam.setUpdateUser(updateUser);
		return awardProjectTeam;
	}

	private AwardPerson createAwardPerson(Award award, InstituteProposalPerson instProposalPerson, String updateUser) {
		AwardPerson awardPerson = new AwardPerson();
		awardPerson.setAwardId(award.getAwardId());
		awardPerson.setAwardNumber(award.getAwardNumber());
		awardPerson.setSequenceNumber(award.getSequenceNumber());
		awardPerson.setPersonId(instProposalPerson.getPersonId());
		awardPerson.setFullName(instProposalPerson.getFullName());
		awardPerson.setPercentageEffort(instProposalPerson.getPercentageOfEffort());
		awardPerson.setIsMultiPi(instProposalPerson.getIsMultiPi());
		awardPerson.setIsPi(instProposalPerson.getIsPi());
		awardPerson.setDesignation(instProposalPerson.getDesignation());
		awardPerson.setPersonRoleId(instProposalPerson.getPersonRoleId());
		awardPerson.setDepartment(instProposalPerson.getDepartment());
		awardPerson.setProposalPersonRole(instProposalPerson.getProposalPersonRole());
		awardPerson.setRolodexId(instProposalPerson.getRolodexId());
		awardPerson.setEmailAddress(instProposalPerson.getEmailAddress());
		awardPerson.setProjectRole(instProposalPerson.getProjectRole());
		awardPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardPerson.setUpdateUser(updateUser);
		List<InstituteProposalPersonUnit> instProposalPersonUnits = instProposalPerson.getUnits();
		if (instProposalPersonUnits != null && !instProposalPersonUnits.isEmpty()) {
			List<AwardPersonUnit> awardPersonUnits = new ArrayList<>();
			for (InstituteProposalPersonUnit instituteProposalPersonUnit : instProposalPersonUnits) {				
				awardPersonUnits.add(createAwardPersonUnit(award, instituteProposalPersonUnit, awardPerson));
			}
			awardPerson.getAwardPersonUnits().addAll(awardPersonUnits);
		}
		List<InstituteProposalPersonAttachment> instProposalPersonAttachments = instProposalPerson.getProposalPersonAttachment();
		if (instProposalPersonAttachments != null && !instProposalPersonAttachments.isEmpty()) {
			List<AwardPersonAttachment> awardPersonAttachments = new ArrayList<>();
			for (InstituteProposalPersonAttachment instProposalPersonAttachment : instProposalPersonAttachments) {
				awardPersonAttachments.add(createAwardPersonAttachment(instProposalPersonAttachment, awardPerson));
			}
			awardPerson.getAwardPersonAttachment().addAll(awardPersonAttachments);
		}
		if (Boolean.TRUE.equals(awardPerson.getIsPi()) || awardPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
			List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForPI(Constants.AWARD_MODULE_CODE);
			if (awardPerson.getPersonId() != null && (Boolean.TRUE.equals(awardPerson.getIsPi())|| awardPerson.getPersonRoleId().equals((Constants.PI_ROLE_CODE)))) {
				for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
					addAwardPersonToAwardRoles(awardPerson, moduleDerivedRole.getRoleId());
				}
			}
		}
		return awardPerson;
	}

	private AwardPersonAttachment createAwardPersonAttachment(InstituteProposalPersonAttachment instProposalPersonAttachments, AwardPerson awardPerson) {
		AwardPersonAttachment awardPersonAttachment = new AwardPersonAttachment();
		awardPersonAttachment.setAwardPerson(awardPerson);
		awardPersonAttachment.setDescription(instProposalPersonAttachments.getDescription());
		awardPersonAttachment.setFileName(instProposalPersonAttachments.getFileName());
		awardPersonAttachment.setFileDataId(instProposalPersonAttachments.getFileDataId());
		awardPersonAttachment.setMimeType(instProposalPersonAttachments.getMimeType());
		awardPersonAttachment.setUpdateUser(awardPerson.getUpdateUser());
		awardPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		return awardPersonAttachment;
	}

	private void addAwardPersonToAwardRoles(AwardPerson awardPerson, Integer roleId) {
		AwardPersonRoles awardPersonRole = new AwardPersonRoles();
		awardPersonRole.setAwardId(awardPerson.getAwardId());
		awardPersonRole.setAwardNumber(awardPerson.getAwardNumber());
		awardPersonRole.setSequenceNumber(awardPerson.getSequenceNumber());
		awardPersonRole.setPersonId(awardPerson.getPersonId());
		awardPersonRole.setRoleId(roleId);
		awardPersonRole.setIsSystemGenerated(true);
		awardPersonRole.setUpdateUser(awardPerson.getUpdateUser());
		awardPersonRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardDao.saveOrUpdateAwardPersonRoles(awardPersonRole);
	}

	private AwardPersonUnit createAwardPersonUnit(Award award, InstituteProposalPersonUnit instituteProposalPersonUnit, AwardPerson awardPerson) {
		AwardPersonUnit awardPersonUnit = new AwardPersonUnit();
		awardPersonUnit.setAwardPerson(awardPerson);
		awardPersonUnit.setAwardId(award.getAwardId());
		awardPersonUnit.setAwardNumber(award.getAwardNumber());
		awardPersonUnit.setSequenceNumber(award.getSequenceNumber());
		awardPersonUnit.setUnitNumber(instituteProposalPersonUnit.getUnitNumber());
		awardPersonUnit.setLeadUnitFlag(instituteProposalPersonUnit.isLeadUnit());
		awardPersonUnit.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardPersonUnit.setUpdateUser(instituteProposalPersonUnit.getUpdateUser());
		return awardPersonUnit;
	}

	private AwardKeyword createAwardScienceKeyword(Integer awardId, InstituteProposalKeywords scienceKeyword, String updateUser) {
		try {
			Award award = awardDao.getAwardDetailsById(awardId);
			AwardKeyword awardKeyword = new AwardKeyword();
			awardKeyword.setAward(award);
			awardKeyword.setScienceKeywordCode(scienceKeyword.getScienceKeywordCode());
			awardKeyword.setScienceKeyword(scienceKeyword.getScienceKeyword());
			awardKeyword.setKeyword(scienceKeyword.getKeyword());
			awardKeyword.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardKeyword.setUpdateUser(updateUser);
			return awardKeyword;
		} catch (Exception e) {
			logger.error("error in createAwardScienceKeyword : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public String generateNextAwardNumber() {
		String nextAwardNumber = null;
		Integer currentAwardNumber = awardDao.updateAwardNextValue();
		nextAwardNumber = String.format("%06d", currentAwardNumber) + "-00001";
		return nextAwardNumber;
	}

	@Override
	public String getReportTrackingDetails(ReportTermsVO reportTermsVo) throws Exception {
		reportTermsVo.setAwardReportTrackingList(awardDao.getReportTrackingDetails(reportTermsVo.getAwardId(), reportTermsVo.getAwardReportTermsId()));
		return commonDao.convertObjectToJSON(reportTermsVo);
	}

	@Override
	public String saveReportTrackingDetails(ReportTermsVO reportTermsVo) throws Exception {
		AwardReportTerms awardReportTerm = reportTermsVo.getAwardReport();
		updateAwardDocumentUpdateUserAndTimestamp(awardReportTerm.getAwardId(), awardReportTerm.getAwardReportTracking().get(0).getUpdateUser());
		awardDao.saveOrUpdateAwardReports(awardReportTerm);
		return commonDao.convertObjectToJSON(reportTermsVo);
	}

	public Map<String, Timestamp> getAwardDates(Map<String, Timestamp> mapOfDates, Integer awardId) {
		try {
			return awardDao.getAwardBaseDates(awardId);
		} catch (Exception e) {
			logger.error("error in getAwardDates: {}", e.getMessage());
			return null;
		}
	}

	public AwardAmountInfo fetchAwardAmountInfoWithHighestTransactionId(List<AwardAmountInfo> awardAmountInfos) {
		return awardAmountInfos.get(awardAmountInfos.size() - 1);
	}

	public List<Date> generateSchedules(AwardReportTerms awardReportTerm, Map<String, Timestamp> mapOfDates)
			throws ParseException {
		List<Date> dates = new ArrayList<>();
		dates.addAll(getDates(awardReportTerm, mapOfDates));
		return dates;
	}

	protected List<Date> getDates(AwardReportTerms awardReportTerm, Map<String, Timestamp> mapOfDates)
			throws ParseException {
		List<Date> dates = new ArrayList<>();
		List<Date> scheduledDates = new ArrayList<>();
		java.util.Date startDate;
		java.util.Date endDate = null;
		Calendar calendar = new GregorianCalendar();
		final String SF_269_EXPENDITURE_REPORT_CODE = "33";
		final String FREQUENCY_ANNUAL = "Annual";
		Frequency frequency = awardDao.getFrequencyByFrequencyCode(awardReportTerm.getFrequencyCode());
		if (awardReportTerm.getReportCode().equalsIgnoreCase(SF_269_EXPENDITURE_REPORT_CODE)
				&& awardReportTerm.getFrequencyBaseCode()
						.equalsIgnoreCase(Constants.AWARD_EXPIRATION_DATE_OF_OBLIGATION)
				&& frequency.getDescription().equalsIgnoreCase(FREQUENCY_ANNUAL)
				&& (mapOfDates.get(Constants.AWARD_EXPIRATION_DATE_OF_OBLIGATION) != null)) {
			calendar.setTime(new Date(mapOfDates.get(Constants.AWARD_EXPIRATION_DATE_OF_OBLIGATION).getTime()));
			startDate = calendar.getTime();
		} else {
			startDate = getStartDate(awardReportTerm, mapOfDates, frequency);
		}
		if (StringUtils.isNotBlank(awardReportTerm.getFrequencyBaseCode())) {
			endDate = getEndDate(awardReportTerm, startDate, mapOfDates, frequency);
		}

		if (startDate != null && endDate != null) {
			calendar.setTime(startDate);
			if (Boolean.TRUE.equals(frequency.getRepeatFlag()) && frequency.getNumberOfMonths() != null) {
				// if the end date is before the start date, set the end date to the start date
				// so the schedule generation doesn't error and creates a single date.
				if (endDate.before(startDate)) {
					endDate = startDate;
				}
				ScheduleSequence scheduleSequence = new XMonthlyScheduleSequenceDecorator(
						new TrimDatesScheduleSequenceDecorator(new DefaultScheduleSequence()),
						Integer.parseInt(frequency.getNumberOfMonths()));
				scheduledDates = scheduleService.getScheduledDates(startDate, endDate, new Time24HrFmt(Constants.ZERO_HOURS),
						scheduleSequence, calendar.get(Calendar.DAY_OF_MONTH));
				dates.addAll(scheduledDates);
			} else if (Boolean.TRUE.equals(frequency.getRepeatFlag()) && frequency.getNumberOfDays() != null) {
					// if the end date is before the start date, set the end date to the start date
					// so the schedule generation doesn't error and creates a single date.
					if (endDate.before(startDate)) {
						endDate = startDate;
					}
					ScheduleSequence scheduleSequence = new WeekScheduleSequenceDecorator(new TrimDatesScheduleSequenceDecorator(new DefaultScheduleSequence()),frequency.getNumberOfDays(), 1);
					scheduledDates = scheduleService.getScheduledDates(startDate, endDate, new Time24HrFmt(Constants.ZERO_HOURS), 1, scheduleSequence);
					dates.addAll(scheduledDates);
				} else {
				dates.add(startDate);
			}
		}
		if ((awardReportTerm.getFrequencyBaseCode().equalsIgnoreCase(Constants.FINANCIAL_YEAR) ||
				awardReportTerm.getFrequencyBaseCode().equalsIgnoreCase(Constants.CALENDAR_YEAR))
				&& Boolean.TRUE.equals(frequency.getRepeatFlag())) {
			dates.add(mapOfDates.get(awardReportTerm.getFrequencyBaseCode()));
		}
		dates.stream().map(date -> offsetDateByFrequencyDays(frequency, date)).collect(Collectors.toList());
		Boolean checkIfBaseDateIncluded = awardDao.fetchBaseDateIncludeStatus(awardReportTerm.getFrequencyBaseCode());
		if (Boolean.TRUE.equals(checkIfBaseDateIncluded) && Boolean.TRUE.equals(frequency.getRepeatFlag()) 
				&& (frequency.getNumberOfMonths() != null || (frequency.getNumberOfDays() != null && frequency.getNumberOfDays() > 0))) {
			calendar.setTime(new Date(mapOfDates.get(awardReportTerm.getFrequencyBaseCode()).getTime()));
			if (!dates.contains(calendar.getTime())) {
				dates.add(0, calendar.getTime());
			}
		}
		return dates;
	}

	protected Date getStartDate(AwardReportTerms awardReportTerm, Map<String, Timestamp> mapOfDates,
			Frequency frequency) {
		Calendar calendar = new GregorianCalendar();
		Date date = null;
		boolean startDateIsNull = false;

		if (mapOfDates.containsKey(awardReportTerm.getFrequencyBaseCode())
				&& mapOfDates.get(awardReportTerm.getFrequencyBaseCode()) != null) {
			calendar.setTime(new Date(mapOfDates.get(awardReportTerm.getFrequencyBaseCode()).getTime()));
		} else if (awardReportTerm.getDueDate() != null) {
			calendar.setTimeInMillis(awardReportTerm.getDueDate().getTime());
		} else {
			startDateIsNull = true;
		}

		if (!startDateIsNull && frequency != null) {
			date = getStartDateFromTheBaseDate(calendar, frequency);
		}

		return date;
	}

	protected Date getStartDateFromTheBaseDate(Calendar calendar, Frequency frequency) {

		addOffSetMonthsToStartDate(frequency, calendar);

		addNumberOfMonthsToStartDate(frequency, calendar);

		return calendar.getTime();
	}

	protected void addOffSetMonthsToStartDate(Frequency frequency, Calendar calendar) {
		if (frequency != null && frequency.getAdvanceNumberOfMonths() != null) {
			calendar.add(Calendar.MONTH, -(Integer.parseInt(frequency.getAdvanceNumberOfMonths())));
		}
	}

	protected void addNumberOfMonthsToStartDate(Frequency frequency, Calendar calendar) {
		if (frequency.getNumberOfMonths() != null) {
			calendar.add(Calendar.MONTH, Integer.parseInt(frequency.getNumberOfMonths()));
		}
	}

	protected Date getEndDate(AwardReportTerms awardReportTerm, Date startDate, Map<String, Timestamp> mapOfDates,
			Frequency frequency) {
		Calendar calendar = new GregorianCalendar();

		if (Constants.FINAL_EXPIRATION_DATE.equals(awardReportTerm.getFrequencyBaseCode())) {
			calendar.setTime(startDate);
			//calendar.add(Calendar.YEAR,
			//		Integer.parseInt(Constants.PERIOD_IN_YEARS_WHEN_FREQUENCY_BASE_IS_FINAL_EXPIRATION_DATE));
		} else {
			calendar.setTime(new Date(mapOfDates.get(Constants.FINAL_EXPIRATION_DATE).getTime()));
			Optional<Frequency> utilFrequency = Optional.ofNullable(frequency);
			utilFrequency.map(Frequency::getNumberOfMonths)
					.ifPresent(months -> calendar.add(Calendar.MONTH, Integer.parseInt(months)));
			utilFrequency.map(Frequency::getNumberOfDays).ifPresent(days -> calendar.add(Calendar.DAY_OF_YEAR, days));
		}
//		if (!Constants.FINAL_EXPIRATION_DATE.equals(awardReportTerm.getFrequencyBaseCode())) {
//			return calendar.getTime().compareTo(new Date(mapOfDates.get(Constants.FINAL_EXPIRATION_DATE).getTime())) > 0 ? new Date(mapOfDates.get(Constants.FINAL_EXPIRATION_DATE).getTime()) :  calendar.getTime();
//		}
		return calendar.getTime();
	}

	protected Date offsetDateByFrequencyDays(Frequency frequency, Date date) {
		if (frequency != null) {
			Calendar offsetCal = Calendar.getInstance();
			offsetCal.setTime(date);
			if (frequency.getNumberOfDays() != null) {
				offsetCal.add(Calendar.DAY_OF_YEAR, frequency.getNumberOfDays());
			} else if (frequency.getAdvanceNumberOfDays() != null) {
				offsetCal.add(Calendar.DAY_OF_YEAR, -frequency.getAdvanceNumberOfDays());
			}
			return offsetCal.getTime();
		}
		return date;
	}

	protected AwardReportTerms generateNewReportTrackings(AwardReportTerms awardReportTerm, List<Date> dates) {
		List<Date> newDates = generateNewDates(awardReportTerm, dates);
		awardReportTerm = buildReportTracking(awardReportTerm, newDates);
		return awardReportTerm;
	}

	public List<Date> generateNewDates(AwardReportTerms awardTerm, List<Date> dates) {
		return dates.stream().filter(
				date -> awardTerm.getAwardReportTracking().stream().noneMatch(rt -> date.equals(rt.getDueDate())))
				.collect(Collectors.toList());
	}

	@Override
	public String getAttachmentDetails(AwardAttachmentsVO attachmentVo) throws Exception {
		attachmentVo.setNarrativeStatus(commonDao.fetchAllNarrativeStatus());
		attachmentVo.setAwardAttachmentTypes(awardDao.fetchAllAwardAttachmentTypes());
		attachmentVo.setIsReplaceAttachmentEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_REPLACE_ATTACHMENTS_AWARD));
		getAllAwardAttachments(attachmentVo);
		return commonDao.convertObjectToJSON(attachmentVo);
	}

	private void getAllAwardAttachments(AwardAttachmentsVO attachmentVo) {
		Boolean isPersonHasPermission = personHasPermission(attachmentVo.getAwardId());
		if (Boolean.FALSE.equals(isPersonHasPermission)) {
			isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_CONFIDENTIAL_AWARD_ATTACHMENTS_RIGHT, attachmentVo.getAwardLeadUnitNumber());
		}
		List <AwardAttachment> awardAttachments = awardDao.getAwardAttachmentsBasedOnParams(attachmentVo.getAwardId(), attachmentVo.getAwardNumber(), attachmentVo.getAwardSequenceNumber(), isPersonHasPermission);
		if (awardAttachments != null && !awardAttachments.isEmpty()) {
			for(AwardAttachment awardAttachment : awardAttachments) {
				if (awardAttachment.getUpdateUser() != null) {
					awardAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(awardAttachment.getUpdateUser()));
				}
			}
		}
		Collections.sort(awardAttachments, (allAwardAttachmentsOne, allAwardAttachmentsTwo) -> allAwardAttachmentsTwo.getUpdateTimestamp().compareTo(allAwardAttachmentsOne.getUpdateTimestamp()));
		attachmentVo.setNewAttachments(awardAttachments);
	}
	
	private Boolean personHasPermission(Integer awardId) {
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.VIEW_CONFIDENTIAL_AWARD_ATTACHMENTS_RIGHT); 
		return commonDao.checkPersonHasRightInModule(Constants.AWARD_MODULE_CODE, awardId, rightNames, AuthenticatedUser.getLoginPersonId());
	}

	@Override
	public String addAwardAttachments(MultipartFile[] files, String formDataJSON) {
		logger.info("-------- addAwardAttachments serviceimpl ---------");
		AwardAttachmentsVO awardAttachmentsVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			awardAttachmentsVO = mapper.readValue(formDataJSON, AwardAttachmentsVO.class);
			saveAwardAttachments(awardAttachmentsVO, files);
		} catch (Exception e) {
			logger.error("error in addAwardAttachments : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(awardAttachmentsVO);
	}

	private void saveAwardAttachments(AwardAttachmentsVO awardAttachmentsVO , MultipartFile[] files) {
		Integer awardId = awardAttachmentsVO.getAwardId();
		String awardNumber = awardAttachmentsVO.getAwardNumber();
		Integer sequenceNumber = awardAttachmentsVO.getAwardSequenceNumber();
		List<AwardAttachment> allAwardAttachments = new ArrayList<>();
		List<AwardAttachment> awardAttachmentDetails = getAwardAttachmentByAwardId(awardId, awardAttachmentsVO.getAwardLeadUnitNumber());
		if (awardAttachmentDetails != null && !awardAttachmentDetails.isEmpty()) {
			allAwardAttachments.addAll(awardAttachmentDetails);
		}
		List<AwardAttachment> previousAwardAttachments = getPreviousAwardAttachments(awardId);
		if (previousAwardAttachments != null && !previousAwardAttachments.isEmpty()) {
			allAwardAttachments.addAll(previousAwardAttachments);
		}
		List<AwardAttachment> attachments = awardAttachmentsVO.getNewAttachments();
		Integer documentId = awardDao.getMaxDocumentIdBasedOnAwardNumber(awardNumber);
		List<AwardAttachment> awardAttachments = new ArrayList<>();
		Integer versionNumber = 0;
		for (int i = 0; i < files.length; i++) {
			for (AwardAttachment attachment : attachments) {
				File file = new File(files[i].getOriginalFilename());
				String fileName = file.getName();
				String replaceFileName = attachment.getFileName();
				boolean isRenameRequired = false;
				int count = 1;
				isRenameRequired = checkForDuplication(attachment.getFileName(), allAwardAttachments);
				while(isRenameRequired) {
					 replaceFileName = attachment.getFileName();
					 replaceFileName = generateFileName(replaceFileName, count);
					 count = count +1;
					 isRenameRequired = checkForDuplication(replaceFileName, allAwardAttachments);
				}
				if (attachment.getAwardAttachmentId() != null) {
					for (AwardAttachment awardAttachment : allAwardAttachments) {
						if (awardAttachment.getAwardAttachmentId() != null && awardAttachment.getAwardAttachmentId().equals(attachment.getAwardAttachmentId())) {
							if (awardAttachment.getAwardId().equals(awardId)) {
								awardAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
							}
							versionNumber = awardAttachment.getVersionNumber();
							documentId = awardAttachment.getDocumentId();
							AwardAttachment tempAwardAttachment = addNewAwardAttachment(attachment, files[i], fileName, versionNumber, documentId, awardId, awardNumber, replaceFileName, sequenceNumber);
							awardDao.saveAttachment(awardAttachment);
							awardAttachments.add(tempAwardAttachment);
						}
					}
				} else {
					if (attachment.getFileName().equals(fileName)) {
						documentId = documentId + 1;
						AwardAttachment newAttachment = addNewAwardAttachment(attachment, files[i], fileName, versionNumber, documentId, awardId, awardNumber, replaceFileName, sequenceNumber);
						awardAttachments.add(newAttachment);
					}
					i++;
				}
			}
		}
		getAllAwardAttachments(awardAttachmentsVO);
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
		}

	private boolean checkForDuplication(String fileName, List<AwardAttachment> attachments) {
		for(AwardAttachment attachment : attachments) {
			if(fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	public AwardAttachment addNewAwardAttachment(AwardAttachment attachment, MultipartFile file, String fileName, Integer versionNumber, Integer documentId, Integer awardId, String awardNumber, String replaceFileName, Integer sequenceNumber) {
		AwardAttachment awardAttachment = new AwardAttachment();
		try {
			if (attachment.getFileName().equals(fileName)) {
				awardAttachment.setAwardId(awardId);
				awardAttachment.setAwardNumber(awardNumber);
				awardAttachment.setSequenceNumber(sequenceNumber);
				awardAttachment.setTypeCode(attachment.getTypeCode());
				awardAttachment.setDescription(attachment.getDescription());
				awardAttachment.setUpdateTimestamp(attachment.getUpdateTimestamp());
				awardAttachment.setUpdateUser(attachment.getUpdateUser());
				awardAttachment.setFileName(replaceFileName);
				awardAttachment.setNarrativeStatusCode(attachment.getNarrativeStatusCode());
				awardAttachment.setMimeType(file.getContentType());
				awardAttachment.setVersionNumber(versionNumber + 1);
				awardAttachment.setAttachmentType(awardDao.getAwardAttachmentTypeById(attachment.getTypeCode()));
				FileData fileData = new FileData();
				fileData.setAttachment(file.getBytes());
				fileData = commonDao.saveFileData(fileData);
				awardAttachment.setFileDataId(fileData.getFileDataId());
				awardAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
				awardAttachment.setDocumentId(documentId);
				if (awardAttachment.getUpdateUser() != null) {
					awardAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(awardAttachment.getUpdateUser()));
				}
				awardDao.saveAttachment(awardAttachment);
			}
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("error in addNewAwardAttachment : {}", e.getMessage());
		}
		return awardAttachment;
	}

	@Override
	public String deleteAwardAttachment(AwardAttachmentsVO attachmentVo) throws Exception {
		Integer awardId = attachmentVo.getAwardId();
		Boolean isDeleteOccured = false;
		AwardAttachment deletedAwardAttachment = null;
		List<AwardAttachment> awardAttachments = awardDao.fetchAwardAttachmentBasedOnAwardIdAndDocumentId(awardId, attachmentVo.getDocumentId());
		if (awardAttachments != null && !awardAttachments.isEmpty()) {
			for (AwardAttachment awardAttachment : awardAttachments) {
				if (awardAttachment.getAwardAttachmentId().equals(attachmentVo.getAwardAttachmentId())) {
					deletedAwardAttachment = new AwardAttachment();
					ReflectionUtils.shallowCopyFieldState(awardAttachment, deletedAwardAttachment);
				}
				commonDao.deleteFileData(commonDao.getFileDataById(awardAttachment.getFileDataId()));
				awardDao.deleteAwardAttachment(awardAttachment.getAwardAttachmentId());
			}
		} else {
			isDeleteOccured = true;
		}
		if (Boolean.TRUE.equals(awardDao.checkDocumentIdExistInAward(attachmentVo.getAwardNumber(), attachmentVo.getDocumentId()))) {
			if (Boolean.TRUE.equals(isDeleteOccured)) {
				deletedAwardAttachment = awardDao.fetchAwardAttachmentById(attachmentVo.getAwardAttachmentId());
			}
			if (deletedAwardAttachment != null) {
				AwardAttachment newAttachment = new AwardAttachment();
				ReflectionUtils.shallowCopyFieldState(deletedAwardAttachment, newAttachment);
				newAttachment.setAwardAttachmentId(null);
				newAttachment.setAwardId(awardId);
				newAttachment.setSequenceNumber(attachmentVo.getAwardSequenceNumber());
				newAttachment.setDocumentStatusCode(3);
				awardDao.saveAttachment(newAttachment);
			}
		}
		return commonDao.convertObjectToJSON(attachmentVo);
	}

	@Override
	public ResponseEntity<byte[]> downloadAwardAttachment(Integer awardAttachmentId) {
		AwardAttachment attachment = awardDao.fetchAwardAttachmentById(awardAttachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("error in downloadAwardAttachment : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String deleteAwardTransaction(AwardDatesandAmountVO vo) throws Exception {
		awardDao.deleteAwardTransaction(vo.getAwardAmountInfo());
		awardDao.deleteAwardAmountTransaction(vo.getAwardAmountInfo().getAwardAmountTransaction());
		return commonDao.convertObjectToJSON("Award Transaction Deleted Successfully");
	}

	@Override
	public String getAwardHierarchyDataList(String awardNumber, String selectedAwardNumber) throws Exception {
		return commonDao.convertObjectToJSON(awardDao.fetchAllawardHierarchy(awardNumber, selectedAwardNumber));
	}

	public AwardReportTerms buildReportTracking(AwardReportTerms awardReportTerm, List<Date> dates) {
		for (int i = 0; i < dates.size(); i++) {
			AwardReportTracking reportTracking = new AwardReportTracking();
			reportTracking.setAwardNumber(awardReportTerm.getAwardNumber());
			reportTracking.setAwardId(awardReportTerm.getAwardId());
			reportTracking.setSequenceNumber(awardReportTerm.getSequenceNumber());
			reportTracking.setAwardReportTerms(awardReportTerm);
			reportTracking.setDueDate(new Timestamp(dates.get(i).getTime()));
			reportTracking.setUpdateTimestamp(awardReportTerm.getUpdateTimestamp());
			reportTracking.setUpdateUser(awardReportTerm.getUpdateUser());
			ReportStatus pending = awardDao.getPendingReportStatus(Constants.PENDING_REPORT_STATUS_CODE);
			reportTracking.setStatusCode(pending.getStatusCode());
			awardReportTerm.getAwardReportTracking().add(reportTracking);
		}
		return awardReportTerm;
	}

	public List<AwardReportTracking> findOutdatedTrackings(AwardReportTerms awardTerm, List<Date> dates) {
		List<AwardReportTracking> updatedReportTrackings = new ArrayList<>();
		List<AwardReportTracking> reportTrackings = awardTerm.getAwardReportTracking();
		for (AwardReportTracking rt : reportTrackings) {
			if (rt.getStatusCode().equals(Constants.PENDING_REPORT_STATUS_CODE) && (!dates.contains(rt.getDueDate()))) {
				updatedReportTrackings.add(rt);
			}
		}
		return updatedReportTrackings;
	}

	@Override
	public String saveAwardHierarchyData(MaintainAwardHierarchyVO vo, AwardVO awardVO) throws Exception {
		String response = null;
		try {
			if (vo.getAcType().equalsIgnoreCase("I")) {
				AwardVO childVO = new AwardVO();
				awardVO.setAwardId(Integer.parseInt(vo.getParentAwardId()));
				awardVO.setUpdateUser(vo.getLoggedUserName());
				awardVO.setIsAwardHierarchy(true);
				awardVO.setCopyOtherInformation(vo.getCopyOtherInformation());
				awardVO.setCopyQuestionnaire(vo.getCopyQuestionnaire());
				awardVersionService.copyAward(awardVO);
				Award childAward = copyChildAwardData(awardVO.getAward(), childVO);
				saveAwardHierarchy(vo, childAward);
				return commonDao.convertObjectToJSON(childAward);
			} else if (vo.getAcType().equalsIgnoreCase(Constants.acTypeDelete)) {
				response = deleteAwardHierarchyData(vo);
			}
		} catch (Exception e) {
			logger.error("error in saveAwardHierarchyData : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(response);
	}

	@SuppressWarnings("unused")
	private void saveAwardPersons(List<AwardPerson> awardPersons, Award award) {
		try {
			List<AwardPersonUnit> awardPersonUnitList = new ArrayList<>();
			List<AwardPerson> awardKeyPerson = new ArrayList<>();
			for (AwardPerson awardPerson : awardPersons) {
				awardPerson.setAwardId(award.getAwardId());
				awardPerson.setAwardNumber(award.getAwardNumber());
				awardPerson.setAwardPersonId(null);
				awardPersonUnitList.addAll(awardPerson.getAwardPersonUnits());
				awardKeyPerson.add(awardPerson);
				awardDao.saveAwardKeyPersonnel(awardPerson);
			}
		} catch (Exception e) {
			logger.error("error in saveAwardPersons : {}", e.getMessage());
		}
	}
	
	private Award copyChildAwardData(Award parentData, AwardVO awardVO) {
		awardVO.setAward(parentData);
		awardVO.getAward().setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_PENDING);
		awardVO.getAward().setAwardDocumentTypeCode(Constants.AWARD_SETUP);
		awardDao.saveOrUpdateAwardDetails(awardVO.getAward());
		return awardVO.getAward();
	}

	private String saveAwardHierarchy(MaintainAwardHierarchyVO vo, Award award) {
		AwardHierarchy awardHierarchy = new AwardHierarchy();
		awardHierarchy.setRootAwardNumber(getRootAwardNumber(vo.getParentAwardNumber()));
		awardHierarchy.setAwardNumber(award.getAwardNumber());
		awardHierarchy.setParentAwardNumber(vo.getParentAwardNumber());
		awardHierarchy.setOriginatingAwardNumber(vo.getParentAwardNumber());
		awardHierarchy.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardHierarchy.setCreateUser(vo.getLoggedUserName());
		awardHierarchy.setActive("Y");
		awardHierarchy.setAwardHierarchyId(null);
		awardHierarchy.setOriginatingAwardNumber(vo.getRootAwardNumber());
		return awardDao.saveOrUpdateAwardHierarchy(awardHierarchy);
	}

	private String getRootAwardNumber(String awardNumber) {
			return awardNumber.substring(0, awardNumber.indexOf('-')) + "-00001";
	}

	@Override
	public void moveDataToAwardHierarchy(Award award) {
		try {
			AwardHierarchy awardHierarchy = new AwardHierarchy();
			awardHierarchy.setRootAwardNumber(award.getAwardNumber());
			awardHierarchy.setAwardNumber(award.getAwardNumber());
			awardHierarchy.setParentAwardNumber("000000-00000");
			awardHierarchy.setOriginatingAwardNumber(award.getAwardNumber());
			awardHierarchy.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardHierarchy.setCreateUser(award.getUpdateUser());
			awardHierarchy.setActive("Y");
			awardHierarchy.setAwardHierarchyId(null);
			awardHierarchy.setOriginatingAwardNumber(award.getAwardNumber());
			awardDao.saveOrUpdateAwardHierarchy(awardHierarchy);
		} catch (Exception e) {
			logger.error("error in moveDataToAwardHierarchy : {}", e.getMessage());
			throw new ApplicationException("Error occurred in moveDataToAwardHierarchy", e, Constants.JAVA_ERROR);
		}
	}

	public Award copyParentDetails(String awardId) {
		return awardDao.fetchAwardByAwardId(awardId);
	}

	public List<AwardPerson> copyAwardPersons(String awardId) {
		return awardDao.fetchAllAwardPesonsByAwardId(awardId);
	}

	@Override
	public String getBaseAwardNumber(String awardNumber) {
		String nxtAwdno = awardDao.getNextChildAwardNumber(awardNumber);
		String baseNumber = awardNumber.substring(0, 6);
		String secondPart = String.format("%05d", Integer.parseInt(nxtAwdno));
		return baseNumber + "-" + secondPart;
	}

	private String deleteAwardHierarchyData(MaintainAwardHierarchyVO vo) {
		return deleteAwardData(getAward(vo.getAwardNumber()), vo.getAwardNumber(), vo);
	}

	private List<AwardHierarchy> getAward(String awardNumber) {
		return awardDao.getAwardHierarchyByAwardNumber(awardNumber);
	}

	private List<AwardHierarchy> getAllChildAwardsbyParentId(String parentAwardNumber) {
		return awardDao.getChildAwards(parentAwardNumber);
	}

	private String deleteAwardData(List<AwardHierarchy> awardHierarchyData, String parntAwardNumber,
			MaintainAwardHierarchyVO vo) {
		List<AwardHierarchy> child = getAllChildAwardsbyParentId(parntAwardNumber);
		List<AwardPerson> awardPersons = getAllAwardPersons(vo.getAwardId());
		String resonse = null;
		if (child.isEmpty() && !awardHierarchyData.isEmpty()) {
			if (!awardPersons.isEmpty()) {
				deleteAwardPersons(awardPersons);
			}
			awardDao.deleteChildAwards(awardHierarchyData.get(0));
			awardDao.deleteAwardByAwardNumber(awardHierarchyData.get(0).getAwardNumber());
			resonse = "success";
		} else {
			resonse = "failed";
		}
		return resonse;
	}

	private void deleteAwardPersons(List<AwardPerson> awardPersons) {
		List<AwardPersonUnit> awardPersonsUnit = new ArrayList<>();
		for (AwardPerson awardPerson : awardPersons) {
			awardPersonsUnit.addAll(awardPerson.getAwardPersonUnits());
			if (!awardPerson.getAwardPersonUnits().isEmpty()) {
				for (AwardPersonUnit awardPersonUnits : awardPersonsUnit) {
					awardDao.deleteAwardPersonnelUnitByUnitId(awardPersonUnits);
				}
			}
			awardDao.deleteAwardPersonnel(awardPerson.getAwardPersonId());
		}
	}

	private List<AwardPerson> getAllAwardPersons(Integer awardId) {
		return awardDao.getAwardPersonList(awardId);
	}

	private List<AwardAttachment> getAwardAttachmentByAwardId(Integer awardId, String awardLeadUnitNumber) {
		Boolean isPersonHasPermission = personHasPermission(awardId);
		if (Boolean.FALSE.equals(isPersonHasPermission) && awardLeadUnitNumber != null) {
			isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_CONFIDENTIAL_AWARD_ATTACHMENTS_RIGHT, awardLeadUnitNumber);
		}
		List <AwardAttachment> awardAttachments = awardDao.getAwardAttachmentsByAwardId(awardId, isPersonHasPermission);
		if (awardAttachments != null && !awardAttachments.isEmpty()) {
			for(AwardAttachment awardAttachment : awardAttachments) {
				if (awardAttachment.getUpdateUser() != null) {
					awardAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(awardAttachment.getUpdateUser()));
				}
			}
		}
		return awardAttachments;
	}

	@Override
	public String saveOrUpdateKeyPersonnel(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		Award award = awardDao.getAwardDetailsById(awardId);
		AwardPerson awardPerson = personnelVO.getAwardPerson();
		if (Boolean.TRUE.equals(awardPerson.getIsPi()) || awardPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
			if (awardPerson.getAwardPersonUnits() != null && !awardPerson.getAwardPersonUnits().isEmpty()) {
				for (AwardPersonUnit awardPersonUnit : awardPerson.getAwardPersonUnits()) {
					if (Boolean.TRUE.equals(awardPersonUnit.getLeadUnitFlag())) {
						award.setLeadUnitNumber(awardPersonUnit.getUnitNumber());
						award.setLeadUnit(commonDao.getLeadUnitByUnitNumber(awardPersonUnit.getUnitNumber()));
						award.setUpdateUser(awardPerson.getUpdateUser());
						award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						award = awardDao.saveOrUpdateAwardDetails(award);
					}
				}
			}
		}
		List<AwardPerson> awardPersons = awardDao.getAwardPersonList(awardId);
		if (Boolean.TRUE.equals(awardPerson.getIsPi()) || awardPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
			for (AwardPerson awardPersonal : awardPersons) {
				if (Boolean.TRUE.equals(awardPersonal.getIsPi()) || awardPersonal.getPersonRoleId().equals(Constants.PI_ROLE_CODE) && awardPersonal.getPersonId() != null) {
					deleteAwardPersonRole(awardPersonal.getPersonId(), awardId);
				}
			}
		} else {
			for (AwardPerson awardPersonal : awardPersons) {
				if (awardPersonal.getPersonId() != null) {
					if (awardPersonal.getPersonId().equals(awardPerson.getPersonId())) {
						deleteAwardPersonRole(awardPersonal.getPersonId(), awardId);
					}
				}
			}
		}
		List<AwardPersonUnit> list = awardPerson.getAwardPersonUnits();
		if (list != null && !list.isEmpty()) {
			List<AwardPersonUnit> updatedList = new ArrayList<>(list);
			Collections.copy(updatedList, list);
			for (AwardPersonUnit awardPersonUnit : list) {
				awardPersonUnit.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				if (Boolean.TRUE.equals(awardPersonUnit.getIsDeleted())) {
					awardDao.deleteAwardPersonUnit(awardPersonUnit);
					updatedList.remove(awardPersonUnit);
				}
			}
			awardPerson.getAwardPersonUnits().clear();
			awardPerson.getAwardPersonUnits().addAll(updatedList);
		}
		awardPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		if (!awardPerson.getAwardPersonAttachment().isEmpty()) {
			AwardPersonAttachment awardPersonAttachment = awardPerson.getAwardPersonAttachment().get(0);
			if (awardPersonAttachment.getAttachmentId() == null) {
				awardPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardPersonAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			}
		}
		awardDao.saveAwardKeyPersonnel(awardPerson);
		List<AwardPerson> updatedAwardPersons = new ArrayList<>(awardPersons);
		String personId = null;
		Collections.copy(updatedAwardPersons, awardPersons);
		if (awardPersons == null) {
			updatedAwardPersons.add(awardPerson);
		} else {
			if (personnelVO.getAcType().equals("I")) {
				updatedAwardPersons.add(awardPerson);
			} else if (personnelVO.getAcType().equals("U")) {
				for (AwardPerson person : awardPersons) {
					if (person.getAwardPersonId().equals(awardPerson.getAwardPersonId())) {
						personId = person.getPersonId();
						updatedAwardPersons.remove(person);
						updatedAwardPersons.add(awardPerson);
					}
				}
			}
		}
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.AWARD_MODULE_CODE);
		if (derivedRoles != null && !derivedRoles.isEmpty()) {
			String createUserId = awardDao.getAwardCreateUser(award.getAwardNumber());
			if (personId != null && createUserId != null && personId.equals(createUserId)) {
				assignDerivedRolesForCreator(award, createUserId, derivedRoles);
			}
		}
		setAwardUpdateUser(awardId, personnelVO.getAwardPerson().getUpdateUser());
		assignDerivedRolesForPerson(awardId, personnelVO.getAwardPerson());
		if (updatedAwardPersons != null && !updatedAwardPersons.isEmpty()) {
			Collections.sort(updatedAwardPersons,
					(updatedAwardPersons1, updatedAwardPersons2) -> updatedAwardPersons1.getProposalPersonRole().getSortId() < updatedAwardPersons2.getProposalPersonRole().getSortId() ? -1
							: updatedAwardPersons1.getProposalPersonRole().getSortId().equals(updatedAwardPersons2.getProposalPersonRole().getSortId()) ? 0 : 1);
		}
		personnelVO.setAwardPersons(updatedAwardPersons);
		personnelVO.setAward(award);
		return commonDao.convertObjectToJSON(personnelVO);
		
	}

	private void assignDerivedRolesForPerson(Integer awardId, AwardPerson awardPerson) {
		assignDerivedRolesForPI(awardPerson, awardId);
		assignDerivedRolesForCOI(awardPerson, awardId);
	}

	private void assignDerivedRolesForCOI(AwardPerson awardPerson, Integer awardId) {
		List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRolesByParams(awardPerson.getPersonId(),awardId, null);
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCOI(Constants.AWARD_MODULE_CODE);
		if (awardPerson.getPersonId() != null && awardPerson.getPersonRoleId().equals((Constants.COI_ROLE_CODE))) {
			for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
				if (ifRoleAlreadyExist(moduleDerivedRole.getRoleId(), awardPersonRoles).equals(Boolean.FALSE)) {
					addAwardPersonToAwardRoles(awardPerson, moduleDerivedRole.getRoleId());
				}
			}
		}
	}

	@Override
	public void assignDerivedRolesForPI(AwardPerson awardPerson, Integer awardId) {
		List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRolesByParams(awardPerson.getPersonId(),awardId, null);
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForPI(Constants.AWARD_MODULE_CODE);
		if (awardPerson.getPersonId() != null && (Boolean.TRUE.equals(awardPerson.getIsPi())|| awardPerson.getPersonRoleId().equals((Constants.PI_ROLE_CODE)))) {
			for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
				if (ifRoleAlreadyExist(moduleDerivedRole.getRoleId(), awardPersonRoles).equals(Boolean.FALSE)) {
					addAwardPersonToAwardRoles(awardPerson, moduleDerivedRole.getRoleId());
				}
			}
		}
	}

	@Override
	public void assignDerivedRolesForCreator(Award award, String createPersonId, List<ModuleDerivedRoles> derivedRoles) {
		AwardPerson awardPerson = setCreartorAsAwardPerson(award, createPersonId);
		List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRolesByParams(awardPerson.getPersonId(),award.getAwardId(), null);
		if (awardPerson.getPersonId() != null) {
			for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
				if (ifRoleAlreadyExist(moduleDerivedRole.getRoleId(), awardPersonRoles).equals(Boolean.FALSE)) {
					addAwardPersonToAwardRoles(awardPerson, moduleDerivedRole.getRoleId());
				}
			}
		}
	}

	private AwardPerson setCreartorAsAwardPerson(Award award, String createPersonId) {
		AwardPerson awardPerson = new AwardPerson();
		awardPerson.setAwardId(award.getAwardId());
		awardPerson.setPersonId(createPersonId);
		awardPerson.setAwardNumber(award.getAwardNumber());
		awardPerson.setSequenceNumber(award.getSequenceNumber());
		awardPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardPerson.setUpdateUser(award.getUpdateUser());
		return awardPerson;
	}

	private Boolean ifRoleAlreadyExist(Integer roleId, List<AwardPersonRoles> awardPersonRoles) {
		for (AwardPersonRoles awPersonRole : awardPersonRoles) {
			if (awPersonRole.getRoleId().equals(roleId)) {
				awPersonRole.setIsSystemGenerated(true);
				awardDao.saveOrUpdateAwardPersonRoles(awPersonRole);
				return true;
			}
		}
		return false;
	}

	@Override
	public String deleteKeyPersonnel(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		Award award =awardDao.getAwardDetailsById(awardId);
		Integer awardPersonId = personnelVO.getAwardPersonalId();
		AwardPerson awardPersonal = awardDao.getAwardPersonById(awardPersonId);
		if (awardPersonal.getPersonId() != null) {
			deleteAwardPersonRole(awardPersonal.getPersonId(), awardId);
		} else {
			deleteAwardPersonRole(awardPersonal.getRolodexId().toString(), awardId);
		}
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.AWARD_MODULE_CODE);
		if (derivedRoles != null && !derivedRoles.isEmpty()) {
			String createUserId = awardDao.getAwardCreateUser(award.getAwardNumber());
			if (awardPersonal.getPersonId() != null && createUserId != null && awardPersonal.getPersonId().equals(createUserId)) {
				assignDerivedRolesForCreator(award, createUserId, derivedRoles);
			}
		}
		timesheetDao.deleteAwardKeyPersonTimesheetByParams(awardPersonId, awardId);
		awardDao.deleteAwardPersonnel(awardPersonId);
		List<AwardPerson> awardPersons = awardDao.getAwardPersonList(awardId);
		List<AwardPerson> updateAwardPersons = new ArrayList<>();
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson awardPerson : awardPersons) {
				if (!awardPerson.getAwardPersonId().equals(awardPersonId)) {
					updateAwardPersons.add(awardPerson);
				}
				personnelVO.setAwardPersons(updateAwardPersons);
			}
		}
		personnelVO.setAward(setAwardUpdateUser(awardId, personnelVO.getUpdateUser()));
		return commonDao.convertObjectToJSON(personnelVO);
	}

	private void deleteAwardPersonRole(String awardPersonId, Integer awardId) {
		List<Integer> roleIds = roleManagementService.getDerivedRoleIdForDelete(Constants.AWARD_MODULE_CODE);
		List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRolesByParams(awardPersonId, awardId, roleIds);
		awardPersonRoles = awardPersonRoles.stream().filter(awardPersonRole -> awardPersonRole.getIsSystemGenerated().equals(Boolean.TRUE)).collect(Collectors.toList());
		if (awardPersonRoles != null && !awardPersonRoles.isEmpty()) {
			for (AwardPersonRoles awardPersonRole : awardPersonRoles) {
				awardDao.deleteAwardPersonRoles(awardPersonRole);
			}
		}
	
	}

	private Award setAwardUpdateUser(Integer awardId, String userName) {
		Award award = awardDao.getAwardDetailsById(awardId);
		award.setUpdateUser(userName);
		award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		award.setDocumentUpdateUser(userName);
		award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		return awardDao.saveOrUpdateAwardDetails(award);
	}

	@Override
	public String saveOrUpdateAwardProjectTeam(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		personnelVO.getAwardProjectTeam().setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		personnelVO.getAwardProjectTeam().setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardDao.saveOrUpdateAwardProjectTeam(personnelVO.getAwardProjectTeam());
		personnelVO.setAwardProjectTeams(awardDao.getAwardProjectTeamList(awardId));
		setAwardUpdateUser(awardId, AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(personnelVO);
	}

	@Override
	public String deleteAwardProjectTeam(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		Integer awardPersonId = personnelVO.getAwardPersonalId();
		awardDao.deleteAwardProjectTeam(awardPersonId);
		personnelVO.setAwardProjectTeams(awardDao.getAwardProjectTeamList(awardId));
		setAwardUpdateUser(awardId, AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(personnelVO);
	}

	@Override
	public String saveOrUpdateAwardContact(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		awardDao.saveOrUpdateAwardContact(personnelVO.getAwardContact());
		personnelVO.setAwardContacts(prepareAwardContactList(awardId));
		setAwardUpdateUser(awardId, personnelVO.getAwardContact().getUpdateUser());
		return commonDao.convertObjectToJSON(personnelVO);
	}

	@Override
	public String deleteAwardContact(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		Integer awardPersonId = personnelVO.getAwardPersonalId();
		awardDao.deleteAwardContact(awardPersonId);
		personnelVO.setAwardContacts(awardDao.getAwardContactList(awardId));
		setAwardUpdateUser(awardId, personnelVO.getUpdateUser());
		return commonDao.convertObjectToJSON(personnelVO);
	}

	@Override
	public String deleteAwardKeyword(AwardVO vo) {
		try {
			Award award = awardDao.getAwardDetailsById(vo.getAwardId());
			List<AwardKeyword> awardKeywords = award.getAwardKeywords();
			List<AwardKeyword> updatedAwardKeywords = new ArrayList<>(awardKeywords);
			Collections.copy(updatedAwardKeywords, awardKeywords);
			for (AwardKeyword awardKeyword : awardKeywords) {
				if (awardKeyword.getAwardKeywordId().equals(vo.getAwardKeywordId())) {
					updatedAwardKeywords.remove(awardKeyword);
				}
			}
			award.getAwardKeywords().clear();
			award.getAwardKeywords().addAll(updatedAwardKeywords);
			award.setDocumentUpdateUser(vo.getUpdateUser());
			award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardDetails(award);
			vo.setAward(award);
			vo.setStatus(true);
			vo.setMessage("Award keyword deleted successfully");
		} catch (Exception e) {
			vo.setStatus(true);
			vo.setMessage("Problem occurred in deleting award keyword");
			logger.error("error in deleteAwardKeyword :{}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAwardPersonUnit(PersonnelVO vo) {
		try {
			AwardPerson awardPerson = awardDao.getAwardPersonById(vo.getAwardPersonalId());
			List<AwardPersonUnit> awardPersonUnits = awardPerson.getAwardPersonUnits();
			List<AwardPersonUnit> updatedAwardPersonUnits = new ArrayList<>(awardPersonUnits);
			Collections.copy(updatedAwardPersonUnits, awardPersonUnits);
			for (AwardPersonUnit awardPersonUnit : awardPersonUnits) {
				if (awardPersonUnit.getAwardPersonUnitId().equals(vo.getAwardPersonUnitId())) {
					updatedAwardPersonUnits.remove(awardPersonUnit);
				}
			}
			awardPerson.getAwardPersonUnits().clear();
			awardPerson.getAwardPersonUnits().addAll(updatedAwardPersonUnits);
			awardDao.saveOrUpdateAwardPersonDetails(awardPerson);
			vo.setAwardPerson(awardPerson);
		} catch (Exception e) {
			logger.error("error in deleteAwardPersonUnit :{}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void canTakeRoutingAction(AwardVO awardVO) {
		Award award = awardVO.getAward();
		String awardId = award.getAwardId().toString();
		Workflow workflow = awardVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(awardId, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		}
		if (workflow != null) {
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(),
					maxApprovalStopNumber);
			if (finalWorkflowDetails != null && !finalWorkflowDetails.isEmpty()) {
				for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
					if (finalWorkflowDetail.getApproverPersonId().equals(awardVO.getPersonId())
							|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
						awardVO.setFinalApprover(true);
					}
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				boolean currentPerson = true;
				if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (currentPerson == true) {
							if (workflowDetail.getApproverPersonId().equals(awardVO.getPersonId())) {
								if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)
										&& workflowDetail.getApprovalStatusCode()
												.equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
									if (workflowDetail.getApprovalStatusCode()
											.equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
										awardVO.setIsApproved(true);
									} else {
										awardVO.setIsApproved(false);
									}
									awardVO.setIsApprover(true);
								}
							}
						}
					}
				}
			}
		}
	}

	@Override
	public String addAwardPersonAttachment(MultipartFile[] files, String formDataJSON) {
		AwardVO awradVO = null;
		List<AwardPersonAttachment> awardPersonAttachments = new ArrayList<>();
		try {
			ObjectMapper mapper = new ObjectMapper();
			awradVO = mapper.readValue(formDataJSON, AwardVO.class);
			List<AwardPersonAttachment> newAttachments = awradVO.getNewPersonAttachments();
			if (files.length == 0 && newAttachments != null && !newAttachments.isEmpty()) {
				for (AwardPersonAttachment newAttachment : newAttachments) {
					if (newAttachment.getReplaceAttachmentId() != null) {
						awardDao.deleteAwardPersonAttachment(newAttachment.getReplaceAttachmentId());
					}
				}
			} else {
				for (int i = 0; i < files.length; i++) {
					for (AwardPersonAttachment newAttachment : newAttachments) {
						if (newAttachment.getReplaceAttachmentId() != null) {
							awardDao.deleteAwardPersonAttachment(newAttachment.getReplaceAttachmentId());
						}
						AwardPersonAttachment awardPersonAttachment = new AwardPersonAttachment();
						awardPersonAttachment.setFileName(newAttachment.getFileName());
						awardPersonAttachment.setDescription(newAttachment.getDescription());
						awardPersonAttachment.setMimeType(newAttachment.getMimeType());
						FileData fileData = new FileData();
						fileData.setAttachment(files[i].getBytes());
						fileData = commonDao.saveFileData(fileData);
						awardPersonAttachment.setFileDataId(fileData.getFileDataId());
						awardPersonAttachment.setUpdateUser(newAttachment.getUpdateUser());
						awardPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						awardPersonAttachment.setAttachment(files[i].getBytes());
						awardPersonAttachments.add(awardPersonAttachment);
					}
				}
			}
			awradVO.setNewPersonAttachments(awardPersonAttachments);
		} catch (Exception e) {
			logger.error("error in addAwardPersonAttachment :{}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(awradVO);
	}

	@Override
	public ResponseEntity<byte[]> downloadAwardPersonAttachment(Integer attachmentid) {
		AwardPersonAttachment attachment = awardDao.fetchAwardPersonAttachmentById(attachmentid);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("error in downloadAwardPersonAttachment :{}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public AwardVO sendAwardNotification(AwardVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.isCancelRequest() ? vo.getModuleItemKey().toString() : vo.getAward().getAwardId().toString());
		emailServiceVO.setPlaceHolder(getAwardPlaceholders(vo));
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && emailServiceVO.getPrompted() != true) {
			vo.setNotificationTypeId(notificationTypeId);
			vo.setBody(emailServiceVO.getBody());
			vo.setSubject(emailServiceVO.getSubject());
		}
		return vo;
	}

	@Override
	public Map<String, String> getAwardPlaceholders(AwardVO awardVO) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{USER_NAME}", awardVO.getUserName() != null ? awardVO.getUserName() : "");
		placeHolder.put("{AWARD_PERSON_ROLE}", awardVO.getUserRole() != null ? awardVO.getUserRole() : "");
		if (awardVO.getServiceRequest() != null) {
			placeHolder.put("{SERVICE_REQUEST_OUTCOME}",
					awardVO.getServiceRequest().getServiceRequestStatus().getDescription() != null
							? awardVO.getServiceRequest().getServiceRequestStatus().getDescription()
							: "");
		}
		placeHolder.put("{WORKFLOW_COMMENT}", awardVO.getApproveComment() != null ? awardVO.getApproveComment() : "No Comments");
		String stopName = commonService.getPlaceHolderDataForRouting(awardVO.getApproverStopNumber(),awardVO.getMapId(), awardVO.getWorkflowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	private void updateAwardBudgetFundCodeOnAwardSave(Award award) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(award.getAwardId());
		if (awardBudgetHeader != null) {
			awardBudgetHeader.setFundCode(award.getAccountNumber());
			awardBudgetHeader.setFundCenter(award.getFundCenter());
			awardBudgetDao.saveBudgetHeader(awardBudgetHeader);
		}
	}

	@Override
	public String showAwardHistory(AwardVO vo) {
		List<Award> awards = awardDao.getAwardHistoryDetails(vo.getAwardNumber());
		getServiceRequestForAwards(awards);
		if (Boolean.TRUE.equals(vo.getIsAwardHistoryTab())) {
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SAP_AWARD_FEED)) {
				getSapFeedStatusOfAwards(awards);
			}
		} else {
			getModuleEditableSectionsForAwards(awards);			
		}
		awards.stream().sorted(Comparator.comparing(Award::getSequenceNumber)).collect(Collectors.toList());
		vo.setAwards(awards);
		return commonDao.convertObjectToJSON(vo);
	}

	private void getModuleEditableSectionsForAwards(List<Award> awards) {
		Set<String> awardIds = awards.stream().filter(award -> award.getSequenceNumber() > 1 && award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_PENDING)).map(Award::getAwardId).map(s->String.valueOf(s)).collect(Collectors.toSet());
		if (!awardIds.isEmpty()) {
			List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSectionsByModuleItemKeys(new ArrayList<>(awardIds), Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE);
			if (!moduleVariableSections.isEmpty()) {
				Map<String, List<ModuleVariableSection>> moduleVariableSectionsMap = moduleVariableSections.stream().collect(Collectors.groupingBy(ModuleVariableSection::getModuleItemKey));
				awards.stream().filter(award -> award.getAwardId() != null).filter(award -> moduleVariableSectionsMap.containsKey(String.valueOf(award.getAwardId())))
				.forEach(award -> award.setModuleVariableSections(moduleVariableSectionsMap.get(String.valueOf(award.getAwardId()))));
				
			}
		}
	}

	private void getServiceRequestForAwards(List<Award> awards) {
		Set<String> awardIds = awards.stream().filter(award -> award.getSequenceNumber() > 1).map(Award::getAwardId).map(s->String.valueOf(s)).collect(Collectors.toSet());
		if (!awardIds.isEmpty()) {
			List<ServiceRequest> serviceRequestDetails = awardDao.getServiceRequestDetailsBasedOnModuleCodeAndOriginatingModuleItemKeys(Constants.MODULE_CODE_AWARD, new ArrayList<>(awardIds));
			if (!serviceRequestDetails.isEmpty()) {
				Map<Integer, ServiceRequest> serviceRequestMap = serviceRequestDetails.stream().collect(Collectors.toMap(serviceRequest -> Integer.parseInt(serviceRequest.getOriginatingModuleItemKey()), serviceRequest -> serviceRequest));
				awards.stream().filter(award -> serviceRequestMap.containsKey(award.getAwardId())).forEach(award -> {
					ServiceRequest serviceRequest = serviceRequestMap.get(award.getAwardId());
					if (serviceRequest != null) {
						award.setServiceRequest(serviceRequest);
						if (serviceRequest.getSubject() != null) {
							award.setServiceRequestSubject(serviceRequest.getSubject());
						}
					}
				});	
			}
		}
	}

	private void getSapFeedStatusOfAwards(List<Award> awards) {
		awards.stream().filter(award -> award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_ARCHIVE)).forEach(award -> {
			award.setSapFeedStatus(awardDao.fetchFeedStatusBasedOnAward(award.getAwardId(), award.getAwardNumber()));
		});
	}

	@Override
	public String fetchAwardPersonRoles(PersonnelVO personnelVO) {
		Integer awardId = personnelVO.getAwardId();
		String response = "";
		if (awardId != null) {
			List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRoles(awardId);
			if (awardPersonRoles != null && !awardPersonRoles.isEmpty()) {
				personnelVO.setAwardPersonRoles(awardPersonRoles);
			}
			List<ModuleDerivedRoles> moduleDerivedRoles = rolesManagementDao.getModuleDerivedRoles(Constants.AWARD_MODULE_CODE);
			if (moduleDerivedRoles != null && !moduleDerivedRoles.isEmpty()) {
				personnelVO.setModuleDerivedRoles(moduleDerivedRoles);
			}
			response = commonDao.convertObjectToJSON(personnelVO);
		}
		return response;
	}

	@Override
	public String maintainAwardPersonRoles(PersonnelVO personnelVO) {
		AwardPersonRoles awardPersonRole = personnelVO.getAwardPersonRole();
		awardPersonRole.setIsSystemGenerated(false);
		awardPersonRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardPersonRole.setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardPersonRole.setRole(rolesManagementDao.getRoleInformation(awardPersonRole.getRoleId()));
		awardPersonRole = awardDao.saveOrUpdateAwardPersonRoles(awardPersonRole);
		awardPersonRole.setPerson(personDao.getPersonDetailById(awardPersonRole.getPersonId()));
		sendAwardPermissionNotification(awardPersonRole, Constants.ADD_AWARD_ROLE_NOTIFICATION_CODE);
		updateAwardDocumentUpdateUserAndTimestamp(awardPersonRole.getAwardId(), AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(awardPersonRole);
	}

	@Override
	public String deleteAwardPersonRoles(PersonnelVO personnelVO) {
		AwardPersonRoles awardPersonRole = awardDao.fetchAwardPersonRoleBasedonAwardPersonRoleId(personnelVO.getAwardPersonRoleId());
		awardDao.deleteAwardPersonRoles(awardPersonRole);
		personnelVO.setMessage("Sucess");
		sendAwardPermissionNotification(awardPersonRole, Constants.REMOVE_AWARD_ROLE_CODE);
		updateAwardDocumentUpdateUserAndTimestamp(awardPersonRole.getAwardId(), AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(personnelVO);
	}

	@Override
	public String addAwardReportTrackingAttachment(MultipartFile[] files, String formDataJson) {
		AwardVO awardVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			awardVO = mapper.readValue(formDataJson, AwardVO.class);
			List<AwardReportTrackingFile> awardReportTrackingFiles = awardVO.getAwardReportTrackingFiles();
			switch (awardVO.getActionType()) {
				case Constants.acTypeInsert:
					addNewReportTrackingAttachment(files, awardReportTrackingFiles);
					break;
				case Constants.acTypeReplace:
					replaceReportTrackingAttachment(files, awardReportTrackingFiles.get(0));
					break;
				case Constants.acTypeDelete:
					awardDao.deleteReportTrackingAttachment(null, awardVO.getAwardReportTrackingId(), null);
					break;
				default:
					break;
			}
			updateAwardDocumentUpdateUserAndTimestamp(awardVO.getAwardId(), AuthenticatedUser.getLoginUserName());
		} catch (Exception e) {
			logger.error("error in addAwardReportTrackingAttachment : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(awardVO);
	}

	private void replaceReportTrackingAttachment(MultipartFile[] files, AwardReportTrackingFile awardReportTrackingFile) throws IOException {
		awardReportTrackingFile.setAwardReportTrackingFileId(null);
		awardReportTrackingFile.setVersionNumber(awardReportTrackingFile.getVersionNumber() + 1);
		awardReportTrackingFile.setDocumentStatusCode("1");
		FileData fileData = new FileData();
		fileData.setAttachment(files[0].getBytes());
		fileData = commonDao.saveFileData(fileData);
		awardReportTrackingFile.setFileId(fileData.getFileDataId());
		awardDao.saveOrUpdateAwardReportTrackingFile(awardReportTrackingFile);
		awardDao.archiveOldAttachmentVersion(awardReportTrackingFile.getAwardReportTrackingId(), awardReportTrackingFile.getVersionNumber() - 1);
	}

	private void addNewReportTrackingAttachment(MultipartFile[] files, List<AwardReportTrackingFile> awardReportTrackingFiles) throws IOException {
		int i = 0;
		while (files.length > i) {
			for (AwardReportTrackingFile attachment : awardReportTrackingFiles) {
				attachment.setFileName(files[i].getOriginalFilename());
				attachment.setContentType(files[i].getContentType());
				attachment.setVersionNumber(1);
				attachment.setDocumentStatusCode("1");
				FileData fileData = new FileData();
				fileData.setAttachment(files[i].getBytes());
				fileData = commonDao.saveFileData(fileData);
				attachment.setFileId(fileData.getFileDataId());
				awardDao.saveOrUpdateAwardReportTrackingFile(attachment);
				i++;
			}
		}
	}

	@Override
	public ResponseEntity<byte[]> downloadAwardReportTrackingAttachment(Integer awardReportTrackingFileId) {
		AwardReportTrackingFile attachment = awardDao.getAwardReportTrackingFileByFileId(awardReportTrackingFileId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("error in downloadAwardReportTrackingAttachment :{}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String deleteAwardReportTrackingAttachment(AwardVO vo) {
		AwardReportTrackingFile awardReportTrackingFile = awardDao.getAwardReportTrackingFileByFileId(vo.getAwardReportTrackingFileId());
		deleteAllReportVersions(awardReportTrackingFile);
		vo.setMessage("success");
		updateAwardDocumentUpdateUserAndTimestamp(awardReportTrackingFile.getAwardId(), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteAllReportVersions(AwardReportTrackingFile awardReportTrackingFile) {
		if (awardReportTrackingFile.getDocumentStatusCode() != null && awardReportTrackingFile.getDocumentStatusCode().equals("1")) {
			List<AwardReportTrackingFile> awardReportTrackingFiles = awardDao.getReportTrackingAttachmentVersions(awardReportTrackingFile.getAwardReportTrackingId());
			awardReportTrackingFiles.forEach(trackingFile -> {
				if (Boolean.FALSE.equals(awardDao.getIsFileDataIdFound(trackingFile.getFileId()))) {
					commonDao.deleteFileData(commonDao.getFileDataById(trackingFile.getFileId()));
				}
				awardDao.deleteAwardReportTrackingFile(trackingFile);
			});
		}
			if (Boolean.FALSE.equals(awardDao.getIsFileDataIdFound(awardReportTrackingFile.getFileId()))) {
				commonDao.deleteFileData(commonDao.getFileDataById(awardReportTrackingFile.getFileId()));
			}
			awardDao.deleteAwardReportTrackingFile(awardReportTrackingFile);
	}

	@Override
	public AwardVO withdrawAward(MultipartFile[] files, String formDataJSON) {
		AwardVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, AwardVO.class);
			Award award = awardDao.getAwardDetailsById(vo.getAwardId());
			vo.setAward(award);
			withdrawAndCancelAward(vo, award, formDataJSON);
			saveWithdrawAwardComment(award, vo.getDescription(), vo.getCommentTypeCode(), vo.getUpdateUser());
			AwardAttachmentsVO awardAttachmentVO = new AwardAttachmentsVO();
			awardAttachmentVO.setAwardId(award.getAwardId());
			awardAttachmentVO.setAwardNumber(award.getAwardNumber());
			awardAttachmentVO.setAwardSequenceNumber(award.getSequenceNumber());
			awardAttachmentVO.setNewAttachments(vo.getAwardAttachments());
			saveAwardAttachments(awardAttachmentVO, files);
			getPendingAwardDetails(vo, award.getAwardNumber(), award.getAwardId());
		} catch (Exception e) {
			logger.error("error in withdrawAward :{}", e.getMessage());
		}
		return vo;
	}

	private AwardVO withdrawWorkFlow(AwardVO vo, String formDataJson) {
		if (formDataJson != null) {
			businessRuleService.approveOrRejectWorkflow(null, formDataJson, Constants.AWARD_MODULE_CODE.toString(), Constants.AWARD_SUBMODULE_CODE.toString());
		} else {
			vo.setFileContent(null);
			businessRuleService.approveOrRejectAwardWorkflow(vo);
		}
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(vo.getAwardId().toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			vo.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(vo.getAwardId().toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				vo.setWorkflowList(workFlows);
			}
		}
		return vo;
	}

	private Award withdrawAwardInAwardSetUP(AwardVO vo, String formDataJson) {
		Award award = vo.getAward();
		award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_WITHDRAWN);
		award.setAwardWorkflowStatus(awardDao.fetchAwardWorkflowStatusByStatusCode(Constants.AWARD_WORKFLOW_STATUS_WITHDRAWN));
		award = awardDao.saveOrUpdateAwardDetails(award); 
		withdrawWorkFlow(vo, formDataJson);
		updateAwardBudgetStatus(award, Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS);
		return award;
	}

	@Override
	public List<AwardContact> prepareAwardContactList(Integer awardId) {
		List<AwardContact> awardContacts = awardDao.getAwardContactList(awardId);
		for (AwardContact awardContact : awardContacts) {
			if (awardContact.getPersonId() != null) {
				Person person = personDao.getPersonDetailById(awardContact.getPersonId());
				if (person != null && person.getEmailAddress() != null) {
					awardContact.setEmailAddress(person.getEmailAddress());
				}
				if (person != null && person.getMobileNumber() != null) {
					awardContact.setPhoneNumber(person.getMobileNumber());
				}
			} else if (awardContact.getRolodexId() != null) {
				Rolodex rolodex = rolodexDao.getRolodexDetailById(awardContact.getRolodexId());
				if (rolodex != null && rolodex.getPhoneNumber() != null) {
					awardContact.setEmailAddress(rolodex.getEmailAddress());
				}
				if (rolodex != null && rolodex.getPhoneNumber() != null) {
					awardContact.setPhoneNumber(rolodex.getPhoneNumber());
				}
			}
		}
		return awardContacts;
	}

	@Override
	public String updateAwardAttachmentDetails(AwardVO vo) {
		AwardAttachment awardAttachment = awardDao.fetchAwardAttachmentById(vo.getAwardAttachment().getAwardAttachmentId());
		awardAttachment.setDescription(vo.getAwardAttachment().getDescription());
		awardAttachment.setNarrativeStatusCode(vo.getAwardAttachment().getNarrativeStatusCode());
		awardAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardAttachment.setUpdateUser(vo.getAwardAttachment().getUpdateUser());
		awardDao.saveAttachment(awardAttachment);
		Boolean isPersonHasPermission = personHasPermission(vo.getAwardId());
		if (Boolean.FALSE.equals(isPersonHasPermission)) {
			isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_CONFIDENTIAL_AWARD_ATTACHMENTS_RIGHT, vo.getLeadUnitNumber());
		}
		List <AwardAttachment> awardAttachments = awardDao.getAwardAttachmentsBasedOnParams(vo.getAwardId(), vo.getAwardNumber(), vo.getAwardSequenceNumber(), isPersonHasPermission);
		if (awardAttachments != null && !awardAttachments.isEmpty()) {
			awardAttachments.stream().forEach(attachment  -> {
				if (attachment.getUpdateUser() != null) {
					attachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
				}
			});
		}
		Collections.sort(awardAttachments, (allAwardAttachmentsOne, allAwardAttachmentsTwo) -> allAwardAttachmentsTwo.getUpdateTimestamp().compareTo(allAwardAttachmentsOne.getUpdateTimestamp()));
		return commonDao.convertObjectToJSON(awardAttachments);
  }

	@Override
	public List<Report> fetchReportByReportClass(String reportClassCode) {
		List<ValidReportClass> validReportClasses = awardDao.fetchValidReportClassByReportClassCode(reportClassCode);
		List<Report> reports = new ArrayList<>();
		if (validReportClasses != null && !validReportClasses.isEmpty()) {
			for (ValidReportClass reportClass : validReportClasses) {
				reports.add(reportClass.getReport());
			}
		}
		return reports;
	}

	@Override
	public String saveServiceRequestFromAward(ServiceRequestVO vo) {
		ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId());
		Integer actionLogId = serviceRequestService.saveServiceRequestActionLog(Constants.EDIT_REQUEST_ACTION_CODE, serviceRequest);
		ServiceRequestHistory serviceRequestHistory = new ServiceRequestHistory();
		serviceRequestHistory.setActionLogId(actionLogId);
		serviceRequestHistory.setServiceRequestId(vo.getServiceRequestId());
		saveServiceRequestStatusHistory(actionLogId, serviceRequest.getServiceRequestId());
		if (!serviceRequest.getSubject().equals(vo.getServiceRequestSubject())) {
			serviceRequestHistory.setPreviousSubject(serviceRequest.getSubject());
			serviceRequestHistory.setSubject(vo.getServiceRequestSubject());
			serviceRequest.setSubject(vo.getServiceRequestSubject());
		}
		if (!serviceRequest.getDescription().equals(vo.getServiceRequestDescription())) {
			serviceRequestHistory.setPreviousDescription(serviceRequest.getDescription());
			serviceRequestHistory.setDescription(vo.getServiceRequestDescription());
			serviceRequest.setDescription(vo.getServiceRequestDescription());
		}
		serviceRequestDao.saveServiceRequestHistory(serviceRequestHistory);
		return commonDao.convertObjectToJSON(serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest));
	}

	private void saveServiceRequestStatusHistory(Integer actionLogId, Integer serviceRequestId) {
		ServiceRequestStatusHistory serviceRequestStatusHistory = new ServiceRequestStatusHistory();
		serviceRequestStatusHistory.setActionLogId(actionLogId);
		serviceRequestStatusHistory.setActionStartTime(commonDao.getCurrentTimestamp());
		serviceRequestStatusHistory.setStatusCode(1);
		serviceRequestStatusHistory.setServiceRequestId(serviceRequestId);
		serviceRequestDao.saveOrUpdateServiceRequestStatusHistory(serviceRequestStatusHistory);
	}

	private void sendAwardOutcomeNotificationForCancelAward(AwardVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		sendAwardNotification(vo, cancelOrWithdrawNotificationType(vo.isCancelRequest()), dynamicEmailRecipients);
	}

	private Integer cancelOrWithdrawNotificationType(boolean isCancelRequest) {
		return isCancelRequest == true ? Constants.SERVICE_REQUEST_CANCEL_NOTIFICATION_CODE : Constants.SERVICE_REQUEST_WITHDRAW_NOTIFICATION_CODE;
	}

	@Override
	public String awardInvitation(EmailServiceVO emailServiceVO) {
		try {
			int limit = 0;
			int mailGroupRecipiantLimit = emailServiceVO.getRecipients().size() / 50;
			mailGroupRecipiantLimit = (int) Math.ceil(mailGroupRecipiantLimit / 100.0);
			while (limit <= mailGroupRecipiantLimit) {
				emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
				emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
				emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				emailServiceVO = emailService.sendEmail(emailServiceVO);
				limit++;
			}
		} catch (Exception e) {
			logger.error("error in awardInvitation :{}", e.getMessage());
		}
		return commonDao.convertObjectToJSON("success");
	}

	@Override
	public void sentAwardRemainders() {
		List<AwardReportReminder> awardReportReminders = awardDao.fetchAllActiveAwardReportReminder();
		if (awardReportReminders != null && !awardReportReminders.isEmpty()) {
			awardReportReminders.forEach(awardReportReminder -> {
				List<AwardReportTerms> awardReportTerms = awardDao.fetchAllAwardReportTermsBasedOnParams(awardReportReminder.getReportClassCode(), awardReportReminder.getReportCode(), awardReportReminder.getFrequencyCode());
				Set<Integer> reportTermIds = new HashSet<>();
				awardReportTerms.forEach(awardReportTerm -> {
					reportTermIds.add(awardReportTerm.getAwardReportTermsId());
				});
				if (reportTermIds != null && !reportTermIds.isEmpty()) {
					List<AwardReportTracking> awardReportTrackings = awardDao.fetchAwardReportTrackingBasedOnReportTermIds(reportTermIds);
					if (awardReportTrackings != null && !awardReportTrackings.isEmpty()) {
						boolean isProgressReportEnabled = commonDao.getParameterValueAsBoolean(Constants.AWARD_PROGRESS_REPORT_ENABLED);
						if (Boolean.TRUE.equals(isProgressReportEnabled)) {
							awardReportTrackings.stream().filter(awardReportTracking -> (awardReportTracking.getDueDate() != null) && (awardReportTracking.getProgressReportId() == null)).forEach(awardReportTracking -> {
								sentAwardReportRemainders(awardReportTracking, awardReportReminder);
							});
						} else {
							awardReportTrackings.stream().filter(awardReportTracking -> (awardReportTracking.getDueDate() != null)).forEach(awardReportTracking -> {
								List<AwardReportTrackingFile> awardReportTrackingFiles = awardDao.fetchAwardReportTrackingFileBasedOnAwardReportTrackingId(awardReportTracking.getAwardReportTrackingId());
								if (awardReportTrackingFiles.isEmpty() || awardReportTrackingFiles == null) {
									sentAwardReportRemainders(awardReportTracking, awardReportReminder);
								}
							});
						}
					}
				}
			});
		}
	}

	private void sentAwardReportRemainders(AwardReportTracking awardReportTracking, AwardReportReminder awardReportReminder) {
		LocalDate currentDate = LocalDate.now();
		Date dueDateAndTime = commonDao.adjustTimezone(awardReportTracking.getDueDate());
		LocalDate dueDate = dueDateAndTime.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		if (dueDate.minusDays(awardReportReminder.getDaysToDueDate()).equals(currentDate)) {
			logger.info("report Tracking Id : " + awardReportTracking.getAwardReportTrackingId() + "...dueDate : "+ dueDate + "....zone : " + ZoneId.systemDefault());
			sentAwardRemainderNotification(awardReportReminder.getDaysToDueDate(), awardReportTracking.getAwardReportTerms(), awardReportReminder.getNotificationTypeId(), awardReportTracking.getDueDate());
		}	
	}

	private void sentAwardRemainderNotification(Integer daysToDueDate, AwardReportTerms awardReportTerm, Integer notificationTypeCode, Timestamp dueDate) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeCode);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setModuleItemKey(awardReportTerm.getAwardId().toString());
		emailServiceVO.setPlaceHolder(getAwardRemainderPlaceholders(daysToDueDate, awardReportTerm, dueDate));
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getAwardRemainderPlaceholders(Integer daysToDueDate, AwardReportTerms awardReportTerm, Timestamp dueDate) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{DAYS_TO_DUE__DATE}", daysToDueDate == null ? "" : daysToDueDate.toString());
		placeHolder.put("{REPORT_CLASS}", awardReportTerm.getReportClassCode() == null ? "" : awardDao.getReportClassByCode(awardReportTerm.getReportClassCode()).getDescription());
		placeHolder.put("{REPORT_TYPE}", awardReportTerm.getReportCode() == null ? "" : awardDao.getReportNameByReportCode(awardReportTerm.getReportCode()));
		placeHolder.put("{DUE_DATE}", awardReportTerm.getDueDate() == null ? "" : commonService.convertDateFormatBasedOnTimeZone(awardReportTerm.getDueDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
		placeHolder.put("{REPORT_TRACKING_DUE_DATE}", dueDate == null ? "" : commonService.convertDateFormatBasedOnTimeZone(dueDate.getTime(), Constants.DEFAULT_DATE_FORMAT));
		return placeHolder;
	}

	@Override
	public String saveAwardPaymentAndInvoices(AwardPaymentAndInvoicesVO vo) {
		Award award = awardDao.getAwardDetailsById(vo.getAwardId());
		award.setBasisOfPaymentCode(vo.getBasisOfPaymentCode());
		if (vo.getBasisOfPaymentCode() != null) {
			award.setAwardBasisOfPayment(awardDao.getAwardBasisOfPaymentById(vo.getBasisOfPaymentCode()));
		}
		award.setMethodOfPaymentCode(vo.getMethodOfPaymentCode());
		if (vo.getMethodOfPaymentCode() != null) {
			award.setAwardMethodOfPayment(awardDao.getAwardMethodOfPaymentById(vo.getMethodOfPaymentCode()));
		}
		award.setPaymentInvoiceFrequencyCode(vo.getPaymentInvoiceFrequencyCode());
		if (vo.getPaymentInvoiceFrequencyCode() != null) {
			award.setFrequency(awardDao.getFrequencyByFrequencyCode(vo.getPaymentInvoiceFrequencyCode()));
		}
		award.setInvoiceNoOfCopies(vo.getInvoiceNoOfCopies());
		award.setFinalInvoiceDue(vo.getFinalInvoiceDue());
		award.setInvoiceInstructions(vo.getInvoiceInstructions());
		award.setDfafsNumber(vo.getDfafsNumber());
		updateAwardDocumentUpdateUserAndTimestamp(award.getAwardId(), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(awardDao.saveOrUpdateAwardDetails(award));
	}

	@Override
	public String getAwardPaymentAndInvoices() {
		AwardPaymentAndInvoicesVO vo = new AwardPaymentAndInvoicesVO();
		vo.setAwardBasisOfPayments(awardDao.getAwardBasisOfPaymentList());
		vo.setAwardMethodOfPayments(awardDao.getAwardMethodOfPaymentList());
		vo.setFrequencies(awardDao.getInvoiceFrequencyList());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateAwardMilestone(AwardMilestoneVO vo) {
		if (vo.getAwardMilestone().getAwardMilestoneId() == null)
			vo.getAwardMilestone().setMilestoneNumber(commonDao.getCurrentTimestamp().toLocalDateTime().toString());
		awardDao.saveOrUpdateAwardMileStone(vo.getAwardMilestone());
		awardDao.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardMilestone().getAwardId());
		vo.getAwardMilestone().setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAwardMilestone(AwardMilestoneVO vo) {
		if (vo.getAwardMilestoneId() != null) {
			vo.setMessage(awardDao.deleteAwardMilestone(vo.getAwardMilestoneId()));
		}
		awardDao.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId());
		return commonDao.convertObjectToJSON(vo);
	}

	private void copyMilestoneFromDevelopmentProposal(AwardLinkInstituteProposalVO awardLinkInstituteProposalVO) {
		List<AwardMileStone> awardMileStones = awardDao.fetchAwardMileStonesBasedOnAwardId(awardLinkInstituteProposalVO.getAwardId());
		if (awardMileStones.isEmpty() || awardMileStones == null) {
			List<ProposalMileStone> proposalMilestones = proposalModuleDao.fetchProposalMileStonesBasedOnProposalId(awardDao.fetchDevProposalByInstProposal(awardLinkInstituteProposalVO.getProposalId()));
			Award award = awardDao.fetchAwardByAwardId(awardLinkInstituteProposalVO.getAwardFundingProposal().getAwardId().toString());
			if (proposalMilestones != null && !proposalMilestones.isEmpty() && award != null) {
				Timestamp awardBeginDate = award.getBeginDate();
				Timestamp awardStartDate = null;
				for (ProposalMileStone proposalMileStone : proposalMilestones) {
					AwardMileStone awardMileStone = new AwardMileStone();
					Integer duration = proposalMileStone.getDuration();
					Integer proposalStartMonth = proposalMileStone.getStartMonth();
					Calendar c1 = Calendar.getInstance();
					c1.setTimeInMillis(awardBeginDate.getTime());
					c1.add(Calendar.MONTH, proposalStartMonth);
					awardStartDate = new Timestamp(c1.getTime().getTime());
					awardMileStone.setStartDate(awardStartDate);
					Calendar c2 = Calendar.getInstance();
					c2.setTimeInMillis(awardStartDate.getTime());
					c2.add(Calendar.MONTH, duration);
					c2.add(Calendar.DATE, -1);
					awardMileStone.setEndDate(new Timestamp(c2.getTime().getTime()));
					awardMileStone.setDuration(getDurationValue(awardMileStone.getStartDate(), awardMileStone.getEndDate()));
					awardMileStone.setMilestone(proposalMileStone.getMileStone());
					awardMileStone.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					awardMileStone.setUpdateUser(awardLinkInstituteProposalVO.getUpdateUser());
					awardMileStone.setAwardId(award.getAwardId());
					awardMileStone.setAwardNumber(award.getAwardNumber());
					awardMileStone.setSequenceNumber(award.getSequenceNumber());
					awardMileStone.setMilestoneNumber(commonDao.getCurrentTimestamp().toLocalDateTime().toString());
					awardDao.saveOrUpdateAwardMileStone(awardMileStone);
				}
				awardLinkInstituteProposalVO.setAwardMileStones(awardDao.fetchAwardMileStonesBasedOnAwardId(award.getAwardId()));
			}
		}
	}

	public void copyKPIFromDevelopmentProposal(AwardLinkInstituteProposalVO vo) {
		List<AwardKPI> awardKpi = awardDao.fetchAllAwardKPI(vo.getAwardFundingProposal().getAwardId());
		if (awardKpi == null || awardKpi.isEmpty()) {
			Award award = awardDao.fetchAwardByAwardId(vo.getAwardFundingProposal().getAwardId().toString());
			List<ProposalKPI> proposalKPIs = proposalDao.fetchAllProposalKPI(awardDao.fetchDevProposalByInstProposal(vo.getProposalId()));
			if (proposalKPIs != null && !proposalKPIs.isEmpty()) {
				for (ProposalKPI proposalKPI : proposalKPIs) {
					List<AwardKPICriteria> awardKPICriterias = new ArrayList<>();
					AwardKPI awardKPI = new AwardKPI();
					for (ProposalKPICriteria proposalKPICriteria : proposalKPI.getProposalKPICriterias()) {
						AwardKPICriteria awardKPICriteria = new AwardKPICriteria();
						awardKPICriteria.setAwardKPI(awardKPI);
						awardKPICriteria.setKpiCriteriaType(proposalKPICriteria.getKpiCriteriaType());
						awardKPICriteria.setKpiCriteriaTypeCode(proposalKPICriteria.getKpiCriteriaTypeCode());
						awardKPICriteria.setTarget(proposalKPICriteria.getTarget());
						awardKPICriteria.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						awardKPICriteria.setUpdateUser(vo.getUpdateUser());
						awardKPICriterias.add(awardKPICriteria);
					}
					awardKPI.setKpiType(proposalKPI.getKpiType());
					awardKPI.setAwardId(award.getAwardId());
					awardKPI.setKpiTypeCode(proposalKPI.getKpiTypeCode());
					awardKPI.setAwardNumber(award.getAwardNumber());
					awardKPI.setSequenceNumber(award.getSequenceNumber());
					awardKPI.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					awardKPI.setUpdateUser(vo.getUpdateUser());
					awardKPI.setAwardKPICriterias(awardKPICriterias);
					awardDao.saveOrUpdateAwardKPI(awardKPI);
				}
			}
		}
	}

	@Override
	public String saveOrUpdateAwardKPI(AwardKPIVO awardKpiVo) {
		awardKpiVo.getAwardKpis().forEach(awardKpi -> awardDao.saveOrUpdateAwardKPI(awardKpi));
		awardKpiVo.getAwardKpis().clear();
		awardKpiVo.setAwardKpis(awardDao.fetchAllAwardKPI(awardKpiVo.getAwardId()));
		updateAwardDocumentUpdateUserAndTimestamp(awardKpiVo.getAwardId(), AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(awardKpiVo);
	}

	@Override
	public String deleteAwardKPI(AwardKPIVO awardKpiVo) {
		awardKpiVo.setMessage(awardDao.deleteAwardKPI(awardKpiVo.getAwardKPIId(), awardKpiVo.getAwardId(), awardKpiVo.getAwardKPICriteriaId()));
		updateAwardDocumentUpdateUserAndTimestamp(awardKpiVo.getAwardId(), AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(awardKpiVo);
	}

	@Override
	public void exportAllAwardAttachments(AwardVO vo, HttpServletResponse response) {
		List<Integer> attachmentIds = vo.getAttachmentIds();
		Integer awardId = vo.getAwardId();
		Award award = awardDao.getAwardDetailsById(awardId);
		if (award != null && attachmentIds != null && !attachmentIds.isEmpty()) {
			String fileName = "Award_#"+ awardId + "_attachments";
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<AwardAttachment> attachments = awardDao.fetchAwardAttachmentBasedOnAttachmentIds(attachmentIds);
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ZipOutputStream zos = new ZipOutputStream(baos);
				if (attachments != null && !attachments.isEmpty()) {
					Integer index = 0;
					for (AwardAttachment attachment : attachments) {
						index = commonService.addFilesToZipFolder(index, attachment.getFileName(), zos);
						FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
						byte[] data = fileData.getAttachment();
						zos.write(data);
					}
				}
				zos.closeEntry();
				zos.flush();
				baos.flush();
				zos.close();
				baos.close();
				ServletOutputStream op = response.getOutputStream();
				op.write(baos.toByteArray());
				op.flush();
				op.close();
			} catch (Exception e) {
				logger.error("error in exportAllAwardAttachments :{}", e.getMessage());
			}
		}
	}

	private void cancelTaskBasedOnAwardId(AwardVO vo) {
		List<Task> tasks = taskDao.fetchTasksByModuleCodeAndModuleItemId(Constants.AWARD_MODULE_CODE, vo.getAwardId());
		if (tasks != null && !tasks.isEmpty()) {
			for (Task taskDetail : tasks) {
				Task task = taskDao.fetchTaskByTaskId(taskDetail.getTaskId());
				task.setTaskStatusCode(Constants.TASK_STATUS_CODE_CANCELLED);
				task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_CANCELLED));
				task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				task.setUpdateUser(vo.getUpdateUser());
				taskDao.saveOrUpdateTask(task);
				TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
				String systemComment = taskType.getDescription() + "Task Cancelled";
				TaskActionLog taskActionLog = taskService.saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_CANCELLED, systemComment, vo.getUpdateUser());
				taskService.saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_CANCELLED, taskActionLog.getActionLogId(), vo.getUpdateUser());
				inboxDao.removeTaskMessageFromInbox(Integer.parseInt(task.getModuleItemId()), task.getTaskId(), task.getModuleCode(), Constants.AWARD_TASK_SUBMODULE_CODE);
			}
		}
	}

	@Override
	public String unlinkGrantCallFromAward(AwardVO awardVO) {
		if (awardVO.getAwardId() != null) {
			Award award = awardDao.fetchAwardByAwardId(awardVO.getAwardId().toString());
			award.setGrantHeaderId(null);
			award.setDocumentUpdateUser(awardVO.getUpdateUser());
			award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardDetails(award);
		}
		return commonDao.convertObjectToJSON(awardVO);
	}

	@Override
	public String saveOrUpdateAwardAreaOfResearch(AwardVO vo) {
		awardDao.saveOrUpdateAwardResearchArea(vo.getAwardResearchArea());
		setAwardUpdateUser(vo.getAwardResearchArea().getAwardId(), vo.getAwardResearchArea().getUpdateUser());
		return commonDao.convertObjectToJSON(awardDao.fetchAwardResearchAreaBasedOnAwardId(vo.getAwardResearchArea().getAwardId()));
	}

	@Override
	public String deleteAwardResearchArea(AwardVO vo) {
		try {
			awardDao.deleteAwardResearchArea(awardDao.fetchAwardResearchArea(vo.getResearchAreaId()));
			vo.setAwardResearchAreas(awardDao.fetchAwardResearchAreaBasedOnAwardId(vo.getAwardId()));
			vo.setStatus(true);
			vo.setMessage("Award research area deleted successfully");
			setAwardUpdateUser(vo.getAwardId(), vo.getUpdateUser());
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting Award research area");
			logger.error("Exception in deleting Award research area: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveDescriptionOfAward(AwardVO vo) {
		Award award = awardDao.getAwardDetailsById(vo.getAwardId());
		award.setResearchDescription(vo.getResearchDescription());
		award.setMultiDisciplinaryDescription(vo.getMultiDisciplinaryDescription());
		award.setUpdateUser(vo.getUpdateUser());
		award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		award.setDocumentUpdateUser(vo.getUpdateUser());
		award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		return commonDao.convertObjectToJSON(awardDao.saveOrUpdateAwardDetails(award));
	}

	@Override
	public String saveAwardWorkflowStatusForSponsor(AwardVO vo) {
		Integer awardId = vo.getAwardId();
		Award award = awardDao.getAwardDetailsById(awardId);
		if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS)) {
			award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_HOLD);
			award.setAwardWorkflowStatus(awardDao.fetchAwardWorkflowStatusByStatusCode(Constants.AWARD_WORKFLOW_STATUS_HOLD));
			award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription());
		} else if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_HOLD)) {
			award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS);
			award.setAwardWorkflowStatus(awardDao.fetchAwardWorkflowStatusByStatusCode(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS));
			award.setWorkFlowStatusName(award.getAwardWorkflowStatus().getDescription());
			award.setFunderApprovalDate(vo.getFunderApprovalDate());
		}
		if (award.getSubmitUser() != null) {
			award.setSubmitUserFullName(personDao.getUserFullNameByUserName(award.getSubmitUser()));
		}
		if (award.getCreateUser() != null) {
			award.setCreateUserFullName(personDao.getUserFullNameByUserName(award.getCreateUser()));
		}
		if (award.getUpdateUser() != null) {
			award.setUpdateUserFullName(personDao.getUserFullNameByUserName(award.getUpdateUser()));
		}
		award.setUpdateUser(vo.getUpdateUser());
		award.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		award.setDocumentUpdateUser(vo.getUpdateUser());
		award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		vo.setAward(awardDao.saveOrUpdateAwardDetails(award));
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(awardId.toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			vo.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(awardId.toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				vo.setWorkflowList(workFlows);
			}
		}
		Integer canApproveRouting = businessRuleDao.canApproveRouting(awardId.toString(), vo.getPersonId(),	Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
		vo.setCanApproveRouting(canApproveRouting.toString());
		return commonDao.convertObjectToJSON(vo);
	}

	private void sendAwardPermissionNotification(AwardPersonRoles awardPersonRole, Integer notificationCode) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		commonService.setNotificationRecipients(awardPersonRole.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		AwardVO awardVO = new AwardVO();
		awardVO.setUserName(awardPersonRole.getPerson().getFullName());
		awardVO.setUserRole(awardPersonRole.getRole().getRoleName());
		Award award = awardDao.getAwardDetailsById(awardPersonRole.getAwardId());
		awardVO.setAward(award);
		sendAwardNotification(awardVO, notificationCode, dynamicEmailRecipients);
	}

	private void saveWithdrawAwardComment(Award award, String comment, String commentTypeCode, String updateUser) {
		AwardComment awardComment = new AwardComment();
		awardComment.setAward(award);
		awardComment.setAwardId(award.getAwardId());
		awardComment.setAwardNumber(award.getAwardNumber());
		awardComment.setComments(comment);
		awardComment.setCommentTypeCode(commentTypeCode);
		awardComment.setSequenceNumber(award.getSequenceNumber());
		awardComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardComment.setUpdateUser(updateUser);
		awardDao.saveOrUpdateAwardComment(awardComment);
	}

	private List<AwardAttachment> getPreviousAwardAttachments(Integer awardId) {
		List<AwardAttachment> awardAttachments = new ArrayList<>();
		String awardNumber = awardDao.getAwardNumberBasedOnAwardId(awardId);
		Integer awardSequenceNumber = awardDao.getAwardSequenceNumberBasedOnAwardId(awardId);
		List<Integer> awardIds = awardDao.getPreviousAwardIdsBasedOnParams(awardNumber, awardSequenceNumber);
		if (awardIds != null && !awardIds.isEmpty()) {
			awardAttachments = awardDao.getAwardAttachmentsByAwardIds(awardIds);
			if (awardAttachments != null && !awardAttachments.isEmpty()) {
				for (AwardAttachment awardAttachment : awardAttachments) {
					if (awardAttachment.getUpdateUser() != null) {
						awardAttachment.setLastUpdateUserFullName(
								personDao.getUserFullNameByUserName(awardAttachment.getUpdateUser()));
					}
				}
			}
		}
		return awardAttachments;
	}

	private String getDurationValue(Timestamp startTime, Timestamp endTime) {
		String duration = "";
		try {
			Date startDate = commonDao.adjustTimezone(new Date(startTime.getTime()));
			Date endDate = commonDao.adjustTimezone(new Date(endTime.getTime()));
			LocalDate localStartDate = new Timestamp(startDate.getTime()).toLocalDateTime().toLocalDate();
			LocalDate localEndDate = new Timestamp(endDate.getTime()).toLocalDateTime().toLocalDate().plusDays(1);
			Period difference = Period.between(localStartDate, localEndDate);
			duration = difference.getYears() + " year(s), " + difference.getMonths() + " month(s) & "
					+ difference.getDays() + " day(s)";
		} catch (Exception e) {
			logger.error("Exception in getDurationValue : {}", e.getMessage());
		}
		return duration;
	}

	@Override
	public String addAwardPersonAttachmentForwaf(AwardVO vo) {
		List<AwardPersonAttachment> awardPersonAttachments = new ArrayList<>();
		try {
			MultipartFile multipartFile = null;
			List<AwardPersonAttachment> newAttachments = vo.getNewPersonAttachments();
			String splicedFile = vo.getFileContent();
			if (splicedFile == null && newAttachments != null && !newAttachments.isEmpty()) {
				for (AwardPersonAttachment newAttachment : newAttachments) {
					if (newAttachment.getReplaceAttachmentId() != null) {
						awardDao.deleteAwardPersonAttachment(newAttachment.getReplaceAttachmentId());
					}
				}
			} else {
				if (splicedFile != null) {
					multipartFile = commonService.uploadMedia(splicedFile, vo.getFileName(), vo.getRemaining(), vo.getLength(), vo.getFileTimestamp(), vo.getPersonId(), vo.getContentType());
				}
				if (newAttachments != null && !newAttachments.isEmpty()) {
					for (AwardPersonAttachment newAttachment : newAttachments) {
						if (multipartFile != null && !multipartFile.isEmpty()) {
							List<AwardPersonAttachment> awardPersonAttachmentDatas = awardDao.fetchAwardPersonAttachmentBasedOnAwardId(vo.getAwardId());
							if (newAttachment.getReplaceAttachmentId() != null) {
								awardDao.deleteAwardPersonAttachment(newAttachment.getReplaceAttachmentId());
							}
							String replaceFileName = newAttachment.getFileName();
							boolean isRenameRequired = false;
							int count = 1;
							isRenameRequired = checkForPersonAttachmentDuplication(newAttachment.getFileName(),
									awardPersonAttachmentDatas);
							while (isRenameRequired) {
								replaceFileName = newAttachment.getFileName();
								replaceFileName = generateFileName(replaceFileName, count);
								count = count + 1;
								isRenameRequired = checkForPersonAttachmentDuplication(replaceFileName,
										awardPersonAttachmentDatas);
							}
							awardPersonAttachments.add(setAwardPersonAttachment(newAttachment, multipartFile));
						}
					}
				}
			}
			vo.setNewPersonAttachments(awardPersonAttachments);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private boolean checkForPersonAttachmentDuplication(String fileName, List<AwardPersonAttachment> awardPersonAttachments) {
		for (AwardPersonAttachment awardPersonAttachment : awardPersonAttachments) {
			if (fileName.equals(awardPersonAttachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String addAwardAttachmentsForwaf(AwardAttachmentsVO awardAttachmentsVO) {
		MultipartFile multipartFile = null;
		Integer awardId = awardAttachmentsVO.getAwardId();
		String awardNumber = awardAttachmentsVO.getAwardNumber();
		Integer sequenceNumber = awardAttachmentsVO.getAwardSequenceNumber();
		List<AwardAttachment> awardAttachmentList = getAwardAttachmentByAwardId(awardId, null);
		AwardAttachment attachment = awardAttachmentsVO.getAwardAttachment();
		String splicedFile = awardAttachmentsVO.getFileContent();
		String timestamp = awardAttachmentsVO.getFileTimestamp();
		if (splicedFile != null) {
			multipartFile = commonService.uploadMedia(awardAttachmentsVO.getFileContent(), awardAttachmentsVO.getFileName(),
					awardAttachmentsVO.getRemaining(), awardAttachmentsVO.getLength(), timestamp,
					awardAttachmentsVO.getPersonId(), awardAttachmentsVO.getContentType());
		}
		if (multipartFile != null && !multipartFile.isEmpty()) {
			Integer documentId = getDocumentIdForAttachment(awardAttachmentList);
			Integer versionNumber = 0;
			awardAttachmentList.addAll(setAwardAttachmentObject(attachment, awardAttachmentList, multipartFile,
					versionNumber, documentId, awardId, sequenceNumber, awardNumber));
			awardAttachmentsVO.setNewAttachments(awardAttachmentList);
			List<AwardAttachment> previousAwardAttachments = getPreviousAwardAttachments(awardId);
			if (previousAwardAttachments != null && !previousAwardAttachments.isEmpty()) {
				awardAttachmentsVO.getNewAttachments().addAll(previousAwardAttachments);
			}
		}
		return commonDao.convertObjectToJSON(awardAttachmentsVO);
	}

	private Integer getDocumentIdForAttachment(List<AwardAttachment> awardAttachmentList) {
		Integer documentId = 0;

		if (awardAttachmentList != null && !awardAttachmentList.isEmpty()) {
			Collections.sort(awardAttachmentList, (awardAttachmentList1,
					awardAttachmentList2) -> awardAttachmentList1.getDocumentId() > awardAttachmentList2.getDocumentId()
							? -1
							: awardAttachmentList1.getDocumentId() == awardAttachmentList2.getDocumentId() ? 0 : 1);
			documentId = awardAttachmentList.get(0).getDocumentId();
		}
		return documentId;
	}

	private List<AwardAttachment> setAwardAttachmentObject(AwardAttachment attachment,
			List<AwardAttachment> awardAttachmentList, MultipartFile files, Integer versionNumber, Integer documentId,
			Integer awardId, Integer sequenceNumber, String awardNumber) {
		List<AwardAttachment> awardAttachments = new ArrayList<AwardAttachment>();
		List<AwardAttachment> allAwardAttachments = new ArrayList<>();
		if (awardAttachmentList != null && !awardAttachmentList.isEmpty()) {
			allAwardAttachments.addAll(awardAttachmentList);
		}
		List<AwardAttachment> previousAwardAttachments = getPreviousAwardAttachments(awardId);
		if (previousAwardAttachments != null && !previousAwardAttachments.isEmpty()) {
			allAwardAttachments.addAll(previousAwardAttachments);
		}
		String replaceFileName = attachment.getFileName();
		boolean isRenameRequired = false;
		int count = 1;
		isRenameRequired = checkForDuplication(attachment.getFileName(), allAwardAttachments);
		while (isRenameRequired) {
			replaceFileName = attachment.getFileName();
			replaceFileName = generateFileName(replaceFileName, count);
			count = count + 1;
			isRenameRequired = checkForDuplication(replaceFileName, allAwardAttachments);
		}
		if (attachment.getAwardAttachmentId() != null) {
			for (AwardAttachment awardAttachment : awardAttachmentList) {
				if (awardAttachment.getAwardAttachmentId() != null && awardAttachment.getAwardAttachmentId().equals(attachment.getAwardAttachmentId())) {
					AwardAttachment tempAwardAttachment = new AwardAttachment();
					File file = new File(files.getOriginalFilename());
					String fileName = file.getName();
					awardAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
					versionNumber = awardAttachment.getVersionNumber();
					documentId = awardAttachment.getDocumentId();
					tempAwardAttachment = addNewAwardAttachment(attachment, files, fileName, versionNumber, documentId, awardId, awardNumber, replaceFileName, sequenceNumber);
					awardDao.saveAttachment(awardAttachment);
					awardAttachments.add(tempAwardAttachment);
				}
			}
		} else {
			File file = new File(files.getOriginalFilename());
			String fileName = file.getName();
			if (attachment.getFileName().equals(fileName)) {
				documentId = documentId + 1;
				AwardAttachment newAttachment = new AwardAttachment();
				newAttachment = addNewAwardAttachment(attachment, files, fileName, versionNumber, documentId, awardId,
						awardNumber, replaceFileName, sequenceNumber);
				awardAttachments.add(newAttachment);
			}
		}
		return awardAttachments;
	}

	@Override
	public String addAwardReportTrackingAttachmentForWaf(AwardVO awardVO) {
		MultipartFile multipartFile = null;
		AwardReportTrackingFile awardReportTrackingFile = awardVO.getAwardReportTrackingFile();
		String splicedFile = awardVO.getFileContent();
		if (splicedFile != null) {
			multipartFile = commonService.uploadMedia(awardVO.getFileContent(), awardReportTrackingFile.getFileName(),
					awardVO.getRemaining(), awardVO.getLength(), awardVO.getFileTimestamp(), awardVO.getPersonId(),
					awardReportTrackingFile.getContentType());
		}
		if (multipartFile != null) {
			try {
				switch (awardVO.getActionType()) {
					case Constants.acTypeInsert:
						addNewReportTrackingAttachment(multipartFile, awardReportTrackingFile);
						break;
					case Constants.acTypeReplace:
						replaceReportTrackingAttachment(multipartFile, awardReportTrackingFile);
						break;					
					default:
						break;
				}
			} catch (Exception e) {
				logger.error("error while adding report tracking file", e);
			}
		} else if(awardVO.getActionType().equals(Constants.acTypeDelete)) {
			awardDao.deleteReportTrackingAttachment(null, awardVO.getAwardReportTrackingId(), null);
		}
		updateAwardDocumentUpdateUserAndTimestamp(awardVO.getAwardId(), AuthenticatedUser.getLoginUserName());
		return commonDao.convertObjectToJSON(awardVO);
	}

	private void replaceReportTrackingAttachment(MultipartFile files, AwardReportTrackingFile awardReportTrackingFile) throws IOException {
		awardReportTrackingFile.setAwardReportTrackingFileId(null);
		awardReportTrackingFile.setVersionNumber(awardReportTrackingFile.getVersionNumber() + 1);
		awardReportTrackingFile.setDocumentStatusCode("1");
		FileData fileData = new FileData();
		fileData.setAttachment(files.getBytes());
		fileData = commonDao.saveFileData(fileData);
		awardReportTrackingFile.setFileId(fileData.getFileDataId());
		awardDao.saveOrUpdateAwardReportTrackingFile(awardReportTrackingFile);
		awardDao.archiveOldAttachmentVersion(awardReportTrackingFile.getAwardReportTrackingId(), awardReportTrackingFile.getVersionNumber() - 1);
	}

	private void addNewReportTrackingAttachment(MultipartFile files, AwardReportTrackingFile awardReportTrackingFiles) throws IOException {
		awardReportTrackingFiles.setFileName(files.getOriginalFilename());
		awardReportTrackingFiles.setContentType(files.getContentType());
		awardReportTrackingFiles.setVersionNumber(1);
		awardReportTrackingFiles.setDocumentStatusCode("1");
		FileData fileData = new FileData();
		fileData.setAttachment(files.getBytes());
		fileData = commonDao.saveFileData(fileData);
		awardReportTrackingFiles.setFileId(fileData.getFileDataId());
		awardDao.saveOrUpdateAwardReportTrackingFile(awardReportTrackingFiles);
	}

	private AwardPersonAttachment setAwardPersonAttachment(AwardPersonAttachment newAttachment,
			MultipartFile multipartFile) {
		AwardPersonAttachment awardPersonAttachment = new AwardPersonAttachment();
		try {
			awardPersonAttachment.setFileName(newAttachment.getFileName());
			awardPersonAttachment.setDescription(newAttachment.getDescription());
			awardPersonAttachment.setMimeType(newAttachment.getMimeType());
			FileData fileData = new FileData();
			fileData.setAttachment(multipartFile.getBytes());
			fileData = commonDao.saveFileData(fileData);
			awardPersonAttachment.setFileDataId(fileData.getFileDataId());
			awardPersonAttachment.setUpdateUser(newAttachment.getUpdateUser());
			awardPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardPersonAttachment.setAttachment(multipartFile.getBytes());
		} catch (IOException e) {
			e.printStackTrace();
		}
		return awardPersonAttachment;
	}

	@Override
	public void updateAwardDocumentUpdateUserAndTimestamp(Integer awardId, String updateUser) {
		Award award = awardDao.fetchAwardByAwardId(awardId.toString());
		award.setDocumentUpdateUser(updateUser);
		award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardDao.saveOrUpdateAwardDetails(award);
	}

	public AwardVO withdrawAndCancelAward(AwardVO vo, Award award, String formDataJSON) {
		if (award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED)) {
			inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, award.getAwardId().toString(), vo.getPersonId(),
					Constants.MESSAGE_TYPE_AWARD_REJECT, Constants.SUBMODULE_ITEM_KEY,
					Constants.AWARD_SUBMODULE_CODE);
		}
		cancelTaskBasedOnAwardId(vo);
		if (!award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
			Award activeAward = awardDao.fetchActiveAwardByAwardNumber(award.getAwardNumber());
			ServiceRequest serviceRequest = awardDao.getServiceRequestBasedOnAwardId(activeAward.getAwardId().toString(), award.getAwardId().toString());
			if (serviceRequest != null) {
				if (vo.isCancelRequest()) {
					vo.setModuleItemKey(award.getAwardId());
					vo.setAward(activeAward);
					vo.setAwardId(activeAward.getAwardId());
					serviceRequest = serviceRequestService.updateSRStatusAndActionLog
							(serviceRequest,Constants.CANCEL_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_CANCEL);
					award.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_CANCELLED);
					datesAndAmountService.updateDatesAndAmounts(award.getAwardId(), award.getAwardNumber(), award.getSequenceNumber(), Constants.CANCELLED_TRANSACTION);
					award.setIsLatest(Boolean.FALSE);
					activeAward.setIsLatest(Boolean.TRUE);
					activeAward.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
					activeAward.setDocumentUpdateUser(AuthenticatedUser.getLoginUserName());
					awardDao.saveOrUpdateAwardDetails(activeAward);
					award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
					award.setDocumentUpdateUser(AuthenticatedUser.getLoginUserName());
					award = awardDao.saveOrUpdateAwardDetails(award);
					vo.getAward().setCanCreateVariationRequest(true);
					setSapFeedStatus(vo);
				} else {
					serviceRequest = serviceRequestService.updateSRStatusAndActionLog
							(serviceRequest,Constants.WITHDRAW_ACTION_CODE, Constants.SERVICE_REQUEST_STATUS_CODE_WITHDRAW);
					award.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_WITHDRAWN);
					award.setAwardWorkflowStatus(awardDao.fetchAwardWorkflowStatusByStatusCode(Constants.AWARD_WORKFLOW_STATUS_WITHDRAWN));
					award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
					award.setDocumentUpdateUser(AuthenticatedUser.getLoginUserName());
					award = awardDao.saveOrUpdateAwardDetails(award);
					withdrawWorkFlow(vo, formDataJSON);
					updateAwardBudgetStatus(award, Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS);
					vo.setAward(award);
					vo.getAward().setServiceRequestType(serviceRequest.getServiceRequestType());
					vo.setServiceRequest(serviceRequest);
				}
				serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
				sendAwardOutcomeNotificationForCancelAward(vo);
			}
		} else {
			award = withdrawAwardInAwardSetUP(vo, formDataJSON);
			vo.setAward(award);
		}
		getEditableSectionForAward(vo, award);
		inboxDao.removeFromInbox(award.getAwardId(), 0, Constants.AWARD_MODULE_CODE);
		return vo;
	}

	@Override
	public String withdrawAwardForWaf(AwardVO vo) {
		try {
			MultipartFile multipartFile = null;
			String splicedFile = vo.getFileContent();
			if (splicedFile != null) {
				multipartFile = commonService.uploadMedia(splicedFile, vo.getFileName(), vo.getRemaining(), vo.getLength(), vo.getFileTimestamp(), vo.getPersonId(), vo.getContentType());
			}
			Award award = awardDao.getAwardDetailsById(vo.getAwardId());
			vo.setAward(award);
			withdrawAndCancelAward(vo, award, null);
			saveWithdrawAwardComment(award, vo.getDescription(), vo.getCommentTypeCode(), vo.getUpdateUser());
			AwardAttachmentsVO awardAttachmentVO = new AwardAttachmentsVO();
			awardAttachmentVO.setAwardId(award.getAwardId());
			awardAttachmentVO.setAwardNumber(award.getAwardNumber());
			awardAttachmentVO.setAwardSequenceNumber(award.getSequenceNumber());
			awardAttachmentVO.setNewAttachments(vo.getAwardAttachments());
			saveAwardAttachmentsForWaf(awardAttachmentVO, multipartFile);
			return getAwardDetails(vo);
		} catch (Exception e) {
			logger.error("error in withdrawAward :{}", e.getMessage());
			e.printStackTrace();
		}
		vo.setCanEnableMilestoneStatus(commonDao.getParameterValueAsBoolean(Constants.SHOW_AWARD_MILESTONE_STATUS));
		return commonDao.convertObjectToJSON(vo);
	}

	private void saveAwardAttachmentsForWaf(AwardAttachmentsVO awardAttachmentVO, MultipartFile multipartFile) {
		Integer awardId = awardAttachmentVO.getAwardId();
		String awardNumber = awardAttachmentVO.getAwardNumber();
		Integer sequenceNumber = awardAttachmentVO.getAwardSequenceNumber();
		List<AwardAttachment> allAwardAttachments = new ArrayList<>();
		List<AwardAttachment> awardAttachmentDetails = getAwardAttachmentByAwardId(awardId, null);
		if (awardAttachmentDetails != null && !awardAttachmentDetails.isEmpty()) {
			allAwardAttachments.addAll(awardAttachmentDetails);
		}
		List<AwardAttachment> previousAwardAttachments = getPreviousAwardAttachments(awardId);
		if (previousAwardAttachments != null && !previousAwardAttachments.isEmpty()) {
			allAwardAttachments.addAll(previousAwardAttachments);
		}
		List<AwardAttachment> attachments = awardAttachmentVO.getNewAttachments();
		Integer documentId = awardDao.getMaxDocumentIdBasedOnAwardNumber(awardNumber);
		List<AwardAttachment> awardAttachments = new ArrayList<>();
		Integer versionNumber = 0;
		if (multipartFile != null && !multipartFile.isEmpty()) {
			for (AwardAttachment attachment : attachments) {
				File file = new File(multipartFile.getOriginalFilename());
				String fileName = file.getName();
				String replaceFileName = attachment.getFileName();
				boolean isRenameRequired = false;
				int count = 1;
				isRenameRequired = checkForDuplication(attachment.getFileName(), allAwardAttachments);
				while(isRenameRequired) {
					 replaceFileName = attachment.getFileName();
					 replaceFileName = generateFileName(replaceFileName, count);
					 count = count +1;
					 isRenameRequired = checkForDuplication(replaceFileName, allAwardAttachments);
				}
				if (attachment.getAwardAttachmentId() != null) {
					for (AwardAttachment awardAttachment : awardAttachmentDetails) {
						if (awardAttachment.getAwardAttachmentId() != null && awardAttachment.getAwardAttachmentId().equals(attachment.getAwardAttachmentId())) {
							awardAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
							versionNumber = awardAttachment.getVersionNumber();
							documentId = awardAttachment.getDocumentId();
							AwardAttachment tempAwardAttachment = addNewAwardAttachment(attachment, multipartFile, fileName, versionNumber, documentId, awardId, awardNumber, replaceFileName, sequenceNumber);
							awardDao.saveAttachment(awardAttachment);
							awardAttachments.add(tempAwardAttachment);
						}
					}
				} else {
					if (attachment.getFileName().equals(fileName)) {
						documentId = documentId + 1;
						AwardAttachment newAttachment = addNewAwardAttachment(attachment, multipartFile, fileName, versionNumber, documentId, awardId, awardNumber, replaceFileName, sequenceNumber);
						awardAttachments.add(newAttachment);
					}
				}
			}
		}
		if (awardAttachmentDetails != null) {
			awardAttachmentDetails.addAll(awardAttachments);
		}
		awardAttachmentVO.getNewAttachments().clear();
		awardAttachmentVO.setNewAttachments(awardAttachmentDetails);
		if (previousAwardAttachments != null && !previousAwardAttachments.isEmpty()) {
			awardAttachmentVO.getNewAttachments().addAll(previousAwardAttachments);
		}
	}

	@Override
	public void updateAwardBudgetStatus(Award award, String budgetStatus) {
		boolean editBudget = sectionWiseEditDao.isChangeBudgetStatus(award.getAwardId().toString(),
				Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE,
				Constants.BUDGET_EDITABLE_SECTION_TYPE_CODE);
		if (award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP) || editBudget) {
			updateAwardBudgetStatusByStatusCode(award.getAwardId(), budgetStatus);
		}
	}

	@Override
	public void updateAwardBudgetStatusByStatusCode(Integer awardId, String budgetStatus) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardId);
		if (awardBudgetHeader != null) {
			awardBudgetHeader.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(budgetStatus));
			awardBudgetHeader.setBudgetStatusCode(budgetStatus);
			awardBudgetDao.saveBudgetHeader(awardBudgetHeader);
		}
	}

	@Override
	public void setSapFeedStatus(AwardVO vo) {
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SAP_AWARD_FEED)) {
			vo.getAward().setSapFeedStatus(awardDao.fetchFeedStatusBasedOnAward(vo.getAwardId(), vo.getAward().getAwardNumber()));
		}
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(vo.getAwardId());
		if (awardBudgetHeader != null) {
			vo.getAward().setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(awardBudgetHeader.getBudgetStatusCode()));
		}
	}

	@Override
	public String getAwardVersions(AwardSummaryVO vo) {
		List<String> sequenceStatuses = new ArrayList<>();
		sequenceStatuses.add(Constants.AWARD_FINAL_STATUS_ACTIVE);
		sequenceStatuses.add(Constants.AWARD_FINAL_STATUS_PENDING);
		List<AwardSummaryDetailsVO> awards = awardDao.fetchAwardByAwardNumbersAndAwardSequenceStatus(vo.getAwardNumber(),
				sequenceStatuses);
		awards.stream().filter(award -> award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_PENDING))
				.forEach(award -> 
					vo.getPendingAwards().add(award)
				);
		awards.stream().filter(award -> award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_ACTIVE))
				.forEach(award -> 
					vo.setActiveAward(award)
				);
		return commonDao.convertObjectToJSON(vo);
	}
  
	@Override
	public String getReportTrackingAttachmentVersions(Integer awardReportTrackingId) {
		Map<String, Object> vo = new HashMap<>();
		List<AwardReportTrackingFile> trackingFiles = awardDao.getReportTrackingAttachmentVersions(awardReportTrackingId);
		getFullNameOfUpdateUser(trackingFiles);
		vo.put("attachmentVersions", trackingFiles);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateReportTracking(ReportTermsVO reportTermsVo) {
		AwardReportTerms reportTerms = new AwardReportTerms();
		reportTerms.setAwardReportTermsId(reportTermsVo.getAwardReportTracking().getAwardReportTermsId());
		reportTermsVo.getAwardReportTracking().setAwardReportTerms(reportTerms);
		awardDao.saveOrUpdateReportTracking(reportTermsVo.getAwardReportTracking());
		return commonDao.convertObjectToJSON(reportTermsVo);
	}

	@Override
	public String deleteReportTracking(Integer awardReportTrackingId) {
		awardDao.deleteReportTrackingAttachment(null, awardReportTrackingId, null);
		awardDao.deleteReportTracking(awardReportTrackingId);
		Map<String, String> vo = new HashMap<>();
		vo.put("status", "success");
		return commonDao.convertObjectToJSON(vo);
	}

	private void getFullNameOfUpdateUser(List<AwardReportTrackingFile> attachments) {
		Set<String> userName = attachments.stream().map(AwardReportTrackingFile::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors
					.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			attachments.stream().filter(item -> item.getUpdateUser() != null)
					.filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase()))
					.forEach(item -> item.setFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	@Override
	public String findAward(String searchString) {
		return commonDao.convertObjectToJSON(awardDao.findAward(searchString));
	}
	
	@Override
	public String updateReportTermsInAward(AwardVO awardVO) {
		Award award = awardVO.getAward();
		Map<String, Boolean> result = new HashMap<String, Boolean>();
		try {	
			awardDao.deleteAwardReportByAwardId(award.getAwardId());
			awardDao.deleteAwardSponsorTermsByAwardId(award.getAwardId());
			if(award.getGrantHeaderId() != null) {
				GrantCall grantCall = grantCallDao.fetchGrantCallById(award.getGrantHeaderId());
				Integer fundingSchemeId = grantCall.getFundingSchemeId();
				String sponsorCode = grantCall.getSponsorCode();
				logger.info("fundingSchemeId : {}", fundingSchemeId);
				logger.info("sponsorCode : {}", sponsorCode);
				addSponserReportIntoAward(fundingSchemeId, sponsorCode, award);	
			}			
			result.put("status", true);
			return commonDao.convertObjectToJSON(result);	
		} catch (Exception e) {
			logger.error("error in updateReportTermsInAward", award.getAwardId());
			result.put("status", false);
			return commonDao.convertObjectToJSON(result);	
		}		
	}
  
	@Override
	public void updateAwardBudgetStatusBasedOnBatchId(Integer batchId, String awardNumber, String budgetStatusCode, String currentBudgetStatusCode) {
		List<Integer> versionIds = awardBudgetDao.getAwardBudgetVersionBasedOnBatchId(batchId, awardNumber, currentBudgetStatusCode);
		if  (versionIds != null && !versionIds.isEmpty()) {
			Set<Integer> versionNumbers = new HashSet<>();
			versionNumbers.addAll(versionIds);
			awardBudgetDao.updateAwardBudgetHeadersBasedOnVersionNumbers(versionIds, budgetStatusCode, awardNumber, currentBudgetStatusCode);
		}
	}

  @Override
  public Set<Integer> getSubModuleCodeBasedOnAwardNumber(String awardNumber) {
            Set<Integer> variationTypeCodes = new HashSet<>();
            variationTypeCodes.add(Constants.AWARD_SUBMODULE_CODE);
            List<String> awardVariationTypeCodes = awardDao.getAwardVariationsBasedOnAwardNumber(awardNumber);
			if (!awardVariationTypeCodes.isEmpty()) {
				if (awardVariationTypeCodes.contains(Constants.SUBMIT_CLOSURE_TYPE_CODE)) {
					variationTypeCodes.add(Constants.AWARD_SUMBIT_CLOSURE_SUBMODULE_CODE);
				}
				if (awardVariationTypeCodes.contains(Constants.PROJECT_CLOSURE_TYPE_CODE)) {
					variationTypeCodes.add(Constants.AWARD_PROJECT_CLOSURE_SUBMODULE_CODE);
				}
			}
      return variationTypeCodes;
  }

  @Override
	public String canDeleteAward(Integer awardId) {
		Award award = awardDao.fetchAwardByAwardId(awardId.toString());
		String createUser = award.getCreateUser();
		String userName = AuthenticatedUser.getLoginUserName();
        String personId = AuthenticatedUser.getLoginPersonId();
		Boolean isPersonCanDelete = personDao.isPersonHasPermission(personId, DELETE_AWARD_RIGHT_NAME, award.getLeadUnitNumber());
		List <String> message = new ArrayList<>();
		Map<String, Object> result = new HashMap<>();
		result.put("status", true);
		if (Boolean.TRUE.equals(isPersonCanDelete) || (userName != null && createUser != null && userName.equals(createUser)) && 
		(award.getAwardSequenceStatus().equals(Constants.AWARD_FINAL_STATUS_PENDING) && award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP) && award.getWorkflowAwardStatusCode().equals(Constants.AWARD_WORKFLOW_STATUS_DRAFT))) {
			    Integer childAward = awardDao.checkChildAwardExisted(award.getAwardNumber());
			    if (childAward != null && childAward > 1) {
					message.add("A child award is already associated with this award.");
					result.put("status", false);
              }
				AwardAmountInfo awardAmountInfo = datesAndAmountDao.getLatestPendingAwardAmountInfo(award.getAwardNumber());
				if (awardAmountInfo != null) {
					message.add("An entry is already there in Dates & Amounts section.");
					result.put("status", false);
				}
				Integer task = taskDao.checkTaskExisted(Constants.AWARD_MODULE_CODE, award.getAwardId().toString());
				if (task != null && task > 0) {
					message.add("A Task is already associated with this award.");
					result.put("status", false);
				}
		} else {
			message.add("You do not have the right to delete this award.");
			result.put("status", false);
		}
		result.put(MESSAGE, message);
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public String deleteAward(Integer awardId) {
		Map<String, String> result = new HashMap<>();
		awardDao.deleteAwardByAwardID(awardId);
	    result.put(MESSAGE, "Award deleted Successfully");
	    return commonDao.convertObjectToJSON(result);
	}

}
