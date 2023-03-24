package com.polus.fibicomp.proposal.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.proposal.pojo.*;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.applicationid.service.ApplicationIdService;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.service.BudgetService;
import com.polus.fibicomp.budget.vo.BudgetVO;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.compilance.dao.ComplianceDao;
import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.compilance.pojo.SpecialReviewUsage;
import com.polus.fibicomp.compilance.vo.ProtocolVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.currentandpending.dao.CurrentAndPendingDao;
import com.polus.fibicomp.currentandpending.pojo.CPReportHeader;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetailExt;
import com.polus.fibicomp.customdataelement.service.CustomDataElementService;
import com.polus.fibicomp.evaluation.dao.EvaluationDao;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanelPersons;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.evaluation.service.EvaluationService;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dao.GrantCallEvaluationPanelDao;
import com.polus.fibicomp.grantcall.dao.GrantCallKPIDao;
import com.polus.fibicomp.grantcall.dao.GrantCallScoringDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPICriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.ip.service.InstitutionalProposalService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonDegree;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.prereview.dao.PreReviewDao;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.print.vo.PrintVO;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.print.service.ProposalPrintService;
import com.polus.fibicomp.proposal.vo.MaintainProjectTeamVO;
import com.polus.fibicomp.proposal.vo.ProposalHistoryVO;
import com.polus.fibicomp.proposal.vo.ProposalPersonRoleVO;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.questionnaire.dao.QuestionnaireDAO;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.roles.service.RoleManagementService;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerAttachment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerComment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerScore;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.ParameterVO;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowMapDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Configuration
@Service(value = "proposalService")
public class ProposalServiceImpl implements ProposalService {

	protected static Logger logger = LogManager.getLogger(ProposalServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	@Qualifier(value = "rolesManagement")
	private RolesManagementDao rolesManagementDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private InstitutionalProposalService institutionalProposalService;

	@Autowired
	private BudgetService budgetService;

	@Autowired
	private PrintService printService;

	@Value("${spring.application.name}")
	private String context;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private ComplianceDao complianceDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private PreReviewDao preReviewDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	public BusinessRuleDao businessRuleDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private EvaluationService evaluationService;

	@Autowired
	private ApplicationIdService applicationIdService;

	@Autowired
	private EvaluationDao evaluationDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	@Qualifier(value = "proposalPrintService")
	private ProposalPrintService proposalPrintService;
	
	@Autowired
	private AuthorizationService authorizationService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	private CommonService commonService;

	@Autowired
	private RolesManagementDao rolesManagement;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private GrantCallKPIDao grantCallKPIDao;

	@Autowired
	private GrantCallScoringDao grantCallScoringDao;

	@Autowired
	private GrantCallEvaluationPanelDao grantCallEvaluationPanelDao;

	@Autowired
	public RolodexDao rolodexDao;

	@Autowired
	private RoleManagementService roleManagementService;

	@Autowired
	public InboxService inboxService;

	@Autowired
	private CurrentAndPendingDao currentAndPendingDao;

	@Autowired
	private ProposalCopyService proposalCopyService;

	@Autowired
	private QuestionnaireDAO questionnaireDAO;

	private static final String SUCCESS = "SUCCESS";
	private static final String PROPOSAL_ID = "proposalId : {}";
	private static final String DELETE_PROPOSAL_RIGHT_NAME = "DELETE_PROPOSAL";

	@Override
	public String createProposal(ProposalVO proposalVO) {
		Integer grantCallId = proposalVO.getGrantCallId();
		Proposal proposal = proposalVO.getProposal();
		proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
		proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS));
		if (grantCallId != null) {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallId);
			proposal.setGrantCallId(grantCallId);
			proposal.setGrantCallType(grantCallDao.fetchGrantCallTypeByGrantTypeCode(grantCall.getGrantTypeCode()));
			proposal.setGrantTypeCode(grantCall.getGrantTypeCode());
			proposal.setGrantCallName(grantCall.getGrantCallName());
			proposal.setGrantCallClosingDate(grantCall.getClosingDate());
			proposal.setInternalDeadLineDate(grantCall.getInternalSubmissionDeadLineDate());
			proposal.setSponsorDeadlineDate(grantCall.getClosingDate());
			proposal.setSponsorCode(grantCall.getSponsorCode());
			proposal.setSponsor(commonDao.getSponsorById(grantCall.getSponsorCode()));
			if (grantCall.getPrimeSponsorCode() != null) {
				proposal.setPrimeSponsorCode(grantCall.getPrimeSponsorCode());
				proposal.setPrimeSponsor(commonDao.getSponsorById(grantCall.getPrimeSponsorCode()));
			}
			setProposalSponsorDetail(proposal);
			proposalVO.setGrantCall(prepareGrantCallData(grantCall));
		}
		loadInitialData(proposalVO);
		proposalVO.setIsCalculationWithPredefinedSalary(commonDao.getParameterValueAsBoolean(Constants.IS_CALCULATION_WITH_PREDEFINED_SALARY));
		proposalVO.setShowOtherInformation(customDataElementService.isOtherInformationPresent(Constants.DEV_PROPOSAL_MODULE_CODE));
		proposalVO.setIsPINameAutoFilledRequired(commonDao.getParameterValueAsBoolean(Constants.PROPOSAL_CREATE_USER_AS_PI_BY_DEFAULT));
		proposalVO.setRcbfFundingStatusCode(commonDao.getParameterValueAsString(Constants.RCBF_FUNDING_SCHEME_STATUS));
		proposalVO.setRcbfTypeCode(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE));
		proposalVO.setNoOfDays(commonDao.getParameter(Constants.NO_OF_DAYS_INTERNAL_DEADLINE_DATE));
		proposalVO.setEnableClosedGrantCallLinkingInProposal(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CLOSED_GRANTCALL_LINKING_IN_PROPOSAL));
		proposalVO.setAcProtocolStatuses(complianceDao.getAcProtocolStatus());
		proposalVO.setIrbProtocolStatuses(complianceDao.getIrbProtocolStatus());
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String addProposalAttachment(MultipartFile[] files, String formDataJSON) {
		ProposalVO proposalVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			List<ProposalAttachment> attachments = proposalModuleDao.fetchProposalAttachmentBasedOnProposalId(proposalVO.getProposalId());
			Integer documentId = 0;
			if (attachments != null && !attachments.isEmpty()) {
				Collections.sort(attachments,
						(attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
								: attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
				documentId = attachments.get(0).getDocumentId();
			}
			List<ProposalAttachment> newAttachments = proposalVO.getNewAttachments();
			Integer versionNumber = 0;
			for (int i = 0; i < files.length; i++) {
				for (ProposalAttachment newAttachment : newAttachments) {
					File file = new File(files[i].getOriginalFilename());
					String fileName = file.getName();
					String replaceFileName = newAttachment.getFileName();
					boolean isRenameRequired = false;
					int count = 1;
					isRenameRequired = checkForDuplication(newAttachment.getFileName(),attachments);
					while(isRenameRequired) {
						 replaceFileName = newAttachment.getFileName();
						 replaceFileName = generateFileName(replaceFileName, count);
						 count = count +1;
						 isRenameRequired = checkForDuplication(replaceFileName,attachments);
					}
					if (newAttachment.getAttachmentId() != null) {
						for (ProposalAttachment attachment : attachments) {
							if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
								attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
								attachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
								versionNumber = attachment.getVersionNumber();
								documentId = attachment.getDocumentId();
								ProposalAttachment proposalAttachment = addNewProposalAttachment(newAttachment, files[i], fileName, versionNumber, documentId, proposalVO.getProposalId(), replaceFileName);
								proposalAttachment.setProposalId(proposalVO.getProposalId());
								proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
							}
						}
					} else {
						if (newAttachment.getFileName().equals(fileName)) {
							documentId = documentId + 1;
							ProposalAttachment proposalAttachment = addNewProposalAttachment(newAttachment, files[i], fileName, versionNumber, documentId, proposalVO.getProposalId(), replaceFileName);
							proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
						}
						i++;
					}
				}
			}
			proposalVO.setProposalAttachments(getProposalAttachmentsByProposalId(proposalVO.getProposalId()));
			setProposalUpdateUser(proposalVO);
		} catch (Exception e) {
			logger.error("exception in addProposalAttachment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
		}

	private boolean checkForDuplication(String fileName, List<ProposalAttachment> attachments) {
		for(ProposalAttachment attachment : attachments) {
			if(fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	public ProposalAttachment addNewProposalAttachment(ProposalAttachment attachment, MultipartFile file, String fileName, Integer versionNumber, Integer documentId, Integer proposalId, String replacedFileName) {
		ProposalAttachment proposalAttachment = new ProposalAttachment();
		try {
			if (attachment.getFileName().equals(fileName)) {
				proposalAttachment.setAttachmentType(attachment.getAttachmentType());
				proposalAttachment.setAttachmentTypeCode(attachment.getAttachmentTypeCode());
				proposalAttachment.setDescription(attachment.getDescription());
				proposalAttachment.setUpdateTimeStamp(attachment.getUpdateTimeStamp());
				proposalAttachment.setUpdateUser(attachment.getUpdateUser());
				proposalAttachment.setFileName(replacedFileName);
				proposalAttachment.setMimeType(file.getContentType());
				proposalAttachment.setVersionNumber(versionNumber + 1);
				FileData fileData = new FileData();
				fileData.setAttachment(file.getBytes());
				fileData = commonDao.saveFileData(fileData);
				proposalAttachment.setFileDataId(fileData.getFileDataId());
				proposalAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
				proposalAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
				proposalAttachment.setDocumentId(documentId);
				proposalAttachment.setAttachment(file.getBytes());
				proposalAttachment.setNarrativeStatus(attachment.getNarrativeStatus());
				proposalAttachment.setNarrativeStatusCode(attachment.getNarrativeStatusCode());
				proposalAttachment.setProposalId(proposalId);
			}
		} catch (Exception e) {
			logger.error("exception in addNewProposalAttachment: {} ", e.getMessage());
		}
		return proposalAttachment;
	}

	@Override
	public String deleteProposalAttachment(ProposalVO vo) {
		try {
			List<ProposalAttachment> proposalAttachments = proposalModuleDao.fetchProposalAttachmentBasedOnProposalIdAndDocumentId(vo.getProposalId(), vo.getDocumentId());
			if (proposalAttachments != null && !proposalAttachments.isEmpty()) {
				for(ProposalAttachment proposalAttachment : proposalAttachments) {
					if (Boolean.FALSE.equals(proposalDao.getIsFileDataIdFound(proposalAttachment.getFileDataId()))) {
						commonDao.deleteFileData(commonDao.getFileDataById(proposalAttachment.getFileDataId()));
					}
					proposalModuleDao.deleteProposalAttachment(proposalAttachment);
				}
			}
			vo.setProposalAttachments(getProposalAttachmentsByProposalId(vo.getProposalId()));
			vo.setStatus(true);
			vo.setMessage("Proposal attachment deleted successfully");
			setProposalUpdateUser(vo);
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal attachment");
			logger.error("exception in deleting proposal attachment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateProposal(ProposalVO vo) {
		Proposal proposal = vo.getProposal();
		if (proposal.getGrantCallId() != null && vo.getPiPersonId() != null) {
			HashMap<String, Object> eligibility = proposalDao.checkGrantCallEligibilty(proposal.getGrantCallId(), vo.getPiPersonId(), Constants.PI_ROLE_CODE);
			vo.setGrantEligibilityStatus(eligibility);
			if ("VE".equals(eligibility.get("status"))) {
				return commonDao.convertObjectToJSON(vo);
			}
		} else {
			Map<String, Object> eligibility = new HashMap<>();
			eligibility.put("status", "TRUE");
			vo.setGrantEligibilityStatus(eligibility);
		}
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		ProposalExtension proposalExtension = vo.getProposalExtension();
		if(proposalExtension != null) {
			proposalExtension.setProposalId(proposal.getProposalId());
			proposalDao.saveOrUpdateProposalExtension(proposalExtension);
		}
		proposal.setStartDate(proposal.getStartDate());
		proposal.setEndDate(proposal.getEndDate());
		proposal.setUnit(commonDao.getLeadUnitByUnitNumber(proposal.getHomeUnitNumber()));
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		if(proposal.getProposalId() != null) {
			proposal.getProposalPersons().clear();
			List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId());
			if (proposalPersons != null && !proposalPersons.isEmpty()) {
				proposal.setProposalPersons(proposalPersons);
			}
		}
		vo.setStatus(true);
		String updateType = vo.getUpdateType();
		if (updateType != null && updateType.equals("SAVE")) {
			if (proposal.getCreateUser() != null) {
				List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.DEV_PROPOSAL_MODULE_CODE);
				if (derivedRoles != null && !derivedRoles.isEmpty()) {
					assignDerivedRolesForCreator(proposal, derivedRoles);
				}
			}
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_ORGANIZATION)) {
				vo.setProposalOrganizations(addDefaultProposalOrganizations(proposal.getHomeUnitNumber(), proposal.getProposalId()));
			}
			vo.setProposalId(proposal.getProposalId());
			vo.setMessage("Proposal saved successfully");
		} else {
			vo.setMessage("Proposal updated successfully");
		}
		Integer grantCallId = proposal.getGrantCallId();
		if (grantCallId != null) {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallId);
			proposal.setGrantCallName(grantCall.getGrantCallName());
			proposal.setGrantCallClosingDate(grantCall.getClosingDate());
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
				saveKPIFromGrantCall(vo.getProposalId(), grantCallId, vo.getUpdateUser());
				vo.setProposalKpis(proposalDao.fetchAllProposalKPI(vo.getProposalId()));
			}
			vo.setGrantCall(prepareGrantCallData(grantCall));	
		}
		vo.setProposal(proposal);
		if (vo.getPiPersonId() != null) {
			addPrincipalInvestigator(vo);
		}
		vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getPersonId(), proposal.getHomeUnitNumber(), proposal.getProposalId()));
		setProposalUpdateUser(vo);
		loadProposalUserFullNames(proposal);
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
			fetchAndSaveProposalEvaluationPanels(vo);
		}
		setLinkedAwardDetails(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void setLinkedAwardDetails(ProposalVO vo) {
		Proposal proposal = vo.getProposal();
		if (proposal.getAwardNumber() != null) {
			Award award = awardDao.fetchActiveAwardByAwardNumber(proposal.getAwardNumber());
			proposal.setAwardId(award.getAwardId());
			proposal.setAwardTitle(award.getTitle());
		}
	}

	private List<ProposalOrganization> addDefaultProposalOrganizations(String unitNumber, Integer proposalId) {
		String organizationId = commonDao.getOrganizationOfUnit(unitNumber);
		List<ProposalOrganization> proposalOrganizations = new ArrayList<>();
		if(organizationId != null) {
			proposalLookUpDao.loadAllOrganizationType().stream().filter(OrganizationType::getIsDefaultEntryRequired).forEach(type -> {
				ProposalOrganization proposalOrganization = new ProposalOrganization();
				proposalOrganization.setProposalId(proposalId);
				proposalOrganization.setOrganizationId(organizationId);
				proposalOrganization.setOrganizationTypeCode(type.getOrganizationTypeCode());
				proposalOrganization.setOrganizationType(type);
				proposalOrganization.setOrganization(commonDao.loadOrganizationDetails(organizationId));
				if (proposalOrganization.getOrganization() != null && proposalOrganization.getOrganization().getContactAddressId() != null)
					proposalOrganization.getOrganization().setContactPersonName(rolodexDao.getRolodexDetailById(proposalOrganization.getOrganization().getContactAddressId()).getFullName());
				proposalDao.saveOrUpdateProposalOrganization(proposalOrganization);
				proposalOrganizations.add(proposalOrganization);
			});
		}
		return proposalOrganizations;
	}
	
	@Override
	public String loadProposalById(Integer proposalId, String personId, String userName, Boolean isProposalComparison) {
		ProposalVO proposalVO = new ProposalVO();
		proposalVO.setRcbfFundingStatusCode(commonDao.getParameterValueAsString(Constants.RCBF_FUNDING_SCHEME_STATUS));
		proposalVO.setRcbfTypeCode(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE));
		proposalVO.setPersonId(personId);
		proposalVO.setProposalId(proposalId);
		proposalVO.setUserName(userName);
		proposalVO.setIsProposalComparison(isProposalComparison);
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		proposalVO.setGrantCallId(proposal.getGrantCallId());	
		ProposalExtension proposalExtension = proposalDao.fetchProposalExtensionById(proposalId);
		proposal.setCategoryCode(grantCallDao.fetchGrantCategoryCodeByGrantTypeCode(proposal.getGrantTypeCode()));
		String unitNumber = proposal.getHomeUnitNumber();
		Integer moduleCode = Constants.DEV_PROPOSAL_MODULE_CODE;
		proposalVO.setAvailableRights(authorizationService.allDepartmentPermission(moduleCode, personId, unitNumber, proposalId));
		proposalVO.setProposal(proposal);
		proposalVO.setProposalExtension(proposalExtension);
		Integer workflowDetailId = workflowDao.fetchWorkflowDetailId(proposalId, personId);
		proposalVO.setIsPersonCanScore(proposalDao.fetchPersonCanScore(proposalId, personId, workflowDetailId));
		loadProposalHomeData(proposalVO);
		loadInitialData(proposalVO);
		if ((!commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION)) && Boolean.FALSE.equals(proposalVO.getIsProposalComparison())) {
			if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_RETURNED)
					|| proposal.getStatusCode().equals(Constants.ADMIN_CHECK_COMPLETED)
					|| proposal.getStatusCode().equals(Constants.COMPLETED)
					|| proposal.getStatusCode().equals(Constants.REVIEW_IN_PROGRESS)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_INACTIVE)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_WITHDRAW)
					|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_NOT_AWARDED)
					|| (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING)
							&& (!proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)))) {
				canTakeRoutingAction(proposalVO);
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposal.getProposalId().toString(),Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				if (workflow != null) {
					workflowService.prepareWorkflowDetails(workflow);
					proposalVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						proposalVO.setWorkflowList(workFlows);
					}
				}
			}
			proposalVO.setIpGenerationOnly(businessRuleService.canCreateInstituteProposal(proposal, proposalVO).equals(Boolean.TRUE)
							|| (proposalVO.getWorkflow() != null && proposalVO.getWorkflow().getMapType() != null
							&& proposalVO.getWorkflow().getMapType().equals("E")));
			Integer canApproveRouting = businessRuleDao.canApproveRouting(proposalId.toString(), proposalVO.getPersonId(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
			proposalVO.setCanApproveRouting(canApproveRouting.toString());
			proposalVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(proposalId.toString(), personId, Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE));
			proposalVO.setIsSubmit("1");
		}
		if (!proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED) && !proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)) {
			proposalVO.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(), proposal.getActivityTypeCode()));
		}
		loadProposalUserFullNames(proposal);
		//proposalVO.setIsCalculationWithPredefinedSalary(getIsCalculationWithPredefinedSalary());
		proposalVO.setIsCalculationWithPredefinedSalary(commonDao.getParameterValueAsBoolean(Constants.IS_CALCULATION_WITH_PREDEFINED_SALARY));
		proposalVO.setShowOtherInformation(customDataElementService.isOtherInformationPresent(Constants.DEV_PROPOSAL_MODULE_CODE));
		proposalVO.setEnableOrganizationLocation(commonDao.getParameterValueAsBoolean(Constants.ENABLE_ORGANIZATION_LOCATION));
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			proposalVO.setProposalKpis(proposalDao.fetchAllProposalKPI(proposalId));
		}				
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SCORING_CRITERIA)) {
			fetchScoringCriteriaByProposal(proposalVO);
		}
		proposalVO.setEnableClosedGrantCallLinkingInProposal(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CLOSED_GRANTCALL_LINKING_IN_PROPOSAL));
		proposalVO.setNoOfDays(commonDao.getParameter(Constants.NO_OF_DAYS_INTERNAL_DEADLINE_DATE));
		proposalVO.setAcProtocolStatuses(complianceDao.getAcProtocolStatus());
		proposalVO.setIrbProtocolStatuses(complianceDao.getIrbProtocolStatus());
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public void canTakeRoutingAction(ProposalVO proposalVO) {
		Proposal proposal = proposalVO.getProposal();
		Workflow workflow = proposalVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		}
		if (workflow != null) {
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(), maxApprovalStopNumber);
			if (finalWorkflowDetails != null && !finalWorkflowDetails.isEmpty()) {
				for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
					if (finalWorkflowDetail.getApproverPersonId().equals(proposalVO.getPersonId())
							|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
						proposalVO.setFinalApprover(true);
					}
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				boolean currentPerson = true;
				if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (currentPerson && (workflowDetail.getApproverPersonId().equals(proposalVO.getPersonId())
								&& (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)
										&& workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)))) {
							if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
								proposalVO.setIsApproved(true);
							} else {
								proposalVO.setIsApproved(false);
							}
							proposalVO.setIsApprover(true);
						}
					}
				}
			}
		}
	}

	@Override
	public String deleteProposalKeyword(ProposalVO vo) {
		try {
			Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
			List<ProposalKeyword> proposalKeywords = proposal.getProposalKeywords();
			List<ProposalKeyword> updatedProposalKeywords = new ArrayList<>(proposalKeywords);
			Collections.copy(updatedProposalKeywords, proposalKeywords);
			for (ProposalKeyword proposalKeyword : proposalKeywords) {
				if (proposalKeyword.getKeywordId().equals(vo.getKeywordId())) {
					updatedProposalKeywords.remove(proposalKeyword);
				}
			}
			proposal.getProposalKeywords().clear();
			proposal.getProposalKeywords().addAll(updatedProposalKeywords);
			proposalDao.saveOrUpdateProposal(proposal);
			vo.setProposal(proposal);
			vo.setStatus(true);
			vo.setMessage("Proposal keyword deleted successfully");
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal keyword");
			logger.error("exception in deleting proposal keyword: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteProposalResearchArea(ProposalVO vo) {
		try {
			proposalModuleDao.deleteProposalResearchArea(proposalModuleDao.fetchProposalResearchArea(vo.getResearchAreaId()));
			vo.setProposalResearchAreas(proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(vo.getProposalId()));
			vo.setStatus(true);
			vo.setMessage("Proposal research area deleted successfully");
			setProposalUpdateUser(vo);
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal research area");
			logger.error("exception in deleting proposal research area: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteProposalPerson(ProposalVO vo) {
		try {
			ProposalPerson proposalPerson = proposalModuleDao.fetchProposalPerson(vo.getProposalPersonId());
			proposalModuleDao.deleteDegreeByProposalPersonId(vo.getProposalPersonId());
			proposalModuleDao.deleteProposalPerson(proposalPerson);
			Integer proposalId = vo.getProposalId();
			Proposal proposal = proposalDao.fetchProposalById(proposalId);
			deleteProposalPersonRole(proposalPerson.getPersonId(), proposalPerson.getProposalId());
			List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.DEV_PROPOSAL_MODULE_CODE);
			if (derivedRoles != null && !derivedRoles.isEmpty()) {
				String createUserPersonId = personDao.getPersonIdByUserName(proposal.getCreateUser());
				if (proposalPerson.getPersonId() != null && proposalPerson.getPersonId().equals(createUserPersonId)) {
					assignDerivedRolesForCreator(proposal, derivedRoles);
				}
			}
			List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId);
			proposalPersons.forEach(person -> {
				isProposalPersonTrainingCompleted(person, proposalId);
			});
			vo.setProposalPersons(proposalPersons);
			proposal.setProposalPersons(proposalPersons);
			vo.setProposal(proposal);
			vo.setStatus(true);
			vo.setMessage("Proposal person deleted successfully");
			setProposalUpdateUser(vo);
			vo.setProposal(proposalDao.fetchProposalById(vo.getProposalId()));
			List<CPReportHeader> cpReportHeaders = new ArrayList<>();
			if (proposalPerson.getPersonId() != null) {
				cpReportHeaders = currentAndPendingDao.fetchCPReportHeadersByParams(proposalPerson.getPersonId(), false, Constants.DEV_PROPOSAL_MODULE_CODE, proposalId.toString());
			} else {
				cpReportHeaders = currentAndPendingDao.fetchCPReportHeadersByParams(proposalPerson.getRolodexId().toString(), true, Constants.DEV_PROPOSAL_MODULE_CODE, proposalId.toString());
			}
			deleteCPReportHeaders(cpReportHeaders);
			questionnaireDAO.deleteQuestAnswerAttachment(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE,
					proposalPerson.getRolodexId() != null ? proposalPerson.getRolodexId().toString() : proposalPerson.getPersonId());
			questionnaireDAO.deleteQuestAnswer(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE,
					proposalPerson.getRolodexId() != null ? proposalPerson.getRolodexId().toString() : proposalPerson.getPersonId());
			questionnaireDAO.deleteQuestAnswerHeader(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE,
					proposalPerson.getRolodexId() != null ? proposalPerson.getRolodexId().toString() : proposalPerson.getPersonId());
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal person");
			throw new ApplicationException("deleting proposal person", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteCPReportHeaders(List<CPReportHeader> cpReportHeaders) {
		if (cpReportHeaders != null && !cpReportHeaders.isEmpty()) {
			for (CPReportHeader cpReportHeader : cpReportHeaders) {
				List<Integer> cpProjectDetailIds = currentAndPendingDao.getCPProjectDetailIdsByCPReportHeaderId(cpReportHeader.getCpReportHeaderId());
				if (!cpProjectDetailIds.isEmpty()) {
					for (Integer cpProjectDetailId : cpProjectDetailIds) {
						CPReportProjectDetailExt cpReportProjectDetailExt = currentAndPendingDao.getCPReportProjectDetailExtById(cpProjectDetailId);
						if (cpReportProjectDetailExt != null) {
							currentAndPendingDao.deleteCPProjectDetailExt(cpReportProjectDetailExt);
						}
					}
				}
			}
			currentAndPendingDao.deleteCPReportHeaders(cpReportHeaders);
		}
	}

	private void deleteProposalPersonRole(String personId, Integer proposalId) {
		List<Integer> roleIds = roleManagementService.getDerivedRoleIdForDelete(Constants.DEV_PROPOSAL_MODULE_CODE);
		List<ProposalPersonRoles> proposalPersonRoles = proposalDao.fetchProposalPersonRolesByParams(personId, proposalId, roleIds);
		if (proposalPersonRoles != null && !proposalPersonRoles.isEmpty()) {
			for (ProposalPersonRoles proposalPersonRole : proposalPersonRoles) {
				proposalDao.deleteProposalPersonRole(proposalPersonRole);
			}
		}
	}

	@Override
	public String deleteProposalSponsor(ProposalVO vo) {
		try {
			proposalModuleDao.deleteProposalSponsor(proposalModuleDao.fetchProposalSponsor(vo.getSponsorId()));
			vo.setProposalSponsors(proposalModuleDao.fetchProposalSponsorBasedOnProposalId(vo.getProposalId()));
			vo.setStatus(true);
			vo.setMessage("Proposal sponsor deleted successfully");
			setProposalUpdateUser(vo);
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal sponsor");
			logger.error("exception in deleting proposal sponsor: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteIrbProtocol(ProposalVO vo) {
		try {
			proposalModuleDao.deleteProposalIrbProtocol(proposalModuleDao.fetchProposalIrbProtocol(vo.getSponsorId()));
			vo.setProposalIrbProtocols(proposalModuleDao.fetchProposalIrbProtocolBasedOnProposalId(vo.getProposalId()));
			vo.setStatus(true);
			vo.setMessage("Proposal protocol deleted successfully");
			setProposalUpdateUser(vo);
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal protocol");
			logger.error("exception in deleting proposal protocol: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<byte[]> downloadProposalAttachment(Integer attachmentId) {
		ProposalAttachment attachment = proposalDao.fetchProposalAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("exception in downloadProposalAttachment: {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String submitProposal(ProposalVO proposalVO) {
		proposalVO.setPersonId(AuthenticatedUser.getLoginPersonId());
		proposalVO.setUpdateUser(AuthenticatedUser.getLoginUserName());
		proposalVO.setUserName(AuthenticatedUser.getLoginUserName());
		String workFlowType = commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE);
		Proposal proposal = proposalDao.fetchProposalById(proposalVO.getProposalId());
		proposal.setSubmissionDate(commonDao.getCurrentTimestamp());
		proposal.setSubmitUser(proposalVO.getUserName());
		proposal.setUpdateUser(proposalVO.getUpdateUser());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalVO.getProposalId()));
		if (proposal.getBaseProposalNumber() != null && !proposal.getBaseProposalNumber().isEmpty()) {
			proposal.setBaseProposalTitle(institutionalProposalService.getIPTitleForMasterProposal(proposal.getBaseProposalNumber()));
		}
		setProposalSponsorDetail(proposal);
		proposalVO.setProposal(proposal);
		BudgetVO budgetVO = new BudgetVO();
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		if (workFlowType != null && workFlowType.equals(Constants.EVALUATION)) {
			addToWorkFlow(proposalVO);
			inboxDao.markAsReadBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId().toString(), Constants.MESSAGE_TYPE_PROPOSAL_REJECT);
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_ELIGIBILITY_CRITERIA)) {
				boolean isEligibilityCriteriaMet = false;
				isEligibilityCriteriaMet = proposalDao.fetchEligibilityCriteria(proposal.getProposalId());
				proposal.setIsEligibilityCriteriaMet(isEligibilityCriteriaMet);
			}
			if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)) {
				if(proposal.getActivityTypeCode() != null) {
					if (proposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE))) {
						evaluationService.createGrantAdminReview(proposal);
						proposal.setStatusCode(commonDao.getParameter(Constants.RCBF_STATUS_CODE));
						proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(commonDao.getParameter(Constants.RCBF_STATUS_CODE)));
					} else {
						proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW);
						proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW));
					}
				}
				else {
					proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW);
					proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW));
				}
				if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
					String applicationId = applicationIdService.generateApplicationIdProposal(proposal);
					logger.info("APPLICATION ID : {}", applicationId);
					proposal.setApplicationId(applicationId);
				}
				proposal = proposalDao.saveOrUpdateProposal(proposal);
				proposalVO = sendProposalNotification(proposalVO, Constants.PROPOSAL_SUBMIT_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else {
				if (proposal.getActivityTypeCode() != null && proposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE))) {
					evaluationService.createGrantAdminReview(proposal);
					proposal.setStatusCode(commonDao.getParameter(Constants.RCBF_STATUS_CODE));
					proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(commonDao.getParameter(Constants.RCBF_STATUS_CODE)));
				} else {
					proposal = evaluationService.createReviewForResubmit(proposalVO.getProposalId());
				}
				proposalVO = sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_RESUBMISSION, dynamicEmailrecipients);
			}
			loadInitialData(proposalVO);
			proposalVO.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(), proposal.getActivityTypeCode()));
		}
		if (workFlowType != null  && (workFlowType.equals(Constants.MAP_ROUTING) || workFlowType.equals(Constants.EVALUATION_MAP_ROUTING))) {
			inboxDao.markAsReadBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId().toString(), Constants.MESSAGE_TYPE_PROPOSAL_REJECT);
			proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS);
			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS));
			proposal = proposalDao.saveOrUpdateProposal(proposal);
			proposalVO = buildWorkflow(proposalVO);
			proposalVO = fetchPreviousWorkFlowsList(proposalVO);
			loadInitialData(proposalVO);
		}
		loadProposalUserFullNames(proposal);
		proposalVO.setProposal(proposal);
		if (proposal.getGrantCallType() != null && (proposal.getGrantCallType().getCategoryCode() != null && proposal.getGrantCallType().getCategoryCode().equals(Constants.GRANT_CALL_TYPE_INTERNAL))) {
			budgetService.loadBudgetInitialData(budgetVO);
			proposalVO.setBudgetVO(budgetVO);
			proposalVO.setNarrativeStatus(commonDao.fetchAllNarrativeStatus());
		}
		proposalDao.saveOrUpdateProposal(proposal);
		proposalVO.setProposal(proposal);
		businessRuleService.evaluateAndSentNotification(proposalVO.getModuleCode(), proposalVO.getSubModuleCode(), proposalVO.getModuleItemKey(), "0", proposalVO.getPersonId(), proposalVO.getUpdateUser(), getProposalPlaceholders(proposalVO));
		if ((proposalVO.getWorkflow() != null && proposalVO.getWorkflow().getWorkflowDetails().isEmpty() || proposalVO.getWorkflow() == null) && (workFlowType != null && workFlowType.equals(Constants.EVALUATION_MAP_ROUTING))) {
			proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW);
			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW));	
		} else if ((proposalVO.getWorkflow() != null && proposalVO.getWorkflow().getWorkflowDetails().isEmpty() || proposalVO.getWorkflow() == null) && (workFlowType != null && workFlowType.equals(Constants.MAP_ROUTING))) {
			proposalVO = awardProposal(proposalVO, proposal);
		}
		if (!commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_BUDGET_VERSIONS)) {
			budgetDao.updateProposalBudgetStatus(proposalVO.getProposalId(), Constants.BUDGET_STATUS_COMPLETED, proposalVO.getUserName());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private void addToWorkFlow(ProposalVO proposalVO) {
		String moduleItemId = proposalVO.getProposal().getProposalId().toString();
		String personId = proposalVO.getPersonId();
		Workflow workFlowData = workflowDao.getActiveWorkFlow(moduleItemId, Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		if (workFlowData != null) {
			workFlowData.setIsWorkflowActive(false);
			workflowDao.saveWorkflow(workFlowData);
		}
		Workflow workflow = new Workflow();
		workflow.setModuleItemId(moduleItemId);
		workflow.setSubModuleItemId(Constants.SUBMODULE_ITEM_KEY);
		workflow.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
		workflow.setSubModuleCode(Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		workflow.setWorkflowStartPerson(personId);
		if (workFlowData != null) {
			workflow.setWorkflowSequence(workFlowData.getWorkflowSequence() + 1);
		} else {
			workflow.setWorkflowSequence(1);
		}
		workflow.setIsWorkflowActive(true);
		workflow.setWorkflowStartDate(commonDao.getCurrentTimestamp());
		workflowDao.saveWorkflow(workflow);
	}

	private ProposalVO awardProposal(ProposalVO proposalVO, Proposal proposal) {
		if (proposal.getGrantCallId() != null) {
			if (proposal.getGrantCallType() !=null && (proposal.getGrantCallType().getGrantTypeCode().equals(1)
					|| proposal.getGrantCallType().getGrantTypeCode().equals(2)
					|| proposal.getGrantCallType().getGrantTypeCode().equals(3))) {
				if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLED_ADMIN_CHK)) {
					proposalVO.getProposal().setStatusCode(Constants.COMPLETED);
					proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.COMPLETED));
					sendProposalNotification(proposalVO, Constants.ADMIN_CHECK_COMPLETE_NOTIFICATION_CODE, new HashSet<>());
				} else {
					proposalVO.getProposal().setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
					proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
					proposal = businessRuleService.generateInstitutionalProposal(proposalVO);
					sendProposalNotification(proposalVO, Constants.PROPOSAL_AWARDED_NOTIFICATION_CODE, new HashSet<>());
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
					proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.COMPLETED));
					ProposalEvaluationScore proposalEvaluationScore = new ProposalEvaluationScore();
					proposalEvaluationScore.setGrantHeaderId(proposal.getGrantCallId());
					proposalEvaluationScore.setProposalId(proposal.getProposalId());
					proposalEvaluationScore.setAdjustedScore(new BigDecimal(0));
					proposalEvaluationScore.setIsShortListed("N");
					grantCallDao.saveOrUpdateProposalEvalautionScore(proposalEvaluationScore);
					proposalVO.setMessage("approval_success");
				} else {
					proposalVO.getProposal().setStatusCode(Constants.ADMIN_CHECK_COMPLETED);
					proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.ADMIN_CHECK_COMPLETED));
					sendProposalNotification(proposalVO, Constants.PROPOSAL_FINAL_ENDORSEMENT_NOTIFICATION_CODE, new HashSet<>());
				}
			}
		} else {
			proposalVO.getProposal().setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
			proposalVO.getProposal().setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
			businessRuleService.generateInstitutionalProposal(proposalVO);
			sendProposalNotification(proposalVO, Constants.PROPOSAL_AWARDED_NOTIFICATION_CODE, new HashSet<>());
		}
		return proposalVO;
	}

	@Override
	public String approveOrRejectProposal(MultipartFile[] files, String formDataJSON) {
		ProposalVO proposalVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			Proposal proposal = proposalDao.fetchProposalById(proposalVO.getProposalId());
			proposal.setUpdateUser(proposalVO.getUpdateUser());
			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			String actionType = proposalVO.getActionType();
			String approverComment = proposalVO.getApproveComment();
			boolean isFinalApprover = true;
			logger.info("actionType : {}",  actionType);
			logger.info("personId : {}",  proposalVO.getPersonId());
			logger.info("approverComment : {}",  approverComment);

			Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			for (WorkflowDetail workflowDetail1 : workflowDetails) {
				if (workflowDetail1.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
					isFinalApprover = false;
				}
			}
			Set<NotificationRecipient> dynamicEmailrecipients = null;
			if (isFinalApprover && actionType.equals("A")) {
				if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.MAP_ROUTING)) {
					String ipNumber = institutionalProposalService.generateInstitutionalProposalNumber();
					logger.info("Initial IP Number : {}",  ipNumber);
					boolean isIPCreated = institutionalProposalService.createInstitutionalProposal(proposal.getProposalId(), ipNumber, proposal.getUpdateUser());
					logger.info("isIPCreated : {}",  isIPCreated);
					if (isIPCreated) {
						logger.info("Generated IP Number : {}",  ipNumber);
						proposal.setIpNumber(ipNumber);
						proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
						proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
						String fyiRecipients = commonDao.getParameterValueAsString(Constants.EMAIL_NOTIFICATION_TEST_ADDRESS);
						if (fyiRecipients != null && !fyiRecipients.isEmpty()) {
							dynamicEmailrecipients = new HashSet<>();
							List<String> recipients = Arrays.asList(fyiRecipients.split(","));
							for (String recipient : recipients) {
								setNotificationRecipients(recipient, Constants.NOTIFICATION_RECIPIENT_TYPE_TO,
										dynamicEmailrecipients);
							}
						}
					}
				}
				if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING)) {
					proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW);
					proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_SUBMITTED_FOR_REVIEW));
				}
				proposal = proposalDao.saveOrUpdateProposal(proposal);
				sendProposalNotification(proposalVO, Constants.PROPOSAL_APPROVE_NOTIFICATION_CODE,
						dynamicEmailrecipients);
			} else if (actionType.equals("R")) {
				isFinalApprover = false;
				proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED);
				proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED));
				proposal = proposalDao.saveOrUpdateProposal(proposal);
				sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_REJECTED, dynamicEmailrecipients);
			}
			proposalVO.setProposal(proposal);
			if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_RETURNED) || proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)) {
				loadInitialData(proposalVO);
			}
			proposalVO.setFinalApprover(isFinalApprover);
			proposalVO.setIsApproved(true);
			if (!proposalVO.getIsSuperUser()) {
				proposalVO.setIsApprover(true);
			}
			workflowService.prepareWorkflowDetails(workflow);
			proposalVO.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				proposalVO.setWorkflowList(workFlows);
			}
			loadProposalUserFullNames(proposal);
			proposalVO.setProposal(proposal);
		} catch (Exception e) {
			logger.error("exception in approveOrRejectProposal: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public void loadInitialData(ProposalVO proposalVO) {
		Proposal proposal = proposalVO.getProposal();
		String unitNumber = proposal.getHomeUnitNumber();
		Integer proposalId = proposal.getProposalId();
		String personId = proposalVO.getPersonId();
		Integer moduleCode = Constants.DEV_PROPOSAL_MODULE_CODE;
		if (Boolean.FALSE.equals(proposalVO.getIsProposalComparison())) {
			proposalVO.setAvailableRights(authorizationService.allDepartmentPermission(moduleCode, personId, unitNumber, proposalId));
			proposalVO.setProposalPersonRoles(proposalLookUpDao.fetchAllProposalPersonRoles());
			proposalVO.setDefaultGrantCallType(grantCallDao.fetchGrantCallTypeByGrantTypeCode(Constants.GRANT_CALL_TYPE_OTHERS));
			proposalVO.setProposalAttachmentTypes(proposalLookUpDao.fetchAllProposalAttachmentTypes());
			Boolean enableActivityGrantMapping = isActivityForGrantTypeEnabled();
			proposalVO.setEnableActivityGrantCallMapping(enableActivityGrantMapping);
			if (Boolean.TRUE.equals(enableActivityGrantMapping)) {
				proposalVO.setActivityTypes(proposalLookUpDao.getAllActivityForGrantType());
			} else {
				proposalVO.setActivityTypes(commonDao.fetchAllActivityTypes());
			}
			if (proposal.getActivityTypeCode() != null && proposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE))) {
				proposal.setIsRcbfProposal(true);
			}
			proposalVO.setNarrativeStatus(commonDao.fetchAllNarrativeStatus());
			proposalVO.setDisciplineClusters(proposalLookUpDao.fetchAllDisciplineCluster());
			proposalVO.setProposalFundingStatus(proposalLookUpDao.fetchAllProposalFundingStatus());
			proposalVO.setAwardType(awardDao.fetchAllAwardTypes());
			proposalVO.setPreReviewClarifications(preReviewDao.fetchAllPreReviewSectionTypes("2"));
			proposalVO.setPreReviewRoutingReview(preReviewDao.fetchAllPreReviewSectionTypes("1"));
			proposalVO.setPreReviewers(preReviewDao.fetchAllPreReviewer());
			proposalVO.setGrantCallTypes(grantCallDao.fetchAllGrantCallTypes());
			proposalVO.setIsCalculationWithPredefinedSalary(commonDao.getParameterValueAsBoolean(Constants.IS_CALCULATION_WITH_PREDEFINED_SALARY));
			proposalVO.setIsFundingSupportDeclarationRequired(commonDao.getParameterValueAsBoolean(Constants.IS_OTHER_FUNDING_SUPPORT_REQUIRED));
			proposal.setIsSimpleBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_SIMPLE_BUDGET_ENABLED));
			proposal.setIsModularBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_MODULAR_BUDGET_ENABLED));
			proposal.setIsDetailedBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_DETAILED_BUDGET_ENABLED));
			proposalVO.setIsAutoCalculateEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AUTO_CALCULATE));
			proposalVO.setIsBudgetVersionEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_BUDGET_VERSIONS));
			proposalVO.setResearchTypes(commonDao.fetchAllResearchTypes());
			proposalVO.setProposalTypes(proposalLookUpDao.fetchAllProposalTypes());
			proposalVO.setReviewTypes(getSpecialReviewTypes());
			proposalVO.setSpecialReviewApprovalTypes(commonDao.fetchAllApprovalStatusTypes());
			proposalVO.setPreReviewTypes(preReviewDao.fetchAllPreReviewTypes());
			proposalVO.setNarrativeStatus(commonDao.fetchAllNarrativeStatus());
			proposalVO.setProposalAttachmentTypes(proposalLookUpDao.fetchAllProposalAttachmentTypes());
			proposalVO.setPreReviewTypes(preReviewDao.fetchAllPreReviewTypes());
			proposalVO.setPreReviewers(preReviewDao.fetchAllPreReviewer());
			Boolean isOrganizationEnabled = commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_ORGANIZATION);
			if(isOrganizationEnabled.equals(Boolean.TRUE)) {
						proposalVO.setOrganizationType(proposalLookUpDao.loadOrganizationType());
			}
			boolean isDeclarationSectionRequired = commonDao.getParameterValueAsBoolean(Constants.IS_REQUIRED_DECLARATION_SECTION);
			if (isDeclarationSectionRequired) {
				proposalVO.setSponsorFundingSchemes(grantCallDao.fetchAllSponsorFundingSchemes());
				proposalVO.setSponsorTypes(grantCallDao.fetchAllSponsorTypes());
			}
			if (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.MAP_ROUTING) || commonDao
					.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING)) {
				if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS)
						|| proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_RETURNED)
						|| (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE)
								.equals(Constants.EVALUATION_MAP_ROUTING)
								&& (!proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)))) {
					Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
					workflowService.prepareWorkflowDetails(workflow);
					proposalVO.setWorkflow(workflow);
					List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
					if (workFlows != null && !workFlows.isEmpty()) {
						workflowService.prepareWorkflowDetailsList(workFlows);
						Collections.sort(workFlows, new WorkflowComparator());
						proposalVO.setWorkflowList(workFlows);
					}
				}
			}
			if (!proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)
					&& !proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)) {
				proposalVO.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(), proposal.getActivityTypeCode()));
			}
		}
		getBudgetAndAwardDetails(proposalVO);
	}

	private void getBudgetAndAwardDetails(ProposalVO proposalVO) {
		if (!(proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(proposalVO.getProposal().getProposalId())).isEmpty()) {
			proposalVO.setIsBudgetHeaderFound(true);
		}
		proposalVO.setCurrencyDetails(commonDao.fetchCurrencyDetails());
		if (proposalVO.getProposal().getProposalId() != null && Boolean.TRUE.equals(proposalVO.getIsBudgetHeaderFound())) {
			BudgetHeader budgetHeader = budgetDao.fetchActiveBudgetByProposalId(proposalVO.getProposal().getProposalId());
			if (budgetHeader != null && budgetHeader.getBudgetId() != null) {
				proposalVO.setBudgetId(budgetHeader.getBudgetId());
			}
		}
		setLinkedAwardDetails(proposalVO);
	}

	public List<SpecialReviewType> getSpecialReviewTypes() {
		List<SpecialReviewType> specialReviewTypes = complianceDao.fetchAllSpecialReviewType();
		List<SpecialReviewUsage> specialReviewUsages = complianceDao.fetchSpecialReviewUsageByModuleCode("3");
		List<SpecialReviewType> reviewTypes = new ArrayList<>();
		for (SpecialReviewType specialReviewType : specialReviewTypes) {
			SpecialReviewUsage itemSpecialReviewUsage = null;
			for (SpecialReviewUsage specialReviewUsage : specialReviewUsages) {
				if (StringUtils.equals(specialReviewUsage.getSpecialReviewTypeCode(), String.valueOf(specialReviewType.getSpecialReviewTypeCode()))) {
					itemSpecialReviewUsage = specialReviewUsage;
					break;
				}
			}
			if (itemSpecialReviewUsage != null && itemSpecialReviewUsage.isActive() && (itemSpecialReviewUsage.isGlobal())) {
					reviewTypes.add(specialReviewType);
			}
		}
		return reviewTypes;
	}

	@Override
	public String deleteProposalSpecialReview(ProposalVO vo) {
		try {
			proposalModuleDao.deleteProposalSpecialReview(proposalModuleDao.fetchProposalSpecialReview(vo.getProposalSpecialReviewId()));
			vo.setProposalSpecialReviews(prepareSpecialReviewDetail(proposalModuleDao.fetchProposalSpecialReviewBasedOnProposalId(vo.getProposalId())));
			vo.setStatus(true);
			vo.setMessage("Proposal special review deleted successfully");
			setProposalUpdateUser(vo);
		} catch (Exception e) {
			vo.setStatus(false);
			vo.setMessage("Problem occurred in deleting proposal special review");
			logger.error("exception in deleting proposal special review: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String sendAttachPINotification(ProposalVO proposalVO) {
		Proposal proposal = proposalDao.fetchProposalById(proposalVO.getProposalId());
		proposalVO.setProposal(proposal);
		Set<NotificationRecipient> dynamicEmailRecepients = null;
		sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_ATTACHMENT_NOTIFICATION, dynamicEmailRecepients);
		return SUCCESS;
	}

	@Override
	public String sendAttachApproverNotification(ProposalVO proposalVO) {
		Integer proposalId = proposalVO.getProposalId();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		proposalVO.setProposal(proposal);
		proposalVO.setWorkflow(workflowDao.fetchActiveWorkflowByParams(proposalId.toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE));
		Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(proposalVO.getWorkflow().getWorkflowId());
		List<WorkflowDetail> finalApprovers = workflowDao.fetchFinalApprover(proposalVO.getWorkflow().getWorkflowId(), maxApprovalStopNumber);
		Set<NotificationRecipient> dynamicEmailRecipients = null;
		if (!finalApprovers.isEmpty()) {
			dynamicEmailRecipients = new HashSet<>();
			for (WorkflowDetail workflowDetail : finalApprovers) {
				setNotificationRecipients(workflowDetail.getEmailAddress(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO,
						dynamicEmailRecipients);
			}
		}
		sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_ATTACHMENT_APPROVER, dynamicEmailRecipients);
		return SUCCESS;
	}

	private Boolean isProposalEmployee(List<ProposalPerson> proposalPersons, String personId) {
		Boolean isProposalPerson = false;
		for (ProposalPerson person : proposalPersons) {
			if (person.getPersonId() != null && personId.equals(person.getPersonId())) {
				isProposalPerson = true;
				break;
			}
		}
		return isProposalPerson;
	}

	@Override
	public String deleteProposal(ProposalVO vo) {
		deleteAllProposalAttachments(vo.getProposalId());
		deleteAllProposalResearchArea(vo.getProposalId());
		deleteAllProposalPerson(vo.getProposalId());
		deleteAllProposalSponsor(vo.getProposalId());
		deleteAllProposalIrbProtocol(vo.getProposalId());
		deleteAllProposalSpecialReview(vo.getProposalId());
		deleteAllProposalPersonAssignedRoles(vo.getProposalId());
		deleteAllProposalPersonRoles(vo.getProposalId());
		deleteAllProposalProjectTeam(vo.getProposalId());
		deleteAllBudgetHeader(vo.getProposalId());
		deleteAllProposalReview(vo.getProposalId());
		deleteAllProposalKPI(vo.getProposalId());
		deleteAllPreReviews(vo.getProposalId());
		deleteAllProposalEvaluationPanel(vo.getProposalId());
		deleteAllProposalMileStone(vo.getProposalId());
		deleteAllProposalCommentAndAttachement(vo.getProposalId());
		vo.setMessage(proposalDao.deleteProposal(vo.getProposalId()));
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteAllProposalCommentAndAttachement(Integer proposalId) {
		List<Integer> proposalCommentIds = proposalDao.getAllProposalComment(proposalId);
		if(proposalCommentIds != null && !proposalCommentIds.isEmpty()) {
			proposalDao.deleteProposalCommentAttachement(proposalCommentIds);
			proposalDao.deleteProposalComment(proposalId);
		}
	}

	private void deleteAllProposalMileStone(Integer proposalId) {
		List<ProposalMileStone> proposalMileStones = proposalModuleDao.fetchProposalMileStonesBasedOnProposalId(proposalId);
		if (!proposalMileStones.isEmpty()) {
			proposalDao.deleteProposalMileStones(proposalMileStones);
		}
	}

//	@Override
//	public String withdrawProposal(ProposalVO vo) {
//		try {
//			logger.info(PROPOSAL_ID, vo.getProposal().getProposalId());
//			Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
//			proposal.setUpdateUser(vo.getUpdateUser());
//			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
//			proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
//			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_WITHDRAW));
//			proposal = proposalDao.saveOrUpdateProposal(proposal);
//			loadProposalUserFullNames(proposal);
//			inboxDao.markReadMessage(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId().toString(), vo.getPersonId(), Constants.MESSAGE_TYPE_PROPOSAL_REJECT, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
//			vo.setProposal(proposal);
//			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
//			if (!commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION)) {
//				Workflow activeWorkflow = workflowDao.fetchActiveWorkflowByParams(vo.getProposal().getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
//				List<WorkflowDetail> workflowListDetails = workflowDao.fetchWorkflowDetailByWorkflowId(activeWorkflow.getWorkflowId());
//				for (WorkflowDetail workflowListDetail : workflowListDetails) {
//					if (!(workflowListDetail.getApprovalStatusCode().equals("T"))) {
//						setNotificationRecipients(workflowListDetail.getEmailAddress(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
//					}
//				}
//			} else {
//				setGrantManagerAndAdminEmailRecipients(vo.getProposalId(), dynamicEmailrecipients);
//			}
//			sendProposalNotification(vo, Constants.NOTIFICATION_WITHDRAWAL_PROPOSAL, dynamicEmailrecipients);
//		} catch (Exception e) {
//			logger.error("exception in withdrawProposal: {} ", e.getMessage());
//		}
//		return commonDao.convertObjectToJSON(vo);
//	}

	@Override
	public String maintainProjectTeam(MaintainProjectTeamVO vo) {
		if (vo != null) {
			if ("D".equals(vo.getProjectTeam().getAcType())) {
				proposalDao.deleteManitainProjectTeam(vo.getProjectTeam().getProposalProjectTeamId());
				return commonDao.convertObjectToJSON("project team deleted successfully");
			} else if ("I".equals(vo.getProjectTeam().getAcType())) {
				proposalDao.saveManitainProjectTeam(vo.getProjectTeam());
				vo.getProjectTeam().setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				vo.getProjectTeam().setAcType("U");
			} else {
				proposalDao.updateManitainProjectTeam(vo.getProjectTeam());
				vo.getProjectTeam().setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			}
			return commonDao.convertObjectToJSON(vo);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private ProposalVO buildWorkflow(ProposalVO proposalVO) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(3);
		evaluateValidationRuleVO.setSubModuleCode(0);
		evaluateValidationRuleVO.setModuleItemKey(proposalVO.getProposalId().toString());
		evaluateValidationRuleVO.setLogginPersonId(proposalVO.getPersonId());
		evaluateValidationRuleVO.setUpdateUser(proposalVO.getUserName());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			proposalVO.setWorkflow(workflowDao.fetchActiveWorkflowByParams(proposalVO.getProposal().getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		Integer workflowDetailId = workflowDao.fetchWorkflowDetailId(proposalVO.getProposalId(), evaluateValidationRuleVO.getLogginPersonId());
		proposalVO.setIsPersonCanScore(proposalDao.fetchPersonCanScore(proposalVO.getProposalId(), evaluateValidationRuleVO.getLogginPersonId(), workflowDetailId));
		proposalVO.setCanApproveRouting(canApproveRouting.toString());
		proposalVO.setIsFinalApprover(isFinalApprover);
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		if (proposalVO.getWorkflow() != null) {
			List<WorkflowDetail> workflowDetails = proposalVO.getWorkflow().getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workflowDetails) {
					if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
						commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
						proposalVO.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
						proposalVO.setMapId(workflowDetail.getMapId());
					}
				}
			}
		}
		sendProposalNotification(proposalVO, Constants.PROPOSAL_SUBMIT_NOTIFICATION_CODE, dynamicEmailrecipients);
		return proposalVO;
	}

	private ProposalVO fetchPreviousWorkFlowsList(ProposalVO proposalVO) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposalVO.getProposal().getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			proposalVO.setWorkflowList(workFlows);
		}
		return proposalVO;
	}

	@Override
	public String maintainProposalPersonRoles(ProposalPersonRoleVO proposalPersonRoleVO) {
		logger.info("maintainProposalPersonRoles service");
		String acType = "";
		String response = "";
		ProposalPersonRoles proposalPersonRoles;
		List<ProposalPersonRoles> proposalPersonRoleDetails = proposalPersonRoleVO.getProposalPersonRoles();
		List<ProposalPersonRoles> proposalPersonRolesListReturn = new ArrayList<>();
		if (proposalPersonRoleDetails != null && !proposalPersonRoleDetails.isEmpty()) {
			for (ProposalPersonRoles personRole : proposalPersonRoleDetails) {
				acType = personRole.getAcType();
				if (acType != null) {
					Person personData = personDao.getPersonDetailById(personRole.getPersonId());
					Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
					if (acType.equalsIgnoreCase(Constants.acTypeInsert)) {
						// insert
						personRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						personRole.setRole(rolesManagementDao.getRoleInformation(personRole.getRoleId()));
						proposalPersonRoles = proposalDao.saveProposalPersonRole(personRole);
						proposalPersonRoles.setPerson(personData);
						proposalPersonRoles.setAcType(Constants.acTypeUpdate);
						proposalPersonRolesListReturn.add(proposalPersonRoles);
						proposalPersonRoleVO.setProposalPersonRoles(proposalPersonRolesListReturn);
						proposalPersonRoleVO.setMessage("Proposal Person Roles saved successfully");
						commonService.setNotificationRecipients(personData.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
						ProposalVO proposalVO = new ProposalVO();
						Proposal proposal = proposalDao.fetchProposalById(proposalPersonRoleVO.getProposalId());
						proposalVO.setProposal(proposal);
						proposalVO.setUserFullName(personData.getFullName());
						Role role = rolesManagement.getRoleInformation(personRole.getRoleId());
						proposalVO.setProposalPersonRole(role.getRoleName());
						sendProposalNotification(proposalVO, Constants.PROPOSAL_PERSON_ROLE_ADD_NOTIFICATION_CODE, dynamicEmailrecipients);
						response = commonDao.convertObjectToJSON(proposalPersonRoleVO);
					} else if (acType.equalsIgnoreCase(Constants.acTypeDelete)) {
						// delete
						commonService.setNotificationRecipients(personData.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
						String message = proposalDao.deleteProposalPersonRole(personRole);
						proposalPersonRoleVO.setMessage(message);
						ProposalVO proposalVO = new ProposalVO();
						Proposal proposal = proposalDao.fetchProposalById(proposalPersonRoleVO.getProposalId());
						proposalVO.setProposal(proposal);
						proposalVO.setUserFullName(personData.getFullName());
						Role role = rolesManagement.getRoleInformation(personRole.getRoleId());
						proposalVO.setProposalPersonRole(role.getRoleName());
						sendProposalNotification(proposalVO, Constants.PROPOSAL_PERSON_ROLE_DELETE_NOTIFICATION_CODE, dynamicEmailrecipients);
						response = commonDao.convertObjectToJSON(proposalPersonRoleVO);
					}
				}
			}
		}
		return response;
	}

	@Override
	public String fetchProposalPersonRoles(ProposalPersonRoleVO proposalPersonRoleVO) {
		Integer proposalId = proposalPersonRoleVO.getProposalId();
		String response = "";
		if (proposalId != null) {
			List<ProposalPersonRoles> proposalPersonRoles = proposalLookUpDao.fetchProposalPersonRoles(proposalId, null);
			if (proposalPersonRoles != null && !proposalPersonRoles.isEmpty()) {
				proposalPersonRoleVO.setProposalPersonRoles(proposalPersonRoles);
			}
			List<ModuleDerivedRoles> moduleDerivedRoles = rolesManagementDao.getModuleDerivedRoles(Constants.DEV_PROPOSAL_MODULE_CODE);
			if (moduleDerivedRoles != null && !moduleDerivedRoles.isEmpty()) {
				proposalPersonRoleVO.setModuleDerivedRoles(moduleDerivedRoles);
			}
			response = commonDao.convertObjectToJSON(proposalPersonRoleVO);
		}
		return response;
	}

	@Override
	public ProposalVO sendProposalNotification(ProposalVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.getProposal().getProposalId().toString());
		emailServiceVO.setPlaceHolder(getProposalPlaceholders(vo));
		emailServiceVO.setSubModuleCode((Constants.DEV_PROPOSAL_SUBMODULE_CODE).toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && !emailServiceVO.getPrompted()) {
			vo.setNotificationTypeId(notificationTypeId);
			vo.setBody(emailServiceVO.getBody());
			vo.setSubject(emailServiceVO.getSubject());
		}
		return vo;
	}

	private Map<String, String> getProposalPlaceholders(ProposalVO vo) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{PERSON_NAME}", vo.getUserName() == null ? "" : vo.getUserName());
		placeHolder.put("{USER_NAME}", vo.getUserFullName() == null ? "" : vo.getUserFullName());
		placeHolder.put("{NUMBER_OF_APPLICATIONS}", vo.getNumberOfApplications() == null ? "" : vo.getNumberOfApplications().toString());
		placeHolder.put("{REVIEW_DEADLINE_DATE}", vo.getReviewDeadLineDate() != null ? commonService.convertDateFormatBasedOnTimeZone(vo.getReviewDeadLineDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{REVIEWER_ROLE}", vo.getReviewerRole() == null ? "" : vo.getReviewerRole());
		placeHolder.put("{PROPOSAL_PERSON_ROLE}", vo.getProposalPersonRole() == null ? "" : vo.getProposalPersonRole());
		placeHolder.put("{WORKFLOW_COMMENT}", vo.getApproveComment() != null ? vo.getApproveComment() : "No Comments");
		placeHolder.put("{REVISION_DEADLINE_DATE}", vo.getPiReviewDeadLineDate() != null ? commonService.convertDateFormatBasedOnTimeZone(vo.getPiReviewDeadLineDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		String stopName = commonService.getPlaceHolderDataForRouting(vo.getApproverStopNumber(),vo.getMapId(),vo.getWorkflowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	@Override
	public String addScienceKeyword(ProposalVO vo) {
		ScienceKeyword scienceKeyword = new ScienceKeyword();
		scienceKeyword.setDescription(vo.getScienceKeyword());
		scienceKeyword.setUpdateUser(vo.getUserName());
		scienceKeyword.setIsActive(Boolean.TRUE);
		if (!proposalDao.isKeywordExist(vo.getScienceKeyword())) {
			scienceKeyword.setCode(proposalDao.getMaxScienceKeyword());
			scienceKeyword.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			return commonDao.convertObjectToJSON(proposalDao.saveOrUpdateScienceKeyword(scienceKeyword));
		}
		return commonDao.convertObjectToJSON(scienceKeyword);
	}

	@Override
	public void printEntireProposal(ProposalVO vo, HttpServletResponse response) {
		Integer proposalId = vo.getProposalId();
		vo.setUserName(AuthenticatedUser.getLoginUserName());
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("Export proposal Details of Proposal ID {}", proposalId);
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		if (proposal != null) {
			Integer budgetHeaderId = null;
			String fileName = "Proposal_#"+proposalId;
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<ProposalAttachment> attachments = proposalModuleDao.fetchProposalAttachmentByProposalIdWithLastVersion(proposalId);
			List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(proposalId);
			for (BudgetHeader budgetHeader : budgetHeaders) {
				if (budgetHeader.getIsLatestVersion()) {
					budgetHeaderId = budgetHeader.getBudgetId();
				}
			}
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ZipOutputStream zos = new ZipOutputStream(baos);
				if (attachments != null && !attachments.isEmpty()) {
					Integer index = 0;
					for (ProposalAttachment attachment : attachments) {
						index = commonService.addFilesToZipFolder(index, attachment.getFileName(), zos);
						FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
						byte[] data = fileData.getAttachment();
						zos.write(data);
					}
				}
				byte[] proposalTemplateByteArray = printService.getTemplateData(Constants.PROPOSAL_LETTER_TEMPLATE_TYPE_CODE);
				byte[] bytesSummary = printService.setMergePlaceHoldersOfProposal(proposalTemplateByteArray, proposalId, vo.getPersonId(), vo.getUserName(), null, null, null);
				zos.putNextEntry(new ZipEntry(new StringBuilder("Proposal_Summary_").append(proposalId.toString()).append(".pdf").toString()));
				zos.write(bytesSummary);
				byte[] bytesBudgetSummary = null;
				if (budgetHeaderId != null) {
					vo.setBudgetId(budgetHeaderId);
					ByteArrayInputStream bis = printService.generateBudgetPDF(setPrintParameters(vo));
					bytesBudgetSummary = IOUtils.toByteArray(bis);
					zos.putNextEntry(new ZipEntry(new StringBuilder("Proposal_Budget_Summary_").append(proposalId).append(".pdf").toString()));
					zos.write(bytesBudgetSummary);
				}
				List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(proposalId);
				Set<Integer> proposalPersonId = new HashSet<>();
				for(ProposalPerson proposalPerson : proposalPersons) {
					proposalPersonId.add(proposalPerson.getProposalPersonId());
				}
				if(proposalPersonId != null && !proposalPersonId.isEmpty()) {
					List<ProposalPersonAttachment> proposalPersonAttachments =
							proposalDao.fetchProposalPersonAttachmentWithLastVersion(proposalPersonId);
					if (proposalPersonAttachments != null && !proposalPersonAttachments.isEmpty()) {
						Integer index = 0;
						for (ProposalPersonAttachment attachment : proposalPersonAttachments) {
							index = commonService.addFilesToZipFolder(index, new StringBuilder("CV-").append(attachment.getFileName()).toString(), zos);
							FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
							byte[] data = fileData.getAttachment();
							zos.write(data);
						}
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
				logger.error("exception in printEntireProposal: {} ", e.getMessage());
			}
		}
	}

	@Override
	public String fetchSortedAttachments(ProposalVO vo) {
		Integer proposalId = vo.getProposalId();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		logger.info(PROPOSAL_ID, proposalId);
		logger.info("sortBy : {}", sortBy);
		logger.info("reverse : {}", reverse);
		vo.setNewAttachments(proposalDao.fetchSortedAttachments(proposalId, sortBy, reverse));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fundTheProposal(ProposalVO proposalVO) {
		Integer proposalId = proposalVO.getProposalId();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		String ipNumber = institutionalProposalService.generateInstitutionalProposalNumber();
		logger.info("Initial IP Number : {}",  ipNumber);
		boolean isIPCreated = institutionalProposalService.createInstitutionalProposal(proposalId, ipNumber, proposal.getUpdateUser());
		logger.info("isIPCreated : {}", isIPCreated);
		if (isIPCreated) {
			logger.info("Generated IP Number : {}", ipNumber);
			proposal.setIpNumber(ipNumber);
			proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
		}
		proposal.setUpdateUser(proposalVO.getUpdateUser());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		loadProposalUserFullNames(proposal);
		proposalVO.setProposal(proposal);
		proposalVO.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId));
		if (isIPCreated) {
			sendProposalNotification(proposalVO, Constants.NOTIFICATION_FUND_PROPOSAL, new HashSet<>());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String saveProposalRankFromDashboard(CommonVO vo) {
		Integer proposalId = vo.getProposalId();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		proposal.setProposalRank(vo.getProposalRank());
		proposal.setUpdateUser(vo.getUserName());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposal.setRecommendationCode(vo.getRecommendationCode());
		proposal.setEvaluationRecommendation(evaluationDao.getEvaluationRecommendationByStatusCode(vo.getRecommendationCode()));
		if (proposal.getIsEndorsedOnce()) {
			List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId);
			if (proposalReviews != null && !proposalReviews.isEmpty()) {
				for (ProposalReview proposalReview : proposalReviews) {
					if ((proposalReview.getHasRank() || proposalReview.getHasRecommendation()) && (proposalReview.getReviewStatusCode().equals(Constants.PROPOSAL_REVIEW_STATUS_COMPLETE))) {
							proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS));
							proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
							proposalReview.setReviewEndDate(null);
							proposalModuleDao.saveOrUpdateProposalReview(proposalReview);
							inboxService.addMessageToInbox(proposal, proposalReview.getReviewerPersonId() , vo.getUserName(), Constants.MESSAGE_TYPE_EVALUATION, Constants.SUBJECT_TYPE_CODE, proposalReview.getReviewId(), proposalReview.getRole().getDescription());
					}
				}
				proposal.setIsEndorsedOnce(false);
			}
			if(!proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_DEC_REVIEW_IN_PROGRESS)) {
				proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_DEC_REVIEW_IN_PROGRESS);
				proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_DEC_REVIEW_IN_PROGRESS));
			}
		}
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		ProposalVO proposalVO = new ProposalVO();
		loadProposalUserFullNames(proposal);
		proposalVO.setProposal(proposal);
		proposalVO.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId));
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String fetchActivityType(ProposalVO vo) {
		vo.setActivityTypes(commonDao.fetchAllActivityTypes());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAllActivityForGrantType() {
		ProposalVO vo = new ProposalVO();
		vo.setActivityTypes(proposalLookUpDao.getAllActivityForGrantType());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String addProposalPersonAttachment(MultipartFile[] files, String formDataJSON) {
		ProposalVO proposalVO = null;
		List<ProposalPersonAttachment> proposalPersonAttachments = new ArrayList<>();
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			List<ProposalPersonAttachment> newAttachments = proposalVO.getNewPersonAttachments();
			if(files.length == 0 && newAttachments != null && !newAttachments.isEmpty()) {
				for (ProposalPersonAttachment newAttachment : newAttachments) {
					if (newAttachment.getReplaceAttachmentId() != null) {
						proposalDao.deleteProposalPersonAttachment(newAttachment.getReplaceAttachmentId());
					}
				}
			} else {
				for (int i = 0; i < files.length; i++) {
					for (ProposalPersonAttachment newAttachment : newAttachments) {
						if (newAttachment.getReplaceAttachmentId() != null) {
							proposalDao.deleteProposalPersonAttachment(newAttachment.getReplaceAttachmentId());
						}
						ProposalPersonAttachment proposalPersonAttachment = new ProposalPersonAttachment();
						proposalPersonAttachment.setFileName(newAttachment.getFileName());
						proposalPersonAttachment.setDescription(newAttachment.getDescription());
						proposalPersonAttachment.setMimeType(newAttachment.getMimeType());
						FileData fileData = new FileData();
						fileData.setAttachment(files[i].getBytes());
						fileData = commonDao.saveFileData(fileData);
						proposalPersonAttachment.setFileDataId(fileData.getFileDataId());
						proposalPersonAttachment.setUpdateUser(newAttachment.getUpdateUser());
						proposalPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						proposalPersonAttachment.setAttachment(files[i].getBytes());
						proposalPersonAttachments.add(proposalPersonAttachment);
					}
				}
			}
			proposalVO.setNewPersonAttachments(proposalPersonAttachments);
		} catch (Exception e) {
			logger.error("exception in addProposalPersonAttachment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public ResponseEntity<byte[]> downloadProposalPersonAttachment(Integer attachmentId) {
		ProposalPersonAttachment attachment = proposalDao.fetchPersonAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("exception in downloadProposalPersonAttachment: {} ", e.getMessage());
		}
		return attachmentData;

	}

	private Boolean isActivityForGrantTypeEnabled() {
		return commonDao.getParameterValueAsBoolean("ENABLED_ACTIVITY_GRANT_TYPE_MAPPPING");
	}

	@Override
	public String saveOrUpdateKeyPerson(ProposalVO vo) {
		Integer proposalId = vo.getProposalId();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		Boolean addKeyPerson = true;
		ProposalPerson proposalPerson = vo.getProposalPerson();
		if (proposalPerson.getProposalPersonId() != null) {
			deleteCertificationQuestAnswer(proposalPerson);
			addKeyPerson = false;
			deletePersonDegree(proposalPerson.getRolodexId(),proposalPerson.getPersonId(),proposalPerson.getProposalPersonId());
		}
		if(proposal.getGrantCallId() != null) {
			HashMap<String, Object> eligibilty = proposalDao.checkGrantCallEligibilty(proposal.getGrantCallId(), proposalPerson.getPersonId(), proposalPerson.getPersonRoleId());
			vo.setGrantEligibilityStatus(eligibilty);
			if ("VE".equals(eligibilty.get("status"))) {
				return commonDao.convertObjectToJSON(vo);
			}
		} else {
			Map<String, Object> eligibilty = new HashMap<>();
			eligibilty.put("status", "TRUE");
			vo.setGrantEligibilityStatus(eligibilty);
		}
		if (proposalPerson.getIsPi() || proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE) && (proposalPerson.getUnits() != null && !proposalPerson.getUnits().isEmpty())) {
			for (ProposalPersonUnit proposalPersonUnit : proposalPerson.getUnits()) {
				if (proposalPersonUnit.isLeadUnit()) {
					proposal.setHomeUnitNumber(proposalPersonUnit.getUnitNumber());
					proposal.setHomeUnitName(commonDao.getLeadUnitByUnitNumber(proposalPersonUnit.getUnitNumber()).getUnitName());
					proposal.setUpdateUser(proposalPerson.getUpdateUser());
					proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					proposal = proposalDao.saveOrUpdateProposal(proposal);
				}
			}
		}
		String piPersonId = proposalModuleDao.getPrincipalInvestigator(proposalId);
		if (proposalPerson.getPersonId() != null && (piPersonId != null && !piPersonId.equals(personDao.getPersonIdByUserName(proposal.getCreateUser())) && (proposalPerson.getIsPi() || piPersonId.equals(proposalPerson.getPersonId())))) {
				deleteProposalPersonRole(piPersonId, proposalId);
		}
		List<ProposalPersonUnit> proposalPersonUnits = proposalPerson.getUnits();
		if (proposalPersonUnits != null && !proposalPersonUnits.isEmpty()) {
			List<ProposalPersonUnit> updatedProposalPersonUnits = new ArrayList<>(proposalPersonUnits);
			Collections.copy(updatedProposalPersonUnits, proposalPersonUnits);
			for (ProposalPersonUnit proposalPersonUnit : proposalPersonUnits) {
				if (proposalPersonUnit.getIsDeleted()) {
					proposalModuleDao.deleteProposalPersonUnit(proposalPersonUnit);
					updatedProposalPersonUnits.remove(proposalPersonUnit);
				}
			}
			proposalPerson.getUnits().clear();
			proposalPerson.getUnits().addAll(updatedProposalPersonUnits);
		}
		if (!proposalPerson.getProposalPersonAttachment().isEmpty()) {
			ProposalPersonAttachment proposalPersonAttachment = proposalPerson.getProposalPersonAttachment().get(0);
			if (proposalPersonAttachment.getAttachmentId() == null) {
				proposalPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				proposalPersonAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			}
		}
		proposalModuleDao.saveOrUpdateProposalPerson(proposalPerson);
		if(Boolean.TRUE.equals(addKeyPerson)) {
		 copyPersonDegree(proposalPerson.getProposalPersonId(),proposalPerson.getPersonId());
		}
		String personId = proposalPerson.getPersonId();
		if (personId != null) {			
			assignDerivedRolesForPerson(proposalId, proposalPerson);
		}
		setProposalUpdateUser(vo);
		List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId);
		vo.setProposalPersons(proposalPersons);
		proposal.setProposalPersons(proposalPersons);
		proposalPerson.setTrainingStatus(proposalDao.isProposalPersonTrainingCompleted(personId, proposalId));
		vo.setProposal(proposal);
		if (vo.getPreviousProposalPersonId() != null) {
			Boolean isCPReportGenerated = currentAndPendingDao.checkIfReportGenerated(vo.getPreviousProposalPersonId(), proposalId.toString());
			if (Boolean.TRUE.equals(isCPReportGenerated)) {
				List<CPReportHeader> cpReportHeaders = currentAndPendingDao.fetchCPReportHeadersByParams(vo.getPreviousProposalPersonId(), vo.getPreviousNonEmployeeFlag(), Constants.DEV_PROPOSAL_MODULE_CODE, proposalId.toString());
				deleteCPReportHeaders(cpReportHeaders);
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void deletePersonDegree(Integer changedRolodexId,String changedPersonId,Integer proposalPersonId){
		ProposalPerson proposalPersondetails = proposalModuleDao.fetchProposalPerson(proposalPersonId);
		if(changedPersonId != null) {
			String personId = proposalPersondetails.getPersonId();
			if(!changedPersonId.equals(personId)) {
				proposalModuleDao.deleteDegreeByProposalPersonId(proposalPersonId);
				copyPersonDegree(proposalPersonId,changedPersonId);
			}
		} else {
			Integer personId = proposalPersondetails.getRolodexId();
			if(!changedRolodexId.equals(personId)) {
				proposalModuleDao.deleteDegreeByProposalPersonId(proposalPersonId);
		    }
		}
		commonDao.detachEntityFromSession(proposalPersondetails);
	}

	private void copyPersonDegree(Integer proposalPersonId,String personId) {
		List<PersonDegree> personDegree = personDao.getAllPersonDegree(personId);
		for (PersonDegree originalPersonDegree : personDegree) {
			ProposalPersonDegree copyProposalPersonDegree = new ProposalPersonDegree();
			copyProposalPersonDegree.setProposalPersonId(proposalPersonId);
			copyProposalPersonDegree.setDegree(originalPersonDegree.getDegree());
			copyProposalPersonDegree.setDegreeCode(originalPersonDegree.getDegreeCode());
			copyProposalPersonDegree.setFieldOfStudy(originalPersonDegree.getFieldOfStudy());
			copyProposalPersonDegree.setGraduationDate(originalPersonDegree.getGraduationDate());
			copyProposalPersonDegree.setSpecialization(originalPersonDegree.getSpecialization());
			copyProposalPersonDegree.setSchool(originalPersonDegree.getSchool());
			copyProposalPersonDegree.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			copyProposalPersonDegree.setUpdateUser(AuthenticatedUser.getLoginUserName());
			proposalModuleDao.addProposalPersonDegree(copyProposalPersonDegree);
		}
	}

	private void deleteCertificationQuestAnswer(ProposalPerson proposalPerson) {
		ProposalPerson oldProposalPerson = proposalModuleDao.fetchProposalPerson(proposalPerson.getProposalPersonId());
		String oldPersonId = oldProposalPerson.getPersonId() == null ? "0" : oldProposalPerson.getPersonId();
		String newPersonId = proposalPerson.getPersonId() == null ? "0" : proposalPerson.getPersonId();
		Integer oldRolodex = oldProposalPerson.getRolodexId() == null ? 0 : oldProposalPerson.getRolodexId();
		Integer newRolodex = proposalPerson.getRolodexId() == null ? 0 : proposalPerson.getRolodexId();
		if (!oldPersonId.equals(newPersonId) || !oldRolodex.equals(newRolodex) || !oldProposalPerson.getPersonRoleId().equals(proposalPerson.getPersonRoleId())) {
			questionnaireDAO.deleteQuestAnswerAttachment(proposalPerson.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE,
					oldProposalPerson.getRolodexId() != null ? oldProposalPerson.getRolodexId().toString() : oldProposalPerson.getPersonId());
			questionnaireDAO.deleteQuestAnswer(proposalPerson.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE,
					oldProposalPerson.getRolodexId() != null ? oldProposalPerson.getRolodexId().toString() : oldProposalPerson.getPersonId());
			questionnaireDAO.deleteQuestAnswerHeader(proposalPerson.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE,
					oldProposalPerson.getRolodexId() != null ? oldProposalPerson.getRolodexId().toString() : oldProposalPerson.getPersonId());
			proposalPerson.setPersonCertified(false);
		}
		commonDao.detachEntityFromSession(oldProposalPerson);
	}

	@Override
	public String saveOrUpdateProjectTeam(ProposalVO vo) {
		proposalModuleDao.saveOrUpdateProposalProjectTeam(vo.getProposalProjectTeam());
		setProposalUpdateUser(vo);
		return commonDao.convertObjectToJSON(proposalModuleDao.fetchProposalProjectTeamBasedOnProposalId(vo.getProposalId()));
	}

	@Override
	public String saveOrUpdateSpecialReview(ProposalVO vo) {
		proposalModuleDao.saveOrUpdateProposalSpecialReview(vo.getProposalSpecialReview());
		setProposalUpdateUser(vo);
		return commonDao.convertObjectToJSON(prepareSpecialReviewDetail(proposalModuleDao.fetchProposalSpecialReviewBasedOnProposalId(vo.getProposalId())));
	}

	@Override
	public String saveOrUpdateFunds(ProposalVO vo) {
		proposalModuleDao.saveOrUpdateProposalSponsor(vo.getProposalSponsor());
		setProposalUpdateUser(vo);
		return commonDao.convertObjectToJSON(proposalModuleDao.fetchProposalSponsorBasedOnProposalId(vo.getProposalId()));
	}

	@Override
	public String saveOrUpdateAreaOfResearch(ProposalVO vo) {
		proposalModuleDao.saveOrUpdateProposalResearchArea(vo.getProposalResearchArea());
		setProposalUpdateUser(vo);
		return commonDao.convertObjectToJSON(proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(vo.getProposalId()));
	}

	@Override
	public String loadProposalAttachment(Integer proposalId, Integer proposalStatusCode) {
		ProposalVO proposalVO = new ProposalVO();
		if (!proposalStatusCode.equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)) {
			proposalVO.setProposalAttachmentTypes(proposalLookUpDao.fetchAllProposalAttachmentTypes());
			proposalVO.setNarrativeStatus(commonDao.fetchAllNarrativeStatus());
		}
		proposalVO.setIsReplaceAttachmentEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_REPLACE_ATTACHMENTS_DEV_PROP));
		proposalVO.setProposalAttachments(getProposalAttachmentsByProposalId(proposalId));
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private List<ProposalAttachment> getProposalAttachmentsByProposalId(Integer proposalId) {
		List <ProposalAttachment> proposalAttachments = proposalModuleDao.fetchProposalAttachmentBasedOnProposalId(proposalId);
		if (proposalAttachments != null && !proposalAttachments.isEmpty()) {
			for(ProposalAttachment proposalAttachment : proposalAttachments) {
				if (proposalAttachment.getUpdateUser() != null) {
					proposalAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(proposalAttachment.getUpdateUser()));
				}
			}
		}
		return proposalAttachments;
	}

	@Override
	public String saveGrantCallFromProposal(ProposalVO vo) {
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		Integer grantCallId = vo.getGrantCallId();
		if (grantCallId == null) {
			logger.info("Delete Grant Call from proposal");
			proposal.setGrantCallId(null);
			proposal.setGrantCallName(null);
			proposal.setGrantCallClosingDate(null);
			List<ProposalPersonRoles> proposalPersons = proposalLookUpDao.fetchProposalPersonRoles(vo.getProposalId(), Constants.REVIEW_PROPOSAL_ROLE_ID);
			if ((proposalPersons != null && !proposalPersons.isEmpty())) {
				rolesManagementDao.deleteProposalPersons(proposalPersons);
			}
			List<ProposalKPI> proposalKPIs = proposalDao.fetchAllProposalKPI(vo.getProposalId());
			if (proposalKPIs != null && !proposalKPIs.isEmpty()) {
				for (ProposalKPI proposalKPI : proposalKPIs) {
					proposalDao.deleteProposalKPI(proposalKPI);
				}
			}
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
				List<ProposalEvaluationPanel> proposalEvaluationPanels = evaluationDao.fetchProposalEvaluationPanelsByProposalId(vo.getProposalId());
				if (proposalEvaluationPanels != null && !proposalEvaluationPanels.isEmpty()) {
						evaluationDao.deleteProposalEvaluationPanels(proposalEvaluationPanels);
				}
			}
		 } else {
			logger.info("Add Grant Call from proposal");
			GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallId);
			proposal.setGrantCallId(grantCallId);
			proposal.setGrantTypeCode(grantCall.getGrantTypeCode());
			proposal.setGrantCallType(grantCallDao.fetchGrantCallTypeByGrantTypeCode(grantCall.getGrantTypeCode()));
			proposal.setGrantCallName(grantCall.getGrantCallName());
			proposal.setGrantCallClosingDate(grantCall.getClosingDate());
			proposal.setInternalDeadLineDate(grantCall.getInternalSubmissionDeadLineDate());
			vo.setGrantCall(prepareGrantCallData(grantCall));				
		}
		proposal.setUpdateUser(vo.getUpdateUser());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalDao.saveOrUpdateProposal(proposal);
		loadProposalUserFullNames(proposal);
		vo.setProposal(proposal);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveDescriptionOfProposal(ProposalVO vo) {
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		proposal.setResearchDescription(vo.getResearchDescription());
		proposal.setMultiDisciplinaryDescription(vo.getMultiDisciplinaryDescription());
		proposal.setUpdateUser(vo.getUpdateUser());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		loadProposalUserFullNames(proposal);
		return commonDao.convertObjectToJSON(proposal);
	}

	@Override
	public String loadEvaluationDetails(Integer proposalId, String personId, String loginPersonUnitNumber) {
		ProposalVO proposalVO = new ProposalVO();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		if (!proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED) && !proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)) {
			proposalVO.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(), proposal.getActivityTypeCode()));
		}
		proposalVO.setProposalId(proposalId);
		proposalVO.setSortBy("");
		proposalVO.setPersonId(personId);
		proposalVO.setLoginPersonUnitNumber(loginPersonUnitNumber);
		proposalVO.setProposalReviews(evaluationService.prepareProposalReviewDetail(proposalVO));
		List<Integer> roleIds = evaluationDao.getDistintRoleIdFromEvaluationsStop();
		if(roleIds != null && !roleIds.isEmpty()) {
			proposalVO.setPersonRoles(evaluationDao.fetchPersonRoleOnEvaluationBasedOnPersonId(personId, roleIds));
		}
		if(proposal.getActivityTypeCode() != null && (proposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE)))) {
				proposal.setIsRcbfProposal(true);
		}
		proposalVO.setProposal(proposal);
		proposalVO.setHasRank(evaluationDao.checkPersonHasRank(proposalId, personId));
		proposalVO.setHasRecommendation(evaluationDao.checkPersonHasRecommendation(proposalId, personId));
		proposalVO.setFinalEvaluationStatus(evaluationDao.getFinalEvaluationStatus());
		proposalVO.setEvaluationRecommendation(evaluationDao.getAllEvaluationRecomendation());
		proposalVO.setWorkflowList(prepareWorkFlowList(proposalId.toString()));
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private List<Workflow> prepareWorkFlowList(String proposalId) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposalId, Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			for (Workflow workFlow : workFlows) {
				if (workFlow.getWorkflowStartPerson() != null) {
					String personName = personDao.getPersonFullNameByPersonId(workFlow.getWorkflowStartPerson());
					if (personName != null) {
						workFlow.setStartPersonName(personName);
					}
				}
				if (workFlow.getWorkflowEndPerson() != null) {
					String personName = personDao.getPersonFullNameByPersonId(workFlow.getWorkflowEndPerson());
					if (personName != null) {
						workFlow.setEndPersonName(personName);
					}
				}
			}
		}
		return workFlows;
	}

	private void deleteAllProposalAttachments(Integer proposalId) {
		List<ProposalAttachment> proposalAttachments = proposalModuleDao.fetchProposalAttachmentBasedOnProposalId(proposalId);
		if (proposalAttachments != null && !proposalAttachments.isEmpty()) {
			for (ProposalAttachment proposalAttachment : proposalAttachments) {
				proposalModuleDao.deleteProposalAttachment(proposalAttachment);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllBudgetHeader(Integer proposalId) {
		List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(proposalId);
		if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
			for (BudgetHeader budgetHeader : budgetHeaders) {
				proposalModuleDao.deleteBudgetHeader(budgetHeader);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalSpecialReview(Integer proposalId) {
		List<ProposalSpecialReview> proposalSpecialReviews = proposalModuleDao.fetchProposalSpecialReviewBasedOnProposalId(proposalId);
		if (proposalSpecialReviews != null && !proposalSpecialReviews.isEmpty()) {
			for (ProposalSpecialReview proposalSpecialReview : proposalSpecialReviews) {
				proposalModuleDao.deleteProposalSpecialReview(proposalSpecialReview);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalPersonAssignedRoles(Integer proposalId) {
		List<ProposalPersonAssignedRoles> proposalPersonAssignedRoles = proposalModuleDao.fetchProposalPersonAssignedRolesBasedOnProposalId(proposalId);
		if (proposalPersonAssignedRoles != null && !proposalPersonAssignedRoles.isEmpty()) {
			for (ProposalPersonAssignedRoles proposalPersonAssignedRole : proposalPersonAssignedRoles) {
				proposalModuleDao.deleteProposalPersonAssignedRoles(proposalPersonAssignedRole);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalPersonRoles(Integer proposalId) {
		List<ProposalPersonRoles> proposalPersonRoles = proposalModuleDao.fetchProposalPersonRolesBasedOnProposalId(proposalId);
		if (proposalPersonRoles != null && !proposalPersonRoles.isEmpty()) {
			for (ProposalPersonRoles proposalPersonRole : proposalPersonRoles) {
				proposalModuleDao.deleteProposalPersonRoles(proposalPersonRole);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalProjectTeam(Integer proposalId) {
		List<ProposalProjectTeam> proposalProjectTeams = proposalModuleDao.fetchProposalProjectTeamBasedOnProposalId(proposalId);
		if (proposalProjectTeams != null && !proposalProjectTeams.isEmpty()) {
			for (ProposalProjectTeam proposalProjectTeam : proposalProjectTeams) {
				proposalModuleDao.deleteProposalProjectTeam(proposalProjectTeam);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalIrbProtocol(Integer proposalId) {
		List<ProposalIrbProtocol> proposalIrbProtocols = proposalModuleDao.fetchProposalIrbProtocolBasedOnProposalId(proposalId);
		if (proposalIrbProtocols != null && !proposalIrbProtocols.isEmpty()) {
			for (ProposalIrbProtocol proposalIrbProtocol : proposalIrbProtocols) {
				proposalModuleDao.deleteProposalIrbProtocol(proposalIrbProtocol);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalResearchArea(Integer proposalId) {
		List<ProposalResearchArea> proposalResearchAreas = proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(proposalId);
		if (proposalResearchAreas != null && !proposalResearchAreas.isEmpty()) {
			for (ProposalResearchArea proposalResearchArea : proposalResearchAreas) {
				proposalModuleDao.deleteProposalResearchArea(proposalResearchArea);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalPerson(Integer proposalId) {
		List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId);
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			for (ProposalPerson proposalPerson : proposalPersons) {
			    proposalModuleDao.deleteDegreeByProposalPersonId(proposalPerson.getProposalPersonId());
				proposalModuleDao.deleteProposalPerson(proposalPerson);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalSponsor(Integer proposalId) {
		List<ProposalSponsor> proposalSponsors = proposalModuleDao.fetchProposalSponsorBasedOnProposalId(proposalId);
		if (proposalSponsors != null && !proposalSponsors.isEmpty()) {
			for (ProposalSponsor proposalSponsor : proposalSponsors) {
				proposalModuleDao.deleteProposalSponsor(proposalSponsor);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalReview(Integer proposalId) {
		List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId);
		if (proposalReviews != null && !proposalReviews.isEmpty()) {
			for (ProposalReview proposalReview : proposalReviews) {
				proposalModuleDao.deleteProposalReview(proposalReview);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllProposalKPI(Integer proposalId) {
		List<ProposalKPI> proposalKPIs = proposalModuleDao.fetchProposalKPIBasedOnProposalId(proposalId);
		if (proposalKPIs != null && !proposalKPIs.isEmpty()) {
			for (ProposalKPI proposalKPI : proposalKPIs) {
				proposalModuleDao.deleteProposalKPI(proposalKPI);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void deleteAllPreReviews(Integer proposalId) {
		List<PreReview> preReviews = proposalModuleDao.fetchPreReviewBasedOnProposalId(proposalId);
		if (preReviews != null && !preReviews.isEmpty()) {
			for (PreReview preReview : preReviews) {
				proposalModuleDao.deletePreReview(preReview);
			}
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	@Override
	public String updateAttachmentDetails(ProposalVO vo) {
		ProposalAttachment proposalAttachment = vo.getProposalAttachment();
		proposalAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalAttachment.setUpdateUser(AuthenticatedUser.getLoginUserFullName());
		proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
		setProposalUpdateUser(vo);
		return commonDao.convertObjectToJSON(getProposalAttachmentsByProposalId(proposalAttachment.getProposalId()));
	}

	private GrantCall prepareGrantCallData(GrantCall grantCall) {
		GrantCall grantCallData = new GrantCall();
		grantCallData.setGrantCallName(grantCall.getGrantCallName());
		grantCallData.setGrantCallId(grantCall.getGrantCallId());
		grantCallData.setGrantCallType(grantCall.getGrantCallType());
		grantCallData.setGrantTypeCode(grantCall.getGrantTypeCode());
		grantCallData.setSponsorFundingScheme(grantCall.getSponsorFundingScheme());
		grantCallData.setFundingSchemeId(grantCall.getFundingSchemeId());
		grantCallData.setSponsor(grantCall.getSponsor());
		grantCallData.setClosingDate(grantCall.getClosingDate());
		grantCallData.setInternalSubmissionDeadLineDate(grantCall.getInternalSubmissionDeadLineDate());
		grantCallData.setGrantTheme(grantCall.getGrantTheme());
		grantCallData.setGrantStatusCode(grantCall.getGrantStatusCode());
		grantCallData.setCreateUser(grantCall.getCreateUser());
		return grantCallData;
	}

	@Override
	public void exportSelectedAttachments(ProposalVO vo, HttpServletResponse response) {
		List<Integer> attachmentIds = vo.getAttachmentIds();
		logger.info(PROPOSAL_ID, vo.getProposalId());
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		if (proposal != null && attachmentIds != null && !attachmentIds.isEmpty()) {
			String fileName = "Proposal_#"+vo.getProposalId()+"_attachments";
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<ProposalAttachment> attachments = proposalModuleDao.fetchProposalAttachmentBasedOnAttachmentIds(attachmentIds);
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ZipOutputStream zos = new ZipOutputStream(baos);
				if (attachments != null && !attachments.isEmpty()) {
					Integer index = 0;
					for (ProposalAttachment attachment : attachments) {
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
				logger.error("exception in exportSelectedAttachments: {} ", e.getMessage());
			}
		}
	}

	@Override
	public void loadProposalHomeData(ProposalVO proposalVO) {
		Proposal proposal = proposalVO.getProposal();
		proposal.setHomeUnitName(proposal.getUnit().getUnitName());
		Integer proposalId = proposal.getProposalId();
		String personId = proposalVO.getPersonId();
		Integer grantCallId = proposal.getGrantCallId();
		if (grantCallId != null) {
			proposalVO.setGrantCall(prepareGrantCallData(grantCallDao.fetchGrantCallById(grantCallId)));
		}
		List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId);
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			proposalVO.setIsProposalPerson(isProposalEmployee(proposalPersons, personId));
			proposalPersons.stream().forEach(person -> {
				getFullNameOfUpdateUser(person.getProposalPersonAttachment());
				isProposalPersonTrainingCompleted(person, proposalId);
			});
			setPersonEmailAddress(proposalPersons);
			setRolodexEmailAddress(proposalPersons);
			proposalVO.setProposalPersons(proposalPersons);
			proposal.setProposalPersons(proposalPersons);
		}
		List<ProposalProjectTeam> proposalProjectTeams = proposalModuleDao.fetchProposalProjectTeamBasedOnProposalId(proposalId);
		if (proposalProjectTeams != null && !proposalProjectTeams.isEmpty()) {
			proposalVO.setProposalProjectTeams(proposalProjectTeams);
		}
		List<ProposalPersonAssignedRoles> proposalPersonAssignedRoles = proposalModuleDao.fetchProposalPersonAssignedRolesBasedOnProposalId(proposalId);
		if (proposalPersonAssignedRoles != null && !proposalPersonAssignedRoles.isEmpty()) {
			proposalVO.setProposalPersonAssignedRoles(proposalPersonAssignedRoles);
		}
		List<ProposalSpecialReview> proposalSpecialReviews = proposalModuleDao.fetchProposalSpecialReviewBasedOnProposalId(proposalId);
		if (proposalSpecialReviews != null && !proposalSpecialReviews.isEmpty()) {
			proposalSpecialReviews = prepareSpecialReviewDetail(proposalSpecialReviews);
			proposalVO.setProposalSpecialReviews(proposalSpecialReviews);
		}
		List<ProposalSponsor> proposalSponsors = proposalModuleDao.fetchProposalSponsorBasedOnProposalId(proposalId);
		if (proposalSponsors != null && !proposalSponsors.isEmpty()) {
			proposalVO.setProposalSponsors(proposalSponsors);
		}
		List<ProposalResearchArea> proposalResearchAreas = proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(proposalId);
		if (proposalResearchAreas != null && !proposalResearchAreas.isEmpty()) {
			proposalVO.setProposalResearchAreas(proposalResearchAreas);
		}
		List<ProposalIrbProtocol> proposalIrbProtocols = proposalModuleDao.fetchProposalIrbProtocolBasedOnProposalId(proposalId);
		if (proposalIrbProtocols != null && !proposalIrbProtocols.isEmpty()) {
			proposalVO.setProposalIrbProtocols(proposalIrbProtocols);
		}
		List<ProposalMileStone> proposalMileStones = proposalModuleDao.fetchProposalMileStonesBasedOnProposalId(proposalId);
		if (proposalMileStones != null && !proposalMileStones.isEmpty()) {
			proposalVO.setProposalMileStones(proposalMileStones);
		}
		if(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_ORGANIZATION)) {
			List<ProposalOrganization> proposalOrganization = proposalDao.loadProposalOrganization(proposalId);
			List<Integer> rolodexId = proposalOrganization.stream().filter(organization -> organization.getOrganization() != null).map(organization -> organization.getOrganization().getContactAddressId()).collect(Collectors.toList());
			if(!rolodexId.isEmpty()){
				List<Rolodex> rolodex = commonDao.getRolodexDetailByRolodexId(rolodexId);
				Map<Integer, String> collect = rolodex.stream().collect(Collectors.toMap(Rolodex :: getRolodexId, Rolodex :: getFullName));
				proposalOrganization.stream()
				.filter(item -> item.getOrganization() != null && item.getOrganization().getContactAddressId() != null && collect.containsKey(item.getOrganization().getContactAddressId()))
						.forEach(item -> item.getOrganization().setContactPersonName(collect.get(item.getOrganization().getContactAddressId())));
			}
			proposalVO.setProposalOrganizations(proposalOrganization);
		}
		if (proposal.getSourceProposalId() != null) {
			proposal.setSourceProposalTitle(proposalDao.getProposalTitleByProposalId(proposal.getSourceProposalId()));
		}
		proposal.setBaseProposalTitle(institutionalProposalService.getIPTitleForMasterProposal(proposal.getBaseProposalNumber()));
		loadProposalUserFullNames(proposal);
		setProposalSponsorDetail(proposal);
	}

	private List<ProposalSpecialReview> prepareSpecialReviewDetail(List<ProposalSpecialReview> proposalSpecialReviews) {
		List<ProposalSpecialReview> specialReviewDetail = new ArrayList<>();
		Map<String, String> nonEmployees = new HashMap<>();
		Map<String, String> persons = new HashMap<>();
		proposalSpecialReviews.forEach(proposalSpecialReview -> {
			specialReviewDetail.add(setIntegratedProposalSpecilReviewDetail(persons,nonEmployees, proposalSpecialReview));
		});
		return specialReviewDetail;
	}

	@Override
	public ProposalSpecialReview setIntegratedProposalSpecilReviewDetail(Map<String, String> persons, Map<String, String> nonEmployees, ProposalSpecialReview proposalSpecialReview) {
		ProposalSpecialReview specialReview = new ProposalSpecialReview();
		BeanUtils.copyProperties(proposalSpecialReview, specialReview);
		if (Boolean.TRUE.equals(specialReview.getIsProtocolIntegrated()) && Constants.SPECIAL_REVIEW_IRB_TYPE_CODE.equals(specialReview.getSpecialReviewTypeCode())
				&& specialReview.getProtocolNumber() != null) {
			IrbProtocol irbProtocol = getIrbProtocols(specialReview.getProtocolNumber());
				if (irbProtocol != null) {
					specialReview.setApplicationDate(irbProtocol.getInitialSubmissionDate());
					specialReview.setApprovalDate(irbProtocol.getApprovalDate());
					specialReview.setExpirationDate(irbProtocol.getExpirationDate());
					specialReview.setApprovalTypeCode(irbProtocol.getProtocolStatusCode());
					specialReview.setApprovalType(setApprovalTypeForIrbProtocol(irbProtocol));
					irbProtocol.setFullName(setFullNameForProtocol(persons, irbProtocol.getPersonId(), irbProtocol.getNonEmployeeFlag()));
					specialReview.setIrbProtocol(irbProtocol);
				}
			} else if (Boolean.TRUE.equals(specialReview.getIsProtocolIntegrated()) && Constants.SPECIAL_REVIEW_AC_TYPE_CODE.equals(specialReview.getSpecialReviewTypeCode())
					&& specialReview.getProtocolNumber() != null) {
				AcProtocol acProtocol = getAcProtocols(specialReview.getProtocolNumber());
				if (acProtocol != null) {
					specialReview.setApplicationDate(acProtocol.getInitialSubmissionDate());
					specialReview.setApprovalDate(acProtocol.getApprovalDate());
					specialReview.setExpirationDate(acProtocol.getExpirationDate());
					specialReview.setApprovalTypeCode(acProtocol.getProtocolStatusCode());
					specialReview.setApprovalType(setApprovalTypeForAcProtocol(acProtocol));
					acProtocol.setFullName(setFullNameForProtocol(nonEmployees, acProtocol.getPersonId(), acProtocol.getNonEmployeeFlag()));
					specialReview.setAcProtocol(acProtocol);
				}
			}
		return specialReview;
	}

	@Override
	public SpecialReviewApprovalType setApprovalTypeForIrbProtocol(IrbProtocol irbProtocol) {
		SpecialReviewApprovalType approvalType = new SpecialReviewApprovalType();
		approvalType.setApprovalTypeCode(irbProtocol.getProtocolStatusCode());
		approvalType.setDescription(irbProtocol.getIrbProtocolStatus().getDescription());
		return approvalType;
	}

	@Override
	public SpecialReviewApprovalType setApprovalTypeForAcProtocol(AcProtocol acProtocol) {
		SpecialReviewApprovalType approvalType = new SpecialReviewApprovalType();
		approvalType.setApprovalTypeCode(acProtocol.getProtocolStatusCode());
		approvalType.setDescription(acProtocol.getAcProtocolStatus().getDescription());
		return approvalType;
	}

	private void isProposalPersonTrainingCompleted(ProposalPerson person, Integer proposalId) {
		person.setTrainingStatus(proposalDao.isProposalPersonTrainingCompleted(person.getPersonId(), proposalId));
	}

	public void setProposalSponsorDetail(Proposal proposal) {
		if (proposal.getSponsor() != null) {
			proposal.setSponsorName(commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
		}
		if (proposal.getPrimeSponsor() != null) {
			proposal.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(proposal.getPrimeSponsor().getSponsorCode(), proposal.getPrimeSponsor().getSponsorName(), proposal.getPrimeSponsor().getAcronym()));
		}
	}

	@Override
	public void loadProposalUserFullNames(Proposal proposal) {
		if (proposal.getCreateUser() != null) {
			proposal.setCreateUserFullName(personDao.getUserFullNameByUserName(proposal.getCreateUser()));
		}
		if (proposal.getUpdateUser() != null) {
			proposal.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(proposal.getUpdateUser()));
		}		
		if (proposal.getSubmitUser() != null) {
			proposal.setSubmitUserFullName(personDao.getUserFullNameByUserName(proposal.getSubmitUser()));
		}
	}

	private void setProposalUpdateUser(ProposalVO proposalVO) {
		Proposal proposal = proposalDao.fetchProposalById(proposalVO.getProposalId());
		proposal.setUpdateUser(proposalVO.getUpdateUser());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalDao.saveOrUpdateProposal(proposal);
	}

	private void setNotificationRecipients(String recipient, String recipientType,
			Set<NotificationRecipient> dynamicEmailrecipients) {
		NotificationRecipient notificationRecipient = new NotificationRecipient();
		notificationRecipient.setEmailAddress(recipient);
		notificationRecipient.setRecipientType(recipientType);
		dynamicEmailrecipients.add(notificationRecipient);
	}

	@Override
	public void saveProposalPersonRole(String personId, Integer proposalId, String updateUser, Integer roleId) {
		ProposalPersonRoles personRole = new ProposalPersonRoles();
		personRole.setPersonId(personId);
		personRole.setProposalId(proposalId);
		personRole.setPerson(personDao.getPersonDetailById(personId));
		personRole.setRoleId(roleId);
		personRole.setRole(rolesManagementDao.getRoleInformation(roleId));
		personRole.setUpdateUser(updateUser);
		personRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalDao.saveProposalPersonRole(personRole);
	}

	private void addPrincipalInvestigator(ProposalVO vo) {
		String personId = vo.getPiPersonId();
		ProposalPerson proposalPerson = new ProposalPerson();
		if (vo.getIsNonEmployee()) {
			Rolodex rolodex = rolodexDao.getRolodexDetailById(Integer.parseInt(personId));
			proposalPerson.setRolodexId(rolodex.getRolodexId());
			proposalPerson.setFullName(rolodex.getFullName());
			proposalPerson.setEmailAddress(rolodex.getEmailAddress());
			proposalPerson.setDesignation(rolodex.getDesignation());
		} else {
			Person person = personDao.getPersonDetailById(personId);
			proposalPerson.setPersonId(person.getPersonId());
			proposalPerson.setFullName(person.getFullName());
			proposalPerson.setEmailAddress(person.getEmailAddress());
			proposalPerson.setDesignation(person.getPrimaryTitle());
		}
		Proposal proposal = vo.getProposal();
		Integer proposalId = proposal.getProposalId();
		proposalPerson.setIsPi(true);
		proposalPerson.setPercentageOfEffort(Constants.PERCENTAGE_OF_EFFORT); 
		proposalPerson.setPersonRoleId(Constants.PI_ROLE_CODE);
		proposalPerson.setProposalPersonRole(proposalDao.fetchProposalPersonRoles(Constants.PI_ROLE_CODE));
		proposalPerson.setProposalId(proposalId);
		ProposalPersonUnit proposalPersonUnit = new ProposalPersonUnit();
		proposalPersonUnit.setUnitNumber(proposal.getHomeUnitNumber());
		proposalPersonUnit.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalPersonUnit.setUpdateUser(vo.getUpdateUser());
		proposalPersonUnit.setUnit(commonDao.getUnitByUnitNumber(proposal.getHomeUnitNumber()));
		proposalPersonUnit.setLeadUnit(true);
		proposalPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalPerson.setUpdateUser(vo.getUpdateUser());
		proposalPersonUnit.setProposalPerson(proposalPerson);
		proposalPerson.getUnits().add(proposalPersonUnit);
		proposalModuleDao.saveOrUpdateProposalPerson(proposalPerson);
		List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId);
		proposalPersons.stream().filter(persons ->(proposalPerson.getPersonId() != null||proposalPerson.getRolodexId() != null) && (Boolean.TRUE.equals(proposalPerson.getIsPi())))
				.forEach(person -> person.setTrainingStatus(proposalDao.isProposalPersonTrainingCompleted(person.getPersonId() , proposalId)));
		vo.setProposalPersons(proposalPersons);
		proposalPersons.forEach(proposalperson -> {
			copyPersonDegree(proposalperson.getProposalPersonId(),proposalperson.getPersonId());
		});
		assignDerivedRolesForPI(proposalPerson, proposalId);
		proposal.setProposalPersons(proposalPersons);
	}

	@SuppressWarnings("unused")
	private void setGrantManagerAndAdminEmailRecipients(Integer proposalId, Set<NotificationRecipient> dynamicEmailrecipients) {
		List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId);
		for (ProposalReview proposalReview : proposalReviews) {
			if (proposalReview.getRoleId().equals(Constants.PROPOSAL_GRANT_MANAGER_ROLE_TYPE_CODE) || proposalReview.getRoleId().equals(Constants.PROPOSAL_GRANT_ADMIN_ROLE_TYPE_CODE)) {
				setNotificationRecipients(proposalReview.getReviewerEmail(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
			}
		}
	}

	@Override
	public String proposalInvitation(EmailServiceVO emailServiceVO) {
		try {
			int limit = 0;
			int mailGroupRecipiantLimit = emailServiceVO.getRecipients().size() / 50;
			mailGroupRecipiantLimit = (int) Math.ceil(mailGroupRecipiantLimit / 100.0);
			while (limit <= mailGroupRecipiantLimit) {
				emailServiceVO.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
				emailServiceVO.setSubModuleCode(Constants.DEV_PROPOSAL_SUBMODULE_CODE.toString());
				emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				emailServiceVO = emailService.sendEmail(emailServiceVO);
				limit++;
			}
		} catch (Exception e) {
			logger.error("exception in proposalInvitation: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON("success");
	}

	@Override
	public String updateAllAttachmentStatus(ProposalVO vo) {
		List<ProposalAttachment> proposalAttachments = getProposalAttachmentsByProposalId(vo.getProposalId());
		for (ProposalAttachment proposalAttachment : proposalAttachments) {
			proposalAttachment.setNarrativeStatusCode(vo.getNarrativeStatusCode());
			proposalAttachment.setNarrativeStatus(commonDao.getNarrativeStatusByCode(vo.getNarrativeStatusCode()));
			proposalAttachment.setUpdateUser(vo.getUserName());
			proposalAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
		}
		setProposalUpdateUser(vo);
		return commonDao.convertObjectToJSON(getProposalAttachmentsByProposalId(vo.getProposalId()));
	}

	@Override
	public String updateProposalStatusAsInactive(ProposalVO vo) {
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_INACTIVE);
		proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_INACTIVE));
		proposal.setUpdateUser(vo.getUpdateUser());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		loadProposalUserFullNames(proposal);
		proposalDao.saveOrUpdateProposal(proposal);
		vo.setProposal(proposal);
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposal.getProposalId().toString(),Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			vo.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				vo.setWorkflowList(workFlows);
			}
		}
		sendProposalNotification(vo, Constants.PROPOSAL_DEACTIVATE_NOTIFICATION_CODE, new HashSet<>());
		inboxDao.markAsExpiredFromActionList(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId().toString(), null, null);
		return commonDao.convertObjectToJSON(vo);
	}

	public String saveOrUpdateProposalMileStone(ProposalVO proposalVO) {
		proposalModuleDao.saveOrUpdateProposalMileStone(proposalVO.getProposalMileStone());
		setProposalUpdateUser(proposalVO);
		return commonDao.convertObjectToJSON(proposalModuleDao.fetchProposalMileStonesBasedOnProposalId(proposalVO.getProposalId()));
	}

	@Override
	public String deleteProposalMileStone(ProposalVO proposalVO) {
		proposalModuleDao.deleteProposalMileStone(proposalModuleDao.fetchProposalMileStone(proposalVO.getProposalMileStoneId()));
		setProposalUpdateUser(proposalVO);
		return commonDao.convertObjectToJSON(proposalModuleDao.fetchProposalMileStonesBasedOnProposalId(proposalVO.getProposalId()));
	}

	@Override
	public void saveKPIFromGrantCall(Integer proposalId, Integer grantCallId, String updateUser) {
		List<ProposalKPI> proposalKPIs = proposalDao.fetchAllProposalKPI(proposalId);
		List<GrantCallKPI> grantCallKPIs = grantCallKPIDao.fetchKPIByGrantCallId(grantCallId);
		if (grantCallKPIs != null && !grantCallKPIs.isEmpty() && (proposalKPIs == null || proposalKPIs.isEmpty())) {
			for (GrantCallKPI grantCallKPI : grantCallKPIs) {
				List<ProposalKPICriteria> criterias = new ArrayList<>();
				ProposalKPI proposalKPI = new ProposalKPI();
				proposalKPI.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				proposalKPI.setUpdateUser(updateUser);
				proposalKPI.setProposalId(proposalId);
				proposalKPI.setKpiTypeCode(grantCallKPI.getKpiTypeCode());
				proposalKPI.setKpiType(grantCallKPI.getKpiType());
				proposalDao.saveOrUpdateProposalKPI(proposalKPI);
				for (GrantCallKPICriteria grantCallKPICriteria : grantCallKPI.getGrantCallKpiCriterias()) {
					ProposalKPICriteria proposalKPICriteria = new ProposalKPICriteria();
					proposalKPICriteria.setProposalKpiId(proposalKPI.getProposalKpiId());
					proposalKPICriteria.setKpiCriteriaTypeCode(grantCallKPICriteria.getKpiCriteriaTypeCode());
					proposalKPICriteria.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					proposalKPICriteria.setUpdateUser(grantCallKPICriteria.getUpdateUser());
					proposalKPICriteria.setKpiCriteriaType(grantCallKPICriteria.getKpiCriteriaType());	
					criterias.add(proposalDao.saveOrUpdateProposalKPICriterias(proposalKPICriteria));
				}
				proposalKPI.setProposalKPICriterias(criterias);
			}
		}
	}

	@Override
	public String saveOrUpdateProposalKPI(ProposalVO vo) {
		List<ProposalKPI> proposalKPIs = vo.getProposalKpis();
		List<ProposalKPICriteria> proposalKPICriterias= new ArrayList<>();
		for(ProposalKPI proposalKPI : proposalKPIs) {
			for(ProposalKPICriteria proposalKPICriteria : proposalKPI.getProposalKPICriterias()) {
				proposalKPICriteria.setKpiCriteriaTypeCode(proposalKPICriteria.getKpiCriteriaTypeCode());
				proposalKPICriteria.setTarget(proposalKPICriteria.getTarget());
				proposalKPICriteria.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				proposalKPICriteria.setUpdateUser(proposalKPICriteria.getUpdateUser());
				proposalKPICriterias.add(proposalKPICriteria);
			}
			proposalKPI.setProposalKPICriterias(proposalKPICriterias);
			proposalKPI.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			proposalKPI.setUpdateUser(proposalKPI.getUpdateUser());
			proposalKPI.setProposalId(proposalKPI.getProposalId());
			proposalKPI.setKpiTypeCode(proposalKPI.getKpiTypeCode());
			proposalDao.saveOrUpdateProposalKPI(proposalKPI);
		}
		vo.setProposalKpis(proposalDao.fetchAllProposalKPI(vo.getProposalId()));
		return commonDao.convertObjectToJSON(vo);	
	}

	@SuppressWarnings("unused")
	private void saveProposalBeforeSubmit(ProposalVO vo) {
		Proposal proposal = vo.getProposal();
		proposal.setStartDate(proposal.getStartDate());
		proposal.setEndDate(proposal.getEndDate());
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		Integer grantCallId = proposal.getGrantCallId();
		if (grantCallId != null) {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallId);
			proposal.setGrantCallName(grantCall.getGrantCallName());
			proposal.setGrantCallClosingDate(grantCall.getClosingDate());
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
				saveKPIFromGrantCall(vo.getProposalId(), grantCallId, vo.getUpdateUser());
				vo.setProposalKpis(proposalDao.fetchAllProposalKPI(vo.getProposalId()));
			}
			vo.setGrantCall(prepareGrantCallData(grantCall));
		}
		vo.setProposal(proposal);
	}

	@Override
	public String assignAggregatorRoleToPI(ProposalVO vo) {
		String personId = vo.getPersonId();
		Integer proposalId = vo.getProposalId();
		logger.info("personId : {}",  personId);
		logger.info(PROPOSAL_ID, proposalId);
		logger.info("updateUser : {}", vo.getUpdateUser());
		if (!vo.getIsNonEmployee()) {
//			assignDerivedRolesForPI(setProposalPersonObject(personId, vo.getUpdateUser()), proposalId);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchScoringCriteriaByProposal(ProposalVO vo) {
		Integer workflowDetailId = vo.getWorkflowDetailId();
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(vo.getPersonId(), Constants.VIEW_PRIVATE_COMMENTS_RIGHT, proposalDao.fetchProposalLeadUnitNumberByProposalId(vo.getProposalId()));
		List<String> rights = new ArrayList<>();
		rights.add(Constants.VIEW_PRIVATE_COMMENTS_RIGHT);
		rights.add(Constants.MAINTAIN_PRIVATE_COMMENTS);
		Boolean isPrivateCommentViewer = proposalDao.isPersonHasRightInProposal(vo.getPersonId(), rights, vo.getProposalId());
		if (workflowDetailId == null) {
			workflowDetailId = workflowDao.fetchWorkflowDetailId(vo.getProposalId(), vo.getPersonId());
			vo.setIsPersonCanScore(proposalDao.fetchPersonCanScore(vo.getProposalId(), vo.getPersonId(), workflowDetailId));
		}
		List<WorkflowReviewerScore> workflowReviewerScoreDetails = proposalDao.fetchAllWorkflowReviewerDetails(workflowDetailId);
		List<WorkflowReviewerScore> workflowReviewerScores = new ArrayList<>();
		for (WorkflowReviewerScore workflowReviewer : workflowReviewerScoreDetails) {
			WorkflowReviewerScore workflowReviewerScore = new WorkflowReviewerScore();
			workflowReviewerScore.setWorkflowReviewerScoreId(workflowReviewer.getWorkflowReviewerScoreId());
			workflowReviewerScore.setWorkflowDetailId(workflowReviewer.getWorkflowDetailId());
			workflowReviewerScore.setDescription(workflowReviewer.getScoringCriteria().getDescription());
			workflowReviewerScore.setScoringCriteria(workflowReviewer.getScoringCriteria());
			workflowReviewerScore.setScoringCriteriaTypeCode(workflowReviewer.getScoringCriteriaTypeCode());
			workflowReviewerScore.setScore(workflowReviewer.getScore());
			workflowReviewerScore.setUpdateTimeStamp(workflowReviewer.getUpdateTimeStamp());
			workflowReviewerScore.setUpdateUser(workflowReviewer.getUpdateUser());
			workflowReviewerScore.setUpdatedUserFullName(personDao.getUserFullNameByUserName(workflowReviewer.getUpdateUser()));
			List<WorkflowReviewerComment> workflowReviewerComments = new ArrayList<>();
			workflowReviewer.getWorkflowReviewerComments().forEach(workflowReviewerComment -> {
				if (Boolean.TRUE.equals(isPrivateCommentViewer)||Boolean.TRUE.equals(isPersonHasPermission)
						|| Boolean.FALSE.equals(workflowReviewerComment.getIsPrivate())
						|| (vo.getUserName().equals(workflowReviewerComment.getUpdateUser()))) {
					workflowReviewerComments.add(workflowReviewerComment);
				}
			});
			workflowReviewerScore.setWorkflowReviewerComments(workflowReviewerComments);
			workflowReviewerScores.add(workflowReviewerScore);
		}
		List<GrantCallScoringCriteria> grantCallScoringCriterias = grantCallScoringDao.fetchScoringCriteriaGrantCallId(vo.getGrantCallId());
		for (GrantCallScoringCriteria grantCallScoringCriteria : grantCallScoringCriterias) {
			if (!checkForReviewExist(workflowReviewerScores, grantCallScoringCriteria)) {
				WorkflowReviewerScore workflowReviewerScore = new WorkflowReviewerScore();
				workflowReviewerScore.setWorkflowDetailId(workflowDetailId);
				workflowReviewerScore.setDescription(grantCallScoringCriteria.getScoringCriteria().getDescription());
				workflowReviewerScore.setScoringCriteria(grantCallScoringCriteria.getScoringCriteria());
				workflowReviewerScore.setScoringCriteriaTypeCode(grantCallScoringCriteria.getScoringCriteriaTypeCode());
				workflowReviewerScore.setUpdateTimeStamp(grantCallScoringCriteria.getUpdateTimestamp());
				workflowReviewerScore.setUpdateUser(vo.getUserName());
				workflowReviewerScore.setWorkflowReviewerComments(new ArrayList<>());
				workflowReviewerScores.add(workflowReviewerScore);
			}
		}
		vo.setWorkflowReviewerScores(workflowReviewerScores.stream().sorted(Comparator.comparing(score ->score.getScoringCriteria().getDescription())).collect(Collectors.toList()));
		return commonDao.convertObjectToJSON(vo);
	}

	private boolean checkForReviewExist(List<WorkflowReviewerScore> workflowReviewerScores, GrantCallScoringCriteria grantCallScoringCriteria) {
		for(WorkflowReviewerScore workflowReviewerScore :workflowReviewerScores) {
			if (workflowReviewerScore.getScoringCriteriaTypeCode().equals(grantCallScoringCriteria.getScoringCriteriaTypeCode())) {
				return true;
			}
		}
		return false;
	}

	private WorkflowReviewerAttachment addNewWorkflowReviewerAttachment(WorkflowReviewerAttachment attachment, MultipartFile file) {
		try {
			if (attachment.getFileName().equals(file.getOriginalFilename())) {
				attachment.setMimeType(file.getContentType());
				FileData fileData = new FileData();
				fileData.setAttachment(file.getBytes());
				fileData = commonDao.saveFileData(fileData);
				attachment.setFileDataId(fileData.getFileDataId());
				attachment.setAttachment(file.getBytes());
			}
		} catch (Exception e) {
			logger.error("exception in addNewWorkflowReviewerAttachment: {} ", e.getMessage());
		}
		return attachment;
	}

	@Override
	public ResponseEntity<byte[]> downloadWorkflowReviewerAttachment(Integer workflowReviewerAttmntsId) {
		WorkflowReviewerAttachment attachment = proposalDao.fetchWorkflowReviewerAttachmentById(workflowReviewerAttmntsId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("exception in downloadWorkflowReviewerAttachment: {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public void fetchAndSaveProposalEvaluationPanels(ProposalVO vo) {
		Integer proposalId = vo.getProposal().getProposalId();
		List<ProposalEvaluationPanel> proposalEvaluationPanels = new ArrayList<>();
		if (proposalId != null) {
			proposalEvaluationPanels = evaluationDao.fetchProposalEvaluationPanelsByProposalId(proposalId);
		}
		if (proposalEvaluationPanels == null || proposalEvaluationPanels.isEmpty()) {
			Integer grantCallId = vo.getProposal().getGrantCallId();
			if (grantCallId != null) {
				List<GrantCallEvaluationPanel> grantcallEvaluationPanels = grantCallEvaluationPanelDao.fetchEvaluationPanelByGrantCallId(grantCallId);
				if (grantcallEvaluationPanels != null && !grantcallEvaluationPanels.isEmpty()) {
					for (GrantCallEvaluationPanel grantEvaluationPanel : grantcallEvaluationPanels) {
						if (grantEvaluationPanel.getIsMainPanel().equalsIgnoreCase("N")) {
							List<ProposalEvaluationPanelPersons> proposalEvaluationPanelPersons = new ArrayList<>();
							ProposalEvaluationPanel proposalEvaluationPanel = new ProposalEvaluationPanel();
							proposalEvaluationPanel.setProposalId(proposalId);
							proposalEvaluationPanel.setMapId(grantEvaluationPanel.getMapId());
							proposalEvaluationPanel.setUpdateUser(vo.getUpdateUser());
							List<WorkflowMapDetail> workflowMapDetails = grantEvaluationPanel.getWorkflowMap().getWorkflowMapDetails();
							if (workflowMapDetails != null && !workflowMapDetails.isEmpty()) {
								for (WorkflowMapDetail workflowMapDetail : workflowMapDetails) {
									ProposalEvaluationPanelPersons proposalEvaluationPanelPerson = new ProposalEvaluationPanelPersons();
									Person person = personDao.getPersonDetailById(workflowMapDetail.getApproverPersonId());
									proposalEvaluationPanelPerson.setApproverNumber(workflowMapDetail.getApproverNumber());
									proposalEvaluationPanelPerson.setApproverPersonId(person.getPersonId());
									proposalEvaluationPanelPerson.setApproverPersonName(person.getFullName());
									proposalEvaluationPanelPerson.setUpdateUser(vo.getUpdateUser());
									proposalEvaluationPanelPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
									proposalEvaluationPanelPerson.setProposalEvaluationPanel(proposalEvaluationPanel);
									proposalEvaluationPanelPersons.add(proposalEvaluationPanelPerson);
								}
								proposalEvaluationPanel.setProposalEvaluationPanelPersons(proposalEvaluationPanelPersons);
							}
							evaluationDao.saveProposalEvaluationPanelDetails(proposalEvaluationPanel);
						}
					}
				}
			}
		}
	}

	public void setProposalAttachmentObject(ProposalAttachment newAttachment,List<ProposalAttachment> attachments, MultipartFile file, Integer versionNumber, Integer documentId, Integer proposalId) {
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
			for (ProposalAttachment attachment : attachments) {
				if (attachment.getAttachmentId() != null
						&& attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
					attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
					attachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
					versionNumber = attachment.getVersionNumber();
					documentId = attachment.getDocumentId();
					ProposalAttachment proposalAttachment = addNewProposalAttachment(newAttachment, file, fileName, versionNumber, documentId, proposalId, replaceFileName);
					proposalAttachment.setProposalId(proposalId);
					proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
				}
			}
		} else {
			if (newAttachment.getFileName().equals(fileName)) {
				documentId = documentId + 1;
				ProposalAttachment proposalAttachment = addNewProposalAttachment(newAttachment, file, fileName, versionNumber, documentId, proposalId, replaceFileName);
				proposalModuleDao.saveOrUpdateProposalAttachment(proposalAttachment);
			}
		}
	}

	@Override
	public String saveOrUpdateProposalComment(MultipartFile[] files, String formDataJSON) {
		ProposalVO proposalVO = new ProposalVO();
		List<ProposalCommentAttachment> proposalCommentAttachments = new ArrayList<>();
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			ProposalComment comment = proposalDao.saveOrUpdateProposalComment(proposalVO.getComment());
			proposalVO.setProposalId(comment.getProposalId());
			comment.setFullName(AuthenticatedUser.getLoginUserFullName());
			List<ProposalCommentAttachment> commentAttachments = proposalVO.getNewCommentAttachments();
			for (int i = 0; i < files.length; i++) {
				for (ProposalCommentAttachment newAttachment : commentAttachments) {
					if (files[i].getOriginalFilename().equals(newAttachment.getFileName())) {
						ProposalCommentAttachment proposalCommentAttachment = new ProposalCommentAttachment();
						proposalCommentAttachment.setProposalCommentId(comment.getProposalCommentId());
						proposalCommentAttachment.setFileName(newAttachment.getFileName());
						proposalCommentAttachment.setMimeType(newAttachment.getMimeType());
						FileData fileData = new FileData();
						fileData.setAttachment(files[i].getBytes());
						fileData = commonDao.saveFileData(fileData);
						proposalCommentAttachment.setFileDataId(fileData.getFileDataId());
						proposalCommentAttachment.setAttachment(files[i].getBytes());
						proposalDao.saveOrUpdateProposalCommentAttachment(proposalCommentAttachment);
						proposalCommentAttachments.add(proposalCommentAttachment);
					}
				}
			}
			comment.setProposalCommentAttachments(proposalCommentAttachments);
			proposalVO.setProposalComment(comment);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateProposalcomment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(proposalVO.getComment());
	}

	@Override
	public ResponseEntity<byte[]> downloadProposalCommentAttachment(Integer attachmentId) {
		ProposalCommentAttachment attachment = proposalDao.getProposalCommentAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
		try {
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("Exception in downloadProposalCommentAttachment : {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String fetchProposalComments(ProposalVO vo) {
		vo.setCommentType(commonDao.fetchCommentTypes(Constants.DEV_PROPOSAL_MODULE_CODE));
		vo.setProposalComments(prepareProposalComment(vo.getProposalId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<ProposalComment> prepareProposalComment(Integer proposalId) {
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.VIEW_PRIVATE_COMMENTS_RIGHT);
		Boolean isPersonHasPermission = commonDao.checkPersonHasRightInModule(Constants.DEV_PROPOSAL_MODULE_CODE, proposalId, rightNames, AuthenticatedUser.getLoginPersonId());
		List<ProposalComment> proposalComments  = proposalDao.fetchProposalCommentsByParams(proposalId, AuthenticatedUser.getLoginUserName(), isPersonHasPermission);
		getFullNameByUserName(proposalComments);
		return proposalComments;
	}

	private void getFullNameByUserName(List<ProposalComment> proposalComments) {
		Set<String> userName = proposalComments.stream().map(ProposalComment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			proposalComments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}	
	}

	@Override
	public String unlinkGrantCallFromProposal(ProposalVO vo) {
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		proposal.setGrantCallId(null);
		proposal.setGrantCallName(null);
		proposal.setGrantCallClosingDate(null);
		List<ProposalPersonRoles> proposalPersons = proposalLookUpDao.fetchProposalPersonRoles(vo.getProposalId(), Constants.REVIEW_PROPOSAL_ROLE_ID);
		if ((proposalPersons != null && !proposalPersons.isEmpty())) {
			rolesManagementDao.deleteProposalPersons(proposalPersons);
		}
		List<ProposalKPI> proposalKPIs = proposalDao.fetchAllProposalKPI(vo.getProposalId());
		if (proposalKPIs != null && !proposalKPIs.isEmpty()) {
			for (ProposalKPI proposalKPI : proposalKPIs) {
				proposalDao.deleteProposalKPI(proposalKPI);
			}
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
			List<ProposalEvaluationPanel> proposalEvaluationPanels = evaluationDao.fetchProposalEvaluationPanelsByProposalId(vo.getProposalId());
			if (proposalEvaluationPanels != null && !proposalEvaluationPanels.isEmpty()) {
				evaluationDao.deleteProposalEvaluationPanels(proposalEvaluationPanels);
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteAllProposalEvaluationPanel(Integer proposalId) {
		List<ProposalEvaluationPanel> proposalEvaluationPanels = evaluationDao.fetchProposalEvaluationPanelsByProposalId(proposalId);
		if (proposalEvaluationPanels != null && !proposalEvaluationPanels.isEmpty()) {
			evaluationDao.deleteProposalEvaluationPanels(proposalEvaluationPanels);
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			setProposalUpdateUser(proposalVO);
		}
	}

	private void assignDerivedRolesForCOI(ProposalPerson proposalPerson, Integer proposalId) {
		List<ProposalPersonRoles> proposalPersonRoles = proposalDao.fetchProposalPersonRolesByParams(proposalPerson.getPersonId(), proposalId, null);
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCOI(Constants.DEV_PROPOSAL_MODULE_CODE);
		if (proposalPerson.getPersonId() != null && proposalPerson.getPersonRoleId().equals((Constants.COI_ROLE_CODE))) {
			for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
				if (ifRoleAlreadyExist(moduleDerivedRole.getRoleId(), proposalPersonRoles).equals(Boolean.FALSE)) {
					saveProposalPersonRole(proposalPerson.getPersonId(), proposalId, proposalPerson.getUpdateUser(), moduleDerivedRole.getRoleId());
				}
			}
		}
	}

	@Override
	public void assignDerivedRolesForPI(ProposalPerson proposalPerson, Integer proposalId) {
		List<ProposalPersonRoles> proposalPersonRoles = proposalDao.fetchProposalPersonRolesByParams(proposalPerson.getPersonId(), proposalId, null);
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForPI(Constants.DEV_PROPOSAL_MODULE_CODE);
		if (proposalPerson.getPersonId() != null && (Boolean.TRUE.equals(proposalPerson.getIsPi())|| proposalPerson.getPersonRoleId().equals((Constants.PI_ROLE_CODE)))) {
			for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
				if (ifRoleAlreadyExist(moduleDerivedRole.getRoleId(), proposalPersonRoles).equals(Boolean.FALSE)) {
					saveProposalPersonRole(proposalPerson.getPersonId(), proposalId, proposalPerson.getUpdateUser(), moduleDerivedRole.getRoleId());
				}
			}
		}
	}

	@Override
	public void assignDerivedRolesForCreator(Proposal proposal, List<ModuleDerivedRoles> derivedRoles) {
		String personId = personDao.getPersonIdByUserName(proposal.getCreateUser());
		List<ProposalPersonRoles> proposalPersonRoles = proposalDao.fetchProposalPersonRolesByParams(personId, proposal.getProposalId(), null);
		if (personId != null) {
			for (ModuleDerivedRoles moduleDerivedRole : derivedRoles) {
				if (ifRoleAlreadyExist(moduleDerivedRole.getRoleId(), proposalPersonRoles).equals(Boolean.FALSE)) {
					saveProposalPersonRole(personId, proposal.getProposalId(), proposal.getUpdateUser(), moduleDerivedRole.getRoleId());
				}
			}
		}
	}

	private Boolean ifRoleAlreadyExist(Integer roleId, List<ProposalPersonRoles> proposalPersonRoles) {
		for (ProposalPersonRoles proposalPersonRole : proposalPersonRoles) {
			if (proposalPersonRole.getRoleId().equals(roleId)) {
				return true;
			}
		}
		return false;
	}

	private void assignDerivedRolesForPerson(Integer proposalId, ProposalPerson proposalPerson) {
		assignDerivedRolesForPI(proposalPerson, proposalId);
		assignDerivedRolesForCOI(proposalPerson, proposalId);
	}

	@Override
	public String addProposalAttachmentForWaf(ProposalVO proposalVO) {
		try {
			MultipartFile multipartFile = null;
			String contentType = null;
			ProposalAttachment newAttachment = proposalVO.getProposalAttachment();
			String name = proposalVO.getFileName();
			String splicedFile = proposalVO.getFileContent();
			Integer remaining = proposalVO.getRemaining();
			Integer length = proposalVO.getLength();
			Integer moduleItemKey = proposalVO.getProposalId();
			String userId = proposalVO.getPersonId();
			String timestamp = proposalVO.getFileTimestamp();
			if (splicedFile != null) {
				contentType = proposalVO.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if(multipartFile != null && !multipartFile.isEmpty()) {
			List<ProposalAttachment> attachments = proposalModuleDao.fetchProposalAttachmentBasedOnProposalId(proposalVO.getProposalId());
			Integer documentId = getDocumentIdForAttachment(attachments);
			Integer versionNumber = 0;
			setProposalAttachmentObject(newAttachment, attachments, multipartFile, versionNumber, documentId, moduleItemKey);
			proposalVO.setProposalAttachments(getProposalAttachmentsByProposalId(moduleItemKey));
			setProposalUpdateUser(proposalVO);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private Integer getDocumentIdForAttachment(List<ProposalAttachment> attachments) {
		Integer documentId = 0;
		if (attachments != null && !attachments.isEmpty()) {
			Collections.sort(attachments,
					(attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
							: attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
			documentId = attachments.get(0).getDocumentId();
		}
		return documentId;
	}

	@Override
	public String addProposalPersonAttachmentForWaf(ProposalVO proposalVO) {
		List<ProposalPersonAttachment> proposalPersonAttachments = new ArrayList<ProposalPersonAttachment>();
		try {
			MultipartFile multipartFile = null;
			String contentType = null;
			List<ProposalPersonAttachment> newAttachments = proposalVO.getNewPersonAttachments();
			String splicedFile = proposalVO.getFileContent();
			Integer remaining = proposalVO.getRemaining();
			Integer length = proposalVO.getLength();
			String userId = proposalVO.getPersonId();
			String timestamp = proposalVO.getFileTimestamp();
			if (newAttachments != null && !newAttachments.isEmpty() && splicedFile == null) {
				for (ProposalPersonAttachment newAttachment : newAttachments) {
					if (newAttachment.getReplaceAttachmentId() != null) {
						proposalDao.deleteProposalPersonAttachment(newAttachment.getReplaceAttachmentId());
					}
				}
			} else {
				String name = proposalVO.getFileName();
				if (splicedFile != null) {
					contentType = proposalVO.getContentType();
					multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
				}
				if (newAttachments != null && !newAttachments.isEmpty()) {
					for (ProposalPersonAttachment newAttachment : newAttachments) {
						if (multipartFile != null && !multipartFile.isEmpty()) {
							if (newAttachment.getReplaceAttachmentId() != null) {
								proposalDao.deleteProposalPersonAttachment(newAttachment.getReplaceAttachmentId());
							}
							ProposalPersonAttachment proposalPersonAttachment = new ProposalPersonAttachment();
							proposalPersonAttachment.setFileName(newAttachment.getFileName());
							proposalPersonAttachment.setDescription(newAttachment.getDescription());
							proposalPersonAttachment.setMimeType(multipartFile.getContentType());
							FileData fileData = new FileData();
							fileData.setAttachment(multipartFile.getBytes());
							fileData = commonDao.saveFileData(fileData);
							proposalPersonAttachment.setFileDataId(fileData.getFileDataId());
							proposalPersonAttachment.setUpdateUser(newAttachment.getUpdateUser());
							proposalPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
							proposalPersonAttachment.setAttachment(multipartFile.getBytes());
							proposalPersonAttachments.add(proposalPersonAttachment);
						}
					}
				}
			}
			proposalVO.setNewPersonAttachments(proposalPersonAttachments);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	public PrintVO setPrintParameters(ProposalVO vo) {
		PrintVO printVO = new PrintVO();
		printVO.setProposalId(vo.getProposalId());
		printVO.setBudgetId(vo.getBudgetId());
		return printVO;
	}

	private void saveOrUpdateWorkflowScoreDetail(WorkflowReviewerScore workflowReviewerScore, Map<String, MultipartFile> files) {
		List<WorkflowReviewerComment> reviewerComments = workflowReviewerScore.getWorkflowReviewerComments();
		for (WorkflowReviewerComment workflowReviewerComment : reviewerComments) {
			List<WorkflowReviewerAttachment> newAttachments = workflowReviewerComment.getWorkflowReviewerAttachments();
			for (WorkflowReviewerAttachment newAttachment : newAttachments) {
				if (files != null) {
					for (Entry<String, MultipartFile> entry : files.entrySet()) {
						String attachmentKey = entry.getKey();
						String fileKey = new StringBuilder(workflowReviewerScore.getScoringCriteriaTypeCode()).append("_").append(newAttachment.getFileName()).toString();
						if (fileKey.equalsIgnoreCase(attachmentKey)) {
							MultipartFile file = entry.getValue();
							addNewWorkflowReviewerAttachment(newAttachment, file);
						}
					}
				}
			}
		}
		proposalDao.saveOrUpdateWorkflowScoreDetail(workflowReviewerScore);
	}

	@Override
	public String deleteWorkflowScoreComments(Integer workflowReviewerCommentsId) {
		try {
			proposalDao.deleteWorkflowScoreComments(workflowReviewerCommentsId);
			return commonDao.convertObjectToJSON("Success");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("Failure");
		}
	}

	@Override
	public String deleteWorkflowReviewerAttachment(Integer workflowReviewerAttmntsId) {
		try {
			proposalDao.deleteWorkflowReviewerAttachment(workflowReviewerAttmntsId);
			return commonDao.convertObjectToJSON("Success");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("Failure");
		}
	}

	@Override
	public String saveOrUpdateWorkflowScoreDetails(String formDataJson, MultipartHttpServletRequest request) {
		ProposalVO vo = new ProposalVO();
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJson, ProposalVO.class);
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			vo.setLoginPersonUnitNumber(AuthenticatedUser.getLoginPersonUnit());
			vo.setUserName(AuthenticatedUser.getLoginUserName());
			List<WorkflowReviewerScore> workflowReviewerScores = vo.getWorkflowReviewerScores();
			for (WorkflowReviewerScore reviewerScore : workflowReviewerScores) {
				saveOrUpdateWorkflowScoreDetail(reviewerScore, request.getFileMap());
			}
			if(vo.getIsSubmit() != null && vo.getIsSubmit().equals("1")) {
				businessRuleService.endorseProposal(vo.getProposalId(), vo.getUserName(), Constants.DEV_PROPOSAL_MODULE_CODE.toString(), vo.getPersonId(), Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				return loadProposalById(vo.getProposalId(), vo.getPersonId(), vo.getUserName(), Boolean.FALSE);
			}
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateWorkflowScoreDetails: {} ", e.getMessage());
		}
		return fetchScoringCriteriaByProposal(vo);
	}

	@Override
	public String withdrawProposal(MultipartFile[] files, String formDataJson) {
		ProposalVO vo = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJson, ProposalVO.class);
			Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
			proposal.setUpdateUser(vo.getUpdateUser());
			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_WITHDRAW));
			proposal = proposalDao.saveOrUpdateProposal(proposal);
			vo.setProposal(proposal);
			withdrawProposal(files, vo, formDataJson);
		} catch (Exception e) {
			logger.error("error in withdrawAward :{}", e.getMessage());
		}
		return loadProposalById(vo.getProposalId(), AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), Boolean.FALSE);
	}

	private void withdrawProposal(MultipartFile[] files, ProposalVO vo, String formDataJson) {
		if (formDataJson != null) {
			businessRuleService.approveOrRejectWorkflow(files, formDataJson, Constants.DEV_PROPOSAL_MODULE_CODE.toString(), Constants.DEV_PROPOSAL_SUBMODULE_CODE.toString());
		}
	}

	@Override
	public String saveOrUpdateProposalOrganization(ProposalVO proposalVO) {
		ProposalOrganization proposalOrganization = proposalVO.getProposalOrganization();
		List<ProposalCongDistrict> proposalCongDistricts = proposalOrganization.getProposalCongDistricts() != null ? proposalOrganization.getProposalCongDistricts() : new ArrayList<>();
		proposalOrganization.setProposalCongDistricts(null);
		proposalDao.saveOrUpdateProposalOrganization(proposalOrganization);
		proposalCongDistricts.forEach(congDistrict -> {
			if(congDistrict.getCongDistrictCode() == null){
				CongressionalDistrict congressionalDistrict = congDistrict.getCongressionalDistrict();
				proposalDao.saveOrUpdateCongDistrict(congressionalDistrict);
				congDistrict.setCongDistrictCode(congressionalDistrict.getCongDistrictCode());
				congDistrict.setCongressionalDistrict(congressionalDistrict);
			}
			congDistrict.setProposalOrganization(proposalOrganization);
			proposalDao.saveOrUpdateProposalCongDistrict(congDistrict);
		});
		proposalOrganization.setProposalCongDistricts(proposalCongDistricts);
		proposalVO.setProposalOrganization(proposalOrganization);
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String saveOrUpdateOrganization(ProposalVO proposalVo) {
		Organization organization = proposalVo.getOrganization();
		proposalVo.setMessage("Organization Id already exists");
		proposalVo.setStatus(false);
		if(commonDao.checkIsOrganizationIdUnique(organization.getOrganizationId())){
			commonDao.saveOrUpdateOrganization(organization);
			proposalVo.setMessage("Organization added successfully");
			proposalVo.setStatus(true);
		}
		return commonDao.convertObjectToJSON(proposalVo);
	}

	@Override
	public String loadOrganizationDetails(String organizationId) {
		ProposalVO proposalVo = new ProposalVO();
		proposalVo.setOrganization(commonDao.loadOrganizationDetails(organizationId));
		return commonDao.convertObjectToJSON(proposalVo);
	}

	@Override
	public String saveOrUpdateCongDistrict(ProposalVO vo) {
		proposalDao.saveOrUpdateCongDistrict(vo.getCongressionalDistrict());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteProposalOrganization(Integer proposalOrganizationId) {
		proposalDao.deleteProposalOrganization(proposalOrganizationId);
		Map<String, String> result = new HashMap<>();
		result.put("message", "deleted");
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public String deleteProposalCongDistrict(Integer proposalCongDistrictId) {
		proposalDao.deleteProposalCongDistrict(proposalCongDistrictId);
		Map<String, String> result = new HashMap<>();
		result.put("message", "deleted");
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public List<CongressionalDistrict> findCongressionalDistricts(String searchString) {
		return commonDao.findCongressionalDistricts(searchString);
	}

	@Override
	public String checkProposalRights(ProposalVO vo) {
		Map<String, Boolean> result = new HashMap<>();
		if(vo.getPersonId() != null && vo.getUnitNumber() != null && vo.getRightName() != null) {		
			if(proposalDao.checkUnitNumberHasRight(vo.getPersonId(), vo.getUnitNumber(), vo.getRightName())) {				
				result.put("isRightExist", true);
				return commonDao.convertObjectToJSON(result);
			}	
		}
		result.put("isRightExist", false);
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public ProposalVO importProposalTemplate(MultipartFile[] files, String formDataJson) {
		ProposalVO vo = null;
		XSSFWorkbook workbook = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJson, ProposalVO.class);
			if (vo.getProposalId() != null && !files[0].isEmpty() && files[0] !=null) {
				vo.setProposal(proposalDao.fetchProposalById(vo.getProposalId()));
				Integer grantCallId = vo.getProposal().getGrantCallId();
			    workbook = (XSSFWorkbook) WorkbookFactory.create(files[0].getInputStream());
				XSSFSheet sheet = workbook.getSheetAt(0);
				commonService.addDetailsInHeader(workbook,sheet);
				Boolean isRowInserted = Boolean.FALSE;
				for (Row row : sheet) {
					if (row != null && Boolean.FALSE.equals(isRowInserted)) {
					for (Cell cell : row) {
						Cell nextCellValue = null;
						if (cell != null && !cell.getCellType().equals(CellType.NUMERIC) && cell.getStringCellValue().equals("Keywords")) {
							isRowInserted = Boolean.TRUE;
							Integer nextSection = 1;
							Integer i = cell.getRow().getRowNum() + 1;
							while (nextSection > 0) {
								Row nextRow = sheet.getRow(i);
								Cell keywordCell = nextRow.getCell(0);
								if (keywordCell != null && !keywordCell.getCellType().equals(CellType.NUMERIC) && !keywordCell.getStringCellValue().equals("") && !keywordCell.getStringCellValue().equals("Research Areas")) {
									String proposalKeywords = keywordCell.getStringCellValue();
									String[] keywords = proposalKeywords.split("[,;]+", 0);
									if (keywords.length > 0 && !keywords[0].equals("")) {
										deleteProposalKeyword(vo.getProposalId());
										for (String keyword : keywords) {
											saveProposalKeywordFromTemplate(keyword, vo);
										}
									}
								}
								if (keywordCell != null && !keywordCell.getCellType().equals(CellType.NUMERIC) && keywordCell.getStringCellValue().equals("Research Areas")) {
									nextSection--;
									Integer cellVlue = keywordCell.getColumnIndex();
									nextCellValue = nextRow.getCell(cellVlue);
								}
								i++;
							}
						}
					  if (nextCellValue != null && nextCellValue.getStringCellValue().equals("Research Areas")) {
						Integer nextSection = 1;
						Integer i = nextCellValue.getRowIndex();
						while (nextSection > 0) {
						  Row nextRow = sheet.getRow(i);
						  Cell nextCell = nextRow.getCell(0);
						  if (nextCell != null && !nextCell.getCellType().equals(CellType.NUMERIC) && nextCell.getStringCellValue() != null 
								  && nextCell.getStringCellValue().equals("Scientific Abstract")) {
							  Row abstractRow = sheet.getRow(i+1);
							  Cell abstractCell = abstractRow.getCell(0);
							  if (abstractCell != null && !abstractCell.getCellType().equals(CellType.NUMERIC) && abstractCell.getStringCellValue() != null && !abstractCell.getStringCellValue().equals("")) {
								 saveScientificAbstractData(abstractCell.getStringCellValue(), vo.getProposal());
						      }
						  }
						  if (nextCell != null && !nextCell.getCellType().equals(CellType.NUMERIC) && nextCell.getStringCellValue() != null
								 && nextCell.getStringCellValue().equals("A.  Research Milestones")) {
							    nextSection--;
								Integer cellVlue = nextCell.getColumnIndex();
								nextCellValue = nextRow.getCell(cellVlue);
						  }
						  i++;
					   }
					 }
					  if (nextCellValue != null && nextCellValue.getStringCellValue().equals("A.  Research Milestones")) {
							Integer nextSection = 1;
							Integer i = nextCellValue.getRowIndex();
							Integer count = 0;
							List<ProposalMileStone> proposalMileStones = new ArrayList<>();
							while (nextSection > 0) {
							 Row nextRow = sheet.getRow(i);
							 Cell milestone = nextRow.getCell(0);
							 ProposalMileStone proposalMileStone = new ProposalMileStone();
							 if (milestone != null && !milestone.getStringCellValue().equals("") && !milestone.getCellType().equals(CellType.NUMERIC) && !milestone.getStringCellValue().contains("A.  Research Milestones")
									&& !milestone.getStringCellValue().equals("B.  Technical Milestones")) {
									if(count == 0 && milestone.getStringCellValue().equals("Research Milestone")) {
										count++;
									} else {
										proposalMileStones = readProposalMilestone(milestone, proposalMileStone, nextRow, proposalMileStones);
									}
							 }
							 if (nextRow.getCell(0) != null && !nextRow.getCell(0).getCellType().equals(CellType.NUMERIC) && nextRow.getCell(0).getStringCellValue().equals("B.  Technical Milestones")) {
									nextSection--;
									nextCellValue = nextRow.getCell(0);
							 }
							 i++;
							 
						 }
						 saveProposalMilestone(proposalMileStones, vo.getProposalId());
					  } 
					  if (nextCellValue != null && nextCellValue.getStringCellValue().equals("B.  Technical Milestones")) {
							Integer nextSection = 1;
							Integer i = nextCellValue.getRowIndex();
							Integer count = 0;
							List<ProposalKPICriteria> proposalCriterias = new ArrayList<>();
							while (nextSection > 0) {
								Row kpiRow = sheet.getRow(i);
								Cell kpiCell = kpiRow.getCell(0);
								if (grantCallId != null && kpiCell != null && !kpiCell.getCellType().equals(CellType.NUMERIC) && !kpiCell.getStringCellValue().equals("") && kpiCell.getStringCellValue() != null 
										  && !kpiCell.getStringCellValue().equals("D.  Referees") && !kpiCell.getStringCellValue().equals("B.  Technical Milestones")) {
									if (count == 0 && kpiCell.getStringCellValue().equals("Category")) {
										count++;
									} else if(count > 0){
										readProposalKpi(kpiCell, proposalCriterias, kpiRow);
									} 
								 }
								 if (kpiCell != null && !kpiCell.getCellType().equals(CellType.NUMERIC) && kpiCell.getStringCellValue() != null
										 && kpiCell.getStringCellValue().equals("D.  Referees")) {
									    nextSection--;
										Integer cellVlue = kpiCell.getColumnIndex();
										nextCellValue = kpiRow.getCell(cellVlue);
								  }
							 i++;
						  }
						saveProposalKPi(proposalCriterias, vo);
					  }
					  if (nextCellValue != null && nextCellValue.getStringCellValue().equals("D.  Referees")) {
						  Integer nextSection = 1;
						  Integer count = 0;
						  Integer i = nextCellValue.getRowIndex();
						  List<ProposalSpecialReview> proposalSpecialReviews = new ArrayList<>();
							while (nextSection > 0) {
								Row ethicsRow = sheet.getRow(i);
								Cell ethicsCell = ethicsRow.getCell(0);
								 if (ethicsCell != null && !ethicsCell.getCellType().equals(CellType.NUMERIC) && ethicsCell.getStringCellValue() != null) {
									   if ((count == 0 && ethicsCell.getStringCellValue().equals("A.  Declaration of Ethics Approval"))
											|| ethicsCell.getStringCellValue().equals("Ethics category")) {
											count++;
										} else if(count > 1) {
											prepareSpecialReview(ethicsCell, vo, ethicsRow, proposalSpecialReviews);
									  }
								 }
								if (ethicsCell != null && !ethicsCell.getCellType().equals(CellType.NUMERIC) && ethicsCell.getStringCellValue() != null
										 && ethicsCell.getStringCellValue().equals("B.  Undertaking by Lead PI")) {
										    nextSection--;
											Integer cellVlue = ethicsCell.getColumnIndex();
											nextCellValue = ethicsRow.getCell(cellVlue);
								}
								i++;
							}
							saveProposalSpecialReview(proposalSpecialReviews, vo);
					  }
				  }
				}
			}
			workbook.close();
		  }
		} catch (Exception e) {
			logger.error("error in importKPITemplate {}" , e.getMessage());
		} finally {
			try {
				if(workbook != null) {
					workbook.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return vo;
	}

	private void saveProposalSpecialReview(List<ProposalSpecialReview> proposalSpecialReviews, ProposalVO vo) {
		if (proposalSpecialReviews != null && !proposalSpecialReviews.isEmpty()) {
			vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
			deleteAllProposalSpecialReview(vo.getProposalId());
			proposalSpecialReviews.stream().forEach(proposalSpecialReview -> {
				proposalModuleDao.saveOrUpdateProposalSpecialReview(proposalSpecialReview);
			});
			setProposalUpdateUser(vo);
		}
	}

	private void prepareSpecialReview(Cell ethicsCell, ProposalVO vo, Row ethicsRow, List<ProposalSpecialReview> proposalSpecialReviews) {
		ProposalSpecialReview specialReview = new ProposalSpecialReview();
		if (ethicsCell != null && !ethicsCell.getCellType().equals(CellType.NUMERIC) && !ethicsCell.getStringCellValue().equals("")) {
			String reviewType = ethicsCell.getStringCellValue().replaceAll("[\r\n]+", " ");;
			specialReview.setSpecialReviewTypeCode(proposalDao.getSpecialReviewType(reviewType));
		}
		 Cell expirationDate = ethicsRow.getCell(11);
		 if (expirationDate != null && expirationDate.getCellType().equals(CellType.NUMERIC) && expirationDate.getDateCellValue() != null) {
			 specialReview.setExpirationDate(new Timestamp((expirationDate.getDateCellValue().getTime())));
		 }
		Cell comments = ethicsRow.getCell(20);
		if (comments != null && !comments.getCellType().equals(CellType.NUMERIC) && !comments.getStringCellValue().equals("")) {
			specialReview.setComments(comments.getStringCellValue());
		}
		specialReview.setProposalId(vo.getProposalId());
        Cell involved = ethicsRow.getCell(1);
        if (involved != null && !involved.getCellType().equals(CellType.NUMERIC) && involved.getStringCellValue().equals("No")) {
            specialReview.setApprovalTypeCode(Constants.SP_REV_APPROVAL_TYPE_NOT_APPLICABLE);
        } else {
            specialReview.setApprovalTypeCode(Constants.SP_REV_APPROVAL_TYPE_PENDING);
        }
		if (specialReview.getSpecialReviewTypeCode() != null) {
			proposalSpecialReviews.add(specialReview);
		}
	}

	private void saveProposalKPi(List<ProposalKPICriteria> proposalCriterias, ProposalVO vo) {
		if (proposalCriterias != null && !proposalCriterias.isEmpty()) {
			proposalDao.deleteProposalKPIs(vo.getProposalId());
			Map<String, List<ProposalKPICriteria>> proposalCriteriaGroups = proposalCriterias.stream()
					.sorted(Comparator.comparing(ProposalKPICriteria:: getKpiTypeCode))
					.collect(Collectors.groupingBy(proposalCriteria -> proposalCriteria.getKpiTypeCode(),LinkedHashMap::new,Collectors.toList()));
			for (Map.Entry<String, List<ProposalKPICriteria>> entry : proposalCriteriaGroups.entrySet()) {
				ProposalKPI proposalKPI = new ProposalKPI();	
				List<ProposalKPICriteria> proposalKPICriterias= new ArrayList<>();
					proposalKPI.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					proposalKPI.setUpdateUser(AuthenticatedUser.getLoginUserName());
					proposalKPI.setProposalId(vo.getProposalId());
					proposalKPI.setKpiTypeCode(entry.getKey());
					proposalDao.saveOrUpdateProposalKPI(proposalKPI);
					for(ProposalKPICriteria proposalKPICriteria : entry.getValue()) {
						proposalKPICriteria.setKpiCriteriaTypeCode(proposalKPICriteria.getKpiCriteriaTypeCode());
						proposalKPICriteria.setTarget(proposalKPICriteria.getTarget());
						proposalKPICriteria.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						proposalKPICriteria.setUpdateUser(AuthenticatedUser.getLoginUserName());
						proposalKPICriteria.setProposalKpiId(proposalKPI.getProposalKpiId());
						proposalKPICriterias.add(proposalDao.saveOrUpdateProposalKPICriterias(proposalKPICriteria));
					}
					proposalKPI.setProposalKPICriterias(proposalKPICriterias);
			}
		}
	}

	private List<ProposalKPICriteria> readProposalKpi(Cell kpiCell, List<ProposalKPICriteria> proposalCriterias, Row nextRow) {
		ProposalKPICriteria kpiCriteria = new ProposalKPICriteria();
		if (kpiCell != null && !kpiCell.getCellType().equals(CellType.NUMERIC) && !kpiCell.getStringCellValue().equals("")) {
			String description = kpiCell.getStringCellValue().replaceAll("[\r\n]+", " ");
			kpiCriteria.setKpiTypeCode(proposalDao.getKpiTypeBasedOnParam(description));
		}
		 Cell criteria = nextRow.getCell(1);
		 if (criteria != null && !criteria.getCellType().equals(CellType.NUMERIC) && !criteria.getStringCellValue().equals("")) {
				String criteriaDescription = criteria.getStringCellValue().replaceAll("[\r\n]+", " ");
				kpiCriteria.setKpiCriteriaTypeCode(proposalDao.getkpiCrieriaBasedOnParam(criteriaDescription));
		}
		 Cell target = nextRow.getCell(7);
		 if (target != null && !target.getCellType().equals(CellType.STRING)) {
				Integer kpiTarget = (int) Math. round(target.getNumericCellValue());
				kpiCriteria.setTarget(kpiTarget);
		}
		if (kpiCriteria.getKpiTypeCode() != null && kpiCriteria.getKpiCriteriaTypeCode() != null && kpiCriteria.getTarget() != null) {
			proposalCriterias.add(kpiCriteria);
		}
		return proposalCriterias;
	}

	private List<ProposalMileStone> readProposalMilestone(Cell milestone, ProposalMileStone proposalMileStone, Row nextRow, List<ProposalMileStone> proposalMileStones) {
		 if (milestone != null && !milestone.getCellType().equals(CellType.NUMERIC) && milestone.getStringCellValue() != null) {
			 proposalMileStone.setMileStone(milestone.getStringCellValue());
		 }
		 Cell startMonth = nextRow.getCell(1);
		 if (startMonth != null) {
			 if (startMonth.getCellType().equals(CellType.NUMERIC) 
					 || (startMonth.getCellType().equals(CellType.BLANK) && startMonth.getNumericCellValue() != 0)) {
				 proposalMileStone.setStartMonth((int) startMonth.getNumericCellValue());
			 } else if(startMonth.getCellType().equals(CellType.STRING) && startMonth.getStringCellValue() != null &&
					 !startMonth.getStringCellValue().isEmpty() && !startMonth.getCellType().equals(CellType.BLANK)) {
				 proposalMileStone.setStartMonth(Integer.parseInt(startMonth.getStringCellValue().trim()));
			 }
		 }
		 Cell duration = nextRow.getCell(7);
		 proposalMileStone.setDuration(getCellValueBasedOnCellType(duration));

		 if(proposalMileStone.getMileStone() != null && proposalMileStone.getStartMonth() != null && proposalMileStone.getDuration() != null) {
			 proposalMileStones.add(proposalMileStone);
		 }
		 return proposalMileStones;
	}

	private Integer getCellValueBasedOnCellType(Cell cell) {
		Integer cellValue = null; 
		 if (cell != null) {
			 if (cell.getCellType().equals(CellType.NUMERIC) 
					 && cell.getNumericCellValue() != 0 || (cell.getCellType().equals(CellType.BLANK) && cell.getNumericCellValue() > 0)) {
				 cellValue = (int) cell.getNumericCellValue();
			 } else if(cell.getCellType().equals(CellType.STRING) && cell.getStringCellValue() != null &&
					 !cell.getStringCellValue().isEmpty() && !(cell.getStringCellValue().trim().equals("0"))) {
				 cellValue = Integer.parseInt(cell.getStringCellValue().trim());
			 }
		 }
		return cellValue;
	}

	private void saveProposalMilestone(List<ProposalMileStone> proposalMileStones, Integer proposalId) {
		if (proposalMileStones != null && !proposalMileStones.isEmpty()) {
			proposalDao.deleteProposalMilestone(proposalId);
			proposalMileStones.forEach(proposalMileStone -> {
				proposalMileStone.setProposalId(proposalId);
				proposalMileStone.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				proposalMileStone.setUpdateUser(AuthenticatedUser.getLoginUserName());
				proposalModuleDao.saveOrUpdateProposalMileStone(proposalMileStone);
			});
		}
	}

	private void saveScientificAbstractData(String abstractValue, Proposal proposal) {
		 proposal.setAbstractDescription(abstractValue);
		 proposalDao.saveOrUpdateProposal(proposal);
	}

	private void deleteProposalKeyword(Integer proposalId) {
		proposalDao.deleteProposalKeyWords(proposalId);
	}

	private void saveProposalKeywordFromTemplate(String keyword, ProposalVO vo) {
		vo.setScienceKeyword(keyword);
		vo.setUserName(AuthenticatedUser.getLoginUserName());
		ProposalKeyword proposalKeyword = new ProposalKeyword();
		if (Boolean.TRUE.equals(proposalDao.isKeywordExist(keyword))) {
			proposalKeyword.setScienceKeywordCode(proposalDao.getScienceKeyword(keyword));
		} else {
			addScienceKeyword(vo);
			proposalKeyword.setScienceKeywordCode(proposalDao.getMaxScienceKeyword());
		}
		proposalKeyword.setProposal(vo.getProposal());
		proposalKeyword.setUpdateUser(AuthenticatedUser.getLoginUserName());
		proposalKeyword.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalDao.saveOrUpdateProposalKeyword(proposalKeyword);
	}

	@Override
	public String loadProposalKeyPersonAttachments(ProposalVO proposalVO) {
		List<ProposalPersonAttachment> proposalPersonAttachments = proposalDao.loadProposalKeyPersonAttachments(proposalVO.getProposalId());
		proposalPersonAttachments.forEach(proposalPersonAttachment -> {
			proposalPersonAttachment.setProposalPersonName(proposalPersonAttachment.getProposalPerson().getFullName());
		});
		getFullNameOfUpdateUser(proposalPersonAttachments);
		return commonDao.convertObjectToJSON(proposalPersonAttachments);
	}

	private void getFullNameOfUpdateUser(List<ProposalPersonAttachment> proposalPersonAttachments) {
		Set<String> userName = proposalPersonAttachments.stream().map(ProposalPersonAttachment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			proposalPersonAttachments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setLastUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	@Override
	public void exportProposalPersonAttachments(ProposalVO vo, HttpServletResponse response) {
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		if (proposal != null) {
			String fileName = "Proposal_#"+vo.getProposalId() + "_attachments";
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(vo.getProposalId());
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ZipOutputStream zos = new ZipOutputStream(baos);
				Set<Integer> proposalPersonIds = new HashSet<>();
				for(ProposalPerson proposalPerson : proposalPersons) {
					proposalPersonIds.add(proposalPerson.getProposalPersonId());
				}
				if(proposalPersonIds != null && !proposalPersonIds.isEmpty()) {
					List<ProposalPersonAttachment> proposalPersonAttachments =
							proposalDao.fetchProposalPersonAttachmentWithLastVersion(proposalPersonIds);
					if (proposalPersonAttachments != null && !proposalPersonAttachments.isEmpty()) {
						for (ProposalPersonAttachment attachment : proposalPersonAttachments) {
							zos.putNextEntry(new ZipEntry("CV-"+attachment.getFileName()));
							FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
							byte[] data = fileData.getAttachment();
							zos.write(data);
						}
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
				logger.error("exception in exportProposalPersonAttachments: {} ", e.getMessage());
			}
		}
	}

	@Override
	public ResponseEntity<String> createProposalAdminCorrection(Integer proposalId){
		try {
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_DEV_PROP_VERSIONS)) {
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposalId(proposalId);
			proposalVO.setUpdateUser(AuthenticatedUser.getLoginUserName());
			proposalVO.setIsProposalArchiveCreation(true);
			proposalVO.setIsProposalAdminCorrection(true);
			proposalCopyService.copyProposal(proposalVO);
			} else {
				proposalDao.updateDocumentStatusCode(proposalId, Constants.PROPOSAL_DOCUMENT_STATUS_ADMIN_CORRECTION);
			}
		} catch (Exception e) {
			return new ResponseEntity<>(loadProposalById(proposalId, AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), false), HttpStatus.INTERNAL_SERVER_ERROR);
		}
		return new ResponseEntity<>(loadProposalById(proposalId, AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), false), HttpStatus.OK);
	}

	@Override
	public String showProposalHistory(Integer proposalId) {
		ProposalHistoryVO vo = new  ProposalHistoryVO();
		List<ProposalHistory> proposalHistory = proposalDao.getProposalHistoryBasedOnProposalId(proposalId);
		getCreateUserFullNames(proposalHistory);
		vo.setParameterValue(getProposalFlagValues());
		vo.setProposalHistory(proposalHistory);
		return commonDao.convertObjectToJSON(vo);
	}

	private ParameterVO getProposalFlagValues() {
		ParameterVO parameter = new ParameterVO();
		parameter.setIsSimpleBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_SIMPLE_BUDGET_ENABLED));
		parameter.setIsModularBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_MODULAR_BUDGET_ENABLED));
		parameter.setIsDetailedBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_DETAILED_BUDGET_ENABLED));
		parameter.setIsPeriodTotalEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_PERIODS_TOTAL_ENABLED));
		parameter.setIsBudgetSummaryEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_SUMMARY_ENABLED));
		return parameter;
	}

	private void getCreateUserFullNames(List<ProposalHistory> proposalHistories) {
		Set<String> userName = proposalHistories.stream().map(ProposalHistory::getCreateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person::getFullName));
			proposalHistories.stream().filter(item -> item.getCreateUser() != null).filter(proposalHistory -> collect.containsKey(proposalHistory.getCreateUser().toUpperCase())).forEach(proposalHistory -> proposalHistory.setCreateUserFullName(collect.get(proposalHistory.getCreateUser().toUpperCase())));
		}	
	}

	@Override
	public ResponseEntity<String> completeProposalAdminCorrection(Integer proposalId) {
		try {
			proposalDao.updateDocumentStatusCode(proposalId, Constants.PROPOSAL_DOCUMENT_STATUS_ACTIVE);
			return new ResponseEntity<>(loadProposalById(proposalId, AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), false), HttpStatus.OK);
		} catch (Exception e) {
			return new ResponseEntity<>(loadProposalById(proposalId, AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), false), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}


	@Override
	public String loadProposalKeyPersonnelAttachmentTypes() {
		List<ProposalKeyPersonnelAttachmentType> data = null;
		try {
			data = proposalLookUpDao.fetchAllKeyPersonalProposalAttachmentTypes();
		} catch (Exception e) {
			logger.error("Error in method loadProposalKeyPersonnelAttachmentTypes {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(data);
	}

	@Override
	public String loadProposalKeyPersonnelPersons(Integer proposalId) {
		List<Map<String, Object>> data = new ArrayList<>();
		try {
			proposalDao.loadProposalKeyPersonnelPersons(proposalId).forEach(objects -> {
				Map<String, Object> obj = new HashMap<>();
				obj.put("proposalPersonId", objects[0]);
				obj.put("fullName", objects[1]);
				data.add(obj);
			});
		} catch (Exception e) {
			logger.error("Error in method loadProposalKeyPersonnelPersons {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(data);
	}

	@Override
	public String deleteKeyPersonnelAttachment(ProposalVO proposalVO) {
		try {
			List<ProposalPersonAttachment> proposalPersonAttachments = proposalDao.loadProposalKeyPersonAttachments(proposalVO.getProposalId(),
					proposalVO.getDocumentId());
			for(ProposalPersonAttachment proposalPersonAttachment: proposalPersonAttachments) {
				if (Boolean.FALSE.equals(proposalDao.getIsFileDataIdFound(proposalPersonAttachment.getFileDataId()))) {
					commonDao.deleteFileData(commonDao.getFileDataById(proposalPersonAttachment.getFileDataId()));
				}
				proposalDao.deleteKeyPersonnelAttachment(proposalPersonAttachment.getAttachmentId());
			}
			proposalVO.setProposalPersonAttachments(proposalDao.loadProposalKeyPersonAttachments(proposalVO.getProposalId()));
			proposalVO.setStatus(true);
			proposalVO.setMessage("Proposal key personnel attachment deleted successfully");
		} catch (Exception e) {
			proposalVO.setStatus(false);
			proposalVO.setMessage("Problem occurred in deleting proposal attachment");
			logger.error("Error in method deleteKeyPersonnelAttachment {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String uploadProposalPersonAttachment(MultipartFile[] files, String formDataJSON) {
		ProposalVO proposalVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			List<ProposalPersonAttachment> newAttachments = proposalVO.getNewPersonAttachments();
			List<ProposalPersonAttachment> proposalPersonAttachments = proposalDao.loadProposalKeyPersonAttachments(proposalVO.getProposalId());
			if(proposalPersonAttachments == null || proposalPersonAttachments.isEmpty()) {
				proposalPersonAttachments = new ArrayList<>();
			} else {
				proposalPersonAttachments.forEach(proposalPersonAttachment -> {
					proposalPersonAttachment.setProposalPersonName(proposalPersonAttachment.getProposalPerson().getFullName());
				});
				getFullNameOfUpdateUser(proposalPersonAttachments);
			}
			Integer documentId = 0;
			if (!proposalPersonAttachments.isEmpty()) {
				documentId = proposalPersonAttachments.get(0).getDocumentId();
			}
			Integer versionNumber = 0;
			List<ProposalPersonAttachment> newPersonAttachments = new ArrayList<>();
			for (int i = 0; i < files.length; i++) {
				for (ProposalPersonAttachment newAttachment : newAttachments) {
					if(newAttachment.getFileName() != null &&
							(files[i] != null && files[i].getOriginalFilename().equals(newAttachment.getFileName()))) {
						ProposalPersonAttachment proposalPersonAttachment = new ProposalPersonAttachment();
						if(newAttachment.getAttachmentId() != null) {
							ProposalPersonAttachment attachmentData = proposalModuleDao.getProposalPersonAttachmentById(newAttachment.getAttachmentId());
							attachmentData.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
							attachmentData.setUpdateUser(AuthenticatedUser.getLoginUserName());
							attachmentData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
							proposalModuleDao.saveOrUpdateProposalPersonAttachment(attachmentData);
							versionNumber = attachmentData.getVersionNumber()+1;
							documentId = attachmentData.getDocumentId();
						} else {
							proposalPersonAttachment = new ProposalPersonAttachment();
							documentId = documentId+1;
							versionNumber = versionNumber+1;
						}

						proposalPersonAttachment.setFileName(newAttachment.getFileName());
						proposalPersonAttachment.setDescription(newAttachment.getDescription());
						proposalPersonAttachment.setProposalPersonId(newAttachment.getProposalPersonId());
						proposalPersonAttachment.setMimeType(files[i].getContentType());
						FileData fileData = new FileData();
						fileData.setAttachment(files[i].getBytes());
						fileData = commonDao.saveFileData(fileData);
						proposalPersonAttachment.setFileDataId(fileData.getFileDataId());
						proposalPersonAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
						proposalPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						proposalPersonAttachment.setAttachmentTypeCode(newAttachment.getAttachmentTypeCode());
						proposalPersonAttachment.setAttachment(files[i].getBytes());
						proposalPersonAttachment.setDocumentId(documentId);
						proposalPersonAttachment.setVersionNumber(versionNumber);
						proposalPersonAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
						proposalModuleDao.saveOrUpdateProposalPersonAttachment(proposalPersonAttachment);
						proposalPersonAttachment.setLastUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
						proposalPersonAttachment.setProposalPersonName(newAttachment.getProposalPersonName());
						proposalPersonAttachment.setAttachmentType(newAttachment.getAttachmentType());
						proposalPersonAttachments.add(proposalPersonAttachment);
						newPersonAttachments.add(proposalPersonAttachment);
					}
				}
			}
			proposalVO.setNewPersonAttachments(newPersonAttachments);
			proposalVO.setProposalPersonAttachments(proposalPersonAttachments);
		} catch (Exception e) {
			logger.error("exception in uploadProposalPersonAttachment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String updateKeyPersonnelAttachment(String formDataJson) {
		ProposalVO proposalVO = new ProposalVO();
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJson, ProposalVO.class);
			if(proposalVO == null || proposalVO.getProposalPersonAttachment() == null) {
				proposalVO.setStatus(Boolean.FALSE);
				proposalVO.setMessage("Attachment cannot be null");
				return commonDao.convertObjectToJSON(proposalVO);
			}
			ProposalPersonAttachment attachment = proposalVO.getProposalPersonAttachment();
			attachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			attachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			attachment.setLastUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
			proposalModuleDao.updateProposalkeyPersonAttachment(attachment.getAttachmentId(), attachment.getDescription(), attachment.getAttachmentTypeCode());
			proposalVO.setProposalPersonAttachment(attachment);
			proposalVO.setStatus(Boolean.TRUE);
			proposalVO.setMessage("Proposal key personnel attachment updated successfully");
		} catch (Exception e) {
			logger.error("exception in updateKeyPersonnelAttachment: {} ", e.getMessage());
			proposalVO.setStatus(Boolean.FALSE);
			proposalVO.setMessage("Problem occurred in deleting proposal attachment");
		}
		return commonDao.convertObjectToJSON(proposalVO);
	}
	
	@Override
	public String updateProposalPersonCertification(ProposalVO proposalVO) {
		proposalDao.updateProposalPersonCertification(proposalVO.getProposalId(), proposalVO.getPersonId(), proposalVO.getStatus(), proposalVO.getIsNonEmployee());
		Proposal propsal = new Proposal();
		propsal.setProposalId(proposalVO.getProposalId());
		proposalVO.setProposal(propsal);
		if (Boolean.TRUE.equals(proposalVO.getStatus())) {
			Boolean sendFlag = Boolean.TRUE;
			if (proposalVO.getProposalPersonRole() != null && proposalVO.getProposalPersonRole().equals(Constants.PI_ROLE_CODE.toString()) && 
					proposalVO.getPersonId().equals(AuthenticatedUser.getLoginPersonId())) {
				sendFlag = Boolean.FALSE;
			}
			if (Boolean.TRUE.equals(sendFlag)) {
				Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
				if (proposalVO.getIsNonEmployee()) {
					proposalVO.setUserName(rolodexDao.getRolodexDetailById(Integer.parseInt(proposalVO.getPersonId())).getFullName());
				} else {
					proposalVO.setUserName(personDao.getPersonDetailById(proposalVO.getPersonId()).getFullName()); 
				}
				sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_PERSON_COMPLETE, dynamicEmailRecipients);	
			}
			inboxDao.markReadMessage(Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL, proposalVO.getProposalId().toString(), proposalVO.getPersonId(), Constants.INBOX_PROPOSAL_PERSON_CERTIFICATION_REQUIRED,
					proposalVO.getPersonId(), 3);
		}
		if (Boolean.TRUE.equals(proposalDao.isAllProposalPersonCertified(proposalVO.getProposalId()))) {
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_PERSON_ALL_COMPLETE, dynamicEmailRecipients);
		}
		proposalVO.setMessage("Success");
		return commonDao.convertObjectToJSON(proposalVO);
	}

	@Override
	public String proposalPersonsForCertification(ProposalVO proposalVO) {
		Map<String, Object> result = new HashMap<>();
		List<ProposalPerson> proposalPersons = proposalDao.proposalPersonsForCertification(proposalVO.getProposalId());
		setPersonEmailAddress(proposalPersons);
		setRolodexEmailAddress(proposalPersons);
		result.put("proposalPersons", proposalPersons);
		if (Boolean.TRUE.equals(proposalVO.getProposalEditable())) {
			proposalPersons.forEach(proposalPerson -> {
				Boolean isNonEmployee = proposalPerson.getPersonId() != null ? Boolean.FALSE : Boolean.TRUE;
				String personId = Boolean.FALSE.equals(isNonEmployee) ? proposalPerson.getPersonId() : proposalPerson.getRolodexId().toString();
				Boolean checkInCompleteQuestionnaireExists = questionnaireDAO.checkInCompleteQuestionnaireExists(proposalVO.getProposalId().toString(), personId, Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE);
				if (Boolean.TRUE.equals(checkInCompleteQuestionnaireExists)) {
					proposalDao.updateProposalPersonCertification(proposalVO.getProposalId(), personId, Boolean.FALSE, isNonEmployee);
					proposalPerson.setPersonCertified(Boolean.FALSE);
				} else {
					proposalDao.updateProposalPersonCertification(proposalVO.getProposalId(), personId, Boolean.TRUE, isNonEmployee);
					proposalPerson.setPersonCertified(Boolean.TRUE);
				}
			});
		}
		return commonDao.convertObjectToJSON(result);
	}

	private void setPersonEmailAddress(List<ProposalPerson> proposalPersons) {
		Set<String> personIds = proposalPersons.stream().map(ProposalPerson::getPersonId).collect(Collectors.toSet());
		if (!personIds.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByPersonId(new ArrayList<>(personIds));
			Map<String, String> collect = personDetails.stream().filter(item -> item.getEmailAddress() != null).collect(Collectors.toMap(Person::getPersonId, Person::getEmailAddress));
			proposalPersons.stream().filter(item -> item.getPersonId() != null).filter(item -> collect.containsKey(item.getPersonId())).
			forEach(item -> item.setEmailAddress(collect.get(item.getPersonId())));
		}
	}

	private void setRolodexEmailAddress(List<ProposalPerson> proposalPersons) {
		Set<Integer> rolodexIds = proposalPersons.stream().map(ProposalPerson::getRolodexId).collect(Collectors.toSet());
		if (!rolodexIds.isEmpty()) {
			List<Rolodex> rolodexDetails = commonDao.getRolodexDetailByRolodexId(new ArrayList<>(rolodexIds));
			Map<Integer, String> collect = rolodexDetails.stream().filter(item -> item.getEmailAddress() != null).collect(Collectors.toMap(Rolodex::getRolodexId, Rolodex::getEmailAddress));
			proposalPersons.stream().filter(item -> item.getRolodexId() != null).filter(item -> collect.containsKey(item.getRolodexId())).
			forEach(item -> item.setEmailAddress(collect.get(item.getRolodexId())));
		}
	}

	@Override
	public String sendPersonCertificationMail(ProposalVO proposalVO) {
		Proposal propsal = new Proposal();
		propsal.setProposalId(proposalVO.getProposalId());
		proposalVO.setProposal(propsal);
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (proposalVO.getPersonId() == null) {
			proposalDao.proposalPersonsForCertification(proposalVO.getProposalId()).forEach(proposalPerson -> {
				if (proposalPerson.getPersonId() != null) {
					commonService.setNotificationRecipients(proposalPerson.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO,	dynamicEmailRecipients);
					sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_PERSONS, dynamicEmailRecipients);
					if (Boolean.FALSE.equals(proposalVO.getPersonCertified())) {
						addToInbox(proposalVO.getProposalId(), proposalPerson.getPersonId(), Constants.INBOX_PROPOSAL_PERSON_CERTIFICATION_REQUIRED, Constants.DEV_PROPOSAL_MODULE_CODE, 3);
					}
				}
				dynamicEmailRecipients.clear();
			});
			proposalVO.setStatus(true);
			return commonDao.convertObjectToJSON(proposalVO);
		}
		commonService.setNotificationRecipients(proposalVO.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO,	dynamicEmailRecipients);
		if (Boolean.FALSE.equals(proposalVO.getPersonCertified())) {
			addToInbox(proposalVO.getProposalId(), proposalVO.getPersonId(), Constants.INBOX_PROPOSAL_PERSON_CERTIFICATION_REQUIRED, Constants.DEV_PROPOSAL_MODULE_CODE, 3);
		}
		sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_PERSONS, dynamicEmailRecipients);
		proposalVO.setStatus(true);
		return commonDao.convertObjectToJSON(proposalVO);
	}

	private void addToInbox(Integer proposalId, String personId, String messageTypeCode,
			Integer devProposalModuleCode, Integer subModuleCode) {
		Inbox inbox = new Inbox();
		inbox.setModuleCode(devProposalModuleCode);
		inbox.setSubModuleCode(subModuleCode);
		inbox.setToPersonId(personId);
		inbox.setModuleItemKey(proposalId.toString());
		inbox.setUserMessage("Proposal Person Certification #"+proposalId.toString());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setSubModuleItemKey(personId);
		inboxDao.saveToInbox(inbox);
	}

	@Override
	public String canDeleteProposal(ProposalVO vo) {
		Boolean viewRightFromPermission = Boolean.FALSE;
		Boolean isPersonCanDelete = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), DELETE_PROPOSAL_RIGHT_NAME, vo.getHomeUnitNumber());
		if (Boolean.FALSE.equals(isPersonCanDelete)) {
			viewRightFromPermission = proposalDao.isPersonHasRightInProposal(AuthenticatedUser.getLoginPersonId(), DELETE_PROPOSAL_RIGHT_NAME, vo.getProposalId());
		}
		if (Boolean.TRUE.equals(isPersonCanDelete) || Boolean.TRUE.equals(viewRightFromPermission)) {
			vo.setCanDeleteProposal(Boolean.TRUE);
		}  else {
			vo.setCanDeleteProposal(Boolean.FALSE);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadProtocolDetail(ProtocolVO vo) {
		if (vo.getSpecialReviewTypeCode().equals(Constants.SPECIAL_REVIEW_IRB_TYPE_CODE)) {
			List<IrbProtocol> irbProtocols = complianceDao.loadIrbProtocolDetail(vo);
			vo.setIrbProtocols(prepareIrbProtocols(irbProtocols));
		} else {
			List<AcProtocol> acProtocols = complianceDao.loadAcProtocolDetail(vo);
			vo.setAcProtocols(prepareAcProtocol(acProtocols));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String setFullNameForProtocol(Map<String, String> persons, String personId, Boolean isNonEmployee) {
		String fullName = null;
		if (!persons.containsKey(personId)) {
			if (Boolean.TRUE.equals(isNonEmployee)) {
				Rolodex rolodex = rolodexDao.getRolodexDetailById(Integer.parseInt(personId));
				fullName = rolodex != null ? rolodex.getFullName() : null;
			} else {
				fullName = personDao.getPersonFullNameByPersonId(personId);
			}
			persons.put(personId, fullName);
		} else {
			fullName = persons.get(personId);
		}
		return fullName;
	}

	private List<IrbProtocol> prepareIrbProtocols(List<IrbProtocol> irbProtocols) {
		Map<String, String> persons = new HashMap<>();
		irbProtocols.forEach(irbProtocol -> {
			irbProtocol.setFullName(setFullNameForProtocol(persons, irbProtocol.getPersonId(), irbProtocol.getNonEmployeeFlag()));
		});
		return irbProtocols;
	}

	private List<AcProtocol> prepareAcProtocol(List<AcProtocol> acProtocols) {
		Map<String, String> persons = new HashMap<>();
		acProtocols.forEach(acProtocol -> {
			acProtocol.setFullName(setFullNameForProtocol(persons, acProtocol.getPersonId(), acProtocol.getNonEmployeeFlag()));
		});
		return acProtocols;
	}

	@Override
	public IrbProtocol getIrbProtocols(String protocolNumber) {
		return complianceDao.fetchLatestIrbPrtocol(protocolNumber);
	}

	@Override
	public AcProtocol getAcProtocols(String protocolNumber) {
		return complianceDao.fetchLatestAcPrtocol(protocolNumber);
	}

	@Override
	public String addProposalPersonDegree(ProposalVO vo) {
		try {
			vo.getProposalPersonDegree().setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			vo.getProposalPersonDegree().setUpdateUser(AuthenticatedUser.getLoginUserName());
			proposalModuleDao.addProposalPersonDegree(vo.getProposalPersonDegree());
		} catch(Exception e) {
			throw new ApplicationException("add Proposal Person Degree", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(proposalModuleDao.getPersonDegree(vo.getProposalPersonDegree().getProposalPersonId()));
	}

	@Override
	public String getPersonDegree(ProposalVO vo) {
		return commonDao.convertObjectToJSON(proposalModuleDao.getPersonDegree(vo.getProposalPersonId()));
	}

	@Override
	public String deleteProposalPersonDegree(Integer proposalPersonDegreeId) {
		try {
			proposalModuleDao.deleteProposalPersonDegree(proposalPersonDegreeId);
			return commonDao.convertObjectToJSON("deleted successfully");
		}catch(Exception e) {
			logger.error("error in deleteProposalPersonDegree: {}", e.getMessage());
			throw new ApplicationException("Failed to delete", e, Constants.JAVA_ERROR);
		}
	}

}
