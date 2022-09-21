package com.polus.fibicomp.ip.service;

import java.io.File;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.compilance.dao.ComplianceDao;
import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.service.CustomDataElementService;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.ip.dao.InstitutionalProposalDao;
import com.polus.fibicomp.ip.dto.InstituteProposalHistoryDto;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionLog;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachments;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetHeader;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetPeriod;
import com.polus.fibicomp.ip.pojo.InstituteProposalComment;
import com.polus.fibicomp.ip.pojo.InstituteProposalKeywords;
import com.polus.fibicomp.ip.pojo.InstituteProposalPerson;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonAttachment;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonUnit;
import com.polus.fibicomp.ip.pojo.InstituteProposalResearchArea;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.ip.pojo.InstituteProposalVersionHistory;
import com.polus.fibicomp.ip.vo.InstProposalVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.ParameterVO;

@Transactional
@Service(value = "institutionalProposalService")
public class InstitutionalProposalServiceImpl implements InstitutionalProposalService {

	protected static Logger logger = LogManager.getLogger(InstitutionalProposalServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "institutionalProposalDao")
	private InstitutionalProposalDao institutionalProposalDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private PrintService printService;

	@Autowired
	private AuthorizationService authorizationService;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private ComplianceDao complianceDao;

	private static final String MODIFY_IP = "4";
	private static final String SUBMIT_IP = "5";
	private static final String MERGE_IP = "10";
	private static final String IP_MODIFICATION = "IP Modification";
	private static final String IP_MERGE = "IP Merge";

	@Override
	public boolean createInstitutionalProposal(Integer proposalId, String ipNumber, String userName) {
		return institutionalProposalDao.createInstitutionalProposal(proposalId, ipNumber, userName);
	}

	@Override
	public String generateInstitutionalProposalNumber() {
		boolean fyBasedIp = commonService.getParameterValueAsBoolean(Constants.FISCAL_YEAR_BASED_IP);
		return fyBasedIp ? getNextIPFiscalYearBased() :
             new DecimalFormat(Constants.DECIMAL_FORMAT).format(commonService.getNextSequenceNumber(Constants.INSTITUTIONAL_PROPSAL_PROPSAL_NUMBER_SEQUENCE));
	}

	private String getNextIPFiscalYearBased() {
        String fiscalYear =  StringUtils.substring(commonService.getCurrentFiscalYear().toString(),2,4);
        String fiscalMonth = StringUtils.leftPad(commonService.getCurrentFiscalMonthForDisplay().toString(), 2, "0");
        Long nextProposalNumberSeq = commonService.getNextSequenceNumber(Constants.INSTITUTIONAL_PROPSAL_PROPSAL_NUMBER_SEQUENCE);
        return fiscalYear + fiscalMonth + new DecimalFormat(Constants.DECIMAL_FORMAT_FOR_NEW_IP).format(nextProposalNumberSeq);
    }
	
	@Override
	public String loadInstProposalById(Integer proposalId, String personId) {
		InstProposalVO instProposalVO = new InstProposalVO();
		try {
			InstituteProposal instProposal = institutionalProposalDao.fetchInstProposalById(proposalId);
			if (instProposal.getBaseProposalNumber() != null && !instProposal.getBaseProposalNumber().isEmpty()) {
				instProposal.setBaseProposalTitle(institutionalProposalDao.getIPTitleForMasterProposal(instProposal.getBaseProposalNumber()));
			}
			List<InstituteProposalPerson> instituteProposalPersons = institutionalProposalDao.loadInstProposalPersonsByProposalId(proposalId);
			instProposalVO.setInstituteProposalPersons(instituteProposalPersons);
			instProposal.setInstProposalPersons(instituteProposalPersons);
			instProposalVO.setInstituteProposalSpecialReviews(preapreInstProposalSpecialReview(proposalId));
			instProposalVO.setInstituteProposalResearchAreas(institutionalProposalDao.loadInstProposalResearchAreaByProposalId(proposalId));
			setIPSponsorDetail(instProposal);
			loadInstituteProposalUserFullNames(instProposal);
			instProposalVO.setInstProposal(instProposal);
			instProposalVO.setInstituteProposalKeywords(institutionalProposalDao.fetchAllIPKeywordByProposal(proposalId));
			instProposalVO.setDevProposalIds(institutionalProposalDao.fetchDevProposalByInstProposal(proposalId));
			instProposalVO.setAvailableRights(authorizationService.allDepartmentPermission(Constants.INSTITUTE_PROPOSAL_MODULE_CODE, personId, instProposal.getHomeUnitNumber(), instProposal.getProposalId()));
			setLookupDatasForInstProposal(instProposalVO);
			if (instProposal.getActivityTypeCode() != null && instProposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString(Constants.RCBF_TYPE_CODE))) {
				instProposal.setIsRcbfProposal(true);
			}
		} catch (Exception e) {
			logger.error("error occured in loadInstProposalById {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(instProposalVO);
	}

	private List<InstituteProposalSpecialReview> preapreInstProposalSpecialReview(Integer proposalId) {
		List<InstituteProposalSpecialReview> instituteProposalSpecialReviews = institutionalProposalDao.loadInstProposalSpecialReviewByProposalId(proposalId);
		List<InstituteProposalSpecialReview> specialReviewDetail = new ArrayList<>();
		Map<String, String> nonEmployees = new HashMap<>();
		Map<String, String> persons = new HashMap<>();
		instituteProposalSpecialReviews.forEach(instituteProposalSpecialReview -> {
			specialReviewDetail.add(setInsSpecialReviewDetail(persons, nonEmployees, instituteProposalSpecialReview));
		});
		return specialReviewDetail;
	}

	@Override
	public InstituteProposalSpecialReview setInsSpecialReviewDetail(Map<String, String> persons, Map<String, String> nonEmployees, InstituteProposalSpecialReview instituteProposalSpecialReview) {
		InstituteProposalSpecialReview specialReview = new InstituteProposalSpecialReview();
		BeanUtils.copyProperties(instituteProposalSpecialReview, specialReview);
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

	private void setLookupDatasForInstProposal(InstProposalVO instProposalVO) {
		instProposalVO.setStatusCodes(institutionalProposalDao.fetchAllInstituteProposalStatus());
		instProposalVO.setIsFundingSupportDeclarationRequired(commonDao.getParameterValueAsBoolean(Constants.IS_OTHER_FUNDING_SUPPORT_REQUIRED));
		instProposalVO.setIsReplaceAttachmentEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_REPLACE_ATTACHMENTS_IP));
		instProposalVO.setProposalPersonRoles(proposalLookUpDao.fetchAllProposalPersonRoles());
		instProposalVO.setProposalAttachmentTypes(institutionalProposalDao.fetchAllIPAttachmentTypes());
		instProposalVO.setProposalTypes(institutionalProposalDao.fetchAllIPTypes());
		instProposalVO.setGrantCallTypes(grantCallDao.fetchAllGrantCallTypes());
		instProposalVO.setAwardTypes(awardDao.fetchAllAwardTypes());
		instProposalVO.setReviewTypes(institutionalProposalDao.getSpecialReviewTypes(Constants.DEV_PROPOSAL_MODULE_CODE));
		instProposalVO.setSpecialReviewApprovalTypes(commonDao.fetchAllApprovalStatusTypes());
		instProposalVO.setResearchTypes(commonDao.fetchAllResearchTypes());
		instProposalVO.setInstituteProposalActionTypes(institutionalProposalDao.fetchIPActionTypes());
		instProposalVO.setIsAwarded(institutionalProposalDao.anyAwardLinkedToIP(instProposalVO.getInstProposal().getProposalNumber()));
		instProposalVO.setDisciplineClusters(proposalLookUpDao.fetchAllDisciplineCluster());
		Boolean enableActivityGrantMapping = isActivityForGrantTypeEnabled();
		instProposalVO.setEnableActivityGrantCallMapping(enableActivityGrantMapping);
		if (Boolean.TRUE.equals(enableActivityGrantMapping)) {
			instProposalVO.setActivityTypes(proposalLookUpDao.getAllActivityForGrantType());
		} else {
			instProposalVO.setActivityTypes(commonDao.fetchAllActivityTypes());
		}
		instProposalVO.setAcProtocolStatuses(complianceDao.getAcProtocolStatus());
		instProposalVO.setIrbProtocolStatuses(complianceDao.getIrbProtocolStatus());
	}

	private Boolean isActivityForGrantTypeEnabled() {
		return commonDao.getParameterValueAsBoolean("ENABLED_ACTIVITY_GRANT_TYPE_MAPPPING");
	}

	private List<ProposalAttachment> fetchProposalAttachmentsByProposalIds(Integer devProposalId) {
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_PROPOSAL_RIGHT, proposalDao.fetchProposalLeadUnitNumberByProposalId(devProposalId));
		Boolean isPersonHasPermissionInProposal = Boolean.FALSE;
		if (Boolean.FALSE.equals(isPersonHasPermission)) {
			List<String> rights = new ArrayList<>();
			rights.add(Constants.VIEW_PROPOSAL_RIGHT);
			isPersonHasPermissionInProposal = proposalDao.isPersonHasRightInProposal(AuthenticatedUser.getLoginPersonId(), rights, devProposalId);
		}
		List <ProposalAttachment> proposalAttachments = new ArrayList<>();
		if (Boolean.TRUE.equals(isPersonHasPermission) || Boolean.TRUE.equals(isPersonHasPermissionInProposal)) {
		proposalAttachments = institutionalProposalDao.fetchProposalAttachmentsByProposalIds(devProposalId);
			Set<String> userName = proposalAttachments.stream().map(ProposalAttachment::getUpdateUser).collect(Collectors.toSet());
			if (!proposalAttachments.isEmpty()) {
				List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
				Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
				proposalAttachments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setLastUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
			}
		}
		return proposalAttachments;
	}
	
	@Override	
	public ResponseEntity<byte[]> downloadInstProposalAttachment(Integer attachmentId) {
		InstituteProposalAttachments attachment = institutionalProposalDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.info("error occured while downloadInstProposalAttachment : {}", e.getMessage());
		}
		return attachmentData;
	}

	private void loadInstituteProposalUserFullNames(InstituteProposal instituteProposal) {
		if (instituteProposal.getCreateUser() != null) {
			instituteProposal.setCreateUserFullName(personDao.getUserFullNameByUserName(instituteProposal.getCreateUser()));
		}
		if (instituteProposal.getUpdateUser() != null) {
			instituteProposal.setUpdateUserFullName(personDao.getUserFullNameByUserName(instituteProposal.getUpdateUser()));
		}
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String saveOrUpdateInstituteProposal(InstProposalVO vo) throws ApplicationException {
		try {
			InstituteProposal instProposal = vo.getInstProposal();
			instProposal.setUpdateUser(AuthenticatedUser.getLoginUserName());
			instProposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			instProposal = institutionalProposalDao.saveOrUpdateInstituteProposal(instProposal);
			loadInstituteProposalUserFullNames(instProposal);
			setIPSponsorDetail(instProposal);
			vo.setInstProposal(instProposal);
			if (Boolean.TRUE.equals(vo.getInstituteProposalDateChanged())) {
				institutionalProposalDao.updateIPBudgetDates(instProposal.getStartDate(), instProposal.getEndDate(), instProposal.getProposalId());
			}
			vo.setDevProposalIds(institutionalProposalDao.fetchDevProposalByInstProposal(instProposal.getProposalId()));
			vo.setStatusCodes(institutionalProposalDao.fetchAllInstituteProposalStatus());
			vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.INSTITUTE_PROPOSAL_MODULE_CODE, vo.getPersonId(), instProposal.getHomeUnitNumber(), instProposal.getProposalId()));
		} catch (Exception e) {
			logger.error(" error occured in : {}", e.getMessage());
			throw new ApplicationException("error occuered while saveOrUpdateInstituteProposal", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getInstituteProposalAttachments(Integer proposalId) {
		InstProposalVO vo = new InstProposalVO();
		vo.setInstituteProposalAttachments(institutionalProposalDao.getInstituteProposalAttachments(proposalId));
		List<Integer> devProposalIds = institutionalProposalDao.fetchDevProposalByInstProposal(proposalId);
		vo.setDevProposalIds(devProposalIds);
		Map<Integer, List<ProposalAttachment>> proposalAttachments = new HashMap<>();
		devProposalIds.forEach(devProposalId -> proposalAttachments.put(devProposalId, fetchProposalAttachmentsByProposalIds(devProposalId)));
		vo.setProposalAttachments(proposalAttachments);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String createNewIPVersion(InstProposalVO vo) throws ApplicationException {
		try {
			Integer proposalId = vo.getProposalId();
			InstituteProposal copyInstProposal = new InstituteProposal();
			copyIPGeneralDetails(proposalId, copyInstProposal);
			loadInstituteProposalUserFullNames(copyInstProposal);
			Integer copyProposalId = copyInstProposal.getProposalId();
			Integer sequenceNumber = copyInstProposal.getSequenceNumber();
			vo.setDevProposalIds(copyPropoosalAdminDetails(proposalId, copyProposalId));
			vo.setInstituteProposalPersons(copyIPPersonDetails(proposalId, copyProposalId, sequenceNumber));
			copyInstProposal.setInstProposalPersons(vo.getInstituteProposalPersons());
			vo.setInstituteProposalSpecialReviews(copyIPSpecialReview(proposalId, copyProposalId, sequenceNumber));
			vo.setInstituteProposalResearchAreas(copyIPResearchAreas(proposalId, copyProposalId, sequenceNumber));
			vo.setInstituteProposalBudgetHeader(copyIPBudgetDetails(proposalId, copyProposalId, sequenceNumber));
			vo.setInstituteProposalAttachments(copyIPAttchments(proposalId, copyProposalId, sequenceNumber));
			vo.setInstituteProposalKeywords(copyIPKeyWords(proposalId, copyProposalId, sequenceNumber));
			copyIPComments(proposalId, copyProposalId, sequenceNumber);
			if(vo.getInstituteProposalKeywords() != null && !vo.getInstituteProposalKeywords().isEmpty()) {
				copyInstProposal.getInstProposalKeywords().addAll(vo.getInstituteProposalKeywords());
			}
			if (copyInstProposal.getBaseProposalNumber() != null && !copyInstProposal.getBaseProposalNumber().isEmpty()) {
				copyInstProposal.setBaseProposalTitle(institutionalProposalDao.getIPTitleForMasterProposal(copyInstProposal.getBaseProposalNumber()));
			}
			setIPSponsorDetail(copyInstProposal);
			vo.setInstProposal(copyInstProposal);
			customDataElementService.copyCustomDataBasedOnModule(proposalId, copyProposalId, Constants.INSTITUTE_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
			logDetailAsHistory(proposalId, copyProposalId, vo.getDevProposalId());
			saveActionLogDetails(copyProposalId, (vo.getDevProposalId() ==null ? MODIFY_IP : MERGE_IP), vo.getDescription());
			vo.setStatusCodes(institutionalProposalDao.fetchAllInstituteProposalStatus());
			vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.INSTITUTE_PROPOSAL_MODULE_CODE, AuthenticatedUser.getLoginPersonId(), copyInstProposal.getHomeUnitNumber(), copyInstProposal.getProposalId()));

		} catch (Exception e) {
			logger.info("error occuered while creating new version of IP");
			throw new ApplicationException("error occuered while creating new version of IP", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void setIPSponsorDetail(InstituteProposal instProposal) {
		if (instProposal.getSponsor() != null) {
			instProposal.setSponsorName(commonService.getSponsorFormatBySponsorDetail(instProposal.getSponsor().getSponsorCode(), instProposal.getSponsor().getSponsorName(), instProposal.getSponsor().getAcronym()));
		}
		if (instProposal.getPrimeSponsor() != null) {
			instProposal.setPrimeSponsorName(commonService.getSponsorFormatBySponsorDetail(instProposal.getPrimeSponsor().getSponsorCode(), instProposal.getPrimeSponsor().getSponsorName(), instProposal.getPrimeSponsor().getAcronym()));
		}
	}

	private List<InstituteProposalKeywords> copyIPKeyWords(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		List<InstituteProposalKeywords> newIPKeywords = new ArrayList<>();
		List<InstituteProposalKeywords> instituteProposalKeywords = institutionalProposalDao.fetchAllIPKeywordByProposal(proposalId);
		instituteProposalKeywords.forEach(proposalKeywords -> {
			InstituteProposalKeywords newProposalKeyword = new InstituteProposalKeywords();
			BeanUtils.copyProperties(proposalKeywords, newProposalKeyword);
			newProposalKeyword.setKeywordId(null);
			newProposalKeyword.setInstProposal(institutionalProposalDao.fetchInstProposalById(copyProposalId));
			newProposalKeyword.setSequenceNumber(sequenceNumber);
			institutionalProposalDao.saveOrUpdateIPKeyword(newProposalKeyword);
			newIPKeywords.add(newProposalKeyword);
		});
		return newIPKeywords;
	}

	private void saveActionLogDetails(Integer proposalId, String actionType, String message) {
		InstituteProposalActionLog actionLog = new InstituteProposalActionLog();
		actionLog.setActionTypeCode(actionType);
		actionLog.setDescription(message);
		actionLog.setProposalId(proposalId);
		institutionalProposalDao.saveOrUpdateActionLog(actionLog);
	}

	private void logDetailAsHistory(Integer proposalId, Integer copyProposalId, Integer devProposalId) {
		InstituteProposalVersionHistory history = new InstituteProposalVersionHistory();
		history.setActiveProposalId(proposalId);
		history.setOriginatedProposalId(copyProposalId);
		history.setRequestType(devProposalId ==null ? IP_MODIFICATION : IP_MERGE);
		institutionalProposalDao.saveIPVersionHistory(history);
	}

	private List<Integer> copyPropoosalAdminDetails(Integer proposalId, Integer copyProposalId) {
		List<Integer> devPorposalIds = new ArrayList<>();
		List<InstituteProposalAdminDetail> instituteProposalAdminDetails = institutionalProposalDao.loadAllProposalAdminDetails(proposalId);
		instituteProposalAdminDetails.forEach(instituteProposalAdminDetail -> {
			InstituteProposalAdminDetail copyIPAdminDetail = new InstituteProposalAdminDetail();
			BeanUtils.copyProperties(instituteProposalAdminDetail, copyIPAdminDetail);
			copyIPAdminDetail.setProposalAdminDetailId(null);
			copyIPAdminDetail.setInstProposalId(copyProposalId);
			institutionalProposalDao.saveOrUpdateIPAdminDetail(copyIPAdminDetail);
			devPorposalIds.add(copyIPAdminDetail.getDevProposalId());
		});
		Collections.reverse(devPorposalIds);
		return devPorposalIds;
	}

	private List<InstituteProposalAttachments> copyIPAttchments(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		List<InstituteProposalAttachments> newIPAttachments = new ArrayList<>();
		List<InstituteProposalAttachments> instituteProposalAttachments = institutionalProposalDao.getInstituteProposalAttachments(proposalId);
		instituteProposalAttachments.forEach(instituteProposalAttachment -> {
			InstituteProposalAttachments copyIPAttachment = new InstituteProposalAttachments();
			BeanUtils.copyProperties(instituteProposalAttachment, copyIPAttachment);
			copyIPAttachment.setAttachmentId(null);
			copyIPAttachment.setProposalId(copyProposalId);
			FileData fileData = commonDao.getFileDataById(copyIPAttachment.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			copyIPAttachment.setFileDataId(file.getFileDataId());
			institutionalProposalDao.saveOrUpdateIPAttachment(copyIPAttachment);
			newIPAttachments.add(copyIPAttachment);
		});
		return newIPAttachments;
	}

	private InstituteProposalBudgetHeader copyIPBudgetDetails(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		InstituteProposalBudgetHeader instituteProposalBudgetHeader = institutionalProposalDao.loadInstPropBudgetDetailsByProposalId(proposalId);
		InstituteProposalBudgetHeader copyIPBudgetHeader = new InstituteProposalBudgetHeader();
		if (instituteProposalBudgetHeader != null) {
			BeanUtils.copyProperties(instituteProposalBudgetHeader, copyIPBudgetHeader);
			copyIPBudgetHeader.setBudgetId(null);
			copyIPBudgetHeader.setProposalId(copyProposalId);
			List<InstituteProposalBudgetPeriod> instPropBudgetPeriods = new ArrayList<>();
			copyIPBudgetHeader.getBudgetPeriods().forEach(instPropBudgetPeriod -> {
				InstituteProposalBudgetPeriod copyIPBudgetPeriod = new InstituteProposalBudgetPeriod();
				BeanUtils.copyProperties(instPropBudgetPeriod, copyIPBudgetPeriod);
				copyIPBudgetPeriod.setBudgetPeriodId(null);
				copyIPBudgetPeriod.setBudgetHeader(copyIPBudgetHeader);
				instPropBudgetPeriods.add(copyIPBudgetPeriod);
			});
			copyIPBudgetHeader.setBudgetPeriods(instPropBudgetPeriods);
			institutionalProposalDao.saveOrUpdateIPBudgetDetails(copyIPBudgetHeader);
		}
		return copyIPBudgetHeader;
		
	}

	private List<InstituteProposalResearchArea> copyIPResearchAreas(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		List<InstituteProposalResearchArea> newIPResearchAreas = new ArrayList<>();
		List<InstituteProposalResearchArea> instituteProposalResearchAreas = institutionalProposalDao.loadInstProposalResearchAreaByProposalId(proposalId);
		instituteProposalResearchAreas.forEach(instituteProposalResearchArea -> {
			InstituteProposalResearchArea copyIPResearchArea = new InstituteProposalResearchArea();
			BeanUtils.copyProperties(instituteProposalResearchArea, copyIPResearchArea);
			copyIPResearchArea.setResearchAreaId(null);
			copyIPResearchArea.setProposalId(copyProposalId);
			copyIPResearchArea.setSequenceNumber(sequenceNumber);
			institutionalProposalDao.saveOrUpdateIPResearchAreas(copyIPResearchArea);
			newIPResearchAreas.add(copyIPResearchArea);
		});
		return newIPResearchAreas;
	}

	private List<InstituteProposalSpecialReview> copyIPSpecialReview(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		List<InstituteProposalSpecialReview> newIPSpecialReviews = new ArrayList<>();
		List<InstituteProposalSpecialReview> instituteProposalSpecialReviews = institutionalProposalDao.loadInstProposalSpecialReviewByProposalId(proposalId);
		Map<String, String> nonEmployees = new HashMap<>();
		Map<String, String> persons = new HashMap<>();
		instituteProposalSpecialReviews.forEach(instituteProposalSpecialReview -> {
			InstituteProposalSpecialReview copyIPSpecialReview = new InstituteProposalSpecialReview();
			BeanUtils.copyProperties(instituteProposalSpecialReview, copyIPSpecialReview);
			copyIPSpecialReview.setSpecialReviewId(null);
			copyIPSpecialReview.setProposalId(copyProposalId);
			copyIPSpecialReview.setSequenceNumber(sequenceNumber);
			institutionalProposalDao.saveOrUpdateSpecialReview(copyIPSpecialReview);
			newIPSpecialReviews.add(setInsSpecialReviewDetail(persons, nonEmployees, copyIPSpecialReview));
		});
		return newIPSpecialReviews;
	}

	private void copyIPComments(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		List<InstituteProposalComment> instituteProposalComments = institutionalProposalDao.fetchInstituteProposalCommentsByParams(proposalId);
		instituteProposalComments.forEach(instituteProposalComment -> {
			InstituteProposalComment copyIPcomment = new InstituteProposalComment();
			BeanUtils.copyProperties(instituteProposalComment, copyIPcomment);
			copyIPcomment.setComments(instituteProposalComment.getComment());
			copyIPcomment.setProposalCommentId(null);
			copyIPcomment.setProposalId(copyProposalId);
			copyIPcomment.setSequenceNumber(sequenceNumber);
			institutionalProposalDao.saveOrUpdateInstituteProposalComment(copyIPcomment);
		});
	}
	
	private void copyIPGeneralDetails(Integer proposalId, InstituteProposal copyInstProposal) {
		InstituteProposal instProposal = institutionalProposalDao.fetchInstProposalById(proposalId);
		BeanUtils.copyProperties(instProposal, copyInstProposal);
		copyInstProposal.setProposalId(null);
		copyInstProposal.setInstProposalKeywords(new ArrayList<>());
		copyInstProposal.setProposalSequenceStatus("PENDING");
		Integer sequenceNumber = institutionalProposalDao.getNextSequenceNumber(copyInstProposal.getProposalNumber());
		copyInstProposal.setSequenceNumber(sequenceNumber+1);
		institutionalProposalDao.saveOrUpdateInstituteProposal(copyInstProposal);
		institutionalProposalDao.updateSequenceStatus(proposalId, "ARCHIVE");
	}

	private List<InstituteProposalPerson> copyIPPersonDetails(Integer proposalId, Integer copyProposalId, Integer sequenceNumber) {
		List<InstituteProposalPerson> newIPPersons = new ArrayList<>();
		List<InstituteProposalPerson> instituteProposalPersons = institutionalProposalDao.loadInstProposalPersonsByProposalId(proposalId);
		instituteProposalPersons.forEach(instituteProposalPerson -> {
			InstituteProposalPerson copyIPPerson = new InstituteProposalPerson();
			BeanUtils.copyProperties(instituteProposalPerson, copyIPPerson);
			copyIPPerson.setProposalPersonId(null);
			copyIPPerson.setProposalId(copyProposalId);
			copyIPPerson.setSequenceNumber(sequenceNumber);
			List<InstituteProposalPersonUnit> instituteProposalPersonUnits = new ArrayList<>();
			copyIPPerson.getUnits().forEach(instPropPersonUnit -> {
				InstituteProposalPersonUnit copyInstPropPersonUnit = new InstituteProposalPersonUnit();
				BeanUtils.copyProperties(instPropPersonUnit, copyInstPropPersonUnit);
				copyInstPropPersonUnit.setPropPersonUnitId(null);
				copyInstPropPersonUnit.setInstProposalPerson(copyIPPerson);
				copyInstPropPersonUnit.setSequenceNumber(sequenceNumber);
				instituteProposalPersonUnits.add(copyInstPropPersonUnit);
			});
			copyIPPerson.setUnits(instituteProposalPersonUnits);
			List<InstituteProposalPersonAttachment> instituteProposalPersonAttachments = new ArrayList<>();
			copyIPPerson.getProposalPersonAttachment().forEach(instPropPersonAttachment -> {
				InstituteProposalPersonAttachment copyIPPersonAttachment = new InstituteProposalPersonAttachment();
				BeanUtils.copyProperties(instPropPersonAttachment, copyIPPersonAttachment);
				copyIPPersonAttachment.setAttachmentId(null);
				copyIPPersonAttachment.setInstProposalPerson(copyIPPerson);
				FileData fileData = commonDao.getFileDataById(copyIPPersonAttachment.getFileDataId());
				FileData file = new FileData();
				file.setAttachment(fileData.getAttachment());
				file = commonDao.saveFileData(file);
				copyIPPersonAttachment.setFileDataId(file.getFileDataId());
				instituteProposalPersonAttachments.add(copyIPPersonAttachment);
			});
			copyIPPerson.setProposalPersonAttachment(instituteProposalPersonAttachments);
			institutionalProposalDao.saveOrUpdateInstituteProposalPerson(copyIPPerson);
			newIPPersons.add(copyIPPerson);
		});
		return newIPPersons;
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String changeIPStatus(InstProposalVO vo) throws ApplicationException {
		try {
			InstituteProposal instituteProposal = institutionalProposalDao.fetchInstProposalById(vo.getProposalId());
			InstituteProposalActionType instituteProposalActionType = vo.getInstituteProposalActionType();
			instituteProposal.setStatusCode(instituteProposalActionType.getInstProposalStatus().getStatusCode());
			institutionalProposalDao.saveOrUpdateInstituteProposal(instituteProposal);
			saveActionLogDetails(vo.getProposalId(), instituteProposalActionType.getActionTypeCode(), vo.getDescription());
			updateProposalTimestampAndUser(vo.getProposalId());
		} catch (Exception e) {
			logger.error("error occured in changeIPStatus : {}", e.getMessage());
			throw new ApplicationException("error occuered while changeIPStatus", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String saveOrUpdateIPKeyPerson(InstProposalVO vo) throws ApplicationException {
		try {
			String loginUserName = AuthenticatedUser.getLoginUserName();
			Timestamp currentTime = commonDao.getCurrentTimestamp();
			InstituteProposalPerson instituteProposalPerson = vo.getInstituteProposalPerson();
			InstituteProposal instituteProposal = institutionalProposalDao.fetchInstProposalById(instituteProposalPerson.getProposalId());
			List<InstituteProposalPersonUnit> instProposalPersonUnits = instituteProposalPerson.getUnits();
			if (instituteProposalPerson.getIsPi().equals(Boolean.TRUE) || instituteProposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE) && (instituteProposalPerson.getUnits() != null && !instituteProposalPerson.getUnits().isEmpty())) {
				instituteProposalPerson.getUnits().forEach(proposalPersonUnit -> {
					if (proposalPersonUnit.isLeadUnit()) {
						instituteProposal.setHomeUnitNumber(proposalPersonUnit.getUnitNumber());
						instituteProposal.setHomeUnitName(commonDao.getLeadUnitByUnitNumber(proposalPersonUnit.getUnitNumber()).getUnitName());
						instituteProposal.setUpdateUser(loginUserName);
						instituteProposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						institutionalProposalDao.saveOrUpdateInstituteProposal(instituteProposal);
					}
				});
			}
			if (instProposalPersonUnits != null && !instProposalPersonUnits.isEmpty()) {
				List<InstituteProposalPersonUnit> updatedInstProposalPersonUnits = new ArrayList<>(instProposalPersonUnits);
				Collections.copy(updatedInstProposalPersonUnits, instProposalPersonUnits);
				instProposalPersonUnits.forEach(proposalPersonUnit -> {
					proposalPersonUnit.setUpdateTimeStamp(currentTime);
					proposalPersonUnit.setUpdateUser(loginUserName);
					if (proposalPersonUnit.getIsDeleted().equals(Boolean.TRUE)) {
						institutionalProposalDao.deleteInstProposalPersonUnit(proposalPersonUnit);
						updatedInstProposalPersonUnits.remove(proposalPersonUnit);
					}
				});
				instituteProposalPerson.getUnits().clear();
				instituteProposalPerson.getUnits().addAll(updatedInstProposalPersonUnits);
			}
			instituteProposalPerson.setUpdateTimeStamp(currentTime);
			instituteProposalPerson.setUpdateUser(loginUserName);
			institutionalProposalDao.saveOrUpdateInstituteProposalPerson(instituteProposalPerson);
			updateProposalTimestampAndUser(instituteProposalPerson.getProposalId());
		} catch (Exception e) {
			logger.error("error occured in saveOrUpdateIPKeyPerson : {}", e.getMessage());
			throw new ApplicationException("error occured in saveOrUpdateIPKeyPerson", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String addIPPersonAttachment(MultipartFile[] files, String formDataJSON) throws ApplicationException {
		InstProposalVO vo = null;
		List<InstituteProposalPersonAttachment> instPropPersonAttachments = new ArrayList<>();
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, InstProposalVO.class);
			List<InstituteProposalPersonAttachment> newAttachments = vo.getNewIPPersonAttachments();
			if(files.length == 0 && newAttachments != null && !newAttachments.isEmpty()) {
				for (InstituteProposalPersonAttachment newAttachment : newAttachments) {
					if (newAttachment.getReplaceAttachmentId() != null) {
						institutionalProposalDao.deleteIPPersonAttachment(newAttachment.getReplaceAttachmentId());
					}
				}
			} else {
				for (int index = 0; index < files.length; index++) {
					for (InstituteProposalPersonAttachment newAttachment : newAttachments) {
						if (newAttachment.getReplaceAttachmentId() != null) {
							institutionalProposalDao.deleteIPPersonAttachment(newAttachment.getReplaceAttachmentId());
						}
						InstituteProposalPersonAttachment proposalPersonAttachment = new InstituteProposalPersonAttachment();
						proposalPersonAttachment.setFileName(newAttachment.getFileName());
						proposalPersonAttachment.setDescription(newAttachment.getDescription());
						proposalPersonAttachment.setMimeType(newAttachment.getMimeType());
						FileData fileData = new FileData();
						fileData.setAttachment(files[index].getBytes());
						fileData = commonDao.saveFileData(fileData);
						proposalPersonAttachment.setFileDataId(fileData.getFileDataId());
						proposalPersonAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
						proposalPersonAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						instPropPersonAttachments.add(proposalPersonAttachment);
					}
				}
			}
			vo.setNewIPPersonAttachments(instPropPersonAttachments);
		} catch (Exception e) {
			logger.error("exception in addIPPersonAttachment: {} ", e.getMessage());
			throw new ApplicationException("error occured in addIPPersonAttachment", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String saveOrUpdateIPSpecialReview(InstProposalVO vo) throws ApplicationException {
		try {
			InstituteProposalSpecialReview instituteProposalSpecialReview = vo.getInstituteProposalSpecialReview();
			instituteProposalSpecialReview.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			instituteProposalSpecialReview.setUpdateUser(AuthenticatedUser.getLoginUserName());
			institutionalProposalDao.saveOrUpdateIPSpecialReview(instituteProposalSpecialReview);
			updateProposalTimestampAndUser(instituteProposalSpecialReview.getProposalId());
			Map<String, String> nonEmployees = new HashMap<>();
			Map<String, String> persons = new HashMap<>();
			vo.setInstituteProposalSpecialReview(setInsSpecialReviewDetail(persons, nonEmployees, instituteProposalSpecialReview));
		} catch (Exception e) {
			logger.error("error occured in saveOrUpdateIPSpecialReview : {}", e.getMessage());
			throw new ApplicationException("error occured in saveOrUpdateIPSpecialReview", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String saveOrUpdateIPAreaOfResearch(InstProposalVO vo) throws ApplicationException {
		try {
			InstituteProposalResearchArea instituteProposalResearchArea = vo.getInstituteProposalResearchArea();
			instituteProposalResearchArea.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			instituteProposalResearchArea.setUpdateUser(AuthenticatedUser.getLoginUserName());
			institutionalProposalDao.saveOrUpdateIPAreaOfResearch(instituteProposalResearchArea);
			updateProposalTimestampAndUser(instituteProposalResearchArea.getProposalId());
		} catch (Exception e) {
			logger.error("error occured in saveOrUpdateIPAreaOfResearch : {}", e.getMessage());
			throw new ApplicationException("error occured in saveOrUpdateIPAreaOfResearch", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String saveOrUpdateIPBudget(InstProposalVO vo) throws ApplicationException {
		try {
		InstituteProposalBudgetHeader instituteProposalBudgetHeader = vo.getInstituteProposalBudgetHeader();
		instituteProposalBudgetHeader.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		instituteProposalBudgetHeader.setUpdateUser(AuthenticatedUser.getLoginUserName());
		institutionalProposalDao.saveOrUpdateIPBudget(instituteProposalBudgetHeader);
		updateProposalTimestampAndUser(instituteProposalBudgetHeader.getProposalId());
		} catch (Exception e) {
			logger.error("error occured in saveOrUpdateIPBudget : {}", e.getMessage());
			throw new ApplicationException("error occured in saveOrUpdateIPBudget", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteIPSpecialReview(Integer specialReviewId, Integer proposalId) {
		try {
			institutionalProposalDao.deleteIPSpecialReview(specialReviewId);
			updateProposalTimestampAndUser(proposalId);
			return commonDao.convertObjectToJSON("successfully deleted");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("failed to delete");
		}
	}

	@Override
	public String deleteIPAreaOfResearch(Integer areaOfResearchId, Integer proposalId) {
		try {
			institutionalProposalDao.deleteIPAreaOfResearch(areaOfResearchId);
			updateProposalTimestampAndUser(proposalId);
			return commonDao.convertObjectToJSON("successfully deleted");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("failed to delete");
		}
	}

	@Override
	public String deleteIPBudgetData(Integer budgetHeaderId, Integer proposalId) {
		try {
			institutionalProposalDao.deleteIPBudgetData(budgetHeaderId);
			updateProposalTimestampAndUser(proposalId);
			return commonDao.convertObjectToJSON("successfully deleted");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("failed to delete");
		}
	}

	@Override
	public String deleteIPKeyPerson(Integer keyPersonId, Integer proposalId) {
		try {
			institutionalProposalDao.deleteIPKeyPerson(keyPersonId);
			updateProposalTimestampAndUser(proposalId);
			return commonDao.convertObjectToJSON("successfully deleted");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("failed to delete");
		}	
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String addIPAttachment(MultipartFile[] files, String formDataJSON) throws ApplicationException {
		InstProposalVO instProposalVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			instProposalVO = mapper.readValue(formDataJSON, InstProposalVO.class);
			List<InstituteProposalAttachments> instPropoAttachments = institutionalProposalDao.getInstituteProposalAttachments(instProposalVO.getProposalId());
			Integer documentId = (instPropoAttachments != null && !instPropoAttachments.isEmpty()) ? 
					instPropoAttachments.stream().map(InstituteProposalAttachments :: getDocumentId).collect(Collectors.toList()).stream().mapToInt(v -> v).max().getAsInt() : 0;
			List<InstituteProposalAttachments> newAttachments = instProposalVO.getInstituteProposalAttachments();
			Integer versionNumber = 0;
			for (int i = 0; i < files.length; i++) {
				for (InstituteProposalAttachments newAttachment : newAttachments) {
					File file = new File(files[i].getOriginalFilename());
					String fileName = file.getName();
					if (newAttachment.getFileName().equals(fileName)) {
					String replaceFileName = newAttachment.getFileName();
					int count = checkForDuplicationCount(newAttachment.getFileName(), instPropoAttachments);
					if( count > 0) {
						 replaceFileName = generateFileName(replaceFileName, count);
					}
					if (newAttachment.getAttachmentId() != null) {
							Optional<InstituteProposalAttachments> optAttachment= instPropoAttachments.stream().filter(x -> x.getAttachmentId().equals(newAttachment.getAttachmentId())).findFirst();
							if (optAttachment.isPresent()) {
								InstituteProposalAttachments attachment = optAttachment.get();
							if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
								attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
								attachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
								versionNumber = attachment.getVersionNumber();
								documentId = attachment.getDocumentId();
								InstituteProposalAttachments instAttachment = addNewInstProposalAttachment(newAttachment, files[i], versionNumber, documentId, replaceFileName);
								institutionalProposalDao.saveOrUpdateIPAttachment(instAttachment);
							}
						}
					} else {
							documentId = documentId + 1;
							InstituteProposalAttachments instAttachment = addNewInstProposalAttachment(newAttachment, files[i], versionNumber, documentId, replaceFileName);
							institutionalProposalDao.saveOrUpdateIPAttachment(instAttachment);
							i++;
					}
					}
				}
			}
			instProposalVO.setInstituteProposalAttachments(institutionalProposalDao.getInstituteProposalAttachments(instProposalVO.getProposalId()));
			updateProposalTimestampAndUser(instProposalVO.getProposalId());
		} catch (Exception e) {
			logger.error("exception in addProposalAttachment: {} ", e.getMessage());
			throw new ApplicationException("error occured in addProposalAttachment", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(instProposalVO);
	}

	
	public InstituteProposalAttachments addNewInstProposalAttachment(InstituteProposalAttachments attachment, MultipartFile file, Integer versionNumber, Integer documentId, String replacedFileName) {
		InstituteProposalAttachments instituteProposalAttachment = new InstituteProposalAttachments();
		BeanUtils.copyProperties(attachment, instituteProposalAttachment);
		try {
			instituteProposalAttachment.setAttachmentId(null);
			instituteProposalAttachment.setFileName(replacedFileName);
			instituteProposalAttachment.setMimeType(file.getContentType());
			instituteProposalAttachment.setVersionNumber(versionNumber + 1);
			FileData fileData = new FileData();
			fileData.setAttachment(file.getBytes());
			fileData = commonDao.saveFileData(fileData);
			instituteProposalAttachment.setNarrativeStatus(commonDao.getNarrativeStatusByCode(attachment.getNarrativeStatusCode()));
			instituteProposalAttachment.setFileDataId(fileData.getFileDataId());
			instituteProposalAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
			instituteProposalAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
			instituteProposalAttachment.setDocumentId(documentId);
			instituteProposalAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			instituteProposalAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
		} catch (Exception e) {
			logger.error("exception in addNewProposalAttachment: {} ", e.getMessage());
		}
		return instituteProposalAttachment;
	}

	private int checkForDuplicationCount(String fileName, List<InstituteProposalAttachments> instPropoAttachments) {
		List<String> fileNames = instPropoAttachments.stream().map(InstituteProposalAttachments :: getFileName).collect(Collectors.toList());
		return Collections.frequency(fileNames, fileName);
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
		}

	@Override
	public String deleteIPAttachment(Integer attachmentId, Integer proposalId) {
		try {
			institutionalProposalDao.deleteIPAttachment(attachmentId);
			updateProposalTimestampAndUser(proposalId);
			return commonDao.convertObjectToJSON("successfully deleted");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("failed to delete");
		}
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public String updateIPAttachmentDetails(InstProposalVO vo) throws ApplicationException {
		InstituteProposalAttachments newInstPropoAttachment = vo.getInstituteProposalAttachment();
		try {
			InstituteProposalAttachments instPropoAttachment = institutionalProposalDao.fetchIPAttachmentById(newInstPropoAttachment.getAttachmentId());
			instPropoAttachment.setDescription(newInstPropoAttachment.getDescription());
			instPropoAttachment.setNarrativeStatusCode(newInstPropoAttachment.getNarrativeStatusCode());
			instPropoAttachment.setNarrativeStatus(newInstPropoAttachment.getNarrativeStatus());
			instPropoAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			instPropoAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			institutionalProposalDao.saveOrUpdateIPAttachment(instPropoAttachment);
		} catch (Exception e) {
			logger.error("error occured in updateIPAttachmentDetails : {}", e.getMessage());
			throw new ApplicationException("error occured in updateIPAttachmentDetails", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(institutionalProposalDao.getInstituteProposalAttachments(newInstPropoAttachment.getProposalId()));
	}

	@Override
	public String submitInstituteProposal(InstProposalVO vo) throws ApplicationException {
		try {
		institutionalProposalDao.updateSequenceStatus(vo.getProposalId(), "ACTIVE");
		saveActionLogDetails(vo.getProposalId(), SUBMIT_IP, null);
		updateProposalTimestampAndUser(vo.getProposalId());
		} catch (Exception e) {
			logger.error("error occured in submitInstituteProposal : {}", e.getMessage());
			throw new ApplicationException("error occured in updateIPAttachmentDetails", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON("Success");
	}

	@Override
	public String deleteIPKeyword(Integer keywordId, Integer proposalId) {
		try {
		institutionalProposalDao.deleteIPKeyword(keywordId);
		updateProposalTimestampAndUser(proposalId);
		return commonDao.convertObjectToJSON("successfully deleted");
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("failed to delete");
		}
	}

	private void updateProposalTimestampAndUser(Integer proposalId) {
		institutionalProposalDao.updateProposalTimestampAndUser(proposalId);
	}

	@Override
	public String loadIPBudgetsByProposalId(Integer proposalId) {
		InstProposalVO instProposalVO = new InstProposalVO();
		InstituteProposalBudgetHeader budgetHeader = institutionalProposalDao.loadInstPropBudgetDetailsByProposalId(proposalId);
		if (budgetHeader != null) {
			if (budgetHeader.getUpdateUser() != null) {
				budgetHeader.setUpdateUserName(personDao.getUserFullNameByUserName(budgetHeader.getUpdateUser()));
			}
			instProposalVO.setBudgetStatus(budgetDao.fetchAllBudgetStatus());
			instProposalVO.setCostShareTypeCode(budgetHeader.getCostSharingTypeCode());
			String ohRateClassTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE);
			boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
			instProposalVO.setEnableCostShareStatus(isEnableCostShareStatus);
			if (isEnableCostShareStatus) {
				instProposalVO.setCostSharingTypes(budgetDao.getCostSharingType());
			}
			instProposalVO.setRateTypes(budgetDao.fetchRateTypeByParams("1", budgetDao.fetchRateClassCodesByType(ohRateClassTypeCode)));
			fetchBudgetParameterValues(instProposalVO);	
		}
		instProposalVO.setInstituteProposalBudgetHeader(budgetHeader);
		return commonDao.convertObjectToJSON(instProposalVO);
	}
	
	private void fetchBudgetParameterValues(InstProposalVO vo) {
		vo.setIsShowCostShareAndUnderrecovery(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_COST_SHARE_AND_UNDERRECOVERY));
		vo.setIsShowInKind(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND));
		vo.setIsShowModifiedDirectCost(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_MODIFIED_DIRECT_COST));
		vo.setIsCampusFlagEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_PROPOSAL));
		vo.setShowBudgetOHRatePercentage(commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
	}

	@Override
	public String saveIPMoreInformation(InstProposalVO vo) {
		InstituteProposal instituteProposal = institutionalProposalDao.fetchInstProposalById(vo.getProposalId());
		instituteProposal.setResearchDescription(vo.getResearchDescription());
		instituteProposal.setMultiDisciplinaryDescription(vo.getMultiDisciplinaryDescription());
		instituteProposal.setUpdateUser(AuthenticatedUser.getLoginUserName());
		instituteProposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		institutionalProposalDao.saveOrUpdateInstituteProposal(instituteProposal);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getIPActionHistory(Integer proposalId) {
		InstProposalVO vo = new InstProposalVO();
		List<InstituteProposalActionLog> actionLogs = institutionalProposalDao.getIPActionHistory(proposalId);
		Set<String> userName = actionLogs.stream().map(InstituteProposalActionLog::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			actionLogs.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
		vo.setInstituteProposalActionLogs(actionLogs);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	@Transactional(rollbackFor = ApplicationException.class)
	public Integer getCreatedIPVersion(InstProposalVO vo) throws ApplicationException {
		vo.setProposalId(institutionalProposalDao.getActiveInstProposalId(vo.getProposalNumber()));
		createNewIPVersion(vo);
		return vo.getInstProposal().getProposalId();
	}

	@Override
	public String mergeProposalToIP(Integer proposalId, InstProposalVO vo) throws ApplicationException {
		try {
			institutionalProposalDao.mergeProposalToIP(proposalId, vo);
			ProposalVO proposalVO = new ProposalVO();
			proposalVO.setProposal(proposalDao.fetchProposalById(vo.getDevProposalId()));
			return commonDao.convertObjectToJSON(proposalVO);
		} catch (Exception e) {
			logger.error("error occured in mergeProposalToIP");
			throw new ApplicationException("error occured in mergeProposalToIP", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public Boolean canMergeDevelopmentProposalToIP(String proposalNumber) {
		if (institutionalProposalDao.anyAwardLinkedToIP(proposalNumber).equals(Boolean.FALSE)) {
			 return institutionalProposalDao.anyAwardInPendingSequence(proposalNumber);
		}
		return Boolean.FALSE;
	}

	@Override
	public String generateInstituteProposal(ProposalVO vo) {
		Proposal proposal  = proposalDao.fetchProposalById(vo.getProposalId());
		vo.setProposal(proposal);
		vo.setIpGenerationOnly(Boolean.TRUE);
		businessRuleService.generateInstitutionalProposal(vo);
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposal.setUpdateUser(AuthenticatedUser.getLoginUserName());
		proposalDao.saveOrUpdateProposal(proposal);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public Boolean checkForPIDifference(String proposalNumber, Integer devProposalId) {
		return institutionalProposalDao.checkForPIDifference(proposalNumber, devProposalId);
	}

	@Override
	public String getIPTitleForMasterProposal(String baseProposalNumber) {
		return institutionalProposalDao.getIPTitleForMasterProposal(baseProposalNumber);
	}

	@Override
	public String fetchInstituteProposalComments(Integer proposalId) {
		InstProposalVO vo = new InstProposalVO();
		vo.setCommentType(commonDao.fetchCommentTypes(Constants.INSTITUTE_PROPOSAL_MODULE_CODE));
		List<InstituteProposalComment> institueProposalComments = institutionalProposalDao.fetchInstituteProposalCommentsByParams(proposalId);
		getFullNameOfUpdateUser(institueProposalComments);
		vo.setInstituteProposalComments(institueProposalComments);
		return commonDao.convertObjectToJSON(vo);
	}
	
	private void getFullNameOfUpdateUser(List<InstituteProposalComment> institueProposalComments) {
		Set<String> userName = institueProposalComments.stream().map(InstituteProposalComment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			institueProposalComments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}	
	}

	@Override
	public String saveOrUpdateInstituteProposalComment(InstProposalVO vo) {
		InstituteProposalComment comment = institutionalProposalDao.saveOrUpdateInstituteProposalComment(vo.getInstituteProposalComment());
		comment.setFullName(AuthenticatedUser.getLoginUserFullName());
		return commonDao.convertObjectToJSON(comment);
	}

	@Override
	public String showInstituteProposalHistory(InstProposalVO vo) {
		List<Object[]> instituteProposalHistories = institutionalProposalDao.fetchIPHistoryDetails(vo.getProposalNumber());
		List<InstituteProposalHistoryDto> proposalHistory= prepareProposalHistory(instituteProposalHistories, vo.getProposalNumber());
		vo.setParameterValue(getProposalFlagValues());
		getCreateUserFullNames(proposalHistory);
		vo.setInstituteProposalHistories(proposalHistory);
		return commonDao.convertObjectToJSON(vo);
	}

	private List<InstituteProposalHistoryDto> prepareProposalHistory(List<Object[]> historyVOs, String proposalNumber) {
		List<InstituteProposalHistoryDto> proposalHistory = new ArrayList<>();
		for (Object[] history : historyVOs) {
			InstituteProposalHistoryDto proposalHistoryVO = new InstituteProposalHistoryDto();
			proposalHistoryVO.setProposalId(history[0] != null ? Integer.parseInt(history[0].toString()) : null);
			proposalHistoryVO.setRequestType(history[1] != null ? history[1].toString() : "");
			proposalHistoryVO.setSequenceNumber(history[2] != null ? Integer.parseInt(history[2].toString()) : null);
			proposalHistoryVO.setProposalNumber(proposalNumber);
			proposalHistoryVO.setProposalSequenceStatus(history[3] != null ? history[3].toString() : "");
			proposalHistoryVO.setCreateUser(history[4] != null ? history[4].toString() : "");
			proposalHistoryVO.setCreateTimestamp(history[5] != null ? Timestamp.valueOf(history[5].toString()) : null);
			proposalHistory.add(proposalHistoryVO);
		}
		return proposalHistory;
	}

	private void getCreateUserFullNames(List<InstituteProposalHistoryDto> proposalHistories) {
		Set<String> userName = proposalHistories.stream().map(InstituteProposalHistoryDto::getCreateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person::getFullName));
			proposalHistories.stream().filter(item -> item.getCreateUser() != null).filter(proposalHistory -> collect.containsKey(proposalHistory.getCreateUser().toUpperCase())).forEach(proposalHistory -> proposalHistory.setCreateUserFullName(collect.get(proposalHistory.getCreateUser().toUpperCase())));
		}	
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

}
