package com.polus.fibicomp.proposal.service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.common.pojo.ValidCeRateType;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetPersonalDetails;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.budget.service.BudgetService;
import com.polus.fibicomp.budget.vo.BudgetVO;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.service.CustomDataElementService;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.PersonDegree;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalComment;
import com.polus.fibicomp.proposal.pojo.ProposalCommentAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalCongDistrict;
import com.polus.fibicomp.proposal.pojo.ProposalExtension;
import com.polus.fibicomp.proposal.pojo.ProposalHistory;
import com.polus.fibicomp.proposal.pojo.ProposalIrbProtocol;
import com.polus.fibicomp.proposal.pojo.ProposalKPI;
import com.polus.fibicomp.proposal.pojo.ProposalKPICriteria;
import com.polus.fibicomp.proposal.pojo.ProposalKeyword;
import com.polus.fibicomp.proposal.pojo.ProposalMileStone;
import com.polus.fibicomp.proposal.pojo.ProposalOrganization;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.pojo.ProposalPersonAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalPersonDegree;
import com.polus.fibicomp.proposal.pojo.ProposalPersonUnit;
import com.polus.fibicomp.proposal.pojo.ProposalProjectTeam;
import com.polus.fibicomp.proposal.pojo.ProposalResearchArea;
import com.polus.fibicomp.proposal.pojo.ProposalSponsor;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "proposalCopyService")
public class ProposalCopyServiceImpl implements ProposalCopyService {

	protected static Logger logger = LogManager.getLogger(ProposalCopyServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private BudgetService budgetService;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	@Qualifier(value = "rolesManagement")
	private RolesManagementDao rolesManagementDao;

	@Override
	public String copyProposal(ProposalVO vo) {
		vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		Proposal originalProposal = proposalDao.fetchProposalById(vo.getProposalId());
		ProposalExtension originalProposalExtension = proposalDao.fetchProposalExtensionById(vo.getProposalId());
		Proposal copyProposal = new Proposal();
		ProposalExtension copyProposalExtension = new ProposalExtension();
		copyProposal = copyProposalMandatoryFields(vo, copyProposal, originalProposal);
		copyProposalNonMandatoryFields(vo, copyProposal, originalProposal, copyProposalExtension, originalProposalExtension);
		vo.setStatus(true);
		vo.setMessage("Proposal data copied successfully");
		vo.setProposal(copyProposal);
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
			proposalService.fetchAndSaveProposalEvaluationPanels(vo);
		}
		if (Boolean.TRUE.equals(vo.getIsProposalArchiveCreation())) {
			if (Boolean.TRUE.equals(vo.getIsProposalAdminCorrection())) {
				createProposalHistoryDetails(originalProposal.getProposalId(),copyProposal.getProposalId(), Constants.PROPOSAL_ADMIN_CORRECTION);
				copyProposalComments(originalProposal, copyProposal);
				originalProposal.setDocumentStatusCode(Constants.PROPOSAL_DOCUMENT_STATUS_ADMIN_CORRECTION);
				originalProposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				originalProposal.setUpdateUser(AuthenticatedUser.getLoginUserName());
				proposalDao.saveOrUpdateProposal(originalProposal);
			} else {
				createProposalHistoryDetails(originalProposal.getProposalId(),copyProposal.getProposalId(), Constants.PROPOSAL_RETURNED);
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void copyProposalComments(Proposal originalProposal, Proposal copyProposal) {
		List<ProposalComment> proposalComments = proposalDao.fetchProposalCommentsByParams(originalProposal.getProposalId(), AuthenticatedUser.getLoginUserName(), Boolean.TRUE);
		proposalComments.forEach(proposalComment -> {
			ProposalComment comment = new ProposalComment();
			List<ProposalCommentAttachment> commentAttachments = new ArrayList<>();
			List<ProposalCommentAttachment> proposalCommentAttachments = proposalComment.getProposalCommentAttachments();
			BeanUtils.copyProperties(proposalComment, comment);
			comment.setProposalId(copyProposal.getProposalId());
			comment.setProposal(copyProposal);
			comment.setProposalCommentId(null);
			proposalDao.saveOrUpdateProposalComment(comment);
			proposalCommentAttachments.forEach(proposalCommentAttachment -> {
				ProposalCommentAttachment attachment = new ProposalCommentAttachment();
				BeanUtils.copyProperties(proposalCommentAttachment, attachment);
				attachment.setProposalCommentId(comment.getProposalCommentId());
				attachment.setCommentAttachmentId(null);
				attachment.setProposalComment(comment);
				proposalDao.saveOrUpdateProposalCommentAttachment(attachment);
				commentAttachments.add(attachment);
			});
			comment.setProposalCommentAttachments(commentAttachments);
		});
	}

	private Proposal copyProposalMandatoryFields(ProposalVO proposalVO, Proposal copyProposal, Proposal originalProposal) {
		copyProposal.setTitle(originalProposal.getTitle());
		copyProposal.setActivityTypeCode(originalProposal.getActivityTypeCode());
		copyProposal.setActivityType(originalProposal.getActivityType());
		copyProposal.setTypeCode(originalProposal.getTypeCode());
		copyProposal.setProposalType(originalProposal.getProposalType());
		copyProposal.setHomeUnitNumber(originalProposal.getHomeUnitNumber());
		copyProposal.setHomeUnitName(originalProposal.getUnit().getUnitName());
		if (originalProposal.getSponsorCode() != null) {
			copyProposal.setSponsorCode(originalProposal.getSponsorCode());
			copyProposal.setSponsor(commonDao.getSponsorById(originalProposal.getSponsorCode()));
		}
		copyProposal.setStartDate(originalProposal.getStartDate());
		copyProposal.setEndDate(originalProposal.getEndDate());
		copyProposal.setDuration(originalProposal.getDuration());
		copyProposal.setSubmissionDate(originalProposal.getSubmissionDate());
		copyProposal.setInternalDeadLineDate(originalProposal.getInternalDeadLineDate());
		copyProposal.setGrantCallClosingDate(originalProposal.getGrantCallClosingDate());
		copyProposal.setGrantCallName(originalProposal.getGrantCallName());
		copyProposal.setAwardNumber(originalProposal.getAwardNumber());
		copyProposal.setCreateUser(proposalVO.getUpdateUser());
		copyProposal.setUpdateUser(proposalVO.getUpdateUser());
		if (Boolean.TRUE.equals(proposalVO.getIsProposalArchiveCreation())) {
			copyProposal.setDocumentStatusCode(Constants.PROPOSAL_DOCUMENT_STATUS_ARCHIVE);
		} else {
			copyProposal.setDocumentStatusCode(Constants.PROPOSAL_DOCUMENT_STATUS_ACTIVE);
			copyProposal.setSourceProposalId(originalProposal.getProposalId());
			copyProposal.setSourceProposalTitle(originalProposal.getTitle());
		}
		copyProposal = proposalDao.saveOrUpdateProposal(copyProposal);
		List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(originalProposal.getProposalId());
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			proposalVO.setProposalPersons(copyProposalPersons(copyProposal.getProposalId(), proposalVO.getUpdateUser(), proposalPersons, proposalVO.getIsProposalArchiveCreation()));
		}
		copyProposal = proposalDao.saveOrUpdateProposal(copyProposal);
		copyProposal.setProposalPersons(proposalVO.getProposalPersons());
		if (Boolean.FALSE.equals(proposalVO.getIsProposalArchiveCreation())) {
			copyProposalPersonRoles(copyProposal);
		}
		return copyProposal;
	}

	private List<ProposalPerson> copyProposalPersons(Integer proposalId, String updateUser, List<ProposalPerson> proposalPersons, Boolean isProposalArchiveCreation) {
		List<ProposalPerson> newProposalPersons = new ArrayList<>();
		for (ProposalPerson copiedPersonDetail : proposalPersons) {
			ProposalPerson personDetail = new ProposalPerson();
			personDetail.setProposalId(proposalId);
			personDetail.setPercentageOfEffort(copiedPersonDetail.getPercentageOfEffort());
			personDetail.setPersonId(copiedPersonDetail.getPersonId());
			personDetail.setRolodexId(copiedPersonDetail.getRolodexId());
			personDetail.setFullName(copiedPersonDetail.getFullName());
			personDetail.setPersonRoleId(copiedPersonDetail.getPersonRoleId());
			personDetail.setProposalPersonRole(copiedPersonDetail.getProposalPersonRole());
			personDetail.setUpdateUser(updateUser);
			personDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			personDetail.setEmailAddress(copiedPersonDetail.getEmailAddress());
			personDetail.setIsPi(copiedPersonDetail.getIsPi());
			personDetail.setIsMultiPi(copiedPersonDetail.getIsMultiPi());
			personDetail.setDesignation(copiedPersonDetail.getDesignation());
			personDetail.setDepartment(copiedPersonDetail.getDepartment());
			personDetail.setProjectRole(copiedPersonDetail.getProjectRole());
			if (Boolean.TRUE.equals(isProposalArchiveCreation)) {
				personDetail.setPersonCertified(copiedPersonDetail.getPersonCertified());
			}
			List<ProposalPersonUnit> units = copiedPersonDetail.getUnits();
			if (units != null && !units.isEmpty()) {
				personDetail.getUnits().addAll(copyProposalPersonUnits(copiedPersonDetail, personDetail, updateUser));
			}
			proposalModuleDao.saveOrUpdateProposalPerson(personDetail);
			copyPersonDegree(personDetail);
			List<ProposalPersonAttachment> personAttachments = copiedPersonDetail.getProposalPersonAttachment();
			if (personAttachments != null && !personAttachments.isEmpty()) {
				personDetail.getProposalPersonAttachment().addAll(copyProposalPersonAttchment(copiedPersonDetail, personDetail, updateUser));
			}
			newProposalPersons.add(personDetail);
		}
		return newProposalPersons;
	}

	private List<ProposalPersonAttachment> copyProposalPersonAttchment(ProposalPerson copiedPersonDetail, ProposalPerson personDetail, String updateUser) {
		List<ProposalPersonAttachment> proposalPersonAttachments = copiedPersonDetail.getProposalPersonAttachment();
		List<ProposalPersonAttachment> newproposalPersonAttachments = new ArrayList<>();
		for (ProposalPersonAttachment copiedProposalPersonAttachment : proposalPersonAttachments) {
			ProposalPersonAttachment attachment = new ProposalPersonAttachment();
			attachment.setProposalPerson(personDetail);
			attachment.setProposalPersonId(attachment.getProposalPerson().getProposalPersonId());
			attachment.setDescription(copiedProposalPersonAttachment.getDescription());
			attachment.setFileName(copiedProposalPersonAttachment.getFileName());
			attachment.setMimeType(copiedProposalPersonAttachment.getMimeType());
			attachment.setAttachmentTypeCode(copiedProposalPersonAttachment.getAttachmentTypeCode());
			attachment.setDocumentId(copiedProposalPersonAttachment.getDocumentId());
			attachment.setDocumentStatusCode(copiedProposalPersonAttachment.getDocumentStatusCode());
			attachment.setVersionNumber(copiedProposalPersonAttachment.getVersionNumber());
			FileData fileData = commonDao.getFileDataById(copiedProposalPersonAttachment.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			attachment.setFileDataId(file.getFileDataId());
			attachment.setUpdateUser(updateUser);
			attachment.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			proposalModuleDao.saveOrUpdateProposalPersonAttachment(attachment);
			newproposalPersonAttachments.add(attachment);
		}
		return newproposalPersonAttachments;
	}

	private List<ProposalPersonUnit> copyProposalPersonUnits(ProposalPerson copiedPersonDetail, ProposalPerson personDetail, String updateUser) {
		List<ProposalPersonUnit> proposalPersonUnits = copiedPersonDetail.getUnits();
		List<ProposalPersonUnit> newProposalPersonUnits = new ArrayList<>();
		for (ProposalPersonUnit copiedPersonPersonUnit : proposalPersonUnits) {
			ProposalPersonUnit personUnit = new ProposalPersonUnit();
			personUnit.setProposalPerson(personDetail);
			personUnit.setUnitNumber(copiedPersonPersonUnit.getUnitNumber());
			personUnit.setLeadUnit(copiedPersonPersonUnit.isLeadUnit());
			personUnit.setUnit(copiedPersonPersonUnit.getUnit());
			personUnit.setUpdateUser(updateUser);
			personUnit.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			newProposalPersonUnits.add(personUnit);
		}
		return newProposalPersonUnits;
	}
	
	private void copyPersonDegree(ProposalPerson proposalPerson) {
		List<PersonDegree> personDegree = personDao.getAllPersonDegree(proposalPerson.getPersonId());
		for (PersonDegree originalPersonDegree : personDegree) {
			ProposalPersonDegree copyProposalPersonDegree = new ProposalPersonDegree();
			copyProposalPersonDegree.setProposalPersonId(proposalPerson.getProposalPersonId());
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

	@Override
	public void copyProposalNonMandatoryFields(ProposalVO proposalVO, Proposal copyProposal, Proposal originalProposal, ProposalExtension copyProposalExtension, ProposalExtension originalProposalExtension) {
		String updateUser = proposalVO.getUpdateUser();
		copyProposal.setGrantCallId(originalProposal.getGrantCallId());
		if (Boolean.FALSE.equals(proposalVO.getIsProposalArchiveCreation())) {
			copyProposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
			copyProposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS));
		} else {
			if (Boolean.FALSE.equals(proposalVO.getIsProposalAdminCorrection())) {
				copyProposal.setStatusCode(proposalVO.getProposalStatusCode());
				copyProposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(proposalVO.getProposalStatusCode()));		
			} else {
				copyProposal.setStatusCode(originalProposal.getStatusCode());
				copyProposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(originalProposal.getStatusCode()));		
			}
		}
		copyProposal.setAbstractDescription(originalProposal.getAbstractDescription());
		copyProposal.setResearchDescription(originalProposal.getResearchDescription());
		copyProposal.setGrantTypeCode(originalProposal.getGrantTypeCode());
		copyProposal.setGrantCallType(originalProposal.getGrantCallType());
		copyProposal.setSponsorProposalNumber(originalProposal.getSponsorProposalNumber());
		copyProposal.setApplicationActivityType(originalProposal.getApplicationActivityType());
		copyProposal.setApplicationType(originalProposal.getApplicationType());
		copyProposal.setApplicationStatus(originalProposal.getApplicationStatus());
		copyProposal.setCreateTimeStamp(committeeDao.getCurrentTimestamp());
		copyProposal.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
		copyProposal.setAwardTypeCode(originalProposal.getAwardTypeCode());
		copyProposal.setAwardType(originalProposal.getAwardType());
		copyProposal.setBaseProposalNumber(originalProposal.getBaseProposalNumber());
		if (originalProposal.getPrimeSponsorCode() != null) {
			copyProposal.setPrimeSponsorCode(originalProposal.getPrimeSponsorCode());
			copyProposal.setPrimeSponsor(commonDao.getSponsorById(originalProposal.getPrimeSponsorCode()));
		}
		copyProposal.setProgramAnnouncementNumber(originalProposal.getProgramAnnouncementNumber());
		copyProposal.setCfdaNumber(originalProposal.getCfdaNumber());
		copyProposal.setMultiDisciplinaryDescription(originalProposal.getResearchDescription());
		copyProposal.setSponsorDeadlineDate(originalProposal.getSponsorDeadlineDate());
		copyProposal.setExternalFundingAgencyId(originalProposal.getExternalFundingAgencyId());
		if (originalProposal.getProposalKeywords() != null && !originalProposal.getProposalKeywords().isEmpty()) {
			copyProposal.getProposalKeywords().addAll(copyProposalKeywords(copyProposal, originalProposal, updateUser));
		}
		List<ProposalResearchArea> proposalResearchAreas = proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(originalProposal.getProposalId());
		if (proposalResearchAreas != null && !proposalResearchAreas.isEmpty()) {
			proposalVO.setProposalResearchAreas(copyProposalResearchAreas(copyProposal.getProposalId(), proposalResearchAreas, updateUser));
		}
		List<ProposalProjectTeam> proposalProjectTeams = proposalModuleDao.fetchProposalProjectTeamBasedOnProposalId(originalProposal.getProposalId());
		if (proposalProjectTeams != null && !proposalProjectTeams.isEmpty()) {
			proposalVO.setProposalProjectTeams(copyProposalProjectTeams(copyProposal.getProposalId(), proposalProjectTeams, updateUser));
		}
		List<ProposalSponsor> proposalSponsors = proposalModuleDao.fetchProposalSponsorBasedOnProposalId(originalProposal.getProposalId());
		if (proposalSponsors != null && !proposalSponsors.isEmpty()) {
			proposalVO.setProposalSponsors(copyProposalSponsors(copyProposal.getProposalId(), proposalSponsors, updateUser));
		}
		List<ProposalIrbProtocol> proposalIrbProtocols = proposalModuleDao.fetchProposalIrbProtocolBasedOnProposalId(originalProposal.getProposalId());
		if (proposalIrbProtocols != null && !proposalIrbProtocols.isEmpty()) {
			proposalVO.setProposalIrbProtocols(copyProposalIrbProtocols(copyProposal.getProposalId(), proposalIrbProtocols, updateUser));
		}
		List<ProposalSpecialReview> proposalSpecialReviews = proposalModuleDao.fetchProposalSpecialReviewBasedOnProposalId(originalProposal.getProposalId());
		if (proposalSpecialReviews != null && !proposalSpecialReviews.isEmpty()) {
			proposalVO.setProposalSpecialReviews(copyProposalSpecialReview(copyProposal.getProposalId(), proposalSpecialReviews, updateUser));
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_ORGANIZATION)) {
			List<ProposalOrganization> proposalOrganizations = proposalDao.loadProposalOrganization(originalProposal.getProposalId());
			if (proposalOrganizations != null && !proposalOrganizations.isEmpty()) {
				proposalVO.setProposalOrganizations(copyProposalOrganizations(copyProposal.getProposalId(), proposalOrganizations));
			}
		}
		if (Boolean.TRUE.equals(proposalVO.getIsProposalArchiveCreation()) || Boolean.TRUE.equals(proposalVO.getCopyAttachment())) {
			List<ProposalAttachment> proposalAttachments = proposalModuleDao.fetchProposalAttachmentBasedOnProposalId(originalProposal.getProposalId());
			if (proposalAttachments != null && !proposalAttachments.isEmpty()) {
				proposalVO.setProposalAttachments(copyProposalAttachments(copyProposal.getProposalId(), proposalAttachments, updateUser, proposalVO.getIsProposalArchiveCreation()));
			}
		}
		if (Boolean.TRUE.equals(proposalVO.getCopyAllBudgetVersion()) || Boolean.TRUE.equals(proposalVO.getCopyFinalBudgetVersion()) || Boolean.TRUE.equals(proposalVO.getIsProposalArchiveCreation())) {
			List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(originalProposal.getProposalId());
			if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
				if (Boolean.TRUE.equals(proposalVO.getCopyAllBudgetVersion())) {
					proposalVO.setBudgetHeaders(copyProposalBudgets(originalProposal.getProposalId(), copyProposal.getProposalId(), originalProposal.getActivityTypeCode(), budgetHeaders, updateUser, Constants.COPY_TYPE_PROPOSAL));
				} else {
					if (Boolean.TRUE.equals(proposalVO.getIsProposalArchiveCreation())) {
						copyProposalFinalBudget(proposalVO, originalProposal.getProposalId(), copyProposal.getProposalId(), originalProposal.getActivityTypeCode(), budgetHeaders, updateUser, Constants.ARCHIVE_TYPE_PROPOSAL);
					} else {
						copyProposalFinalBudget(proposalVO, originalProposal.getProposalId(), copyProposal.getProposalId(), originalProposal.getActivityTypeCode(), budgetHeaders, updateUser, Constants.COPY_TYPE_PROPOSAL);
					}
				}
			}
		}
		if (Boolean.TRUE.equals(proposalVO.getCopyQuestionnaire()) || Boolean.TRUE.equals(proposalVO.getIsProposalArchiveCreation())) {
			questionnaireService.copyQuestionnaireDatas(originalProposal.getProposalId(), copyProposal.getProposalId(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		}
		if (Boolean.TRUE.equals(proposalVO.getCopyOtherInformation()) || Boolean.TRUE.equals(proposalVO.getIsProposalArchiveCreation())) {
			customDataElementService.copyCustomDataBasedOnModule(originalProposal.getProposalId(), copyProposal.getProposalId(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.DEV_PROPOSAL_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
		}
		if (originalProposal.getDisciplineCluster() != null) {
			copyProposal.setClusterCode(originalProposal.getClusterCode());
			copyProposal.setDisciplineCluster(originalProposal.getDisciplineCluster());
		}
		copyProposal = proposalDao.saveOrUpdateProposal(copyProposal);
		if (originalProposalExtension != null) {
			copyProposalExtension.setIsForeignActivities(originalProposalExtension.getIsForeignActivities());
			copyProposalExtension.setIsSubcontract(originalProposalExtension.getIsSubcontract());
			copyProposalExtension.setIsMultiSiteStudy(originalProposalExtension.getIsMultiSiteStudy());
			copyProposalExtension.setIsDomesticSite(originalProposalExtension.getIsDomesticSite());
			copyProposalExtension.setProposalId(copyProposal.getProposalId());
			proposalDao.saveOrUpdateProposalExtension(copyProposalExtension);
		}
		List<ProposalMileStone> originalProposalMileStones = proposalModuleDao.fetchProposalMileStonesBasedOnProposalId(originalProposal.getProposalId());
		if (originalProposalMileStones != null && !originalProposalMileStones.isEmpty()) {
			proposalVO.setProposalMileStones(copyProposalMileStones(copyProposal.getProposalId(), originalProposalMileStones, updateUser));
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			List<ProposalKPI> proposalKPIs = proposalDao.fetchAllProposalKPI(originalProposal.getProposalId());
			if (proposalKPIs != null && !proposalKPIs.isEmpty()) {
				proposalVO.setProposalKpis(copyProposalKPIs(copyProposal.getProposalId(), proposalKPIs, updateUser));
			}
		}
	}

	private List<ProposalOrganization> copyProposalOrganizations(Integer proposalId, List<ProposalOrganization> proposalOrganizations) {
		List<ProposalOrganization> newProposalOrganizations = new ArrayList<>();
		proposalOrganizations.stream().forEach(proposalOrganization -> {
			ProposalOrganization newproposalOrganization = new ProposalOrganization();
			newproposalOrganization.setProposalId(proposalId);
			newproposalOrganization.setOrganizationTypeCode(proposalOrganization.getOrganizationTypeCode());
			newproposalOrganization.setOrganizationType(proposalOrganization.getOrganizationType());
			newproposalOrganization.setOrganizationId(proposalOrganization.getOrganizationId());
			newproposalOrganization.setOrganization(proposalOrganization.getOrganization());
			newproposalOrganization.setLocation(proposalOrganization.getLocation());
			newproposalOrganization.setRolodexId(proposalOrganization.getRolodexId());
			List<ProposalCongDistrict> proposalCongDistricts = proposalOrganization.getProposalCongDistricts();
			if (proposalCongDistricts != null && !proposalCongDistricts.isEmpty()) {
				newproposalOrganization.getProposalCongDistricts().addAll(copyProposalCongDistricts(proposalCongDistricts, newproposalOrganization));
			}
			proposalDao.saveOrUpdateProposalOrganization(newproposalOrganization);
			newProposalOrganizations.add(newproposalOrganization);
		});
		return newProposalOrganizations;
	}

	private List<ProposalCongDistrict> copyProposalCongDistricts(List<ProposalCongDistrict> proposalCongDistricts, ProposalOrganization newproposalOrganization) {
		List<ProposalCongDistrict> newProposalCongDistricts = new ArrayList<>();
		proposalCongDistricts.stream().forEach(proposalCongDistrict -> {
			ProposalCongDistrict newproposalCongDistrict = new ProposalCongDistrict();
			newproposalCongDistrict.setCongDistrictCode(proposalCongDistrict.getCongDistrictCode());
			newproposalCongDistrict.setCongressionalDistrict(proposalCongDistrict.getCongressionalDistrict());
			newproposalCongDistrict.setProposalOrganization(newproposalOrganization);
			newProposalCongDistricts.add(newproposalCongDistrict);
		});
		return newProposalCongDistricts;
	}

	public List<ProposalAttachment> copyProposalAttachments(Integer proposalId, List<ProposalAttachment> proposalAttachments, String updateUser, Boolean isArchiveCreation) {
		List<ProposalAttachment> copiedProposalAttachments = new ArrayList<>(proposalAttachments);
		Collections.copy(copiedProposalAttachments, proposalAttachments);
		List<ProposalAttachment> newAttachments = new ArrayList<>();
		for (ProposalAttachment copiedAttachmentDetail : copiedProposalAttachments) {
			ProposalAttachment attachmentDetail = new ProposalAttachment();
			attachmentDetail.setProposalId(proposalId);
			attachmentDetail.setAttachmentTypeCode(copiedAttachmentDetail.getAttachmentTypeCode());
			attachmentDetail.setAttachmentType(copiedAttachmentDetail.getAttachmentType());
			attachmentDetail.setDescription(copiedAttachmentDetail.getDescription());
			attachmentDetail.setFileName(copiedAttachmentDetail.getFileName());
			attachmentDetail.setMimeType(copiedAttachmentDetail.getMimeType());
			attachmentDetail.setUpdateUser(updateUser);
			attachmentDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			attachmentDetail.setNarrativeStatusCode(copiedAttachmentDetail.getNarrativeStatusCode());
			attachmentDetail.setNarrativeStatus(copiedAttachmentDetail.getNarrativeStatus());
			attachmentDetail.setVersionNumber(copiedAttachmentDetail.getVersionNumber());
			attachmentDetail.setDocumentStatusCode(copiedAttachmentDetail.getDocumentStatusCode());
			attachmentDetail.setDocumentStatus(copiedAttachmentDetail.getDocumentStatus());
			if (Boolean.TRUE.equals(isArchiveCreation)) {
				attachmentDetail.setFileDataId(copiedAttachmentDetail.getFileDataId());
			} else {
				FileData fileData = commonDao.getFileDataById(copiedAttachmentDetail.getFileDataId());
				FileData file = new FileData();
				file.setAttachment(fileData.getAttachment());
				file = commonDao.saveFileData(file);
				attachmentDetail.setFileDataId(file.getFileDataId());
			}
			attachmentDetail.setDocumentId(copiedAttachmentDetail.getDocumentId());
			proposalModuleDao.saveOrUpdateProposalAttachment(attachmentDetail);
			newAttachments.add(attachmentDetail);
		}
		return newAttachments;
	}

	private List<ProposalSpecialReview> copyProposalSpecialReview(Integer proposalId, List<ProposalSpecialReview> proposalSpecialReviews, String updateUser) {
		List<ProposalSpecialReview> copiedProposalSpecialReviews = new ArrayList<>(proposalSpecialReviews);
		Collections.copy(copiedProposalSpecialReviews, proposalSpecialReviews);
		List<ProposalSpecialReview> newSpecialReviews = new ArrayList<>();
		for (ProposalSpecialReview copiedSpecialReviewDetail : copiedProposalSpecialReviews) {
			ProposalSpecialReview specialReviewDetail = new ProposalSpecialReview();
			specialReviewDetail.setProposalId(proposalId);
			specialReviewDetail.setSpecialReviewTypeCode(copiedSpecialReviewDetail.getSpecialReviewTypeCode());
			specialReviewDetail.setSpecialReviewType(copiedSpecialReviewDetail.getSpecialReviewType());
			specialReviewDetail.setApprovalTypeCode(copiedSpecialReviewDetail.getApprovalTypeCode());
			specialReviewDetail.setApprovalType(copiedSpecialReviewDetail.getApprovalType());
			specialReviewDetail.setProtocolNumber(copiedSpecialReviewDetail.getProtocolNumber());
			specialReviewDetail.setProtocolStatus(copiedSpecialReviewDetail.getProtocolStatus());
			specialReviewDetail.setApplicationDate(copiedSpecialReviewDetail.getApplicationDate());
			specialReviewDetail.setApprovalDate(copiedSpecialReviewDetail.getApprovalDate());
			specialReviewDetail.setExpirationDate(copiedSpecialReviewDetail.getExpirationDate());
			specialReviewDetail.setComments(copiedSpecialReviewDetail.getComments());
			specialReviewDetail.setUpdateUser(updateUser);
			specialReviewDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			specialReviewDetail.setIsProtocolIntegrated(copiedSpecialReviewDetail.getIsProtocolIntegrated());
			proposalModuleDao.saveOrUpdateProposalSpecialReview(specialReviewDetail);
			newSpecialReviews.add(specialReviewDetail);
		}
		return newSpecialReviews;
	}

	private List<ProposalKeyword> copyProposalKeywords(Proposal copyProposal, Proposal proposal, String updateUser) {
		List<ProposalKeyword> proposalKeywords = proposal.getProposalKeywords();
		List<ProposalKeyword> copiedProposalKeywords = new ArrayList<>(proposalKeywords);
		Collections.copy(copiedProposalKeywords, proposalKeywords);
		List<ProposalKeyword> newKeywords = new ArrayList<>();
		for (ProposalKeyword copiedKeywordDetail : copiedProposalKeywords) {
			ProposalKeyword keywordtDetail = new ProposalKeyword();
			keywordtDetail.setProposal(copyProposal);
			keywordtDetail.setScienceKeywordCode(copiedKeywordDetail.getScienceKeywordCode());
			keywordtDetail.setScienceKeyword(copiedKeywordDetail.getScienceKeyword());
			keywordtDetail.setUpdateUser(updateUser);
			keywordtDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			keywordtDetail.setKeyword(copiedKeywordDetail.getKeyword());
			newKeywords.add(keywordtDetail);
		}
		return newKeywords;
	}

	private List<ProposalIrbProtocol> copyProposalIrbProtocols(Integer proposalId, List<ProposalIrbProtocol> proposalIrbProtocols, String updateUser) {
		List<ProposalIrbProtocol> copiedProposalIrbProtocols = new ArrayList<>(proposalIrbProtocols);
		Collections.copy(copiedProposalIrbProtocols, proposalIrbProtocols);
		List<ProposalIrbProtocol> newIrbProtocols = new ArrayList<>();
		for (ProposalIrbProtocol copiedIrbProtocolDetail : copiedProposalIrbProtocols) {
			ProposalIrbProtocol irbProtocolDetail = new ProposalIrbProtocol();
			irbProtocolDetail.setProposalId(proposalId);
			irbProtocolDetail.setProtocolId(copiedIrbProtocolDetail.getProtocolId());
			irbProtocolDetail.setUpdateUser(updateUser);
			irbProtocolDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			proposalModuleDao.saveOrUpdateProposalIrbProtocol(irbProtocolDetail);
			newIrbProtocols.add(irbProtocolDetail);
		}
		return newIrbProtocols;
	}

	private List<ProposalResearchArea> copyProposalResearchAreas(Integer proposalId, List<ProposalResearchArea> proposalResearchAreas, String updateUser) {
		List<ProposalResearchArea> copiedProposalResearchAreas = new ArrayList<>(proposalResearchAreas);
		Collections.copy(copiedProposalResearchAreas, proposalResearchAreas);
		List<ProposalResearchArea> newproposalResearchAreas = new ArrayList<>();
		for (ProposalResearchArea copiedResearchAreaDetail : copiedProposalResearchAreas) {
			ProposalResearchArea researchAreaDetail = new ProposalResearchArea();
			researchAreaDetail.setProposalId(proposalId);
			researchAreaDetail.setResearchTypeCode(copiedResearchAreaDetail.getResearchTypeCode());
			researchAreaDetail.setResearchType(copiedResearchAreaDetail.getResearchType());
			researchAreaDetail.setResearchTypeAreaCode(copiedResearchAreaDetail.getResearchTypeAreaCode());
			researchAreaDetail.setResearchTypeArea(copiedResearchAreaDetail.getResearchTypeArea());
			researchAreaDetail.setResearchTypeSubAreaCode(copiedResearchAreaDetail.getResearchTypeSubAreaCode());
			researchAreaDetail.setResearchTypeSubArea(copiedResearchAreaDetail.getResearchTypeSubArea());
			researchAreaDetail.setUpdateUser(updateUser);
			researchAreaDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			proposalModuleDao.saveOrUpdateProposalResearchArea(researchAreaDetail);
			newproposalResearchAreas.add(researchAreaDetail);
		}
		return newproposalResearchAreas;
	}

	private List<ProposalSponsor> copyProposalSponsors(Integer proposalId, List<ProposalSponsor> proposalSponsors, String updateUser) {
		List<ProposalSponsor> copiedProposalSponsors = new ArrayList<>(proposalSponsors);
		Collections.copy(copiedProposalSponsors, proposalSponsors);
		List<ProposalSponsor> newProposalSponsors = new ArrayList<>();
		for (ProposalSponsor copiedProposalSponsorsDetail : copiedProposalSponsors) {
			ProposalSponsor sponsorsDetail = new ProposalSponsor();
			sponsorsDetail.setProposalId(proposalId);
			sponsorsDetail.setSponsorCode(copiedProposalSponsorsDetail.getSponsorCode());
			sponsorsDetail.setSponsor(copiedProposalSponsorsDetail.getSponsor());
			sponsorsDetail.setStartDate(copiedProposalSponsorsDetail.getStartDate());
			sponsorsDetail.setEndDate(copiedProposalSponsorsDetail.getEndDate());
			sponsorsDetail.setAmount(copiedProposalSponsorsDetail.getAmount());
			sponsorsDetail.setUpdateUser(updateUser);
			sponsorsDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			sponsorsDetail.setFundingStatusCode(copiedProposalSponsorsDetail.getFundingStatusCode());
			sponsorsDetail.setProposalFundingStatus(copiedProposalSponsorsDetail.getProposalFundingStatus());
			sponsorsDetail.setSponsorTypeCode(copiedProposalSponsorsDetail.getSponsorTypeCode());
			sponsorsDetail.setSponsorType(copiedProposalSponsorsDetail.getSponsorType());
			sponsorsDetail.setFullName(copiedProposalSponsorsDetail.getFullName());
			sponsorsDetail.setSponsorName(copiedProposalSponsorsDetail.getSponsorName());
			sponsorsDetail.setPersonRoleId(copiedProposalSponsorsDetail.getPersonRoleId());
			sponsorsDetail.setProposalPersonRole(copiedProposalSponsorsDetail.getProposalPersonRole());
			sponsorsDetail.setPercentageOfEffort(copiedProposalSponsorsDetail.getPercentageOfEffort());
			sponsorsDetail.setGrantCallName(copiedProposalSponsorsDetail.getGrantCallName());
			sponsorsDetail.setProjectTitle(copiedProposalSponsorsDetail.getProjectTitle());
			sponsorsDetail.setCurrencyCode(sponsorsDetail.getCurrencyCode());
			sponsorsDetail.setCurrency(sponsorsDetail.getCurrency());
			proposalModuleDao.saveOrUpdateProposalSponsor(sponsorsDetail);
			newProposalSponsors.add(sponsorsDetail);
		}
		return newProposalSponsors;
	}

	@Override
	public List<BudgetPeriod> copyBudgetPeriods(BudgetHeader copyBudget, BudgetHeader originalBudget, String activityTypeCode, String updateUser) {
		List<BudgetPeriod> budgetPeriods = originalBudget.getBudgetPeriods();
		List<BudgetPeriod> copiedBudgetPeriods = new ArrayList<>(budgetPeriods);
		Collections.copy(copiedBudgetPeriods, budgetPeriods);
		List<BudgetPeriod> newPeriods = new ArrayList<>();
		for (BudgetPeriod originalPeriod : copiedBudgetPeriods) {
			BudgetPeriod copyPeriod = new BudgetPeriod();
			copyPeriod.setModuleItemCode(originalPeriod.getModuleItemCode());
			copyPeriod.setModuleItemKey(originalPeriod.getModuleItemKey());
			copyPeriod.setVersionNumber(originalPeriod.getVersionNumber());
			copyPeriod.setBudgetPeriod(originalPeriod.getBudgetPeriod());
			copyPeriod.setStartDate(originalPeriod.getStartDate());
			copyPeriod.setEndDate(originalPeriod.getEndDate());
			copyPeriod.setTotalCost(originalPeriod.getTotalCost());
			copyPeriod.setTotalDirectCost(originalPeriod.getTotalDirectCost());
			copyPeriod.setTotalIndirectCost(originalPeriod.getTotalIndirectCost());
			copyPeriod.setPeriodLabel(originalPeriod.getPeriodLabel());
			copyPeriod.setIsObligatedPeriod(originalPeriod.getIsObligatedPeriod());
			copyPeriod.setCostSharingAmount(originalPeriod.getCostSharingAmount());
			copyPeriod.setUnderrecoveryAmount(originalPeriod.getUnderrecoveryAmount());
			copyPeriod.setTotalCostLimit(originalPeriod.getTotalCostLimit());
			copyPeriod.setComments(originalPeriod.getComments());
			copyPeriod.setTotalDirectCostLimit(originalPeriod.getTotalDirectCostLimit());
			copyPeriod.setTotalFundRequested(originalPeriod.getTotalFundRequested());
			copyPeriod.setTotalModifiedDirectCost(originalPeriod.getTotalModifiedDirectCost());
			copyPeriod.setTotalInKind(originalPeriod.getTotalInKind());
			copyPeriod.setTotalOfTotalCost(originalPeriod.getTotalOfTotalCost());	
			copyPeriod.setBudget(copyBudget);
			if (originalPeriod.getBudgetDetails() != null && !originalPeriod.getBudgetDetails().isEmpty()) {
				copyBudgetDetails(copyPeriod, originalPeriod, activityTypeCode, updateUser);
			} else {
				List<BudgetPerson> budgetPersonnels = budgetDao.getBudgetPersons(originalBudget.getBudgetId());
				if (budgetPersonnels != null && !budgetPersonnels.isEmpty()) {
					 List<BudgetPerson> persons = copyBudgetPersonnels(budgetPersonnels, copyPeriod);
					boolean checkBudgetPersonInBudget = false;
					for (BudgetPerson person : persons) {
						checkBudgetPersonInBudget = budgetDao.checkBudgetPersonInBudget(person.getBudgetId(),person.getTbnId(), person.getJobCodeType(), person.getPersonType(), person.getPersonId(), person.getRolodexId());
						if (checkBudgetPersonInBudget) {
							 budgetDao.saveOrUpdateProposalBudgetPerson(person);
						}
					}
				}
			}
			copyPeriod.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			copyPeriod.setUpdateUser(copyBudget.getUpdateUser());
			copyPeriod.setSubcontractCost(originalPeriod.getSubcontractCost());
			newPeriods.add(copyPeriod);
		}
		return newPeriods;
	}

	private List<BudgetPerson> copyBudgetPersonnels(List<BudgetPerson> budgetPersonnels, BudgetPeriod copiedPeriod) {
		List<BudgetPerson> budgetPersons = new ArrayList<>();
		for (BudgetPerson copiedBudgetPerson : budgetPersonnels) {
			BudgetPerson budgetPerson = new BudgetPerson();
			budgetPerson.setAppointmentType(copiedBudgetPerson.getAppointmentType());
			budgetPerson.setAppointmentTypeCode(copiedBudgetPerson.getAppointmentTypeCode());
			budgetPerson.setBudgetId(copiedPeriod.getBudget().getBudgetId());
			budgetPerson.setBudgetPersonId(null);
			budgetPerson.setCalculationBase(copiedBudgetPerson.getCalculationBase());
			budgetPerson.setDurationCost(copiedBudgetPerson.getDurationCost());
			budgetPerson.setEffectiveDate(copiedBudgetPerson.getEffectiveDate());
			budgetPerson.setJobCode(copiedBudgetPerson.getJobCode());
			budgetPerson.setJobCodeType(copiedBudgetPerson.getJobCodeType());
			budgetPerson.setNonEmployeeFlag(copiedBudgetPerson.getNonEmployeeFlag());
			budgetPerson.setPersonId(copiedBudgetPerson.getPersonId());
			budgetPerson.setPersonName(copiedBudgetPerson.getPersonName());
			budgetPerson.setPersonType(copiedBudgetPerson.getPersonType());
			budgetPerson.setRolodexId(copiedBudgetPerson.getRolodexId());
			budgetPerson.setSalaryAnniversaryDate(copiedBudgetPerson.getSalaryAnniversaryDate());
			budgetPerson.setTbnId(copiedBudgetPerson.getTbnId());
			budgetPerson.setUpdateTimeStamp(copiedBudgetPerson.getUpdateTimeStamp());
			budgetPerson.setUpdateUser(copiedBudgetPerson.getUpdateUser());
			budgetPersons.add(budgetPerson);
		}
		return budgetPersons;
	}

	private void copyBudgetDetails(BudgetPeriod copyPeriod, BudgetPeriod period, String activityTypeCode, String updateUser) {
		List<BudgetDetail> budgetDetails = period.getBudgetDetails();
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			List<BudgetDetail> copiedBudgetDetails = new ArrayList<>(budgetDetails);
			Collections.copy(copiedBudgetDetails, budgetDetails);
			List<BudgetDetail> newDetailLineItems  = new ArrayList<>();
			for (BudgetDetail budgetDetail : copiedBudgetDetails) {
				BudgetDetail copyBudgetDetail = new BudgetDetail();
				copyBudgetDetail.setBudgetCategory(budgetDetail.getBudgetCategory());
				copyBudgetDetail.setBudgetCategoryCode(budgetDetail.getBudgetCategoryCode());
				copyBudgetDetail.setBudgetJustification(budgetDetail.getBudgetJustification());
				copyBudgetDetail.setBudgetPeriod(budgetDetail.getBudgetPeriod());
				copyBudgetDetail.setEndDate(budgetDetail.getEndDate());
				copyBudgetDetail.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
				copyBudgetDetail.setSystemGeneratedCEType(budgetDetail.getSystemGeneratedCEType());
				copyBudgetDetail.setIsApplyInflationRate(budgetDetail.getIsApplyInflationRate());
				copyBudgetDetail.setUnderrecoveryAmount(budgetDetail.getUnderrecoveryAmount());
				copyBudgetDetail.setApplyInRateFlag(budgetDetail.getApplyInRateFlag());
				copyBudgetDetail.setQuantity(budgetDetail.getQuantity());
				copyBudgetDetail.setSubmitCostSharingFlag(budgetDetail.getSubmitCostSharingFlag());
				copyBudgetDetail.setLineItemNumber(budgetDetail.getLineItemNumber());
				copyBudgetDetail.setCostSharingAmount(budgetDetail.getCostSharingAmount());
				copyBudgetDetail.setCostSharingPercentage(budgetDetail.getCostSharingPercentage());
				copyBudgetDetail.setSponsorRequestedAmount(budgetDetail.getSponsorRequestedAmount());
				// apply inflation here
				CostElement costElement = budgetDetail.getCostElement();
				costElement = budgetDao.fetchCostElementsById(costElement.getCostElement());
				copyBudgetDetail.setCostElement(costElement);
				copyBudgetDetail.setCostElementCode(budgetDetail.getCostElementCode());
				BigDecimal lineItemCost = budgetDetail.getLineItemCost();
				BigDecimal updatedLineItemCost = BigDecimal.ZERO;
				List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
				BudgetDetailCalcAmount budgetCalculatedAmount = null;
				if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
					for (ValidCeRateType ceRateType : ceRateTypes) {
						FibiProposalRate applicableRate = budgetDao.fetchApplicableProposalRate(copyPeriod.getBudget().getBudgetId(), copyPeriod.getStartDate(), ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(), activityTypeCode);
						if (applicableRate != null && (applicableRate.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_INFLATION) && Constants.RATE_CLASS_CODE_INFLATION.equals(applicableRate.getRateClassCode()))) {
							BigDecimal validRate = BigDecimal.ZERO;
							validRate = validRate.add(applicableRate.getApplicableRate());
							if (validRate.compareTo(BigDecimal.ZERO) > 0) {
								BigDecimal hundred = new BigDecimal(100);
								BigDecimal percentageFactor = validRate.divide(hundred, 2, RoundingMode.HALF_UP);
								BigDecimal calculatedCost = (lineItemCost.multiply(percentageFactor));
								updatedLineItemCost = updatedLineItemCost.add(calculatedCost);
								budgetCalculatedAmount = budgetService.getNewBudgetCalculatedAmount(copyPeriod, budgetDetail, applicableRate);
								budgetCalculatedAmount.setCalculatedCost(calculatedCost);
								copyBudgetDetail.getBudgetDetailCalcAmounts().add(budgetCalculatedAmount);
							}
						}
					}
				}
				copyBudgetDetail.setLineItemCost(lineItemCost.setScale(2, RoundingMode.HALF_UP));
				copyBudgetDetail.setLineItemDescription(budgetDetail.getLineItemDescription());
				copyBudgetDetail.setLineItemNumber(budgetDetail.getLineItemNumber());
				copyBudgetDetail.setOnOffCampusFlag(budgetDetail.getOnOffCampusFlag());
				copyBudgetDetail.setPeriod(copyPeriod);
				copyBudgetDetail.setPrevLineItemCost(budgetDetail.getPrevLineItemCost());
				copyBudgetDetail.setStartDate(budgetDetail.getStartDate());
				copyBudgetDetail.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
				copyBudgetDetail.setUpdateUser(copyPeriod.getUpdateUser());
				copyBudgetDetail.setFullName(budgetDetail.getFullName());
				copyBudgetDetail.setRolodexId(budgetDetail.getRolodexId());
				copyBudgetDetail.setPersonId(budgetDetail.getPersonId());
				copyBudgetDetail.setTbnId(budgetDetail.getTbnId());
				copyBudgetDetail.setTbnPerson(budgetDetail.getTbnPerson());
				copyBudgetDetail.setPersonType(budgetDetail.getPersonType());
				List<BudgetPersonalDetails> originalPersonDetails  = budgetDetail.getPersonsDetails();
				List<BudgetPerson> newBudgetPersons = new ArrayList<>();
				/*if (personDetails != null && !personDetails.isEmpty()) {
					copyBudgetDetail.getPersonsDetails().addAll(copyBudgetPersonalDetails(copyBudgetDetail, budgetDetail, updateUser));
				}*/
				boolean checkBudgetPersonInBudget = false;
				if (originalPersonDetails != null && !originalPersonDetails.isEmpty()) {
					List<BudgetPerson> persons = copyBudgetPersons(originalPersonDetails, copyPeriod);
					for (BudgetPerson person : persons) {
						BudgetPerson budgetPerson = null;
						checkBudgetPersonInBudget = budgetDao.checkBudgetPersonInBudget(person.getBudgetId(),person.getTbnId(), person.getJobCodeType(), person.getPersonType(), person.getPersonId(), person.getRolodexId());
						if (checkBudgetPersonInBudget) {
							budgetPerson = budgetDao.saveOrUpdateProposalBudgetPerson(person);
						} else {
							if (person.getPersonType().equals(Constants.TBN_PERSON_TYPE)) {
								budgetPerson = budgetDao.getBugetTbnPersonByTbnId(person.getBudgetId(), person.getPersonType(), person.getTbnId(), person.getAppointmentTypeCode(), person.getJobCodeType());
							} else if (person.getPersonType().equals(Constants.EMPLOYEE_PERSON_TYPE)) {
								budgetPerson = budgetDao.getBugetPersonByPersonId(person.getBudgetId(), person.getPersonType(), person.getPersonId(), person.getAppointmentTypeCode(), person.getJobCodeType());
							} else if (person.getPersonType().equals(Constants.PROPOSAL_PERSON_TYPE) && person.getPersonId() != null) {
								budgetPerson = budgetDao.getBugetPersonByPersonId(person.getBudgetId(), person.getPersonType(), person.getPersonId(), person.getAppointmentTypeCode(), person.getJobCodeType());
							} else {
								budgetPerson = budgetDao.getBugetRolodexPersonByRolodexId(person.getBudgetId(), person.getPersonType(), person.getRolodexId(), person.getAppointmentTypeCode(), person.getJobCodeType());
							}
						}
						newBudgetPersons.add(budgetPerson);
					}
					List<BudgetPersonalDetails> budgetPersonalDetails = new ArrayList<>();
					List<BudgetPersonalDetails> budgetPersonDetails = new ArrayList<>();
					budgetPersonalDetails.addAll(copyBudgetPersonalDetails(copyBudgetDetail, budgetDetail, updateUser));
					for (BudgetPerson budgetPersons : newBudgetPersons) {
						for (BudgetPersonalDetails budgetPersonalDetail : budgetPersonalDetails) {
							if (budgetPersons.getPersonType().equals(budgetPersonalDetail.getPersonType())) {
								if (budgetPersonalDetail.getPersonType().equals(Constants.TBN_PERSON_TYPE)) {
									if (budgetPersons.getTbnId().equals(budgetPersonalDetail.getTbnId())) {
										budgetPersonalDetail.setBudgetPersonId(budgetPersons.getBudgetPersonId());
										budgetPersonalDetail.setBudgetPerson(budgetPersons);
									}
								} else if (budgetPersonalDetail.getPersonType().equals(Constants.EMPLOYEE_PERSON_TYPE)) {
									if (budgetPersons.getPersonId().equals(budgetPersonalDetail.getPersonId())) {
										budgetPersonalDetail.setBudgetPersonId(budgetPersons.getBudgetPersonId());
										budgetPersonalDetail.setBudgetPerson(budgetPersons);
									}
								} else if (budgetPersonalDetail.getPersonType().equals(Constants.NON_EMPLOYEE_TYPE)) {
									if (budgetPersons.getRolodexId().equals(budgetPersonalDetail.getRolodexId())) {
										budgetPersonalDetail.setBudgetPersonId(budgetPersons.getBudgetPersonId());
										budgetPersonalDetail.setBudgetPerson(budgetPersons);
									}
								} else if (budgetPersonalDetail.getPersonType().equals(Constants.PROPOSAL_PERSON_TYPE)) {
									if ((budgetPersons.getRolodexId() != null && budgetPersonalDetail.getRolodexId() != null && budgetPersons.getRolodexId().equals(budgetPersonalDetail.getRolodexId()))) {
										budgetPersonalDetail.setRolodexId(budgetPersons.getRolodexId());
									} else if ((budgetPersons.getPersonId() != null && budgetPersonalDetail.getPersonId() != null && budgetPersons.getPersonId().equals(budgetPersonalDetail.getPersonId()))) {
										budgetPersonalDetail.setBudgetPersonId(budgetPersons.getBudgetPersonId());
									}
									budgetPersonalDetail.setBudgetPerson(budgetPersons);
								}
								if (budgetPersonDetails.isEmpty()) {
									budgetPersonDetails.add(budgetPersonalDetail);
								} else {
									if (!budgetPersonDetails.contains(budgetPersonalDetail)) {
										budgetPersonDetails.add(budgetPersonalDetail);
									}
								}
							}
						}
					}
					copyBudgetDetail.getPersonsDetails().addAll(budgetPersonDetails);
					budgetPersonDetails.clear();
				}
				List<BudgetPerson> budgetPersonnels = budgetDao.getBudgetPersons(period.getBudget().getBudgetId());
				if (budgetPersonnels != null && !budgetPersonnels.isEmpty()) {
					List<BudgetPerson> persons = copyBudgetPersonnels(budgetPersonnels, copyPeriod);
					for (BudgetPerson person : persons) {
						checkBudgetPersonInBudget = budgetDao.checkBudgetPersonInBudget(person.getBudgetId(),person.getTbnId(), person.getJobCodeType(), person.getPersonType(), person.getPersonId(), person.getRolodexId());
						if (checkBudgetPersonInBudget) {
							budgetDao.saveOrUpdateProposalBudgetPerson(person);
						}
					}
				}
				newDetailLineItems .add(copyBudgetDetail);
			}
			copyPeriod.getBudgetDetails().addAll(newDetailLineItems );
		}
	}

	private List<BudgetPerson> copyBudgetPersons(List<BudgetPersonalDetails> originalBudgetPersonDetails, BudgetPeriod copiedPeriod) {
		List<BudgetPerson> budgetPersons = new ArrayList<>();
		for (BudgetPersonalDetails copiedBudgetPersonDetail : originalBudgetPersonDetails) {
			BudgetPerson budgetPerson = new BudgetPerson();
			BudgetPerson copiedBudgetPerson = copiedBudgetPersonDetail.getBudgetPerson();
			budgetPerson.setAppointmentType(copiedBudgetPerson.getAppointmentType());
			budgetPerson.setAppointmentTypeCode(copiedBudgetPerson.getAppointmentTypeCode());
			budgetPerson.setBudgetId(copiedPeriod.getBudget().getBudgetId());
			budgetPerson.setBudgetPersonId(null);
			budgetPerson.setCalculationBase(copiedBudgetPerson.getCalculationBase());
			budgetPerson.setDurationCost(copiedBudgetPerson.getDurationCost());
			budgetPerson.setEffectiveDate(copiedBudgetPerson.getEffectiveDate());
			budgetPerson.setJobCode(copiedBudgetPerson.getJobCode());
			budgetPerson.setJobCodeType(copiedBudgetPerson.getJobCodeType());
			budgetPerson.setNonEmployeeFlag(copiedBudgetPerson.getNonEmployeeFlag());
			budgetPerson.setPersonId(copiedBudgetPerson.getPersonId());
			budgetPerson.setPersonName(copiedBudgetPerson.getPersonName());
			budgetPerson.setPersonType(copiedBudgetPerson.getPersonType());
			budgetPerson.setRolodexId(copiedBudgetPerson.getRolodexId());
			budgetPerson.setSalaryAnniversaryDate(copiedBudgetPerson.getSalaryAnniversaryDate());
			budgetPerson.setTbnId(copiedBudgetPerson.getTbnId());
			budgetPerson.setUpdateTimeStamp(copiedBudgetPerson.getUpdateTimeStamp());
			budgetPerson.setUpdateUser(copiedBudgetPerson.getUpdateUser());
			budgetPersons.add(budgetPerson);
		}
		return budgetPersons;
	}

	private List<BudgetHeader> copyProposalBudgets(Integer oldProposalId, Integer proposalId, String activityTypeCode, List<BudgetHeader> budgetHeaders, String updateUser, String copyType) {
		List<BudgetHeader> copiedBudgetHeaders = new ArrayList<>(budgetHeaders);
		Collections.copy(copiedBudgetHeaders, budgetHeaders);
		List<BudgetHeader> newBudgetHeaders = new ArrayList<>();
		for (BudgetHeader copiedBudgetHeader : copiedBudgetHeaders) {
			BudgetVO budgetVO = new BudgetVO();
			budgetVO.setProposalId(oldProposalId);
			budgetVO.setCopiedProposalId(proposalId);
			budgetVO.setBudgetId(copiedBudgetHeader.getBudgetId());
			budgetVO.setCopyType(copyType);
			budgetVO.setUserFullName(personDao.getUserFullNameByUserName(updateUser));
			budgetVO.setUserName(updateUser);
			budgetVO.setActivityTypeCode(activityTypeCode);
			budgetService.copyProposalBudget(budgetVO);
		}
		return newBudgetHeaders;
	}

	private List<BudgetPersonalDetails> copyBudgetPersonalDetails(BudgetDetail copyBudgetDetail, BudgetDetail budgetDetail, String updateUser) {
		List<BudgetPersonalDetails> budgetPersonDetails = budgetDetail.getPersonsDetails();
		List<BudgetPersonalDetails> copiedBudgetPersonDetails = new ArrayList<>(budgetPersonDetails);
		Collections.copy(copiedBudgetPersonDetails, budgetPersonDetails);
		List<BudgetPersonalDetails> newBudgetPersonalDetails = new ArrayList<>();
		for (BudgetPersonalDetails copiedBudgetPersonDetail : copiedBudgetPersonDetails) {
			BudgetPersonalDetails budgetPersonDetail = new BudgetPersonalDetails();
			budgetPersonDetail.setBudgetDetail(copyBudgetDetail);
			budgetPersonDetail.setRolodexId(copiedBudgetPersonDetail.getRolodexId());
			budgetPersonDetail.setUnderRecoveryAmount(copiedBudgetPersonDetail.getUnderRecoveryAmount());
			budgetPersonDetail.setPercentageCharged(copiedBudgetPersonDetail.getPercentageCharged());
			budgetPersonDetail.setPercentageEffort(copiedBudgetPersonDetail.getPercentageEffort());
			budgetPersonDetail.setCostSharingAmount(copiedBudgetPersonDetail.getCostSharingAmount());
			budgetPersonDetail.setCostSharingPercentage(copiedBudgetPersonDetail.getCostSharingPercentage());
			budgetPersonDetail.setSalaryRequested(copiedBudgetPersonDetail.getSalaryRequested());
			budgetPersonDetail.setTotalSalary(copiedBudgetPersonDetail.getTotalSalary());
			budgetPersonDetail.setNoOfMonths(copiedBudgetPersonDetail.getNoOfMonths());
			budgetPersonDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			budgetPersonDetail.setUpdateUser(updateUser);	
			budgetPersonDetail.setStartDate(copiedBudgetPersonDetail.getStartDate());
			budgetPersonDetail.setEndDate(copiedBudgetPersonDetail.getEndDate());
			budgetPersonDetail.setSponsorRequestedAmount(copiedBudgetPersonDetail.getSponsorRequestedAmount());
			budgetPersonDetail.setSalary(copiedBudgetPersonDetail.getSalary());
			budgetPersonDetail.setSalaryRequested(copiedBudgetPersonDetail.getSalaryRequested());
			budgetPersonDetail.setTbnId(copiedBudgetPersonDetail.getBudgetPerson().getTbnId());
			budgetPersonDetail.setTbnPerson(copiedBudgetPersonDetail.getBudgetPerson().getTbnPerson());
			budgetPersonDetail.setPersonType(copiedBudgetPersonDetail.getBudgetPerson().getPersonType());
			if (copiedBudgetPersonDetail.getBudgetPerson().getTbnPerson() != null) {
				budgetPersonDetail.setPersonName(copiedBudgetPersonDetail.getBudgetPerson().getTbnPerson().getPersonName());
			} else {
				budgetPersonDetail.setPersonName(copiedBudgetPersonDetail.getPersonName());
			}
			budgetPersonDetail.setPersonId(copiedBudgetPersonDetail.getBudgetPerson().getPersonId());
			budgetPersonDetail.setBudgetPersonId(copiedBudgetPersonDetail.getBudgetPersonId());
			newBudgetPersonalDetails.add(budgetPersonDetail);
		}
		return newBudgetPersonalDetails;
	}

	private List<ProposalProjectTeam> copyProposalProjectTeams(Integer proposalId, List<ProposalProjectTeam> proposalProjectTeams, String updateUser) {
		List<ProposalProjectTeam> copiedProposalProjects = new ArrayList<>(proposalProjectTeams);
		Collections.copy(copiedProposalProjects, proposalProjectTeams);
		List<ProposalProjectTeam> newProposalProjectTeams = new ArrayList<>();
		for (ProposalProjectTeam copiedProposalProjectTeam : copiedProposalProjects) {
			ProposalProjectTeam proposalProjectTeam = new ProposalProjectTeam();
			proposalProjectTeam.setProposalId(proposalId);
			proposalProjectTeam.setPersonId(copiedProposalProjectTeam.getPersonId());
			proposalProjectTeam.setFullName(copiedProposalProjectTeam.getFullName());
			proposalProjectTeam.setProjectRole(copiedProposalProjectTeam.getProjectRole());
			proposalProjectTeam.setNonEmployeeFlag(copiedProposalProjectTeam.getNonEmployeeFlag());
			proposalProjectTeam.setPercentageCharged(copiedProposalProjectTeam.getPercentageCharged());
			proposalProjectTeam.setStartDate(copiedProposalProjectTeam.getStartDate());
			proposalProjectTeam.setEndDate(copiedProposalProjectTeam.getEndDate());
			proposalProjectTeam.setIsActive(copiedProposalProjectTeam.getIsActive());
			proposalProjectTeam.setDesignation(copiedProposalProjectTeam.getDesignation());
			proposalProjectTeam.setUpdateUser(updateUser);
			proposalProjectTeam.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			proposalModuleDao.saveOrUpdateProposalProjectTeam(proposalProjectTeam);
			newProposalProjectTeams.add(proposalProjectTeam);
		}
		return newProposalProjectTeams;
	}

	private void copyProposalPersonRoles(Proposal proposal) {
		List<ModuleDerivedRoles> derivedRoles = rolesManagementDao.grantModuleDerivedRolesForCreator(Constants.DEV_PROPOSAL_MODULE_CODE);
		if (derivedRoles != null && !derivedRoles.isEmpty()) {
			proposalService.assignDerivedRolesForCreator(proposal, derivedRoles);
		}
		proposalService.assignDerivedRolesForPI(proposal.getInvestigator(), proposal.getProposalId());
	}

	private List<ProposalMileStone> copyProposalMileStones(Integer proposalId, List<ProposalMileStone> originalProposalMileStones, String updateUser) {
		List<ProposalMileStone> copyMileStones = new ArrayList<>();
		for (ProposalMileStone originalProposalMileStone : originalProposalMileStones) {
			ProposalMileStone copyProposalMileStone = new ProposalMileStone();
			copyProposalMileStone.setDuration(originalProposalMileStone.getDuration());
			copyProposalMileStone.setMileStone(originalProposalMileStone.getMileStone());
			copyProposalMileStone.setProposalId(proposalId);
			copyProposalMileStone.setStartMonth(originalProposalMileStone.getStartMonth());
			copyProposalMileStone.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			copyProposalMileStone.setUpdateUser(updateUser);
			copyMileStones.add(proposalModuleDao.saveOrUpdateProposalMileStone(copyProposalMileStone));
		}
		return copyMileStones;
	}

	private List<ProposalKPI> copyProposalKPIs(Integer proposalId, List<ProposalKPI> proposalKPIs, String updateUser) {
		List<ProposalKPI> proposalKPIList = new ArrayList<>();
		List<ProposalKPICriteria> proposalKPICriterias = new ArrayList<>();
		for (ProposalKPI proposalKPI : proposalKPIs) {
			ProposalKPI copyProposalKPI = new ProposalKPI();
			copyProposalKPI.setProposalId(proposalId);
			copyProposalKPI.setKpiTypeCode(proposalKPI.getKpiTypeCode());
			copyProposalKPI.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			copyProposalKPI.setUpdateUser(updateUser);
			proposalDao.saveOrUpdateProposalKPI(copyProposalKPI);
			for (ProposalKPICriteria proposalKPICriteria : proposalKPI.getProposalKPICriterias()) {
				ProposalKPICriteria copyproposalKPICriteria = new ProposalKPICriteria();
				copyproposalKPICriteria.setProposalKpiId(copyProposalKPI.getProposalKpiId());
				copyproposalKPICriteria.setKpiCriteriaTypeCode(proposalKPICriteria.getKpiCriteriaTypeCode());
				copyproposalKPICriteria.setTarget(proposalKPICriteria.getTarget());
				copyproposalKPICriteria.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
				copyproposalKPICriteria.setUpdateUser(updateUser);
				proposalKPICriterias.add(proposalDao.saveOrUpdateProposalKPICriterias(copyproposalKPICriteria));
			}
			copyProposalKPI.setProposalKPICriterias(proposalKPICriterias);
			proposalKPIList.add(copyProposalKPI);
		}
		return proposalKPIList;
	}

	private void copyProposalFinalBudget(ProposalVO proposalVO, Integer originalProposalId, Integer copiedProposalId,
			String activityTypeCode, List<BudgetHeader> budgetHeaders, String updateUser, String copyType) {
		List<BudgetHeader> copiedBudgetHeader = new ArrayList<>();
		Optional<BudgetHeader> approvedBudget = budgetHeaders.stream().filter(budgetHeader -> budgetHeader.getIsApprovedBudget().equals(true)).findAny();
		if (approvedBudget.isPresent()) {
			copiedBudgetHeader.add(approvedBudget.get());
		} else {
			Optional<BudgetHeader> finalBudget = budgetHeaders.stream().filter(budgetHeader -> budgetHeader.getIsFinalBudget().equals(true)).findAny();
			if (finalBudget.isPresent()) {
				copiedBudgetHeader.add(finalBudget.get());
			} else {
				Optional<BudgetHeader> latestBudget = budgetHeaders.stream().filter(budgetHeader -> budgetHeader.getIsLatestVersion()).findAny();
				if (latestBudget.isPresent()) {
					copiedBudgetHeader.add(latestBudget.get());
				}
			}
		}
		if (!copiedBudgetHeader.isEmpty()) {
			proposalVO.setBudgetHeaders(copyProposalBudgets(originalProposalId, copiedProposalId, activityTypeCode, copiedBudgetHeader, updateUser, copyType));
		}
	}

	private void createProposalHistoryDetails(Integer activeProposalId, Integer archiveProposalId, String requestType) {
		ProposalHistory proposalHistory = new ProposalHistory();
		proposalHistory.setActiveProposalId(activeProposalId);
		proposalHistory.setArchiveProposalId(archiveProposalId);
		proposalHistory.setRequestType(requestType);
		proposalDao.saveOrUpdateProposalHistory(proposalHistory);
		
	}
}
