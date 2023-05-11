
package com.polus.fibicomp.coi.service;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Calendar;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiFileData;
import com.polus.fibicomp.coi.pojo.CoiProjectAward;
import com.polus.fibicomp.coi.pojo.CoiProjectProposal;
import com.polus.fibicomp.coi.pojo.CoiProjectType;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureTraveler;
import com.polus.fibicomp.coi.pojo.DisclComment;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.security.AuthenticatedUser;


@Service(value = "conflictOfInterestService")
@Transactional
public class ConflictOfInterestServiceImpl implements ConflictOfInterestService {

	protected static Logger logger = LogManager.getLogger(ConflictOfInterestServiceImpl.class.getName());
	
	@Autowired
	@Qualifier(value = "conflictOfInterestDao")
	private ConflictOfInterestDao conflictOfInterestDao;
	
	@Autowired
	private PersonDao personDao;
	
	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;
	
	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardDao awardDao;

	private static final Integer DISCLOSURE_VERSION_NUMBER = 1;
	private static final String DISCLOSURE_SEQUENCE_STATUS_CODE = "1";
	private static final String DISCLOSURE_PENDING_CONFLICT_STATUS_CODE = "4";
	private static final String DISPOSITION_STATUS_TYPE_CODE = "1";
	private static final String DISPOSITION_STATUS_PENDING = "1";
	private static final String REVIEW_STATUS_TYPE_CODE = "1";
	private static final String REVIEW_STATUS_PENDING = "1";
	private static final String RISK_CATEGORY_LOW = "3";
	private static final String CURRENTDISCLOSURE = "CURRENT_DISCLOSURES";
	private static final String PROPOSALDISCLOSURE = "PROPOSAL_DISCLOSURES";
	private static final String TRAVELDISCLOSURE = "TRAVEL_DISCLOSURES";
	private static final String REVIEW_IN_PROGRESS = "2";
	private static final String SUBMITTED_FOR_REVIEW = "2";
	private static final String ACTIVE = "2";
	private static final String COMPLETE = "3";
	private static final String DELETE_MSG = "deleted successfully";
	private static final String COMPLETE_ACTIVIVITY ="4";
	private static final String START_ACTIVIVITY ="3";
	private static final String CREATE_ACTIVIVITY ="2";
	private static final String DISCLOSURE_COMMENT_TYPE_CODE = "1";
	private static final String APPROVED = "3";
	private static final String REVIEW_STATUS_COMPLETE = "4";
	private static final String DISCLOSURE_NO_CONFLICT_STATUS_CODE = "1";
	private static final String DISCLOSURE_VERSION_STATUS = "Active";
	private static final String DISCLOSURE_REVIEW_IN_PROGRESS = "3";
	private static final String DISCLOSURE_REVIEW_COMPLETED = "4";

	@Override
	//TODO revisit and check logics, statuses
	public ResponseEntity<Object> createDisclosure(ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosure coiDisclosure = conflictOfInterestVO.getCoiDisclosure() != null?conflictOfInterestVO.getCoiDisclosure():new CoiDisclosure();
		if(conflictOfInterestVO.getCoiProjectProposal()!=null) {
			CoiProjectProposal coiProjectProposal = conflictOfInterestDao.saveOrUpdateCoiProjectProposal(conflictOfInterestVO.getCoiProjectProposal());
			coiDisclosure.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
			coiDisclosure.setFcoiTypeCode("2");
			coiDisclosure.setModuleItemKey(coiProjectProposal.getProposalNumber());
			coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
//			coiDisclosure.setExpirationDate(coiProjectProposal.getProposalEndDate());
		}
		else if(conflictOfInterestVO.getCoiProjectAward()!=null) {
			CoiProjectAward coiProjectAward = conflictOfInterestDao.saveOrUpdateCoiProjectAward(conflictOfInterestVO.getCoiProjectAward());
			coiDisclosure.setModuleCode(Constants.AWARD_MODULE_CODE);
			coiDisclosure.setFcoiTypeCode("3");
			coiDisclosure.setModuleItemKey(coiProjectAward.getAwardNumber());
			coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
//			coiDisclosure.setExpirationDate(coiProjectAward.getAwardEndDate());
		}
		else if(conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode()!=null && !conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode().isEmpty()) {
			if (!conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode().equals("4")) {
				if(conflictOfInterestDao.isMasterDisclosurePresent(conflictOfInterestVO.getCoiDisclosure().getPersonId())) {
					return new ResponseEntity<>("Could not create master disclosure ",
							HttpStatus.METHOD_NOT_ALLOWED);
				}
				coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
			}
		}
		else if(conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode()!=null && !conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode().isEmpty()) {
			if (conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode().equals("3")) {
				coiDisclosure.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
				coiDisclosure.setFcoiTypeCode("2");
				coiDisclosure.setModuleItemKey(conflictOfInterestVO.getCoiDisclosure().getModuleItemKey());
			} else if (conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode().equals("1")) {
				coiDisclosure.setModuleCode(Constants.AWARD_MODULE_CODE);
				coiDisclosure.setFcoiTypeCode("3");
				coiDisclosure.setModuleItemKey(conflictOfInterestVO.getCoiDisclosure().getModuleItemKey());
			}
			coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
		}
		coiDisclosure.setCoiDispositionStatusType(conflictOfInterestDao.getDispositionStatusByCode(DISPOSITION_STATUS_PENDING));
		coiDisclosure.setCoiReviewStatusType(conflictOfInterestDao.getReviewStatusByCode(REVIEW_STATUS_PENDING));
		coiDisclosure.setCoiRiskCategory(conflictOfInterestDao.getRiskCategoryStatusByCode(RISK_CATEGORY_LOW));
		coiDisclosure.setVersionNumber(1);
		coiDisclosure.setVersionStatus("Pending");
		coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
		coiDisclosure.setReviewStatusCode(REVIEW_STATUS_PENDING);
		coiDisclosure.setRiskCategoryCode(RISK_CATEGORY_LOW);
		coiDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		conflictOfInterestDao.saveOrUpdateCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
		if(coiDisclosure.getFcoiTypeCode().equals("1")) { // if type is FCOI
			conflictOfInterestDao.syncProjectWithDisclosure(coiDisclosure.getDisclosureId(),
					coiDisclosure.getDisclosureNumber(), null);
		} else {
			prepareDisclosureEntityProjectRelation(conflictOfInterestVO);
		}
		return loadDisclosure(coiDisclosure.getDisclosureId());
	}
	
	private void prepareDisclosureEntityProjectRelation(ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosure coiDisclosure = conflictOfInterestVO.getCoiDisclosure();
		List<PersonEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(coiDisclosure.getPersonId());
		if(sfiDetails.isEmpty()) {
			saveDisclosureDetail(coiDisclosure, null, conflictOfInterestVO);
		} else {
			 sfiDetails.forEach(sfiDetail -> {
				saveDisclosureDetail(coiDisclosure, sfiDetail, conflictOfInterestVO);
			});
		}
	}

	private void prepareProposalDisclosureProjectRelation(ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosure CoiDisclosureOld = conflictOfInterestVO.getCoiDisclosure();
		List<PersonEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(AuthenticatedUser.getLoginPersonId());
		if(sfiDetails.isEmpty()) {
			saveDisclosureDetail(CoiDisclosureOld, null, conflictOfInterestVO);
		} else {
			 sfiDetails.forEach(sfiDetail -> {
				saveDisclosureDetail(CoiDisclosureOld, sfiDetail, conflictOfInterestVO);
			});
		}
		conflictOfInterestVO.setProposalIdlinkedInDisclosure(conflictOfInterestVO.getModuleItemId().toString());
	}

	private void saveDisclosureDetail(CoiDisclosure coiDisclosure, PersonEntity sfiDetail, ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclEntProjDetails coiDisclEntProjDetails = new CoiDisclEntProjDetails();
		coiDisclEntProjDetails.setCoiDisclosure(coiDisclosure);
		coiDisclEntProjDetails.setDisclosureId(coiDisclosure.getDisclosureId());
		coiDisclEntProjDetails.setDisclosureNumber(coiDisclosure.getDisclosureNumber());
		coiDisclEntProjDetails.setPersonEntityId(sfiDetail != null ? sfiDetail.getPersonEntityId() : null);
		coiDisclEntProjDetails.setPersonEntity(sfiDetail != null ? sfiDetail : null);
		coiDisclEntProjDetails.setEntityId(sfiDetail != null ? sfiDetail.getEntityId() : null);
		coiDisclEntProjDetails.setEntityNumber(sfiDetail != null ? sfiDetail.getEntityNumber() : null);
		coiDisclEntProjDetails.setModuleCode(coiDisclosure.getModuleCode());
		coiDisclEntProjDetails.setModuleItemKey(coiDisclosure.getModuleItemKey());
		conflictOfInterestDao.saveOrUpdateCoiDisclEntProjDetails(coiDisclEntProjDetails);
	}

//	private void generateDisclosureCategoryType(String disclosureCategoryType, CoiDisclosure CoiDisclosureOld) {
//		switch (disclosureCategoryType) {
//		case CURRENTDISCLOSURE:
//			CoiDisclosureOld.setDisclosureCategoryTypeCode(Constants.CURRENT_DISCLOSURE);
//			break;
//		case PROPOSALDISCLOSURE:
//			CoiDisclosureOld.setDisclosureCategoryTypeCode(Constants.PROPOSAL_DISCLOSURE);
//			break;
//		case TRAVELDISCLOSURE:
//			CoiDisclosureOld.setDisclosureCategoryTypeCode(Constants.TRAVEL_DISCLOSURE);
//			break;
//		default:
//			break;
//		}
//	}

	@Override
	public ResponseEntity<Object> loadDisclosure(Integer disclosureId) {
//		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
//		CoiDisclosure coiDisclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
//		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
//		conflictOfInterestVO.setPerson(personDao.getPersonDetailById(coiDisclosure.getPersonId()));
//		conflictOfInterestVO.setNumberOfSFI(conflictOfInterestDao.getSFICountBasedOnParams(coiDisclosure.getVersionStatus(),
//				coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
//		conflictOfInterestVO.setCoiReviewActivitys(conflictOfInterestDao.fetchCoiReviewActivity());
//		conflictOfInterestVO.setAdminGroup(commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.MODULE_CODE_COI_DISCLOSURE));
//		if (Constants.PROPOSAL_DISCLOSURE.equals(coiDisclosure.getFcoiTypeCode()) ) {
//			conflictOfInterestVO.setProposalIdlinkedInDisclosure(conflictOfInterestDao.getProposalIdLinkedInDisclosure(disclosureId));
//		} else {
//			conflictOfInterestVO.setNumberOfAward(getNumberOfAwardInDisclosure(coiDisclosure.getConflictStatusCode(),
//					coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
//			conflictOfInterestVO.setNumberOfProposal(getNumberOfProposalInDisclosure(coiDisclosure.getConflictStatusCode(),
//					coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
//		}
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		CoiDisclosure coiDisclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
		String coiTypeCode = coiDisclosure.getFcoiTypeCode();
		if (Constants.PROPOSAL_DISCLOSURE.equals(coiTypeCode)) {
			List<DisclosureDetailDto> projDetailObjs = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE,
					AuthenticatedUser.getLoginPersonId(), disclosureId, null);
			conflictOfInterestVO.setProjectDetail(projDetailObjs == null || projDetailObjs.isEmpty() ? null : projDetailObjs.get(0));
		} else if (Constants.AWARD_DISCLOSURE.equals(coiTypeCode)) {
			List<DisclosureDetailDto> projDetailObjs = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
					AuthenticatedUser.getLoginPersonId(), disclosureId, null);
			conflictOfInterestVO.setProjectDetail(projDetailObjs == null || projDetailObjs.isEmpty() ? null : projDetailObjs.get(0));
		}
		coiDisclosure.setNumberOfSFI(conflictOfInterestDao.getNumberOfSFIBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
		coiDisclosure.setNumberOfProposals(conflictOfInterestDao.getNumberOfProposalsBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
		coiDisclosure.setNumberOfAwards(conflictOfInterestDao.getNumberOfAwardsBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
		coiDisclosure.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getPersonId()));
		coiDisclosure.setCoiDisclosureFcoiType(conflictOfInterestDao.getCoiDisclosureFcoiTypeByCode(coiTypeCode));
		coiDisclosure.setPerson(personDao.getPersonDetailById(coiDisclosure.getPersonId()));
		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setCoiSectionsType(conflictOfInterestDao.fetchCoiSections());
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	private Integer getNumberOfAwardInDisclosure (String disclosureStatusCode, String personId, Integer disclosureId) {
		List<DisclosureDetailDto> disclosureDetails;
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode)) {
			disclosureDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE, personId,
					null, disclosureStatusCode);
		} else {
			disclosureDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE, personId,
					disclosureId, disclosureStatusCode);
		}
		return disclosureDetails.size();
	}

	private Integer getNumberOfProposalInDisclosure(String disclosureStatusCode, String personId, Integer disclosureId) {
		List<DisclosureDetailDto> proposalDetails;
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode)) {
			proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, personId,
					null, disclosureStatusCode);
		} else {
			proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, personId,
					disclosureId, disclosureStatusCode);
		}
		return proposalDetails.size();
	}

	@Override
	public String getDisclosureRelations(ConflictOfInterestVO vo) {
		vo.setCoiConflictStatusTypes(conflictOfInterestDao.getCoiConflictStatusTypes());
		vo.setCoiProjConflictStatusTypes(conflictOfInterestDao.getProjConflictStatusTypes());
		prepareProposalDisclosureDetails(vo);
		prepareAwardDisclosureDetails(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void prepareAwardDisclosureDetails(ConflictOfInterestVO vo) {
		List<DisclosureDetailDto> disclosureDetails;
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode())) {
			disclosureDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE, vo.getPersonId(),
					null, vo.getDisclosureStatusCode());
		} else {
			disclosureDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE, vo.getPersonId(),
					vo.getDisclosureId(), vo.getDisclosureStatusCode());
		}
		for (DisclosureDetailDto disclosureDetail : disclosureDetails) {
			disclosureDetail.setSfiCompleted(Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode()) ?
					conflictOfInterestDao.checkIsSFICompletedForProject(Constants.AWARD_MODULE_CODE, disclosureDetail.getModuleItemId(),
							vo.getDisclosureId(), vo.getPersonId()) : Boolean.TRUE);
			disclosureDetail.setDisclosureStatusCount(conflictOfInterestDao.disclosureStatusCount(Constants.AWARD_MODULE_CODE,
					disclosureDetail.getModuleItemId(), vo.getDisclosureId(), vo.getPersonId()));
		}
		vo.setAwards(disclosureDetails);
	}

	private void prepareProposalDisclosureDetails(ConflictOfInterestVO vo) {
		List<DisclosureDetailDto> proposalDetails;
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode())) {
			proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getPersonId(),
					null, vo.getDisclosureStatusCode());
		} else {
			proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getPersonId(),
					 vo.getDisclosureId(), vo.getDisclosureStatusCode());
		}
		for (DisclosureDetailDto disclosureDetail : proposalDetails) {
			disclosureDetail.setSfiCompleted(conflictOfInterestDao.checkIsSFICompletedForProject(Constants.DEV_PROPOSAL_MODULE_CODE,
					disclosureDetail.getModuleItemId(), vo.getDisclosureId(), vo.getPersonId()));
			disclosureDetail.setDisclosureStatusCount(conflictOfInterestDao.disclosureStatusCount(Constants.DEV_PROPOSAL_MODULE_CODE,
					disclosureDetail.getModuleItemId(), vo.getDisclosureId(), vo.getPersonId()));
		}
		vo.setProposals(proposalDetails);
	}

	private void prepareProposalDetails(Proposal proposal, List<DisclosureDetailDto> proposalDetails, String personId,
										Integer disclosureId, String disclosureStatusCode) {
		DisclosureDetailDto detail = new DisclosureDetailDto();
		detail.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
		detail.setModuleItemId(proposal.getProposalId());
		detail.setTitle(proposal.getTitle());
		detail.setModuleStatus(proposal.getProposalStatus().getDescription());
		detail.setStartDate(proposal.getStartDate());
		detail.setEndDate(proposal.getEndDate());
		detail.setUnitNumber(proposal.getHomeUnitNumber());
		detail.setUnitName(proposal.getHomeUnitName());
		if (proposal.getSponsorCode() != null) {
			detail.setSponsor(commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(),
					proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
		}
		if (proposal.getPrimeSponsorCode() != null) {
			detail.setPrimeSponsor(commonService.getSponsorFormatBySponsorDetail(proposal.getPrimeSponsor().getSponsorCode(),
					proposal.getPrimeSponsor().getSponsorName(), proposal.getPrimeSponsor().getAcronym()));
		}
		List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId());
		proposalPersons.stream().filter(proposalPerson -> proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)).forEach(proposalPerson -> { 
			detail.setPrincipalInvestigator(proposalPerson.getFullName());
		});
		detail.setSfiCompleted(Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode) ?
				conflictOfInterestDao.checkIsSFICompletedForProject(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId(),
						disclosureId, personId) : Boolean.TRUE);
		detail.setDisclosureStatusCount(conflictOfInterestDao.disclosureStatusCount(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId(), disclosureId, personId));
		proposalDetails.add(detail);
	}

	@Override
	public ResponseEntity<Object> getSFIOfDisclosure(ConflictOfInterestVO vo) {
		/*Need to finalize the logic*/
		List<PersonEntity> personEntities = new ArrayList<>();
		if (REVIEW_STATUS_PENDING.equals(vo.getReviewStatus())) {
			personEntities = conflictOfInterestDao.getSFIOfDisclosure(vo.getPersonId());
		} else {
			personEntities = conflictOfInterestDao.getSFIBasedOnDisclosureId(vo.getDisclosureId());
		}
//		personEntities.forEach(personEntity -> personEntity.setValidPersonEntityRelTypes(conflictOfInterestDao
//				.getValidPersonEntityRelTypes(personEntity.getPersonEntityId())));
		vo.setPersonEntities(personEntities);
		return new ResponseEntity<>(personEntities, HttpStatus.OK);
	}

	@Override
	public List<CoiEntity> searchEnitiy(String searchString) {
		return conflictOfInterestDao.searchEnitiy(searchString);
	}
	
	@Override
	public ResponseEntity<Object> loadAddSFILookups() {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		conflictOfInterestVO.setEntityStatus(conflictOfInterestDao.fetchEntityStatus());
		conflictOfInterestVO.setEntityType(conflictOfInterestDao.fetchEntityType());
		conflictOfInterestVO.setPersonEntityRelType(conflictOfInterestDao.fetchPersonEntityRelType());
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getSFIDetails(Integer coiFinancialEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setPersonEntityRelationships(conflictOfInterestDao.getCoiFinancialEntityDetails(coiFinancialEntityId));
		vo.setPersonEntity(conflictOfInterestDao.getSFIDetails(coiFinancialEntityId));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> createSFI(ConflictOfInterestVO vo) {
		PersonEntity personEntity = vo.getPersonEntity();
		personEntity.setVersionNumber(1);
		personEntity.setVersionStatus("Pending");
		personEntity.setIsRelationshipActive(true);
		personEntity.setPersonId(AuthenticatedUser.getLoginPersonId());
		conflictOfInterestDao.saveOrUpdateSFI(personEntity);
		if (vo.getDisclosureId() != null && vo.getDisclosureNumber() != null) {
			conflictOfInterestDao.syncProjectWithDisclosure(vo.getDisclosureId(), vo.getDisclosureNumber(), personEntity.getPersonEntityId());
		}
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public PersonEntityRelationship saveOrUpdatePersonEntityRelationship(PersonEntityRelationship personEntityRelationship) {
		personEntityRelationship.setValidPersonEntityRelType(conflictOfInterestDao.getValidPersonEntityRelTypeByTypeCode(personEntityRelationship.getValidPersonEntityRelTypeCode()));
		conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(personEntityRelationship);
		return conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityRelId(personEntityRelationship.getPersonEntityRelId());
	}

	@Override
	public ResponseEntity<Object> certifyDisclosure(CoiDisclosure coiDisclosure) {
		Boolean isDisclosureQuestionnaire = conflictOfInterestDao.evaluateDisclosureQuestionnaire(8,0,coiDisclosure.getDisclosureId());
		if(isDisclosureQuestionnaire) {
			if(!conflictOfInterestDao.isSFIDefined(conflictOfInterestDao.loadDisclosure(coiDisclosure.getDisclosureId()).getPersonId())) {
				return new ResponseEntity<>("Atleast one SFI has to be defined", HttpStatus.METHOD_NOT_ALLOWED);
			}
			if(!conflictOfInterestDao.isRelationshipDefined(coiDisclosure.getDisclosureId())) {
				return new ResponseEntity<>("Atleast one relationship has to be defined", HttpStatus.METHOD_NOT_ALLOWED);
			}
		}
		coiDisclosure.setCertifiedBy(AuthenticatedUser.getLoginPersonId());
		coiDisclosure.setCertifiedAt(commonDao.getCurrentTimestamp());
		coiDisclosure.setReviewStatusCode(SUBMITTED_FOR_REVIEW);
		coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, 1);
		cal.add(Calendar.DAY_OF_MONTH, -1);
		coiDisclosure.setExpirationDate(cal.getTime());
		conflictOfInterestDao.certifyDisclosure(coiDisclosure);
		CoiDisclosure coiDisclosureObj = conflictOfInterestDao.loadDisclosure(coiDisclosure.getDisclosureId());
		coiDisclosureObj.setCreateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getCreateUser()));
		coiDisclosureObj.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getUpdateUser()));
		return new ResponseEntity<>(coiDisclosureObj, HttpStatus.OK);
	}

	@Override
	public String getEntityProjectRelations(ConflictOfInterestVO vo) {
		List<PersonEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(vo.getPersonId());
		if (vo.getProposalIdlinkedInDisclosure() != null) {
			setProposalDisclosureHeaderDetail(vo);
		}
		List<CoiDisclEntProjDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(vo.getModuleCode(),
				vo.getModuleItemId(), vo.getPersonId(), vo.getDisclosureId());
		disclosureDetails.forEach(disclosureDetail -> disclosureDetail.setDisclComment(conflictOfInterestDao.getDisclEntProjRelationComment(disclosureDetail.getDisclosureDetailsId())));
		vo.setCoiDisclEntProjDetails(prepareDisclosureRelationDetails(sfiDetails, disclosureDetails, vo.getDisclosureStatusCode()));
		return commonDao.convertObjectToJSON(vo);
	}

	private void setProposalDisclosureHeaderDetail(ConflictOfInterestVO vo) {
		Proposal proposal = proposalDao.fetchProposalById(Integer.parseInt(vo.getProposalIdlinkedInDisclosure()));
		List<DisclosureDetailDto> proposals = new ArrayList<>();
		prepareProposalDetails(proposal, proposals, vo.getPersonId(), vo.getDisclosureId(), vo.getDisclosureStatusCode());
		vo.setProposals(proposals);
	}

	@Override
	public ConflictOfInterestVO saveEntityProjectRelation(ConflictOfInterestVO vo) {
		List<CoiDisclEntProjDetails> entityProjectRelations = vo.getCoiDisclEntProjDetails();
		entityProjectRelations.forEach(entityProjectRelation -> {
			conflictOfInterestDao.saveOrUpdateCoiDisclEntProjDetails(entityProjectRelation);
			DisclComment disclComment = entityProjectRelation.getDisclComment();
			disclComment.setComponentTypeCode("1");		//Disclosure detail comment
			disclComment.setCommentType("1");		//Disclosure detail comment
			disclComment.setCommentPersonId(AuthenticatedUser.getLoginPersonId());
			disclComment.setDocumentOwnerPersonId(vo.getPersonId());
			disclComment.setIsPrivate(false);
			disclComment.setComponentReferenceId(entityProjectRelation.getDisclosureDetailsId());
			disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
		});
		return vo;
	}

	@Override
	public String checkSFICompleted(ConflictOfInterestVO vo) {
		if (Constants.DEV_PROPOSAL_MODULE_CODE.equals(vo.getModuleCode())) {
			vo.setSfiCompleted(conflictOfInterestDao.checkIsSFICompletedForProject(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getModuleItemId(), vo.getDisclosureId(), vo.getPersonId()));
		} else if (Constants.AWARD_MODULE_CODE.equals(vo.getModuleCode())) {
			vo.setSfiCompleted(conflictOfInterestDao.checkIsSFICompletedForProject(Constants.AWARD_MODULE_CODE, vo.getModuleItemId(), vo.getDisclosureId(), vo.getPersonId()));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private List<CoiDisclEntProjDetails> prepareDisclosureRelationDetails(List<PersonEntity> sfiDetails, List<CoiDisclEntProjDetails> disclEntProjDetails,
																		  String disclosureStatusCode) {
		List<CoiDisclEntProjDetails> disclosureDetails = new ArrayList<>();
		Set<Integer> coiFinancialIds = new HashSet<>();
		if (!disclEntProjDetails.isEmpty()) {
			disclEntProjDetails.forEach(disclosureDetail -> coiFinancialIds.add(disclosureDetail.getPersonEntityId()));
			disclosureDetails.addAll(disclEntProjDetails);
		}
		if (!sfiDetails.isEmpty()) {
			sfiDetails.forEach(sfiDetail -> {
				if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode) && !coiFinancialIds.contains(sfiDetail.getPersonEntityId())) {
					CoiDisclEntProjDetails coiDisclEntProjDetails = new CoiDisclEntProjDetails();
					coiDisclEntProjDetails.setPersonEntity(sfiDetail);
//					CoiDisclosureOldDetail.setComment(new CoiDisclosureOldDetailsComments());
					disclosureDetails.add(coiDisclEntProjDetails);
				}
			});
		}
		return disclosureDetails;
	}

	@Override
	public ResponseEntity<Object> loadDisclosureAdminDashboardCounts() {
		ConflictOfInterestVO externalReviewVO = new ConflictOfInterestVO();
		externalReviewVO.setConflictIdentifiedCount(4);
		externalReviewVO.setNewSubmissionsCount(7);
		externalReviewVO.setUnassignedCount(5);
		externalReviewVO.setPendingEntityApproval(2);
		externalReviewVO.setReviewCommentsCount(conflictOfInterestDao.getReviewCommentsCount());
		return new ResponseEntity<>(externalReviewVO, HttpStatus.OK);
	}

	@Override
	public String reviseDisclosure(ConflictOfInterestVO vo) {
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(vo.getDisclosureId());
		if (!disclosure.getReviewStatusCode().equals("4")) {  // review status code 4 -> completed
			throw new ApplicationException("You are attempting to revise a pending version of disclosure. You can only have one revision at a time.",
					Constants.JAVA_ERROR);
		}
		CoiDisclosure copyDisclosure = new CoiDisclosure();
		copyDisclosure.setRevisionComment(vo.getRevisionComment());
		copyDisclosure(disclosure, copyDisclosure);
		vo.setCoiDisclosure(copyDisclosure);
		vo.setDisclosureId(copyDisclosure.getDisclosureId());
		copyDisclosureDetails(disclosure, copyDisclosure);
		if(copyDisclosure.getFcoiTypeCode().equals("1")) { // if type is FCOI
			conflictOfInterestDao.syncProjectWithDisclosure(copyDisclosure.getDisclosureId(),
					copyDisclosure.getDisclosureNumber(), null);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private CoiDisclosure copyDisclosure(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		copyDisclosure.setFcoiTypeCode(disclosure.getFcoiTypeCode());
		copyDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_TYPE_CODE);
		copyDisclosure.setReviewStatusCode(REVIEW_STATUS_TYPE_CODE);
		copyDisclosure.setVersionStatus(DISCLOSURE_SEQUENCE_STATUS_CODE);
		copyDisclosure.setVersionNumber(disclosure.getVersionNumber() + 1);
		copyDisclosure.setPersonId(AuthenticatedUser.getLoginPersonId());
		copyDisclosure.setDisclosureNumber(disclosure.getDisclosureNumber());
		copyDisclosure.setExpirationDate(disclosure.getExpirationDate());
		return conflictOfInterestDao.saveOrUpdateCoiDisclosure(copyDisclosure);
	}

	private void copyDisclosureDetails(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		List<CoiDisclEntProjDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(null ,
				null, disclosure.getPersonId(), disclosure.getDisclosureId());
		for (CoiDisclEntProjDetails disclosureDetail: disclosureDetails) {
			CoiDisclEntProjDetails copyDisclosureDetail = new CoiDisclEntProjDetails();
			BeanUtils.copyProperties(disclosureDetail, copyDisclosureDetail);
//			CoiDisclosureOldDetailsComments copyComment = new CoiDisclosureOldDetailsComments();
//			BeanUtils.copyProperties(disclosureDetail.getComment(), copyComment);
//			copyComment.setDisclosureDetailsCommentId(null);
			copyDisclosureDetail.setDisclosureDetailsId(null);
			copyDisclosureDetail.setCoiDisclosure(copyDisclosure);
			copyDisclosureDetail.setDisclosureId(copyDisclosure.getDisclosureId());
			copyDisclosureDetail.setDisclosureNumber(copyDisclosure.getDisclosureNumber());
//			copyComment.setCoiDisclosureOldDetails(copyDisclosureDetail);
//			copyDisclosureDetail.setComment(copyComment);
			conflictOfInterestDao.saveOrUpdateCoiDisclEntProjDetails(copyDisclosureDetail);
		}
	}

	@Override
	public boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo) {
		Boolean isDisclosureQuestionnaire = conflictOfInterestDao.evaluateDisclosureQuestionnaire(vo.getModuleCode(),vo.getSubmoduleCode(),vo.getModuleItemId());
//		conflictOfInterestDao.setDisclosureQuestionnaire(isDisclosureQuestionnaire,vo.getModuleItemId());
		return isDisclosureQuestionnaire;
	}

	@Override
	public ResponseEntity<Object> getDisclosureDetailsForSFI(Integer coiFinancialEntityId) {
		List<Integer> disclosureIds = conflictOfInterestDao.getDisclosureIdsByCOIFinancialEntityId(coiFinancialEntityId);
		List<CoiDisclosure> disclosures = new ArrayList<>();
		if (disclosureIds != null && !disclosureIds.isEmpty()) {
			List<String> sequenceStatusCodes = new ArrayList<>();
			sequenceStatusCodes.add(Constants.DISCLOSURE_SEQUENCE_STATUS_PENDING);
			sequenceStatusCodes.add(Constants.DISCLOSURE_SEQUENCE_STATUS_ACTIVE);
			disclosures = conflictOfInterestDao.getActiveAndPendingCoiDisclosureDetailsByDisclosureIdsAndSequenceStatus(disclosureIds, sequenceStatusCodes);
			if (disclosures != null && !disclosures.isEmpty()) {
				disclosures.forEach(disclosure -> {
					disclosure.setUpdateUserFullName(personDao.getUserFullNameByUserName(disclosure.getUpdateUser()));
				});
			}
		}
		return new ResponseEntity<>(disclosures, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getDisclosureRelationsForSFI(Integer coiFinancialEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiFinancialEntityId(coiFinancialEntityId);
		prepareDisclosureProposalRelations(vo);
		prepareDisclosureAwardRelations(vo);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	private void prepareDisclosureAwardRelations(ConflictOfInterestVO vo) {
		List<String> awardIds = conflictOfInterestDao.getModuleItemKeysByCOIFinancialEntityIdAndModuleCode(vo.getCoiFinancialEntityId(), Constants.AWARD_MODULE_CODE);
		List<DisclosureDetailDto> awardDetails = new ArrayList<>();
		if (awardIds != null && !awardIds.isEmpty()) {
			List<Integer> awardIdDetails = new ArrayList<>();
			awardIds.forEach(awardId -> {
				awardIdDetails.add(Integer.parseInt(awardId));
			});
			List<Award> awards = conflictOfInterestDao.getAwardsBasedOnAwardIds(awardIdDetails);
			if (awards != null && !awards.isEmpty()) {
				awards.forEach(award -> {
					DisclosureDetailDto detail = new DisclosureDetailDto();
					detail.setModuleCode(Constants.AWARD_MODULE_CODE);
					detail.setModuleItemId(award.getAwardId());
					detail.setModuleItemKey(award.getAwardNumber());
					detail.setModuleStatus(award.getAwardStatus().getDescription());
					detail.setTitle(award.getTitle());
					if (award.getSponsorCode() != null) {
						detail.setSponsor(commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
					}
					List<AwardPerson> awardPersons = awardDao.getAwardPersons(award.getAwardId());
					awardPersons.forEach(awardPerson -> {
						if (awardPerson.getPersonRoleId() != null && awardPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
							detail.setPrincipalInvestigator(awardPerson.getFullName());
						}
					});
					awardDetails.add(detail);
				});
			}
		}
		vo.setAwards(awardDetails);
	}

	private void prepareDisclosureProposalRelations(ConflictOfInterestVO vo) {
		List<String> proposalIds = conflictOfInterestDao.getModuleItemKeysByCOIFinancialEntityIdAndModuleCode(vo.getCoiFinancialEntityId(), Constants.DEV_PROPOSAL_MODULE_CODE);
		List<DisclosureDetailDto> proposalDetails = new ArrayList<>();
		if (proposalIds != null && !proposalIds.isEmpty()) {
			List<Integer> proposalIdList = new ArrayList<>();
			proposalIds.forEach(proposalId -> {
				proposalIdList.add(Integer.parseInt(proposalId));
			});
			List<Proposal> proposals = conflictOfInterestDao.getProposalsBasedOnProposalIds(proposalIdList);
			if (proposals != null && !proposals.isEmpty()) {
				proposals.forEach(proposal -> {
					DisclosureDetailDto detail = new DisclosureDetailDto();
					detail.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
					detail.setModuleItemId(proposal.getProposalId());
					detail.setModuleStatus(proposal.getProposalStatus().getDescription());
					detail.setTitle(proposal.getTitle());
					if (proposal.getSponsorCode() != null) {
						detail.setSponsor(commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
					}
					List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId());
					proposalPersons.forEach(proposalPerson -> {
						if (proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)) {
							detail.setPrincipalInvestigator(proposalPerson.getFullName());
						}
					});
					proposalDetails.add(detail);
				});
			}
		}
		vo.setProposals(proposalDetails);
	}

	@Override
	public CoiReview saveOrUpdateCoiReview(ConflictOfInterestVO vo){
		CoiReview coiReview = vo.getCoiReview();
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		if (coiReview.getCoiReviewId() == null) {
		 coiReview.setReviewStatusTypeCode(REVIEW_STATUS_TYPE_CODE);
		 coiReview.setCoiReviewStatus(conflictOfInterestDao.getReviewStatus(REVIEW_STATUS_TYPE_CODE));
		}
		conflictOfInterestDao.saveOrUpdateCoiReview(vo.getCoiReview());
		/*Need clarification*/
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(CREATE_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		/*Need clarification*/
		return coiReview;
	}

	@Override
	public List<CoiReview> getCoiReview(Integer disclosureId){
		List<CoiReview> coiReviews = conflictOfInterestDao.getCoiReview(disclosureId);
		coiReviews.forEach(coiReview -> {
			coiReview.setAssigneePersonName(personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId()));
		});
		return conflictOfInterestDao.getCoiReview(disclosureId);
	}

	@Override
	public CoiReview startReview(ConflictOfInterestVO vo){
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		conflictOfInterestDao.startReview(DISCLOSURE_REVIEW_IN_PROGRESS,vo.getCoiReview().getCoiReviewId());
		String personName = vo.getCoiReview().getAssigneePersonName();
		CoiReview coiReview = conflictOfInterestDao.loadCoiReview(vo.getCoiReview().getCoiReviewId());
		coiReview.setAssigneePersonName(personName);
		vo.setCoiReview(coiReview);
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(START_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		return coiReview;
	}
	
	@Override
	public String saveOrUpdateCoiReviewComments(MultipartFile[] files,String formDataJSON){
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		CoiReviewComments coiReviewComment = new CoiReviewComments();
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ConflictOfInterestVO.class);
			coiReviewComment = vo.getCoiReviewComment();
			vo.getCoiReviewComment().setCommentedByPersonId(AuthenticatedUser.getLoginPersonId());
			conflictOfInterestDao.saveOrUpdateCoiReviewComments(coiReviewComment);
			coiReviewComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(coiReviewComment.getUpdateUser()));
		    vo.setCoiReviewComment(coiReviewComment);
		    List<CoiReviewCommentTag> coiReviewCommentTag = addTagPerson(coiReviewComment.getCoiReviewCommentTag(), coiReviewComment.getCoiReviewCommentId(),
		    		coiReviewComment.getCoiReviewId());
		    coiReviewComment.setCoiReviewCommentTag(coiReviewCommentTag);
		    List<CoiReviewCommentAttachment> coiReviewCommentAttachments = addReviewAttachment(files, vo.getCoiReviewComment().getCoiReviewCommentId());
		    vo.setCoiReviewCommentAttachment(coiReviewCommentAttachments);
		} catch (Exception e) {
			throw new ApplicationException("error in saveOrUpdateCoiReviewComments", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}
	
	private List<CoiReviewCommentAttachment> addReviewAttachment(MultipartFile[] files, Integer coiReviewCommentId) {
		List<CoiReviewCommentAttachment> CoiDisclosureOldAttachments = new ArrayList<>();
		try {
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					CoiReviewCommentAttachment attachment = new CoiReviewCommentAttachment();
					File file = new File(files[i].getOriginalFilename());
					CoiFileData fileData = new CoiFileData();
					fileData.setData(files[i].getBytes());
					fileData = conflictOfInterestDao.saveFileData(fileData);
					attachment.setFileDataId(fileData.getFileDataId());
					attachment.setCoiReviewCommentId(coiReviewCommentId);
					attachment.setFileName(file.getName());
					attachment.setMimeType(files[i].getContentType());
					attachment = conflictOfInterestDao.saveOrUpdateAttachment(attachment);
					CoiDisclosureOldAttachments.add(attachment);
				}
			}
		} catch (Exception e) {
			throw new ApplicationException("error in addReviewAttachment", e, Constants.JAVA_ERROR);
		}
		return CoiDisclosureOldAttachments;
	}

	private List<CoiReviewCommentTag> addTagPerson(List<CoiReviewCommentTag> coiReviewCommentTags, Integer coiReviewCommentId,  Integer coiReviewId) {
		try {
			coiReviewCommentTags.forEach(coiReviewCommentTag ->{
				coiReviewCommentTag.setCoiReviewCommentId(coiReviewCommentId);
				coiReviewCommentTag.setCoiReviewId(coiReviewId);
				conflictOfInterestDao.saveOrUpdateCoiReviewCommentTag(coiReviewCommentTag);
			});
		} catch (Exception e) {
			throw new ApplicationException("error in addTagPerson", e, Constants.JAVA_ERROR);
		}
		return coiReviewCommentTags;
	}

	@Override
	public String loadCoiReviewComments(ConflictOfInterestVO vo){
		if (vo.getPersonId() != null) {
			vo.setTagGroupId(commonDao.getAdminGroupIdsBasedOnPersonId(vo.getPersonId()));
		}
		conflictOfInterestDao.loadCoiReviewComments(vo);
		List<CoiReviewComments> coiReviewComments = vo.getCoiReviewComments();
		coiReviewComments.forEach(reviewComments -> {
			reviewComments.setCoiReviewCommentAttachment(conflictOfInterestDao.fetchReviewCommentAttachment(reviewComments.getCoiReviewCommentId()));
			reviewComments.setUpdateUserFullName(personDao.getUserFullNameByUserName(reviewComments.getUpdateUser()));
			CoiDisclosure coiDisclosure = reviewComments.getCoiDisclosure();
			coiDisclosure.setCreateUserFullName(personDao.getUserFullNameByUserName(coiDisclosure.getCreateUser()));
			coiDisclosure.setUpdateUserFullName(personDao.getUserFullNameByUserName(coiDisclosure.getUpdateUser()));
			reviewComments.setCoiReviewCommentTag(conflictOfInterestDao.fetchCoiReviewCommentTag(reviewComments.getCoiReviewCommentId()));
			reviewComments.getCoiReviewCommentTag().forEach(reviewCommentTag -> {
				if (reviewCommentTag.getTagPersonId() != null) {
					reviewCommentTag.setTagPersonFullName(personDao.getPersonFullNameByPersonId(reviewCommentTag.getTagPersonId()));
				}
				if (reviewCommentTag.getTagGroupId() != null) {
					reviewCommentTag.setTagGroupName(conflictOfInterestDao.fetchadminGroupName(reviewCommentTag.getTagGroupId()));
				}
			});
			loadSubSection(reviewComments);
		});
		return commonDao.convertObjectToJSON(vo);
	}

	private void loadSubSection(CoiReviewComments reviewComments){
		if (reviewComments.getCoiSubSectionsId() != null) {
			switch (reviewComments.getCoiSectionsTypeCode()) {
			case Constants.SFI:
				reviewComments.setPersonEntity(conflictOfInterestDao.getSFIDetails(reviewComments.getCoiSubSectionsId()));
				break;
			case Constants.PROJECT_RELATIONSHIP:
				reviewComments.setDisclEntProjDetails(conflictOfInterestDao.getProjectRelationship(reviewComments.getCoiSubSectionsId()));
				break;
			default:
				break;
			}
		}
	}

	@Override
	public CoiReview completeReview(ConflictOfInterestVO vo){
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		conflictOfInterestDao.startReview(DISCLOSURE_REVIEW_COMPLETED,vo.getCoiReview().getCoiReviewId());
		String personName = vo.getCoiReview().getAssigneePersonName();
		CoiReview coiReview = conflictOfInterestDao.loadCoiReview(vo.getCoiReview().getCoiReviewId());
		coiReview.setAssigneePersonName(personName);
		vo.setCoiReview(coiReview);
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(COMPLETE_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		return coiReview;
	}

	@Override
	public String deleteReview(Integer coiReviewId){
		try {
			conflictOfInterestDao.deleteReviewAssigneeHistory(coiReviewId);
			List<CoiReviewCommentAttachment> coiReviewCommentAttachments = conflictOfInterestDao.fetchReviewAttachmentByReviewId(coiReviewId);
			coiReviewCommentAttachments.forEach(coiReviewCommentAttachment -> {
				conflictOfInterestDao.deleteFileData(conflictOfInterestDao.getFileDataById(coiReviewCommentAttachment.getFileDataId()));
			});
			conflictOfInterestDao.deleteReviewTagByReviewId(coiReviewId);
			conflictOfInterestDao.deleteReviewCommentAttachment(coiReviewId);
			conflictOfInterestDao.deleteReviewComment(coiReviewId);
			conflictOfInterestDao.deleteReview(coiReviewId);
			return commonDao.convertObjectToJSON(DELETE_MSG);
		} catch(Exception e) {
			throw new ApplicationException("deleteCoiReview",e, Constants.JAVA_ERROR);
		}
	}
	
	@Override
	public String deleteReviewComment(Integer coiReviewCommentId){
		try {
			List<CoiReviewCommentAttachment> coiReviewCommentAttachments = conflictOfInterestDao.fetchReviewAttachmentByCommentId(coiReviewCommentId);
			coiReviewCommentAttachments.forEach(coiReviewCommentAttachment -> {
				conflictOfInterestDao.deleteFileData(conflictOfInterestDao.getFileDataById(coiReviewCommentAttachment.getFileDataId()));
			});
			conflictOfInterestDao.deleteReviewTagByCommentId(coiReviewCommentId);
			conflictOfInterestDao.deleteReviewAttachmentByCommentId(coiReviewCommentId);
			conflictOfInterestDao.deleteReviewCommentByCommentId(coiReviewCommentId);
			return commonDao.convertObjectToJSON(DELETE_MSG);
		}  catch(Exception e) {
			throw new ApplicationException("deleteCoiReviewComment",e, Constants.JAVA_ERROR);
		}
		
	}
	
	@Override
	public String deleteReviewAttachment(Integer coiReviewCommentAttId){
		try {
			conflictOfInterestDao.deleteAttachment(coiReviewCommentAttId);
			return commonDao.convertObjectToJSON(DELETE_MSG);
		}  catch(Exception e) {
			throw new ApplicationException("deleteReviewAttachment",e, Constants.JAVA_ERROR);
		}
		
	}

	@Override
	public ResponseEntity<byte[]> downloadCoiReviewAttachment(Integer attachmentId) {
		CoiReviewCommentAttachment attachment = conflictOfInterestDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			CoiFileData fileData = conflictOfInterestDao.getFileDataById(attachment.getFileDataId());
			byte[] data = fileData.getData();
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType(attachment.getMimeType()));
			String filename = attachment.getFileName();
			headers.setContentDispositionFormData(filename, filename);
			headers.setContentLength(data.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
		} catch (Exception e) {
			throw new ApplicationException("downloadCoiReviewAttachment",e, Constants.JAVA_ERROR);
		}
		return attachmentData;
	}

	@Override
	public ResponseEntity<Object> completeDisclosureReview(Integer disclosureId, Integer disclosureNumber){
		if (conflictOfInterestDao.numberOfInCompleteReview(disclosureId).equals(0)) {
			CoiDisclosure coiDisclosure = new CoiDisclosure();
			coiDisclosure.setDisclosureId(disclosureId);
			coiDisclosure.setDispositionStatusCode(APPROVED);
			coiDisclosure.setReviewStatusCode(REVIEW_STATUS_COMPLETE);
			coiDisclosure.setVersionStatus(DISCLOSURE_VERSION_STATUS);
			conflictOfInterestDao.completeDisclosureReview(coiDisclosure);
			CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
			if (disclosure.getFcoiTypeCode().equals("1")) {
				conflictOfInterestDao.archiveDisclosureOldVersions(disclosureId, disclosureNumber);
			}
			return new ResponseEntity<>(loadDisclosure(disclosureId), HttpStatus.OK);
		}
		return new ResponseEntity<>("REVIEW_STATUS_NOT_COMPLETE", HttpStatus.OK);
	}

	@Override
	public CoiDisclEntProjDetails updateProjectConflictStatus(CoiDisclEntProjDetails disclEntProjDetails){
		conflictOfInterestDao.addReviewerStatus(disclEntProjDetails);
		CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
//		coiConflictHistory.setComment(disclEntProjDetails.getComment().getComments());
//		coiConflictHistory.setCoiDetStatusCode(disclEntProjDetails.getCoiReviewerStatusCode());
		coiConflictHistory.setDisclosureDetailsId(disclEntProjDetails.getDisclosureDetailsId());
		conflictOfInterestDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
		return conflictOfInterestDao.getProjectRelationship(disclEntProjDetails.getDisclosureDetailsId());
	} 

	@Override
	public List<CoiConflictHistory> getCoiConflictHistory(Integer disclosureDetailsId){
		List<CoiConflictHistory> coiConflictHistory = conflictOfInterestDao.getCoiConflictHistory(disclosureDetailsId);
		coiConflictHistory.forEach(conflictHistory -> {
			conflictHistory.setUpdateUserFullName(personDao.getUserFullNameByUserName(conflictHistory.getUpdateUser()));
		});
		return coiConflictHistory;
	}

	@Override
	public String loadProposalsForDisclosure(ConflictOfInterestVO vo) {
		List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE,
				AuthenticatedUser.getLoginPersonId(), null, null);
		return commonDao.convertObjectToJSON(proposalDetails);
	}

	@Override
	public String loadDisclosureHistory(ConflictOfInterestVO vo) {
		List<CoiDisclosure> coiDisclosures = conflictOfInterestDao.getCoiDisclosuresByDisclosureNumber(vo.getDisclosureNumber());
		if (coiDisclosures != null && !coiDisclosures.isEmpty()) {
			Set<String> userName = coiDisclosures.stream().map(CoiDisclosure::getUpdateUser).collect(Collectors.toSet());
			if (!userName.isEmpty()) {
				List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
				Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(),
						person -> person.getFullName()));
				coiDisclosures.stream().filter(item -> item.getUpdateUser() != null).filter(item ->
						collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item ->
						item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
			}
			vo.setPerson(coiDisclosures.get(0).getPersonId() != null ? personDao.getPersonDetailById(coiDisclosures.get(0).getPersonId()) : null);
			vo.setCoiDisclosures(coiDisclosures);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ConflictOfInterestVO saveSingleEntityProjectRelation(ConflictOfInterestVO vo) {
		try {
			CoiDisclEntProjDetails entityProjectRelation = vo.getCoiDisclEntProjDetail();
			conflictOfInterestDao.saveOrUpdateCoiDisclEntProjDetails(entityProjectRelation);
			DisclComment disclComment = entityProjectRelation.getDisclComment();
			disclComment.setComponentTypeCode("1");		//Disclosure detail comment
			disclComment.setCommentType("1");		//Disclosure detail comment
			disclComment.setCommentPersonId(AuthenticatedUser.getLoginPersonId());
			disclComment.setDocumentOwnerPersonId(vo.getPersonId());
			disclComment.setIsPrivate(false);
			disclComment.setComponentReferenceId(entityProjectRelation.getDisclosureDetailsId());
			disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
			vo.setCoiDisclEntProjDetail(entityProjectRelation);
		} catch (Exception e) {
			logger.error("saveSingleEntityProjectRelation : {}", e.getMessage());
			throw new ApplicationException("Failed to save Entity Project Relation", e, Constants.JAVA_ERROR);
		}
		return vo;
	}
	
	@Override
	//TODO update & versioning is pending
	public ResponseEntity<Object> saveOrUpdateCoiEntity(ConflictOfInterestVO vo) {
		CoiEntity coiEntity = vo.getCoiEntity();
		coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
		if (coiEntity.getEntityId() == null) {
			coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
			coiEntity.setEntityStatusCode("3"); //Unverified
			coiEntity.setIsActive(true); // Y
			coiEntity.setVersionStatus("Active");
			coiEntity.setVersionNumber(1);
			coiEntity.setEntityNumber(conflictOfInterestDao.generateMaxCoiEntityNumber());
		}
		conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getEntityDetails(Integer coiEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiEntity(conflictOfInterestDao.getCoiEntityDetailsById(coiEntityId));
		vo.getCoiEntity().setUpdatedUserFullName(personDao.getUserFullNameByUserName(vo.getCoiEntity().getUpdateUser()));
		vo.getCoiEntity().setCreateUserFullName(personDao.getUserFullNameByUserName(vo.getCoiEntity().getCreateUser()));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getActiveDisclosure() {
		String personId = AuthenticatedUser.getLoginPersonId();
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		conflictOfInterestVO.setCoiDisclosures(conflictOfInterestDao.getActiveDisclosure(personId));
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public String getCOIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = conflictOfInterestDao.getCOIDashboard(vo);
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getCOIAdminDashboard(@Valid CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = conflictOfInterestDao.getCOIAdminDashboard(vo);
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSFIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = conflictOfInterestDao.getSFIDashboard(vo);

		return commonDao.convertObjectToJSON(dashBoardProfile);
	}
	
	@Override
	public String getCOIDashboardCount(CoiDashboardVO vo) {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		vo.setTabName("IN_PROGRESS_DISCLOSURES");
		Integer inProgressDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		conflictOfInterestVO.setInProgressDisclosureCount(inProgressDisclosureCount);
		vo.setTabName("APPROVED_DISCLOSURES");
		Integer approvedDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		conflictOfInterestVO.setApprovedDisclosureCount(approvedDisclosureCount);
		vo.setTabName("TRAVEL_DISCLOSURES");
//		Integer travelDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		Integer travelDisclosureCount = 0;
		conflictOfInterestVO.setTravelDisclosureCount(travelDisclosureCount);
		vo.setTabName("DISCLOSURE_HISTORY");
//		Integer disclosureHistoryCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		Integer disclosureHistoryCount = 0;
		conflictOfInterestVO.setDisclosureHistoryCount(disclosureHistoryCount);
		return commonDao.convertObjectToJSON(conflictOfInterestVO);
	}

	@Override
	public ResponseEntity<Object> getAllEntityList(ConflictOfInterestVO vo) {
		String personId = AuthenticatedUser.getLoginPersonId();
		vo.setPersonId(personId);
		vo.setCoiEntityList(conflictOfInterestDao.getAllEntityList(vo));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> setEntityStatus(ConflictOfInterestVO vo) {
		conflictOfInterestDao.setEntityStatus(vo);
		return new ResponseEntity<>(vo, HttpStatus.OK);	
	}

	@Override
	public ResponseEntity<Object> getAllSystemEntityList(CoiDashboardVO vo) {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		conflictOfInterestVO.setCoiEntityList(conflictOfInterestDao.getAllSystemEntityList(vo));
		conflictOfInterestVO.setEntityCount(conflictOfInterestDao.getAllSystemEntityListCount(vo));
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getAllCoiTravelDisclosureList() {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiTravelDisclosureList(conflictOfInterestDao.getAllCoiTravelDisclosureList(vo));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public CoiTravelDisclosure getCoiTravelDisclosureDetailsById(Integer travelDisclosureId) {
		return conflictOfInterestDao.getCoiTravelDisclosureDetailsById(travelDisclosureId);
	}

	@Override
	public ResponseEntity<Object> getCoiProjectTypes() {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		List<CoiProjectType> coiProjectTypes = conflictOfInterestDao.getCoiProjectTypes();
		List<CoiProjectType> filteredProjectTypes = coiProjectTypes.stream()
		        .filter(projectType -> !projectType.getDescription().contains("Ad-hoc"))
		        .collect(Collectors.toList());
		vo.setCoiProjectTypes(filteredProjectTypes);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getPersonEntityDashboard(CoiDashboardVO vo) {
		return new ResponseEntity<>(conflictOfInterestDao.getPersonEntityDashboard(vo), HttpStatus.OK);
	}


	@Override
	public ResponseEntity<Object> getCOIReviewerDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = conflictOfInterestDao.getCOIReviewerDashboard(vo);
			return new ResponseEntity<>(dashBoardProfile, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Error in method getCOIReviewerDashboard", e);
			return new ResponseEntity<>(dashBoardProfile, HttpStatus.INTERNAL_SERVER_ERROR);
		}

	}

	@Override
	public ResponseEntity<Object> loadDisclosureReviewerQuickCardCounts() {
		try {
			ConflictOfInterestVO conflictOfInterestVO = conflictOfInterestDao
					.loadDisclosureQuickCardCounts("REVIEWER", AuthenticatedUser.getLoginPersonId());
			return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Error in method loadDisclosureReviewerQuickCardCounts", e);
			return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
	@Override
	public ResponseEntity<Object> getCoiEntityDetails(Integer personEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiEntity(conflictOfInterestDao.getCoiEntityDetailsByEntityId(personEntityId));
		vo.getCoiEntity().setUpdatedUserFullName(personDao.getUserFullNameByUserName(vo.getCoiEntity().getUpdateUser()));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getPersonEntityDetails(Integer personEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setPersonEntity(conflictOfInterestDao.getPersonEntityDetailsByEntityId(personEntityId));
		List<PersonEntityRelationship> PersonEntityRelationships = conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityId(personEntityId);
		PersonEntityRelationships.forEach(PersonEntityRelationship -> {
			conflictOfInterestDao.getValidPersonEntityRelTypeByTypeCode(PersonEntityRelationship.getValidPersonEntityRelTypeCode());
		});
		vo.setPersonEntityRelationships(PersonEntityRelationships);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> getRelatioshipDetails(String tabName) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setTabName(tabName);
		vo = getDisclosureTypecode(vo);
		vo.setValidPersonEntityRelTypes(conflictOfInterestDao.getRelatioshipDetails(vo.getDisclosureTypeCode()));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	private ConflictOfInterestVO getDisclosureTypecode(ConflictOfInterestVO vo) {
		if(vo.getTabName().equals("FINANCIAL")) {
			vo.setDisclosureTypeCode("1");
		}
		else if(vo.getTabName().equals("COMMITMENT")) {
			vo.setDisclosureTypeCode("2");
		}
		else if(vo.getTabName().equals("TRAVEL")) {
			vo.setDisclosureTypeCode("4");
		}		
		return vo;
	}

	@Override
	public ResponseEntity<Object> getPersonEntityRelationship(ConflictOfInterestVO vo) {
		vo = getDisclosureTypecode(vo);
		vo.setPersonEntityRelationships(conflictOfInterestDao.getRelatioshipDetails(vo));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> createCoiTravelDisclosure(ConflictOfInterestVO vo) {
		CoiTravelDisclosure coiTravelDisclosure = new CoiTravelDisclosure();
		coiTravelDisclosure.setTravelDisclosureId(vo.getTravelDisclosureId());
		coiTravelDisclosure.setVersionNumber(1);
		coiTravelDisclosure.setEntityId(vo.getEntityId());
		coiTravelDisclosure.setEntityNumber(vo.getEntityNumber());
		coiTravelDisclosure.setTravelTitle(vo.getTravelTitle());
		coiTravelDisclosure.setTravelstate(vo.getTravelState());
		coiTravelDisclosure.setDestinationCity(vo.getDestinationCity());
		coiTravelDisclosure.setPurposeOfTheTrip(vo.getPurposeOfTheTrip());
		coiTravelDisclosure.setRelationshipToYourResearch(vo.getRelationshipToYourResearch());
		coiTravelDisclosure.setTravelStartDate(vo.getTravelStartDate());
		coiTravelDisclosure.setTravelEndDate(vo.getTravelEndDate());
		coiTravelDisclosure.setTravelStatusCode(vo.getTravelStatusCode());
		coiTravelDisclosure.setIsSponsoredTravel(vo.getIsSponsoredTravel());
		coiTravelDisclosure.setTravelAmount(vo.getTravelAmount());
		coiTravelDisclosure.setDestinationCountry(vo.getDestinationCountry());
		coiTravelDisclosure.setNoOfDays(vo.getNoOfDays());
		coiTravelDisclosure.setIsInterNationalTravel(vo.getIsInternationalTravel());
		coiTravelDisclosure.setTravelNumber(conflictOfInterestDao.generateMaxTravelNumber());
		coiTravelDisclosure.setPersonId(vo.getPersonId());
		if (vo.getPersonId() != null && vo.getEntityId() != null) {
			Integer personEntityId;
			personEntityId = conflictOfInterestDao.fetchMaxPersonEntityId(vo.getPersonId(), vo.getEntityId());
			if (personEntityId != null) {
				coiTravelDisclosure.setPersonEntityId(personEntityId);
			} else {
				personEntityId = conflictOfInterestDao.generateMaxPersonEntityId();
				PersonEntity personEntity = new PersonEntity();
				personEntity.setPersonId(vo.getPersonId());
				personEntity.setEntityId(vo.getEntityId());
				personEntity.setEntityNumber(vo.getEntityNumber());
				personEntity.setPersonEntityId(personEntityId);
				personEntity.setVersionNumber(vo.getVersionNumber());
				personEntity.setVersionStatus(vo.getVersionStatus());
				personEntity.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				personEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
				personEntity.setCreateTimestamp(commonDao.getCurrentTimestamp());
				personEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
			}
		}
		coiTravelDisclosure.setVersionStatus(vo.getVersionStatus());
		coiTravelDisclosure.setIsInterNationalTravel(vo.getIsInternationalTravel());
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		List<String> travellerTypeCodeList = new ArrayList<>();
		vo.getTravellerTypeCode().forEach(typeCode -> {
			CoiTravelDisclosureTraveler coiTravelDisclosureTraveller = new CoiTravelDisclosureTraveler();
			coiTravelDisclosureTraveller.setTravelTravelerId(vo.getTravelTravellerId());
			coiTravelDisclosureTraveller.setTravelDisclosureId(coiTravelDisclosure.getTravelDisclosureId());
			coiTravelDisclosureTraveller.setUpdateUser(AuthenticatedUser.getLoginUserName());
			coiTravelDisclosureTraveller.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			conflictOfInterestDao.saveOrUpdateCoiTravelDisclosureTraveller(coiTravelDisclosureTraveller);
			travellerTypeCodeList.add(typeCode);;
		});
		coiTravelDisclosure.setCoiTravellerTypeCodeList(travellerTypeCodeList);
		return new ResponseEntity<>(coiTravelDisclosure, HttpStatus.OK); 
	}
   	
	@Override
	public ResponseEntity<Object> loadTravellerTypesLookup() {
		return new ResponseEntity<>(conflictOfInterestDao.loadTravellerTypesLookup(),  HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> loadTravelStatusTypesLookup() {
		return new ResponseEntity<>(conflictOfInterestDao.loadTravelStatusTypesLookup(),  HttpStatus.OK);
	}

	@Override
	public Object checkEntityAdded(Integer entityId) {
		return conflictOfInterestDao.checkEntityAdded(entityId);
	}
}
