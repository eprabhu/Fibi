
package com.polus.fibicomp.coi.service;

import java.io.File;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityDetails;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclosureOld;
import com.polus.fibicomp.coi.pojo.CoiDisclosureOldDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosureOldDetailsComments;
import com.polus.fibicomp.coi.pojo.CoiFileData;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
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
	private static final String DISPOSITION_STATUS_TYPE_CODE = "1";
	private static final String REVIEW_STATUS_TYPE_CODE = "1";
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


	@Override
	public ResponseEntity<Object> createDisclosure(ConflictOfInterestVO conflictOfInterestVO){
		CoiDisclosureOld CoiDisclosureOld = new CoiDisclosureOld();
		generateDisclosureCategoryType(conflictOfInterestVO.getDisclosureCategoryType(),CoiDisclosureOld);
		if(Constants.CURRENT_DISCLOSURE.equals(CoiDisclosureOld.getDisclosureCategoryTypeCode()) &&
				conflictOfInterestDao.getNumberOfDisclosure(CoiDisclosureOld.getDisclosureCategoryTypeCode()) > 0) {
			return new ResponseEntity<>("DISCLOSURE_EXISTS",HttpStatus.FORBIDDEN);
		}
		CoiDisclosureOld.setDisclosureStatusCode(Constants.DISCLOSURE_STATUS_PENDING);
		CoiDisclosureOld.setCoiDisclosureOldStatus(conflictOfInterestDao.getDisclosureStatusByCode(Constants.DISCLOSURE_STATUS_PENDING));
		CoiDisclosureOld.setDispositionStatusTypeCode(DISPOSITION_STATUS_TYPE_CODE);
		CoiDisclosureOld.setCoiDispositionStatus(conflictOfInterestDao.getDispositionStatusByCode(DISPOSITION_STATUS_TYPE_CODE));
		CoiDisclosureOld.setReviewStatusTypeCode(REVIEW_STATUS_TYPE_CODE);
		CoiDisclosureOld.setCoiReviewStatus(conflictOfInterestDao.getReviewStatus(REVIEW_STATUS_TYPE_CODE));
		CoiDisclosureOld.setDisclosureSequenceStatusCode(DISCLOSURE_SEQUENCE_STATUS_CODE);
		CoiDisclosureOld.setDisclosureVersionNumber(DISCLOSURE_VERSION_NUMBER);
		CoiDisclosureOld.setCoiDisclosureOldCategoryType(conflictOfInterestDao.getDisclosureCategoryTypeByCode(CoiDisclosureOld.getDisclosureCategoryTypeCode()));
		CoiDisclosureOld.setPersonId(AuthenticatedUser.getLoginPersonId());
		CoiDisclosureOld.setDisclosureNumber("1000-"+String.format("%04d",(conflictOfInterestDao.generateMaxDisclosureId())));
		CoiDisclosureOld.setExpirationDate(Timestamp.valueOf(LocalDateTime.now().plusYears(1)));
		conflictOfInterestDao.saveOrUpdateCoiDisclosureOld(CoiDisclosureOld);
		conflictOfInterestVO.setCoiDisclosureOld(CoiDisclosureOld);
		conflictOfInterestVO.setPerson(personDao.getPersonDetailById(AuthenticatedUser.getLoginPersonId()));
		conflictOfInterestVO.setNumberOfSFI(conflictOfInterestDao.getSFICountBasedOnParams(Constants.DISCLOSURE_STATUS_PENDING, CoiDisclosureOld.getPersonId(), CoiDisclosureOld.getDisclosureId()));
		if (Constants.PROPOSAL_DISCLOSURE.equals(CoiDisclosureOld.getDisclosureCategoryTypeCode())) {
			prepareProposalDisclosureProjectRelation(conflictOfInterestVO);
		} else {
			conflictOfInterestVO.setNumberOfAward(getNumberOfAwardInDisclosure(CoiDisclosureOld.getDisclosureStatusCode(), CoiDisclosureOld.getPersonId(), CoiDisclosureOld.getDisclosureId()));
			conflictOfInterestVO.setNumberOfProposal(getNumberOfProposalInDisclosure(CoiDisclosureOld.getDisclosureStatusCode(), CoiDisclosureOld.getPersonId(), CoiDisclosureOld.getDisclosureId()));
		}
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	private void prepareProposalDisclosureProjectRelation(ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosureOld CoiDisclosureOld = conflictOfInterestVO.getCoiDisclosureOld();
		List<COIFinancialEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(AuthenticatedUser.getLoginPersonId());
		if(sfiDetails.isEmpty()) {
			saveDisclosureDetail(CoiDisclosureOld, null, conflictOfInterestVO);
		} else {
			 sfiDetails.forEach(sfiDetail -> {
				saveDisclosureDetail(CoiDisclosureOld, sfiDetail, conflictOfInterestVO);
			});
		}
		conflictOfInterestVO.setProposalIdlinkedInDisclosure(conflictOfInterestVO.getModuleItemId().toString());
	}

	private void saveDisclosureDetail(CoiDisclosureOld CoiDisclosureOld, COIFinancialEntity sfiDetail, ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosureOldDetails proposalDisclosure = new CoiDisclosureOldDetails();
		proposalDisclosure.setCoiDisclosureOld(CoiDisclosureOld);
		proposalDisclosure.setDisclosureId(CoiDisclosureOld.getDisclosureId());
		proposalDisclosure.setDisclosureNumber(CoiDisclosureOld.getDisclosureNumber());
		proposalDisclosure.setCoiFinancialEntityId(sfiDetail != null ? sfiDetail.getCoiFinancialEntityId() : null);
		proposalDisclosure.setCoiFinancialEntity(sfiDetail != null ? sfiDetail : null);
		CoiDisclosureOldDetailsComments comment = new CoiDisclosureOldDetailsComments();
		comment.setDisclosureNumber(CoiDisclosureOld.getDisclosureNumber());
		comment.setCommentTypeCode(DISCLOSURE_COMMENT_TYPE_CODE);
		comment.setCoiDisclosureOldDetails(proposalDisclosure);
		proposalDisclosure.setComment(comment);
		proposalDisclosure.setModuleCode(conflictOfInterestVO.getModuleCode());
		proposalDisclosure.setModuleItemKey(conflictOfInterestVO.getModuleItemId().toString());
		conflictOfInterestDao.saveOrUpdateCoiDisclosureOldDetail(proposalDisclosure);
	}

	private void generateDisclosureCategoryType(String disclosureCategoryType, CoiDisclosureOld CoiDisclosureOld) {
		switch (disclosureCategoryType) {
		case CURRENTDISCLOSURE:
			CoiDisclosureOld.setDisclosureCategoryTypeCode(Constants.CURRENT_DISCLOSURE);
			break;
		case PROPOSALDISCLOSURE:
			CoiDisclosureOld.setDisclosureCategoryTypeCode(Constants.PROPOSAL_DISCLOSURE);
			break;
		case TRAVELDISCLOSURE:
			CoiDisclosureOld.setDisclosureCategoryTypeCode(Constants.TRAVEL_DISCLOSURE);
			break;
		default:
			break;
		}
	}

	@Override
	public ResponseEntity<Object> loadDisclosure(Integer disclosureId) {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		CoiDisclosureOld CoiDisclosureOld = conflictOfInterestDao.loadDisclosure(disclosureId);
		conflictOfInterestVO.setCoiDisclosureOld(CoiDisclosureOld);
		conflictOfInterestVO.setPerson(personDao.getPersonDetailById(CoiDisclosureOld.getPersonId()));
		conflictOfInterestVO.setNumberOfSFI(conflictOfInterestDao.getSFICountBasedOnParams(CoiDisclosureOld.getDisclosureStatusCode(), CoiDisclosureOld.getPersonId(), CoiDisclosureOld.getDisclosureId()));
		conflictOfInterestVO.setCoiSectionsType(conflictOfInterestDao.fetchCoiSections());
		conflictOfInterestVO.setCoiReviewActivitys(conflictOfInterestDao.fetchCoiReviewActivity());
		conflictOfInterestVO.setAdminGroup(commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.MODULE_CODE_COI_DISCLOSURE));
		if (Constants.PROPOSAL_DISCLOSURE.equals(CoiDisclosureOld.getDisclosureCategoryTypeCode()) ) {
			conflictOfInterestVO.setProposalIdlinkedInDisclosure(conflictOfInterestDao.getProposalIdLinkedInDisclosure(disclosureId));
		} else {
			conflictOfInterestVO.setNumberOfAward(getNumberOfAwardInDisclosure(CoiDisclosureOld.getDisclosureStatusCode(), CoiDisclosureOld.getPersonId(), CoiDisclosureOld.getDisclosureId()));
			conflictOfInterestVO.setNumberOfProposal(getNumberOfProposalInDisclosure(CoiDisclosureOld.getDisclosureStatusCode(), CoiDisclosureOld.getPersonId(), CoiDisclosureOld.getDisclosureId()));
		}
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
		vo.setCoiDisclosureOldDetailStatuses(conflictOfInterestDao.getCoiDisclosureOldDetailStatuses());
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
			disclosureDetail.setSfiCompleted(Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode()) ?
					conflictOfInterestDao.checkIsSFICompletedForProject(Constants.DEV_PROPOSAL_MODULE_CODE, disclosureDetail.getModuleItemId(),
							vo.getDisclosureId(), vo.getPersonId()) : Boolean.TRUE);
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
		List<COIFinancialEntity> coiFinancialEntitys = new ArrayList<>();
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode())) {
			coiFinancialEntitys = conflictOfInterestDao.getSFIOfDisclosure(vo.getPersonId());
		} else if (!Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode())) {
			coiFinancialEntitys = conflictOfInterestDao.getSFIBasedOnDisclosureId(vo.getDisclosureId());
		}
		vo.setCoiFinancialEntitys(coiFinancialEntitys);
		coiFinancialEntitys.forEach(coiFinancialEntity -> coiFinancialEntity.setCoiFinancialEntityDetails(conflictOfInterestDao.getCoiFinancialEntityDetails(coiFinancialEntity.getCoiFinancialEntityId())));
		return new ResponseEntity<>(coiFinancialEntitys, HttpStatus.OK);
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
		conflictOfInterestVO.setCoiFinancialEntityRelType(conflictOfInterestDao.fetchCOIFinancialEntityRelType());
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getSFIDetails(Integer coiFinancialEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiFinancialEntityDetails(conflictOfInterestDao.getCoiFinancialEntityDetails(coiFinancialEntityId));
		vo.setCoiFinancialEntity(conflictOfInterestDao.getSFIDetails(coiFinancialEntityId));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> createSFI(ConflictOfInterestVO vo) {
		COIFinancialEntity coiFinancialEntity = vo.getCoiFinancialEntity();
		coiFinancialEntity.setPersonId(AuthenticatedUser.getLoginPersonId());
		coiFinancialEntity.setEntityVersionNumber(DISCLOSURE_VERSION_NUMBER);
		coiFinancialEntity.setIsActive(true);
		if(coiFinancialEntity.getCoiEntityId() == null) {
			CoiEntity coiEntity = coiFinancialEntity.getCoiEntity();
			conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
			coiFinancialEntity.setCoiEntityId(coiEntity.getEntityId());
		}
		conflictOfInterestDao.saveOrUpdateCoiSFI(coiFinancialEntity);
		vo.setCoiFinancialEntityDetails(conflictOfInterestDao.getCoiFinancialEntityDetails(coiFinancialEntity.getCoiFinancialEntityId()));
		if (Boolean.TRUE.equals(vo.getProposalDisclosureWithNoSfi())) {
			conflictOfInterestDao.updateFinacialEntityInDisclosureRelation(vo.getDisclosureId(), coiFinancialEntity.getCoiFinancialEntityId());
		}
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public COIFinancialEntityDetails saveOrUpdateCoiFinancialEntityDetails(COIFinancialEntityDetails coiFinancialEntityDetails) {
		return conflictOfInterestDao.saveOrUpdateCoiFinancialEntityDetails(coiFinancialEntityDetails);
	}

	@Override
	public ResponseEntity<Object> certifyDisclosure(CoiDisclosureOld CoiDisclosureOld) {
		CoiDisclosureOld.setCertifiedBy(AuthenticatedUser.getLoginPersonId());
		CoiDisclosureOld.setCertifiedTimestamp(commonDao.getCurrentTimestamp());
		CoiDisclosureOld.setDisclosureStatusCode(REVIEW_IN_PROGRESS);
		CoiDisclosureOld.setDispositionStatusTypeCode(DISPOSITION_STATUS_TYPE_CODE);
		CoiDisclosureOld.setReviewStatusTypeCode(SUBMITTED_FOR_REVIEW);
		conflictOfInterestDao.certifyDisclosure(CoiDisclosureOld);
		CoiDisclosureOld CoiDisclosureOldDetails = conflictOfInterestDao.loadDisclosure(CoiDisclosureOld.getDisclosureId());
		CoiDisclosureOldDetails.setCreateUserFullName(personDao.getPersonFullNameByPersonId(CoiDisclosureOld.getCreateUser()));
		CoiDisclosureOldDetails.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(CoiDisclosureOld.getUpdateUser()));
		return new ResponseEntity<>(CoiDisclosureOldDetails, HttpStatus.OK);
	}

	@Override
	public String getEntityProjectRelations(ConflictOfInterestVO vo) {
		List<COIFinancialEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(vo.getPersonId());
		if (vo.getProposalIdlinkedInDisclosure() != null) {
			setProposalDisclosureHeaderDetail(vo);
		}
		List<CoiDisclosureOldDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(vo.getModuleCode(),
				vo.getModuleItemId(), vo.getPersonId(), vo.getDisclosureId());
		vo.setCoiDisclosureOldDetails(prepareDisclosureRelationDetails(sfiDetails, disclosureDetails, vo.getDisclosureStatusCode()));
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
		List<CoiDisclosureOldDetails> entityProjectRelations = vo.getCoiDisclosureOldDetails();
		entityProjectRelations.forEach(entityProjectRelation -> {
			CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
			conflictOfInterestDao.saveOrUpdateCoiDisclosureOldDetail(entityProjectRelation);
			coiConflictHistory.setComment(entityProjectRelation.getComment().getComments());
			coiConflictHistory.setCoiDetStatusCode(entityProjectRelation.getDiscDetStatusCode());
			coiConflictHistory.setDisclosureDetailsId(entityProjectRelation.getDisclosureDetailsId());
			conflictOfInterestDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
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

	private List<CoiDisclosureOldDetails> prepareDisclosureRelationDetails(List<COIFinancialEntity> sfiDetails, List<CoiDisclosureOldDetails> CoiDisclosureOldDetails, String disclosureStatusCode) {
		List<CoiDisclosureOldDetails> disclosureDetails = new ArrayList<>();
		Set<Integer> coiFinancialIds = new HashSet<>();
		if (!CoiDisclosureOldDetails.isEmpty()) {
			CoiDisclosureOldDetails.forEach(disclosureDetail -> coiFinancialIds.add(disclosureDetail.getCoiFinancialEntityId()));
			disclosureDetails.addAll(CoiDisclosureOldDetails);
		}
		if (!sfiDetails.isEmpty()) {
			sfiDetails.forEach(sfiDetail -> {
				if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode) && !coiFinancialIds.contains(sfiDetail.getCoiFinancialEntityId())) {
					CoiDisclosureOldDetails CoiDisclosureOldDetail = new CoiDisclosureOldDetails();
					CoiDisclosureOldDetail.setCoiFinancialEntity(sfiDetail);
					CoiDisclosureOldDetail.setComment(new CoiDisclosureOldDetailsComments());
					disclosureDetails.add(CoiDisclosureOldDetail);
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
		CoiDisclosureOld disclosure = conflictOfInterestDao.loadDisclosure(vo.getDisclosureId());
		CoiDisclosureOld copyDisclosure = new CoiDisclosureOld();
		copyDisclosure.setReviseComment(vo.getReviseComment());
		copyDisclosure(disclosure, copyDisclosure);
		vo.setCoiDisclosureOld(copyDisclosure);
		vo.setDisclosureId(copyDisclosure.getDisclosureId());
		copyDisclosureDetails(disclosure, copyDisclosure);
		return commonDao.convertObjectToJSON(vo);
	}

	private CoiDisclosureOld copyDisclosure(CoiDisclosureOld disclosure, CoiDisclosureOld copyDisclosure) {
		copyDisclosure.setDisclosureCategoryTypeCode(disclosure.getDisclosureCategoryTypeCode());
		copyDisclosure.setDisclosureStatusCode(Constants.DISCLOSURE_STATUS_PENDING);
		copyDisclosure.setDispositionStatusTypeCode(DISPOSITION_STATUS_TYPE_CODE);
		copyDisclosure.setReviewStatusTypeCode(REVIEW_STATUS_TYPE_CODE);
		copyDisclosure.setDisclosureSequenceStatusCode(DISCLOSURE_SEQUENCE_STATUS_CODE);
		copyDisclosure.setDisclosureVersionNumber(disclosure.getDisclosureVersionNumber() + 1);
		copyDisclosure.setPersonId(AuthenticatedUser.getLoginPersonId());
		copyDisclosure.setDisclosureNumber(disclosure.getDisclosureNumber());
		copyDisclosure.setExpirationDate(disclosure.getExpirationDate());
		return conflictOfInterestDao.saveOrUpdateCoiDisclosureOld(copyDisclosure);
	}

	private void copyDisclosureDetails(CoiDisclosureOld disclosure, CoiDisclosureOld copyDisclosure) {
		List<CoiDisclosureOldDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(null , null, disclosure.getPersonId(), disclosure.getDisclosureId());
		for (CoiDisclosureOldDetails disclosureDetail: disclosureDetails) {
			CoiDisclosureOldDetails copyDisclosureDetail = new CoiDisclosureOldDetails();
			BeanUtils.copyProperties(disclosureDetail, copyDisclosureDetail);
			CoiDisclosureOldDetailsComments copyComment = new CoiDisclosureOldDetailsComments();
			BeanUtils.copyProperties(disclosureDetail.getComment(), copyComment);
			copyComment.setDisclosureDetailsCommentId(null);
			copyDisclosureDetail.setDisclosureDetailsId(null);
			copyDisclosureDetail.setCoiDisclosureOld(copyDisclosure);
			copyDisclosureDetail.setDisclosureId(copyDisclosure.getDisclosureId());
			copyDisclosureDetail.setDisclosureNumber(copyDisclosure.getDisclosureNumber());
			copyComment.setCoiDisclosureOldDetails(copyDisclosureDetail);
			copyDisclosureDetail.setComment(copyComment);
			conflictOfInterestDao.saveOrUpdateCoiDisclosureOldDetail(copyDisclosureDetail);
		}
	}

	@Override
	public boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo) {
		Boolean isDisclosureQuestionnaire = conflictOfInterestDao.evaluateDisclosureQuestionnaire(vo.getModuleCode(),vo.getSubmoduleCode(),vo.getModuleItemId());
		conflictOfInterestDao.setDisclosureQuestionnaire(isDisclosureQuestionnaire,vo.getModuleItemId());
		return isDisclosureQuestionnaire;
	}

	@Override
	public ResponseEntity<Object> getDisclosureDetailsForSFI(Integer coiFinancialEntityId) {
		List<Integer> disclosureIds = conflictOfInterestDao.getDisclosureIdsByCOIFinancialEntityId(coiFinancialEntityId);
		List<CoiDisclosureOld> disclosures = new ArrayList<>();
		if (disclosureIds != null && !disclosureIds.isEmpty()) {
			List<String> sequenceStatusCodes = new ArrayList<>();
			sequenceStatusCodes.add(Constants.DISCLOSURE_SEQUENCE_STATUS_PENDING);
			sequenceStatusCodes.add(Constants.DISCLOSURE_SEQUENCE_STATUS_ACTIVE);
			disclosures = conflictOfInterestDao.getActiveAndPendingCoiDisclosureOldDetailsByDisclosureIdsAndSequenceStatus(disclosureIds, sequenceStatusCodes);
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
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(CREATE_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
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
		conflictOfInterestDao.startReview(Constants.SUBMITTED_FOR_REVIEW,vo.getCoiReview().getCoiReviewId());
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
			CoiDisclosureOld CoiDisclosureOld = reviewComments.getCoiDisclosureOld();
			CoiDisclosureOld.setCreateUserFullName(personDao.getUserFullNameByUserName(CoiDisclosureOld.getCreateUser()));
			CoiDisclosureOld.setUpdateUserFullName(personDao.getUserFullNameByUserName(CoiDisclosureOld.getUpdateUser()));
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
				reviewComments.setCoiFinancialEntity(conflictOfInterestDao.getSFIDetails(reviewComments.getCoiSubSectionsId()));
				break;
			case Constants.PROJECT_RELATIONSHIP:
				reviewComments.setCoiDisclosureOldDetails(conflictOfInterestDao.getProjectRelationship(reviewComments.getCoiSubSectionsId()));
				break;
			default:
				break;
			}
		}
	}

	@Override
	public CoiReview completeReview(ConflictOfInterestVO vo){
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		conflictOfInterestDao.startReview(COMPLETE,vo.getCoiReview().getCoiReviewId());
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
	public ResponseEntity<Object> completeDisclosureReview(Integer disclosureId){
		if (conflictOfInterestDao.numberOfInCompleteReview(disclosureId).equals(0)) {
			CoiDisclosureOld CoiDisclosureOld = new CoiDisclosureOld();
			CoiDisclosureOld.setDisclosureId(disclosureId);
			CoiDisclosureOld.setDisclosureStatusCode(COMPLETE);
			CoiDisclosureOld.setDispositionStatusTypeCode(ACTIVE);
			CoiDisclosureOld.setReviewStatusTypeCode(COMPLETE);
			CoiDisclosureOld.setDisclosureSequenceStatusCode(ACTIVE);
			conflictOfInterestDao.completeDisclosureReview(CoiDisclosureOld);
			return new ResponseEntity<>(conflictOfInterestDao.loadDisclosure(disclosureId), HttpStatus.OK);
		}
		return new ResponseEntity<>("REVIEW_STATUS_NOT_COMPLETE", HttpStatus.OK);
	}

	@Override
	public CoiDisclosureOldDetails updateProjectConflictStatus(CoiDisclosureOldDetails CoiDisclosureOldDetails){
		conflictOfInterestDao.addReviewerStatus(CoiDisclosureOldDetails);
		CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
		coiConflictHistory.setComment(CoiDisclosureOldDetails.getComment().getComments());
		coiConflictHistory.setCoiDetStatusCode(CoiDisclosureOldDetails.getCoiReviewerStatusCode());
		coiConflictHistory.setDisclosureDetailsId(CoiDisclosureOldDetails.getDisclosureDetailsId());
		conflictOfInterestDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
		return conflictOfInterestDao.getProjectRelationship(CoiDisclosureOldDetails.getDisclosureDetailsId());
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
		List<CoiDisclosureOld> CoiDisclosureOlds = conflictOfInterestDao.getCoiDisclosureOldsByDisclosureNumber(vo.getDisclosureNumber());
		if (CoiDisclosureOlds != null && !CoiDisclosureOlds.isEmpty()) {
			Set<String> userName = CoiDisclosureOlds.stream().map(CoiDisclosureOld::getUpdateUser).collect(Collectors.toSet());
			if (!userName.isEmpty()) {
				List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
				Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
				CoiDisclosureOlds.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
			}
			vo.setPerson(CoiDisclosureOlds.get(0).getPersonId() != null ? personDao.getPersonDetailById(CoiDisclosureOlds.get(0).getPersonId()) : null);
			vo.setCoiDisclosureOlds(CoiDisclosureOlds);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ConflictOfInterestVO saveSingleEntityProjectRelation(ConflictOfInterestVO vo) {
		try {
			CoiDisclosureOldDetails entityProjectRelation = vo.getCoiDisclosureOldDetail();
			conflictOfInterestDao.saveOrUpdateCoiDisclosureOldDetail(entityProjectRelation);
			vo.setCoiDisclosureOldDetail(entityProjectRelation);
		} catch (Exception e) {
			logger.error("saveSingleEntityProjectRelation : {}", e.getMessage());
			throw new ApplicationException("Failed to save Entity Project Relation", e, Constants.JAVA_ERROR);
		}
		return vo;
	}
	
	@Override
	public ResponseEntity<Object> saveOrUpdateCoiEntity(ConflictOfInterestVO vo) {
		COIFinancialEntity coiFinancialEntity = vo.getCoiFinancialEntity();
		CoiEntity coiEntity = coiFinancialEntity.getCoiEntity();
		conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getEntityDetails(Integer coiEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiEntity(conflictOfInterestDao.getCoiEntityDetailsById(coiEntityId));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getActiveDisclosure() {
		String personId = AuthenticatedUser.getLoginPersonId();
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		conflictOfInterestVO.setCoiDisclosureOlds(conflictOfInterestDao.getActiveDisclosure(personId));
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public String getCOIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = conflictOfInterestDao.getCOIDashboard(vo);
		} catch (Exception e) {
			logger.error("Error in method getCOIDashboard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getCOIAdminDashboard(@Valid CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = conflictOfInterestDao.getCOIAdminDashboard(vo);
		} catch (Exception e) {
			logger.error("Error in method getCOIAdminDashboard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSFIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = conflictOfInterestDao.getSFIDashboard(vo);
		} catch (Exception e) {
			logger.error("Error in method getSFIDashboard", e);
		}
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
		Integer travelDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		conflictOfInterestVO.setTravelDisclosureCount(travelDisclosureCount);
		vo.setTabName("DISCLOSURE_HISTORY");
		Integer disclosureHistoryCount = conflictOfInterestDao.getCOIDashboardCount(vo);
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
	public ResponseEntity<Object> getAllSystemEntityList() {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiEntityList(conflictOfInterestDao.getAllSystemEntityList(vo));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}
	
}
