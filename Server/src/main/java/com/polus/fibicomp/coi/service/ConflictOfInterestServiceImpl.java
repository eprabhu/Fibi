
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
import com.polus.fibicomp.coi.pojo.COIEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityDetails;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetailsComments;
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
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
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
		CoiDisclosure coiDisclosure = new CoiDisclosure();
		generateDisclosureCategoryType(conflictOfInterestVO.getDisclosureCategoryType(),coiDisclosure);
		if(Constants.CURRENT_DISCLOSURE.equals(coiDisclosure.getDisclosureCategoryTypeCode()) &&
				conflictOfInterestDao.getNumberOfDisclosure(coiDisclosure.getDisclosureCategoryTypeCode()) > 0) {
			return new ResponseEntity<>("DISCLOSURE_EXISTS",HttpStatus.FORBIDDEN);
		}
		coiDisclosure.setDisclosureStatusCode(Constants.DISCLOSURE_STATUS_PENDING);
		coiDisclosure.setCoiDisclosureStatus(conflictOfInterestDao.getDisclosureStatusByCode(Constants.DISCLOSURE_STATUS_PENDING));
		coiDisclosure.setDispositionStatusTypeCode(DISPOSITION_STATUS_TYPE_CODE);
		coiDisclosure.setCoiDispositionStatus(conflictOfInterestDao.getDispositionStatusByCode(DISPOSITION_STATUS_TYPE_CODE));
		coiDisclosure.setReviewStatusTypeCode(REVIEW_STATUS_TYPE_CODE);
		coiDisclosure.setCoiReviewStatus(conflictOfInterestDao.getReviewStatus(REVIEW_STATUS_TYPE_CODE));
		coiDisclosure.setDisclosureSequenceStatusCode(DISCLOSURE_SEQUENCE_STATUS_CODE);
		coiDisclosure.setDisclosureVersionNumber(DISCLOSURE_VERSION_NUMBER);
		coiDisclosure.setCoiDisclosureCategoryType(conflictOfInterestDao.getDisclosureCategoryTypeByCode(coiDisclosure.getDisclosureCategoryTypeCode()));
		coiDisclosure.setPersonId(AuthenticatedUser.getLoginPersonId());
		coiDisclosure.setDisclosureNumber("1000-"+String.format("%04d",(conflictOfInterestDao.generateMaxDisclosureId())));
		coiDisclosure.setExpirationDate(Timestamp.valueOf(LocalDateTime.now().plusYears(1)));
		conflictOfInterestDao.saveOrUpdateCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setPerson(personDao.getPersonDetailById(AuthenticatedUser.getLoginPersonId()));
		conflictOfInterestVO.setNumberOfSFI(conflictOfInterestDao.getSFICountBasedOnParams(Constants.DISCLOSURE_STATUS_PENDING, coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
		if (Constants.PROPOSAL_DISCLOSURE.equals(coiDisclosure.getDisclosureCategoryTypeCode())) {
			prepareProposalDisclosureProjectRelation(conflictOfInterestVO);
		} else {
			conflictOfInterestVO.setNumberOfAward(getNumberOfAwardInDisclosure(coiDisclosure.getDisclosureStatusCode(), coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
			conflictOfInterestVO.setNumberOfProposal(getNumberOfProposalInDisclosure(coiDisclosure.getDisclosureStatusCode(), coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
		}
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	private void prepareProposalDisclosureProjectRelation(ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosure coiDisclosure = conflictOfInterestVO.getCoiDisclosure();
		List<COIFinancialEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(AuthenticatedUser.getLoginPersonId());
		if(sfiDetails.isEmpty()) {
			saveDisclosureDetail(coiDisclosure, null, conflictOfInterestVO);
		} else {
			 sfiDetails.forEach(sfiDetail -> {
				saveDisclosureDetail(coiDisclosure, sfiDetail, conflictOfInterestVO);
			});
		}
		conflictOfInterestVO.setProposalIdlinkedInDisclosure(conflictOfInterestVO.getModuleItemId().toString());
	}

	private void saveDisclosureDetail(CoiDisclosure coiDisclosure, COIFinancialEntity sfiDetail, ConflictOfInterestVO conflictOfInterestVO) {
		CoiDisclosureDetails proposalDisclosure = new CoiDisclosureDetails();
		proposalDisclosure.setCoiDisclosure(coiDisclosure);
		proposalDisclosure.setDisclosureId(coiDisclosure.getDisclosureId());
		proposalDisclosure.setDisclosureNumber(coiDisclosure.getDisclosureNumber());
		proposalDisclosure.setCoiFinancialEntityId(sfiDetail != null ? sfiDetail.getCoiFinancialEntityId() : null);
		proposalDisclosure.setCoiFinancialEntity(sfiDetail != null ? sfiDetail : null);
		CoiDisclosureDetailsComments comment = new CoiDisclosureDetailsComments();
		comment.setDisclosureNumber(coiDisclosure.getDisclosureNumber());
		comment.setCommentTypeCode(DISCLOSURE_COMMENT_TYPE_CODE);
		comment.setCoiDisclosureDetails(proposalDisclosure);
		proposalDisclosure.setComment(comment);
		proposalDisclosure.setModuleCode(conflictOfInterestVO.getModuleCode());
		proposalDisclosure.setModuleItemKey(conflictOfInterestVO.getModuleItemId().toString());
		conflictOfInterestDao.saveOrUpdateCoiDisclosureDetail(proposalDisclosure);
	}

	private void generateDisclosureCategoryType(String disclosureCategoryType, CoiDisclosure coiDisclosure) {
		switch (disclosureCategoryType) {
		case CURRENTDISCLOSURE:
			coiDisclosure.setDisclosureCategoryTypeCode(Constants.CURRENT_DISCLOSURE);
			break;
		case PROPOSALDISCLOSURE:
			coiDisclosure.setDisclosureCategoryTypeCode(Constants.PROPOSAL_DISCLOSURE);
			break;
		case TRAVELDISCLOSURE:
			coiDisclosure.setDisclosureCategoryTypeCode(Constants.TRAVEL_DISCLOSURE);
			break;
		default:
			break;
		}
	}

	@Override
	public ResponseEntity<Object> loadDisclosure(Integer disclosureId) {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		CoiDisclosure coiDisclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setPerson(personDao.getPersonDetailById(coiDisclosure.getPersonId()));
		conflictOfInterestVO.setNumberOfSFI(conflictOfInterestDao.getSFICountBasedOnParams(coiDisclosure.getDisclosureStatusCode(), coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
		conflictOfInterestVO.setCoiSectionsType(conflictOfInterestDao.fetchCoiSections());
		conflictOfInterestVO.setCoiReviewActivitys(conflictOfInterestDao.fetchCoiReviewActivity());
		conflictOfInterestVO.setAdminGroup(commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.MODULE_CODE_COI_DISCLOSURE));
		if (Constants.PROPOSAL_DISCLOSURE.equals(coiDisclosure.getDisclosureCategoryTypeCode()) ) {
			conflictOfInterestVO.setProposalIdlinkedInDisclosure(conflictOfInterestDao.getProposalIdLinkedInDisclosure(disclosureId));
		} else {
			conflictOfInterestVO.setNumberOfAward(getNumberOfAwardInDisclosure(coiDisclosure.getDisclosureStatusCode(), coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
			conflictOfInterestVO.setNumberOfProposal(getNumberOfProposalInDisclosure(coiDisclosure.getDisclosureStatusCode(), coiDisclosure.getPersonId(), coiDisclosure.getDisclosureId()));
		}
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	private Integer getNumberOfAwardInDisclosure (String disclosureStatusCode, String personId, Integer disclosureId) {
		List<Integer> awardIds = new ArrayList<>();
		List<Award> awards = new ArrayList<>();
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode)) {
			awardIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.AWARD_MODULE_CODE, personId, null, null);
			if (!awardIds.isEmpty()) {
				awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, null, null, null);
			}
		} else {
			awardIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.AWARD_MODULE_CODE, personId, null, disclosureId);
			if (!awardIds.isEmpty()) {
				awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, null, null, null);
			}
		}
		return awards.size();
	}

	private Integer getNumberOfProposalInDisclosure(String disclosureStatusCode, String personId, Integer disclosureId) {
		List<Proposal> proposals = new ArrayList<>();
		List<Integer> proposalIds = new ArrayList<>();
		List<Integer> proposalStatuses = new ArrayList<>();
		getProposalStatus(proposalStatuses);
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode)) {
			proposalIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, personId, proposalStatuses, null);
			if (!proposalIds.isEmpty()) {
				proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, null);
			}
		} else {
			proposalIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, personId, proposalStatuses, disclosureId);
			if (!proposalIds.isEmpty()) {
				proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, null);
			}
		}
		return proposals.size();
	}

	@Override
	public String getDisclosureRelations(ConflictOfInterestVO vo) {
		vo.setCoiDisclosureDetailStatuses(conflictOfInterestDao.getCoiDisclosureDetailStatuses());
		prepareDisclosureRelation(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void getProposalStatus(List<Integer> proposalStatuses) {
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_INACTIVE);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_REVISION_REQUESTED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_HOD_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_GRANT_ADMIN_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_GRANT_MANAGER_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_NOT_SUBMITTED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_RETURNED);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_WITHDRAW);
		proposalStatuses.add(Constants.PROPOSAL_STATUS_CODE_AWARDED);
	}

	private void prepareAwardDisclosureDetails(ConflictOfInterestVO vo) {
		List<Integer> awardIds = new ArrayList<>();
		List<DisclosureDetailDto> awardDetails = new ArrayList<>();
		List<Award> awards = new ArrayList<>();
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode())) {
			awardIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.AWARD_MODULE_CODE, vo.getPersonId(), null, null);
			if (!awardIds.isEmpty()) {
				awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, null, null, null);
				awards.forEach(award -> {
					prepareAwardDetail(award, awardDetails, vo.getPersonId(), vo.getDisclosureId(), vo.getDisclosureStatusCode());
				});
			}
		} else {
			awardIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.AWARD_MODULE_CODE, vo.getPersonId(), null, vo.getDisclosureId());
			if (!awardIds.isEmpty()) {
				awards = awardProjectOutcomeDao.getAwardsBasedOnParams(awardIds, null, null, null);
				awards.forEach(award -> {
					prepareAwardDetail(award, awardDetails, vo.getPersonId(), vo.getDisclosureId(), vo.getDisclosureStatusCode());
				});
			}
		}
		vo.setAwards(awardDetails);
	}

	private void prepareAwardDetail(Award award, List<DisclosureDetailDto> awardDetails, String personId, Integer disclosureId, String disclosureStatusCode) {
		DisclosureDetailDto detail = new DisclosureDetailDto();
		detail.setModuleCode(Constants.AWARD_MODULE_CODE);
		detail.setModuleItemId(award.getAwardId());
		detail.setModuleItemKey(award.getAwardNumber());
		detail.setTitle(award.getTitle());
		detail.setStartDate(award.getBeginDate());
		detail.setEndDate(award.getFinalExpirationDate());
		if (award.getSponsorCode() != null) {
			detail.setSponsor(commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
		}
		AwardPerson awardPerson = awardProjectOutcomeDao.getAwardPiDetails(award.getAwardId());
		detail.setPrincipalInvestigator(awardPerson.getFullName());
		detail.setModuleStatus(award.getAwardStatus().getDescription());
		detail.setSfiCompleted(Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode) ? conflictOfInterestDao.checkIsSFICompletedForProject(Constants.AWARD_MODULE_CODE, award.getAwardId(), disclosureId, personId) : Boolean.TRUE);
		awardDetails.add(detail);
	}

	private void prepareProposalDisclosureDetails(ConflictOfInterestVO vo) {
		List<DisclosureDetailDto> proposalDetails = new ArrayList<>();
		List<Proposal> proposals = new ArrayList<>();
		List<Integer> proposalIds = new ArrayList<>();
		List<Integer> proposalStatuses = new ArrayList<>();
		getProposalStatus(proposalStatuses);
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(vo.getDisclosureStatusCode())) {
			proposalIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getPersonId(), proposalStatuses, null);
			if (!proposalIds.isEmpty()) {
				proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, null);
				proposals.forEach(proposal -> {
					prepareProposalDetails(proposal, proposalDetails, vo.getPersonId(), vo.getDisclosureId(), vo.getDisclosureStatusCode());
				});
			}
		} else {
			proposalIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getPersonId(), proposalStatuses, vo.getDisclosureId());
			if (!proposalIds.isEmpty()) {
				proposals = awardProjectOutcomeDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, null);
				proposals.forEach(proposal -> {
					prepareProposalDetails(proposal, proposalDetails, vo.getPersonId(), vo.getDisclosureId(), vo.getDisclosureStatusCode());
				});
			}
		}
		vo.setProposals(proposalDetails);
	}

	private void prepareProposalDetails(Proposal proposal, List<DisclosureDetailDto> proposalDetails, String personId, Integer disclosureId, String disclosureStatusCode) {
		DisclosureDetailDto detail = new DisclosureDetailDto();
		detail.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
		detail.setModuleItemId(proposal.getProposalId());
		detail.setTitle(proposal.getTitle());
		detail.setModuleStatus(proposal.getProposalStatus().getDescription());
		detail.setStartDate(proposal.getStartDate());
		detail.setEndDate(proposal.getEndDate());
		if (proposal.getSponsorCode() != null) {
			detail.setSponsor(commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
		}
		List<ProposalPerson> proposalPersons = proposalDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId());
		proposalPersons.stream().filter(proposalPerson -> proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE)).forEach(proposalPerson -> { 
			detail.setPrincipalInvestigator(proposalPerson.getFullName());
		});
		detail.setSfiCompleted(Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode) ? conflictOfInterestDao.checkIsSFICompletedForProject(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId(), disclosureId, personId) : Boolean.TRUE);
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
	public List<COIEntity> searchEnitiy(String searchString) {
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

	private void prepareDisclosureRelation(ConflictOfInterestVO vo) {
		prepareProposalDisclosureDetails(vo);
		prepareAwardDisclosureDetails(vo);
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
			COIEntity coiEntity = coiFinancialEntity.getCoiEntity();
			conflictOfInterestDao.saveOrUpdateCOIEntity(coiEntity);
			coiFinancialEntity.setCoiEntityId(coiEntity.getCoiEntityId());
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
	public ResponseEntity<Object> certifyDisclosure(CoiDisclosure coiDisclosure) {
		coiDisclosure.setCertifiedBy(AuthenticatedUser.getLoginPersonId());
		coiDisclosure.setCertifiedTimestamp(commonDao.getCurrentTimestamp());
		coiDisclosure.setDisclosureStatusCode(REVIEW_IN_PROGRESS);
		coiDisclosure.setDispositionStatusTypeCode(DISPOSITION_STATUS_TYPE_CODE);
		coiDisclosure.setReviewStatusTypeCode(SUBMITTED_FOR_REVIEW);
		conflictOfInterestDao.certifyDisclosure(coiDisclosure);
		CoiDisclosure coiDisclosureDetails = conflictOfInterestDao.loadDisclosure(coiDisclosure.getDisclosureId());
		coiDisclosureDetails.setCreateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getCreateUser()));
		coiDisclosureDetails.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getUpdateUser()));
		return new ResponseEntity<>(coiDisclosureDetails, HttpStatus.OK);
	}

	@Override
	public String getEntityProjectRelations(ConflictOfInterestVO vo) {
		List<COIFinancialEntity> sfiDetails = conflictOfInterestDao.getSFIOfDisclosure(vo.getPersonId());
		if (vo.getProposalIdlinkedInDisclosure() != null) {
			setProposalDisclosureHeaderDetail(vo);
		}
		List<CoiDisclosureDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(vo.getModuleCode() , vo.getModuleItemId(), vo.getPersonId(), vo.getDisclosureId());
		vo.setCoiDisclosureDetails(prepareDisclosureRelationDetails(sfiDetails, disclosureDetails, vo.getDisclosureStatusCode()));
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
		List<CoiDisclosureDetails> entityProjectRelations = vo.getCoiDisclosureDetails();
		entityProjectRelations.forEach(entityProjectRelation -> {
			CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
			conflictOfInterestDao.saveOrUpdateCoiDisclosureDetail(entityProjectRelation);
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

	private List<CoiDisclosureDetails> prepareDisclosureRelationDetails(List<COIFinancialEntity> sfiDetails, List<CoiDisclosureDetails> coiDisclosureDetails, String disclosureStatusCode) {
		List<CoiDisclosureDetails> disclosureDetails = new ArrayList<>();
		Set<Integer> coiFinancialIds = new HashSet<>();
		if (!coiDisclosureDetails.isEmpty()) {
			coiDisclosureDetails.forEach(disclosureDetail -> coiFinancialIds.add(disclosureDetail.getCoiFinancialEntityId()));
			disclosureDetails.addAll(coiDisclosureDetails);
		}
		if (!sfiDetails.isEmpty()) {
			sfiDetails.forEach(sfiDetail -> {
				if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode) && !coiFinancialIds.contains(sfiDetail.getCoiFinancialEntityId())) {
					CoiDisclosureDetails coiDisclosureDetail = new CoiDisclosureDetails();
					coiDisclosureDetail.setCoiFinancialEntity(sfiDetail);
					coiDisclosureDetail.setComment(new CoiDisclosureDetailsComments());
					disclosureDetails.add(coiDisclosureDetail);
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
		CoiDisclosure copyDisclosure = new CoiDisclosure();
		copyDisclosure.setReviseComment(vo.getReviseComment());
		copyDisclosure(disclosure, copyDisclosure);
		vo.setCoiDisclosure(copyDisclosure);
		vo.setDisclosureId(copyDisclosure.getDisclosureId());
		copyDisclosureDetails(disclosure, copyDisclosure);
		return commonDao.convertObjectToJSON(vo);
	}

	private CoiDisclosure copyDisclosure(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		copyDisclosure.setDisclosureCategoryTypeCode(disclosure.getDisclosureCategoryTypeCode());
		copyDisclosure.setDisclosureStatusCode(Constants.DISCLOSURE_STATUS_PENDING);
		copyDisclosure.setDispositionStatusTypeCode(DISPOSITION_STATUS_TYPE_CODE);
		copyDisclosure.setReviewStatusTypeCode(REVIEW_STATUS_TYPE_CODE);
		copyDisclosure.setDisclosureSequenceStatusCode(DISCLOSURE_SEQUENCE_STATUS_CODE);
		copyDisclosure.setDisclosureVersionNumber(disclosure.getDisclosureVersionNumber() + 1);
		copyDisclosure.setPersonId(AuthenticatedUser.getLoginPersonId());
		copyDisclosure.setDisclosureNumber(disclosure.getDisclosureNumber());
		copyDisclosure.setExpirationDate(disclosure.getExpirationDate());
		return conflictOfInterestDao.saveOrUpdateCoiDisclosure(copyDisclosure);
	}

	private void copyDisclosureDetails(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		List<CoiDisclosureDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(null , null, disclosure.getPersonId(), disclosure.getDisclosureId());
		for (CoiDisclosureDetails disclosureDetail: disclosureDetails) {
			CoiDisclosureDetails copyDisclosureDetail = new CoiDisclosureDetails();
			BeanUtils.copyProperties(disclosureDetail, copyDisclosureDetail);
			CoiDisclosureDetailsComments copyComment = new CoiDisclosureDetailsComments();
			BeanUtils.copyProperties(disclosureDetail.getComment(), copyComment);
			copyComment.setDisclosureDetailsCommentId(null);
			copyDisclosureDetail.setDisclosureDetailsId(null);
			copyDisclosureDetail.setCoiDisclosure(copyDisclosure);
			copyDisclosureDetail.setDisclosureId(copyDisclosure.getDisclosureId());
			copyDisclosureDetail.setDisclosureNumber(copyDisclosure.getDisclosureNumber());
			copyComment.setCoiDisclosureDetails(copyDisclosureDetail);
			copyDisclosureDetail.setComment(copyComment);
			conflictOfInterestDao.saveOrUpdateCoiDisclosureDetail(copyDisclosureDetail);
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
		List<CoiReviewCommentAttachment> coiDisclosureAttachments = new ArrayList<>();
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
					coiDisclosureAttachments.add(attachment);
				}
			}
		} catch (Exception e) {
			throw new ApplicationException("error in addReviewAttachment", e, Constants.JAVA_ERROR);
		}
		return coiDisclosureAttachments;
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
				reviewComments.setCoiFinancialEntity(conflictOfInterestDao.getSFIDetails(reviewComments.getCoiSubSectionsId()));
				break;
			case Constants.PROJECT_RELATIONSHIP:
				reviewComments.setCoiDisclosureDetails(conflictOfInterestDao.getProjectRelationship(reviewComments.getCoiSubSectionsId()));
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
			CoiDisclosure coiDisclosure = new CoiDisclosure();
			coiDisclosure.setDisclosureId(disclosureId);
			coiDisclosure.setDisclosureStatusCode(COMPLETE);
			coiDisclosure.setDispositionStatusTypeCode(ACTIVE);
			coiDisclosure.setReviewStatusTypeCode(COMPLETE);
			coiDisclosure.setDisclosureSequenceStatusCode(ACTIVE);
			conflictOfInterestDao.completeDisclosureReview(coiDisclosure);
			return new ResponseEntity<>(conflictOfInterestDao.loadDisclosure(disclosureId), HttpStatus.OK);
		}
		return new ResponseEntity<>("REVIEW_STATUS_NOT_COMPLETE", HttpStatus.OK);
	}

	@Override
	public CoiDisclosureDetails updateProjectConflictStatus(CoiDisclosureDetails coiDisclosureDetails){
		conflictOfInterestDao.addReviewerStatus(coiDisclosureDetails);
		CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
		coiConflictHistory.setComment(coiDisclosureDetails.getComment().getComments());
		coiConflictHistory.setCoiDetStatusCode(coiDisclosureDetails.getCoiReviewerStatusCode());
		coiConflictHistory.setDisclosureDetailsId(coiDisclosureDetails.getDisclosureDetailsId());
		conflictOfInterestDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
		return conflictOfInterestDao.getProjectRelationship(coiDisclosureDetails.getDisclosureDetailsId());
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
		List<Integer> proposalStatuses = new ArrayList<>();
		List<DisclosureDetailDto> proposalDetails = new ArrayList<>();
		getProposalStatus(proposalStatuses);
		List<Integer> proposalIds = conflictOfInterestDao.getProjectIdsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, AuthenticatedUser.getLoginPersonId(), proposalStatuses, null);
		if (!proposalIds.isEmpty()) {
			proposalDetails = conflictOfInterestDao.getProposalsBasedOnParams(proposalIds, proposalStatuses, vo.getSearchString());
		}
		return commonDao.convertObjectToJSON(proposalDetails);
	}

	@Override
	public String loadDisclosureHistory(ConflictOfInterestVO vo) {
		List<CoiDisclosure> coiDisclosures = conflictOfInterestDao.getCoiDisclosuresByDisclosureNumber(vo.getDisclosureNumber());
		if (coiDisclosures != null && !coiDisclosures.isEmpty()) {
			Set<String> userName = coiDisclosures.stream().map(CoiDisclosure::getUpdateUser).collect(Collectors.toSet());
			if (!userName.isEmpty()) {
				List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
				Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
				coiDisclosures.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
			}
			vo.setPerson(coiDisclosures.get(0).getPersonId() != null ? personDao.getPersonDetailById(coiDisclosures.get(0).getPersonId()) : null);
			vo.setCoiDisclosures(coiDisclosures);
		}
		return commonDao.convertObjectToJSON(vo);
	}

}
