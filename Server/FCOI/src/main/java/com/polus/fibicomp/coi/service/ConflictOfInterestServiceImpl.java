
package com.polus.fibicomp.coi.service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.NoResultException;
import javax.validation.Valid;

import com.polus.fibicomp.coi.repository.ActionLogDao;
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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.AttachmentsDto;
import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.dto.COIValidateDto;
import com.polus.fibicomp.coi.dto.CoiAssignTravelDisclosureAdminDto;
import com.polus.fibicomp.coi.dto.CoiConflictStatusTypeDto;
import com.polus.fibicomp.coi.dto.CoiDisclEntProjDetailsDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.CoiReviewCommentsDto;
import com.polus.fibicomp.coi.dto.CoiSectionTypeDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureActionsDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureCertifyDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiTravelHistoryDto;
import com.polus.fibicomp.coi.dto.CommonRequestDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.dto.DisclosureHistoryResponse;
import com.polus.fibicomp.coi.dto.NotesDto;
import com.polus.fibicomp.coi.dto.NotificationBannerDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.PersonEntityRelationshipDto;
import com.polus.fibicomp.coi.dto.ProjectRelationshipResponseDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.WithdrawDisclosureDto;
import com.polus.fibicomp.coi.pojo.Attachments;
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
import com.polus.fibicomp.coi.pojo.CoiRiskCategory;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.CoiTravelConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureTraveler;
import com.polus.fibicomp.coi.pojo.CoiTravelDocumentStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelReviewStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelerType;
import com.polus.fibicomp.coi.pojo.DisclAttaType;
import com.polus.fibicomp.coi.pojo.DisclComment;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.pojo.EntityRiskCategory;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.pojo.Notes;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.pojo.ValidPersonEntityRelType;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.opa.dao.OPADao;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPADashboardResponseDto;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;
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
	private QuestionnaireService questionnaireService;

	@Autowired
    private ActionLogService actionLogService;

	@Autowired
    private COIFileAttachmentService coiFileAttachmentService;

	@Autowired
	private OPADao opaDao;

	@Autowired
	private ActionLogDao actionLogDao;

	private static final String DISPOSITION_STATUS_TYPE_CODE = "1";
	private static final String DISPOSITION_STATUS_PENDING = "1";
	private static final String REVIEW_STATUS_TYPE_CODE = "1";
	private static final String REVIEW_STATUS_PENDING = "1";
	private static final String RISK_CATEGORY_LOW = "3";
	private static final String SUBMITTED_FOR_REVIEW = "2";
	private static final String DELETE_MSG = "deleted successfully";
	private static final String COMPLETE_ACTIVIVITY ="4";
	private static final String START_ACTIVIVITY ="3";
	private static final String CREATE_ACTIVIVITY ="2";
	private static final String APPROVED = "3";
	private static final String REVIEW_STATUS_COMPLETE = "4";
	private static final String DISCLOSURE_REVIEW_IN_PROGRESS = "3";
	private static final String REVIEW_IN_PROGRESS = "Review in progress";
	private static final String DISCLOSURE_REVIEW_COMPLETED = "4";
	private static final String RISK_CAT_CODE_LOW = "3";
	private static final String REVIEW_STATUS_WITHDRAWN = "6";
	private static final String REVIEW_STATUS_RETURNED = "5";
	private static final String ACTION_LOG_CREATED = "1";
	private static final String ACTION_LOG_SUBMITTED = "2";
	private static final String ACTION_LOG_WITHDRAWN = "3";
	private static final String ACTION_LOG_ASSIGN_ADMIN = "4";
	private static final String ACTION_LOG_REASSIGN_ADMIN = "5";
	private static final String ACTION_LOG_RETURNED = "6";
	private static final String ACTION_LOG_ADMIN_REVIEW_COMPLETED = "11";
	private static final String ACTION_LOG_ASSIGNED_FOR_REVIEW = "7";
	private static final String ACTION_LOG_ASSIGNED_REVIEW_COMPLETED = "8";
	private static final String ACTION_LOG_APPROVED = "13";
	private static final String TRAVEL_DISCLOSURE_STATUS_NO_CONFLICT = "1";
	private static final String ACTION_LOG_RISK_ADDED = "9";
	private static final String ACTION_LOG_DISCLOSURE_STATUS_CREATED = "14";
	private static final String ACTION_LOG_DISCLOSURE_STATUS_CHANGED = "15";
	private static final String TYPE_DISCLOSURE_DETAIL_COMMENT = "1";
	private static final String RISK_CATEGORY_LOW_DESCRIPTION = "Low";
	private static final String TRAVEL_DISCLOSURE_CONFLICT_COMMENT = "2";
	private static final String FILTER_TYPE_ALL = "ALL";
	private static final String FILTER_TYPE_OPA = "OPA";
	private static final String TAB_TYPE_TRAVEL_DISCLOSURES = "TRAVEL_DISCLOSURES";
	private static final String TAB_TYPE_MY_DASHBOARD = "MY_DASHBOARD";
	private static final String TAB_TYPE_IN_PROGRESS_DISCLOSURES = "IN_PROGRESS_DISCLOSURES";
	private static final String TAB_TYPE_APPROVED_DISCLOSURES = "APPROVED_DISCLOSURES";
	

	@Override
	public ResponseEntity<Object> createDisclosure(ConflictOfInterestVO conflictOfInterestVO) {
		if (conflictOfInterestVO.getCoiDisclosure() != null && conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode() == null) {
			Map<String, Object> validatedObject = conflictOfInterestDao.validateProjectDisclosure(AuthenticatedUser.getLoginPersonId(),
					conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode().equals("3") ?
							Constants.DEV_PROPOSAL_MODULE_CODE : Constants.AWARD_MODULE_CODE,
					conflictOfInterestVO.getCoiDisclosure().getModuleItemKey());
			if (validatedObject.get("pendingProject") != null ) {
				CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure((Integer) validatedObject.get("pendingProject"));
				CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
				BeanUtils.copyProperties(disclosure, coiDisclosureDto);
				coiDisclosureDto.setHomeUnitName(disclosure.getUnit() != null ? disclosure.getUnit().getUnitName() : null);
				coiDisclosureDto.setReviewStatus(disclosure.getCoiReviewStatusType() != null ? disclosure.getCoiReviewStatusType().getDescription() : null);
				coiDisclosureDto.setDispositionStatus(disclosure.getCoiDispositionStatusType() != null ? disclosure.getCoiDispositionStatusType().getDescription() : null);
				coiDisclosureDto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
				coiDisclosureDto.setCreateUserFullName(personDao.getUserFullNameByUserName(disclosure.getCreateUser()));
				coiDisclosureDto.setDisclosurePersonFullName(personDao.getPersonFullNameByPersonId(disclosure.getPersonId()));
				return new ResponseEntity<>(coiDisclosureDto, HttpStatus.METHOD_NOT_ALLOWED);
			}
		} else if (conflictOfInterestVO.getCoiDisclosure() != null && conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode().equals("1")) {
			CoiDisclosure fcoiDisclosure = conflictOfInterestDao.isFCOIDisclosureExists(AuthenticatedUser.getLoginPersonId(), "1", Constants.COI_PENDING_STATUS);
			if (fcoiDisclosure != null) {
				return new ResponseEntity<>(fcoiDisclosure, HttpStatus.METHOD_NOT_ALLOWED);
			}
			fcoiDisclosure = conflictOfInterestDao.isFCOIDisclosureExists(AuthenticatedUser.getLoginPersonId(), "1", Constants.COI_ACTIVE_STATUS);
			if (fcoiDisclosure != null) {
				return new ResponseEntity<>(fcoiDisclosure, HttpStatus.METHOD_NOT_ALLOWED);
			}
		}
		CoiDisclosure coiDisclosure = conflictOfInterestVO.getCoiDisclosure() != null?conflictOfInterestVO.getCoiDisclosure():new CoiDisclosure();
		if(conflictOfInterestVO.getCoiProjectProposal()!=null) {
			CoiProjectProposal coiProjectProposal = conflictOfInterestDao.saveOrUpdateCoiProjectProposal(conflictOfInterestVO.getCoiProjectProposal());
			coiDisclosure.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
			coiDisclosure.setFcoiTypeCode("2");
			coiDisclosure.setModuleItemKey(coiProjectProposal.getProposalNumber());
			coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
		} else if(conflictOfInterestVO.getCoiProjectAward()!=null) {
			CoiProjectAward coiProjectAward = conflictOfInterestDao.saveOrUpdateCoiProjectAward(conflictOfInterestVO.getCoiProjectAward());
			coiDisclosure.setModuleCode(Constants.AWARD_MODULE_CODE);
			coiDisclosure.setFcoiTypeCode("3");
			coiDisclosure.setModuleItemKey(coiProjectAward.getAwardNumber());
			coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
		} else if(conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode()!=null && !conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode().isEmpty()) {
			if (!conflictOfInterestVO.getCoiDisclosure().getFcoiTypeCode().equals("4")) {
				if(conflictOfInterestDao.isMasterDisclosurePresent(conflictOfInterestVO.getCoiDisclosure().getPersonId())) {
					return new ResponseEntity<>("Could not create master disclosure ",
							HttpStatus.METHOD_NOT_ALLOWED);
				}
				coiDisclosure.setDisclosureNumber(conflictOfInterestDao.generateMaxDisclosureNumber());
			}
		} else if(conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode()!=null && !conflictOfInterestVO.getCoiDisclosure().getCoiProjectTypeCode().isEmpty()) {
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
		coiDisclosure.setVersionNumber(1);
		coiDisclosure.setVersionStatus(Constants.COI_PENDING_STATUS);
		coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
		coiDisclosure.setReviewStatusCode(REVIEW_STATUS_PENDING);
		coiDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		conflictOfInterestDao.saveOrUpdateCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
		if(coiDisclosure.getFcoiTypeCode().equals("1")) { // if type is FCOI
			conflictOfInterestDao.syncProjectWithDisclosure(coiDisclosure.getDisclosureId(),
					coiDisclosure.getDisclosureNumber(), null, null, null, Constants.TYPE_FCOI);
		} else {
			conflictOfInterestDao.syncProjectWithDisclosure(coiDisclosure.getDisclosureId(),
					coiDisclosure.getDisclosureNumber(), null, coiDisclosure.getModuleCode(), coiDisclosure.getModuleItemKey(), Constants.TYPE_PROJECT_DISCLOSURE);
		}
		try {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_CREATED)
					.disclosureId(coiDisclosure.getDisclosureId()).disclosureNumber(coiDisclosure.getDisclosureNumber())
					.fcoiTypeCode(coiDisclosure.getFcoiTypeCode()).revisionComment(coiDisclosure.getRevisionComment())
					.reporter(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("createDisclosure : {}", e.getMessage());
		}
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> loadDisclosure(Integer disclosureId) {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		CoiDisclosure coiDisclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
		String coiTypeCode = coiDisclosure.getFcoiTypeCode();
		if (Constants.PROPOSAL_DISCLOSURE.equals(coiTypeCode)) {
			List<DisclosureDetailDto> projDetailObjs = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE,
					conflictOfInterestDao.getDisclosurePersonIdByDisclosureId(disclosureId), disclosureId, null);
			conflictOfInterestVO.setProjectDetail(projDetailObjs == null || projDetailObjs.isEmpty() ? null : projDetailObjs.get(0));
		} else if (Constants.AWARD_DISCLOSURE.equals(coiTypeCode)) {
			List<DisclosureDetailDto> projDetailObjs = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
					conflictOfInterestDao.getDisclosurePersonIdByDisclosureId(disclosureId), disclosureId, null);
			conflictOfInterestVO.setProjectDetail(projDetailObjs == null || projDetailObjs.isEmpty() ? null : projDetailObjs.get(0));
		}
		coiDisclosure.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getPersonId()));
		coiDisclosure.setCoiDisclosureFcoiType(conflictOfInterestDao.getCoiDisclosureFcoiTypeByCode(coiTypeCode));
		coiDisclosure.setPerson(personDao.getPersonDetailById(coiDisclosure.getPersonId()));
		coiDisclosure.setAdminGroupName(coiDisclosure.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(coiDisclosure.getAdminGroupId()).getAdminGroupName() : null);
		coiDisclosure.setAdminPersonName(coiDisclosure.getAdminPersonId() != null ? personDao.getPersonFullNameByPersonId(coiDisclosure.getAdminPersonId()) : null);
		Person person = personDao.getPersonDetailById(coiDisclosure.getPersonId());
		coiDisclosure.setPersonEmail(person.getEmailAddress());
		coiDisclosure.setPersonPrimaryTitle(person.getPrimaryTitle());
		conflictOfInterestVO.setCoiDisclosure(coiDisclosure);
		conflictOfInterestVO.setCoiSectionsType(conflictOfInterestDao.fetchCoiSections());
		conflictOfInterestVO.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder()
				.personId(coiDisclosure.getPersonId()).build()));
		conflictOfInterestVO.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(coiDisclosure.getPersonId()));
		conflictOfInterestVO.setPersonNotesCount(conflictOfInterestDao.personNotesCount(coiDisclosure.getPersonId()));
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public String getDisclosureRelations(ConflictOfInterestVO vo) {
		vo.setCoiConflictStatusTypes(conflictOfInterestDao.getCoiConflictStatusTypes());
		vo.setCoiProjConflictStatusTypes(conflictOfInterestDao.getProjConflictStatusTypes());
		vo.setPersonId(conflictOfInterestDao.getDisclosurePersonIdByDisclosureId(vo.getDisclosureId()));
		List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
				vo.getPersonId(), vo.getDisclosureId(), null);
		List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getPersonId(),
				vo.getDisclosureId(), null);
		vo.setAwards(awardDetails);
		vo.setProposals(proposalDetails);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<Object> getSFIOfDisclosure(ConflictOfInterestVO vo) {
		Map<String, Object> responseData = new HashMap<>();
		List<PersonEntity> personEntities  = conflictOfInterestDao.getSFIOfDisclosure(vo);
		Integer disclosureId = vo.getDisclosureId() != null ? vo.getDisclosureId() : null;
		String personId = disclosureId == null ? vo.getPersonId() : null;
		List<PersonEntityRelationshipDto> personEntityRelationshipDto = conflictOfInterestDao.getRelatedEntityInfo(disclosureId, personId, null);
		personEntities.forEach(personEntity -> personEntity.setValidPersonEntityRelTypes(conflictOfInterestDao
					.getValidPersonEntityRelTypes(personEntity.getPersonEntityId())));
		personEntities.forEach(personEntity -> personEntity.setPersonEntityRelationshipDto(personEntityRelationshipDto
				.stream().filter(dto -> personEntity.getPersonEntityId().equals(dto.getPersonEntityId())).findFirst()
				.orElse(null)));
		responseData.put("personEntities", personEntities);
		responseData.put("count", conflictOfInterestDao.getSFIOfDisclosureCount(vo));
		return new ResponseEntity<>(responseData, HttpStatus.OK);
	}

	@Override
	public List<CoiEntity> searchEntity(ConflictOfInterestVO vo) {
		return conflictOfInterestDao.searchEntity(vo);
	}
	
	@Override
	public ResponseEntity<Object> loadAddSFILookups() {
		ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
		conflictOfInterestVO.setEntityStatus(conflictOfInterestDao.fetchEntityStatus());
		conflictOfInterestVO.setEntityType(conflictOfInterestDao.fetchEntityType());
		conflictOfInterestVO.setPersonEntityRelType(conflictOfInterestDao.fetchPersonEntityRelType());
		conflictOfInterestVO.setEntityRiskCategories(conflictOfInterestDao.fetchEntityRiskCategory());
		conflictOfInterestVO.setValidPersonEntityRelTypes(conflictOfInterestDao.fetchAllValidPersonEntityRelTypes());
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
	public ResponseEntity<Object> createSFI(PersonEntity personEntity) {
		String loginUserName = AuthenticatedUser.getLoginUserName();
		String loginPersonId = AuthenticatedUser.getLoginPersonId();
		List<PersonEntity> persEntityObj = conflictOfInterestDao.fetchPersonEntityByEntityNum(personEntity.getEntityNumber(), loginPersonId);
		if (persEntityObj != null && !persEntityObj.isEmpty()) {
			return new ResponseEntity<>(persEntityObj.get(0), HttpStatus.METHOD_NOT_ALLOWED);
		}
		personEntity.setVersionNumber(Constants.COI_INITIAL_VERSION_NUMBER);
		personEntity.setPersonEntityNumber(conflictOfInterestDao.getMaxPersonEntityNumber()+1);
		personEntity.setVersionStatus(Constants.COI_PENDING_STATUS); //Draft
		personEntity.setPersonId(AuthenticatedUser.getLoginPersonId());
		personEntity.setUpdateUser(loginUserName);
		personEntity.setCreateUser(loginUserName);
		conflictOfInterestDao.saveOrUpdateSFI(personEntity);
		personEntity.getValidPersonEntityRelTypeCodes().forEach(code -> {
			PersonEntityRelationship personEntityRelation = new PersonEntityRelationship();
//			personEntityRelation.setQuestionnaireAnsHeaderId(personEntityRelationship.getQuestionnaireAnsHeaderId());
			personEntityRelation.setPersonEntityId(personEntity.getPersonEntityId());
			personEntityRelation.setValidPersonEntityRelTypeCode(code);
			personEntityRelation.setUpdateUser(loginUserName);
			conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(personEntityRelation);
		});
		PersonEntityDto personEntityDto = new PersonEntityDto();
		personEntityDto.setPersonEntityId(personEntity.getPersonEntityId());
		personEntityDto.setPersonEntityNumber(personEntity.getPersonEntityNumber());
		personEntityDto.setEntityName(conflictOfInterestDao.getCoiEntityDetailsById(personEntity.getEntityId()).getEntityName());
		personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_CREATED);
		actionLogService.savePersonEntityActionLog(personEntityDto);
		return new ResponseEntity<>(personEntity, HttpStatus.OK);
	}

	@Override
    public ResponseEntity<Object> saveOrUpdatePersonEntityRelationship(PersonEntityRelationship personEntityRelationship) {
        if (conflictOfInterestDao.isRelationshipAdded(personEntityRelationship.getValidPersonEntityRelTypeCodes(),
				personEntityRelationship.getPersonEntityId())) {
			return new ResponseEntity<>("Relationship already added", HttpStatus.METHOD_NOT_ALLOWED);
		}
		List<PersonEntityRelationship> personEntityRelationshipList = new ArrayList<>();
        Map<Integer, ValidPersonEntityRelType> validPersonEntityRelTypeMap = new HashMap<>();
        List<ValidPersonEntityRelType> validPersonEntityRelTypes = conflictOfInterestDao.getValidPersonEntityRelType();
        for (ValidPersonEntityRelType validPersonEntityRelType : validPersonEntityRelTypes) {
        	validPersonEntityRelTypeMap.put(validPersonEntityRelType.getValidPersonEntityRelTypeCode(), validPersonEntityRelType);
        }
		List<String> relationshipNames = new ArrayList<>();
        personEntityRelationship.getValidPersonEntityRelTypeCodes().forEach(code -> {
            PersonEntityRelationship personEntityRelation = new PersonEntityRelationship();
            personEntityRelation.setQuestionnaireAnsHeaderId(personEntityRelationship.getQuestionnaireAnsHeaderId());
            personEntityRelation.setPersonEntityId(personEntityRelationship.getPersonEntityId());
            personEntityRelation.setValidPersonEntityRelTypeCode(code);
            personEntityRelation.setValidPersonEntityRelType(validPersonEntityRelTypeMap.get(code));
            conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(personEntityRelation);
			relationshipNames.add(validPersonEntityRelTypeMap.get(code).getDescription());
            personEntityRelationshipList.add(conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityRelId(personEntityRelation.getPersonEntityRelId()));
        });
		conflictOfInterestDao.updatePersonEntityUpdateDetails(personEntityRelationship.getPersonEntityId());
		PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityRelationship.getPersonEntityId());
		PersonEntityDto personEntityDto = new PersonEntityDto();
		personEntityDto.setPersonEntityId(personEntity.getPersonEntityId());
		personEntityDto.setPersonEntityNumber(personEntity.getPersonEntityNumber());
		personEntityDto.setEntityName(personEntity.getCoiEntity().getEntityName());
		personEntityDto.setRelationshipName(String.join(",", relationshipNames));
		personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_REL_ADDED);
		actionLogService.savePersonEntityActionLog(personEntityDto);
		personEntityRelationshipList.stream().forEach(relationship -> relationship.setPersonEntity(personEntity));
        return new ResponseEntity<>(personEntityRelationshipList, HttpStatus.OK);
    }

	@Override
	public ResponseEntity<Object> certifyDisclosure(CoiDisclosure coiDisclosure) {
		coiDisclosure.setCertifiedBy(AuthenticatedUser.getLoginPersonId());
		coiDisclosure.setCertifiedAt(commonDao.getCurrentTimestamp());
		CoiDisclosure coiDisclosureObj = conflictOfInterestDao.loadDisclosure(coiDisclosure.getDisclosureId());
		if (coiDisclosureObj.getReviewStatusCode().equals(DISCLOSURE_REVIEW_IN_PROGRESS) || coiDisclosureObj.getReviewStatusCode().equals(SUBMITTED_FOR_REVIEW)) {
			return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		setDisclosureReviewStatusCode(coiDisclosure, coiDisclosureObj);
		coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, 1);
		cal.add(Calendar.DAY_OF_MONTH, -1);
		coiDisclosure.setExpirationDate(cal.getTime());
		conflictOfInterestDao.certifyDisclosure(coiDisclosure);
		conflictOfInterestDao.validateConflicts(coiDisclosure.getDisclosureId());
		CoiRiskCategory riskCategory = null;
		if (coiDisclosureObj.getReviewStatusCode().equals(REVIEW_STATUS_PENDING)) {
			riskCategory = conflictOfInterestDao.syncDisclosureRisk(coiDisclosureObj.getDisclosureId(), coiDisclosureObj.getDisclosureNumber());
		}
		if(riskCategory == null) {
			CoiDisclosureDto coiDisclosureDto = CoiDisclosureDto.builder().disclosureId(coiDisclosure.getDisclosureId())
					.riskCategoryCode(RISK_CATEGORY_LOW).build();
			conflictOfInterestDao.updateDisclosureRiskCategory(coiDisclosureDto);
		}
		coiDisclosureObj.setCreateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getCreateUser()));
		coiDisclosureObj.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getUpdateUser()));
		try {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_SUBMITTED)
					.disclosureId(coiDisclosureObj.getDisclosureId())
					.disclosureNumber(coiDisclosureObj.getDisclosureNumber())
					.riskCategory(riskCategory != null? riskCategory.getDescription(): RISK_CATEGORY_LOW_DESCRIPTION)
					.fcoiTypeCode(coiDisclosureObj.getFcoiTypeCode()).reporter(AuthenticatedUser.getLoginUserFullName()).build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("certifyDisclosure : {}", e.getMessage());
		}
		return new ResponseEntity<>(coiDisclosureObj, HttpStatus.OK);
	}
	
	private void setDisclosureReviewStatusCode(CoiDisclosure coiDisclosure, CoiDisclosure coiDisclosureObj) {
		String reviewStatusCode = coiDisclosureObj.getReviewStatusCode();
		if (reviewStatusCode.equals(REVIEW_STATUS_RETURNED)) {
			if (Boolean.TRUE.equals(conflictOfInterestDao.isReviewerAssigned(coiDisclosureObj.getDisclosureId()))) {
				if (Boolean.TRUE.equals(conflictOfInterestDao.isReviewerReviewCompleted(coiDisclosureObj.getDisclosureId()))) {
					coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED);
				} else {
					coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_ASSIGNED);
				}
			} else if ((coiDisclosureObj.getAdminGroupId() != null || coiDisclosureObj.getAdminPersonId() != null)) {
				coiDisclosure.setReviewStatusCode(DISCLOSURE_REVIEW_IN_PROGRESS);
			}
		} else {
			coiDisclosure.setReviewStatusCode(SUBMITTED_FOR_REVIEW);
		}
	}

	@Override
	public ResponseEntity<Object> getDisclosureProjectRelations(ConflictOfInterestVO vo) {
		List<CoiDisclEntProjDetailsDto> disclosureDetails = new ArrayList<>();
		List<PersonEntityRelationshipDto> personEntityRelationshipDto =  conflictOfInterestDao.getRelatedEntityInfo(vo.getDisclosureId(), null, null);
		conflictOfInterestDao.getProjectRelationshipByParam(vo.getModuleCode(), vo.getModuleItemId(), vo.getPersonId(),
				vo.getDisclosureId()).forEach(disclosureDetail -> {
			CoiDisclEntProjDetailsDto coiDisclEntProjDetails = new CoiDisclEntProjDetailsDto();
			BeanUtils.copyProperties(disclosureDetail, coiDisclEntProjDetails, "coiDisclosure", "coiEntity", "personEntity");
			if (disclosureDetail.getCoiEntity() != null) {
				CoiEntityDto coiEntityDto = new CoiEntityDto();
				BeanUtils.copyProperties(disclosureDetail.getCoiEntity(), coiEntityDto, "entityStatus", "entityType", "coiProjConflictStatusType");
				coiDisclEntProjDetails.setCoiEntity(coiEntityDto);
			}
			coiDisclEntProjDetails.setDisclComment(conflictOfInterestDao.getDisclEntProjRelationComment(disclosureDetail.getDisclosureDetailsId()));
			coiDisclEntProjDetails.setPersonEntityRelationshipDto(personEntityRelationshipDto.stream()
					.filter(dto -> coiDisclEntProjDetails.getEntityId().equals(dto.getEntityId()))
					.findFirst().orElse(null));
			disclosureDetails.add(coiDisclEntProjDetails);
		});
		return new ResponseEntity<>(disclosureDetails, HttpStatus.OK);
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
			vo.setSfiCompleted(conflictOfInterestDao.checkIsSFICompletedForProject(Constants.DEV_PROPOSAL_MODULE_CODE, vo.getModuleItemId(), vo.getDisclosureId()));
		} else if (Constants.AWARD_MODULE_CODE.equals(vo.getModuleCode())) {
			vo.setSfiCompleted(conflictOfInterestDao.checkIsSFICompletedForProject(Constants.AWARD_MODULE_CODE, vo.getModuleItemId(), vo.getDisclosureId()));
		}
		return commonDao.convertObjectToJSON(vo);
	}


	@Override
	public ResponseEntity<Object> reviseDisclosure(ConflictOfInterestVO vo) {
		CoiDisclosure fcoiDisclosure = conflictOfInterestDao.isFCOIDisclosureExists(AuthenticatedUser.getLoginPersonId(), "1", Constants.COI_PENDING_STATUS);
		if (fcoiDisclosure != null ) {
			return new ResponseEntity<>(fcoiDisclosure, HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(vo.getDisclosureId());
		if (!disclosure.getReviewStatusCode().equals("4")) {  // review status code 4 -> completed
			throw new ApplicationException("You are attempting to revise a pending version of disclosure. You can only have one revision at a time.",
					Constants.JAVA_ERROR);
		}
		CoiDisclosure copyDisclosure = new CoiDisclosure();
		copyDisclosure.setRevisionComment(vo.getRevisionComment());
		copyDisclosure.setHomeUnit(vo.getHomeUnit());
		copyDisclosure(disclosure, copyDisclosure);
		vo.setCoiDisclosure(copyDisclosure);
		vo.setDisclosureId(copyDisclosure.getDisclosureId());
		copyDisclosureDetails(disclosure, copyDisclosure);
		if(copyDisclosure.getFcoiTypeCode().equals("1")) { // if type is FCOI
			conflictOfInterestDao.syncProjectWithDisclosure(copyDisclosure.getDisclosureId(),
					copyDisclosure.getDisclosureNumber(), null, null, null, Constants.TYPE_REVISE_FCOI);
		}
		copyDisclosureQuestionnaireData(disclosure, copyDisclosure);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	private CoiDisclosure copyDisclosure(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		copyDisclosure.setFcoiTypeCode(disclosure.getFcoiTypeCode());
		copyDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_TYPE_CODE);
		copyDisclosure.setReviewStatusCode(REVIEW_STATUS_TYPE_CODE);
		copyDisclosure.setVersionStatus(Constants.COI_PENDING_STATUS);
		copyDisclosure.setVersionNumber(disclosure.getVersionNumber() + 1);
		copyDisclosure.setPersonId(AuthenticatedUser.getLoginPersonId());
		copyDisclosure.setDisclosureNumber(disclosure.getDisclosureNumber());
		copyDisclosure.setCreateUser(AuthenticatedUser.getLoginUserName());
		copyDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		return conflictOfInterestDao.saveOrUpdateCoiDisclosure(copyDisclosure);
	}

	private void copyDisclosureQuestionnaireData(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		List<Integer> submoduleCodes = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setActionPersonId(AuthenticatedUser.getLoginPersonId());
		questionnaireDataBus.setActionUserId(AuthenticatedUser.getLoginUserName());
		questionnaireDataBus.setModuleItemCode(Constants.COI_MODULE_CODE);
		questionnaireDataBus.setModuleItemKey(disclosure.getDisclosureId().toString());
		submoduleCodes.add(Constants.COI_SUBMODULE_CODE);
		questionnaireDataBus.getModuleSubItemCodes().addAll(submoduleCodes);
		questionnaireDataBus.setModuleSubItemKey("0");
		questionnaireDataBus.setCopyModuleItemKey(copyDisclosure.getDisclosureId().toString());
		questionnaireService.copyQuestionnaireForVersion(questionnaireDataBus);
	}

	private void copyDisclosureDetails(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
		List<CoiDisclEntProjDetails> disclosureDetails = conflictOfInterestDao.getProjectRelationshipByParam(null ,
				null, disclosure.getPersonId(), disclosure.getDisclosureId());
		for (CoiDisclEntProjDetails disclosureDetail: disclosureDetails) {
			CoiDisclEntProjDetails copyDisclosureDetail = new CoiDisclEntProjDetails();
			BeanUtils.copyProperties(disclosureDetail, copyDisclosureDetail);
			copyDisclosureDetail.setDisclosureDetailsId(null);
			copyDisclosureDetail.setCoiDisclosure(copyDisclosure);
			copyDisclosureDetail.setDisclosureId(copyDisclosure.getDisclosureId());
			copyDisclosureDetail.setDisclosureNumber(copyDisclosure.getDisclosureNumber());
			copyDisclosureDetail.setUpdateUser(AuthenticatedUser.getLoginUserName());
			copyDisclosureDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			conflictOfInterestDao.saveOrUpdateCoiDisclEntProjDetails(copyDisclosureDetail);
			DisclComment disclComment = conflictOfInterestDao.getDisclEntProjRelationComment(disclosureDetail.getDisclosureDetailsId());
			if (disclComment != null) {
				disclComment.setComponentTypeCode("1");		//Disclosure detail comment
				disclComment.setCommentType("1");		//Disclosure detail comment
				disclComment.setCommentPersonId(AuthenticatedUser.getLoginPersonId());
				disclComment.setDocumentOwnerPersonId(AuthenticatedUser.getLoginPersonId());
				disclComment.setIsPrivate(false);
				disclComment.setComponentReferenceId(copyDisclosureDetail.getDisclosureDetailsId());
				conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
			}
		}
	}

	@Override
	public boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo) {
		return conflictOfInterestDao.evaluateDisclosureQuestionnaire(vo.getModuleCode(),vo.getSubmoduleCode(),vo.getModuleItemId());
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
	public ResponseEntity<Object> saveOrUpdateCoiReview(ConflictOfInterestVO vo){
		String actionTypeCode = null;
		CoiReview coiReview = vo.getCoiReview();
		if (coiReview.getCoiReviewId() == null && conflictOfInterestDao.isReviewAdded(coiReview)) {
			return new ResponseEntity<>(commonDao.convertObjectToJSON("Review already added"), HttpStatus.INTERNAL_SERVER_ERROR);
		} else if (coiReview.getCoiReviewId() != null) {
			if (conflictOfInterestDao.isReviewStatusChanged(coiReview)) {
				return new ResponseEntity<>(commonDao.convertObjectToJSON("Review status changed"), HttpStatus.METHOD_NOT_ALLOWED);
			}
			if (conflictOfInterestDao.isReviewPresent(coiReview)) {
				return new ResponseEntity<>(commonDao.convertObjectToJSON("Review already added"), HttpStatus.INTERNAL_SERVER_ERROR);
			}
		}
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		boolean isCreate = true;
		if (coiReview.getCoiReviewId() == null) {
			if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITH_REVIEWER;
			} else {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITHOUT_REVIEWER;
			}
		}
		else {
			isCreate= false;
			if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITH_REVIEWER;
			} else {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITHOUT_REVIEWER;
			}
		}
		String assigneePersonId = coiReview.getCoiReviewId() != null ? conflictOfInterestDao.loadCoiReviewAssigneePersonName(coiReview.getCoiReviewId()) : null;
		String assigneePersonName = assigneePersonId != null ? personDao.getPersonFullNameByPersonId(assigneePersonId) : null;
		conflictOfInterestDao.saveOrUpdateCoiReview(vo.getCoiReview());
		CoiDisclosure coiDisclosure = new CoiDisclosure();
		coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
		coiDisclosure.setVersionStatus(Constants.COI_PENDING_STATUS);
		coiDisclosure.setDisclosureId(coiReview.getDisclosureId());
		if (coiReview.getReviewStatusTypeCode() != null &&
				coiReview.getReviewStatusTypeCode().equals(Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED) &&
				conflictOfInterestDao.numberOfReviewNotOfStatus(coiReview.getDisclosureId(), Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED).equals(0)) {
			coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED);
		} else {
			coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_ASSIGNED);
		}
		conflictOfInterestDao.completeDisclosureReview(coiDisclosure);
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(coiReview.getDisclosureId());
		try {
			DisclosureActionLogDto  actionLogDto = DisclosureActionLogDto.builder()
					.actionTypeCode(actionTypeCode).disclosureId(disclosure.getDisclosureId())
					.disclosureNumber(disclosure.getDisclosureNumber()).fcoiTypeCode(disclosure.getFcoiTypeCode())
					.revisionComment(coiReview.getDescription())
					.oldReviewer(assigneePersonName!=null ? assigneePersonName :coiReview.getAssigneePersonName())
					.newReviewer(coiReview.getAssigneePersonName())
					.administratorName(AuthenticatedUser.getLoginUserFullName())
					.reviewerStatusType(coiReview.getReviewerStatusType())
					.reviewLocationType(coiReview.getReviewLocationType())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
			coiReview.setCoiDisclosure(disclosure);
		} catch (Exception e) {
			logger.error("saveOrUpdateCoiReview : {}", e.getMessage());
		}
		conflictOfInterestDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
		/*Need clarification*/
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(CREATE_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		/*Need clarification*/
		return new ResponseEntity<>(coiReview, HttpStatus.OK);
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
	public ResponseEntity<Object> startReview(ConflictOfInterestVO vo){
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		if (conflictOfInterestDao.isReviewStatus(vo.getCoiReview().getCoiReviewId(),
				Arrays.asList(Constants.COI_REVIEWER_REVIEW_STATUS_START, Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED))) {
			return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		conflictOfInterestDao.startReview(DISCLOSURE_REVIEW_IN_PROGRESS,vo.getCoiReview().getCoiReviewId(), null);
		CoiReview coiReview = conflictOfInterestDao.loadCoiReview(vo.getCoiReview().getCoiReviewId());
		vo.setCoiReview(coiReview);
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(START_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		conflictOfInterestDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
		try {
			String actionTypeCode;
			String reviewerName = "";
			if (coiReview.getAssigneePersonId() != null &&
					coiReview.getAssigneePersonId().equalsIgnoreCase(AuthenticatedUser.getLoginPersonId())) {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_START_REVIEW;
				reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
			} else if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITH_REVIEWER;
				reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
			} else {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITHOUT_REVIEWER;
			}
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder()
					.actionTypeCode(actionTypeCode)
					.disclosureId(coiReview.getDisclosureId())
					.disclosureNumber(coiReview.getCoiDisclosure().getDisclosureNumber())
					.fcoiTypeCode(coiReview.getCoiDisclosure().getFcoiTypeCode())
					.reviewername(reviewerName)
					.reviewLocationType(coiReview.getReviewLocationType())
					.administratorName(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("startReview : {}", e.getMessage());
		}
		return new ResponseEntity<>(coiReview, HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> saveOrUpdateCoiReviewComments(MultipartFile[] files,String formDataJSON){
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		CoiReviewCommentsDto coiReviewComment = new CoiReviewCommentsDto();
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ConflictOfInterestVO.class);
			coiReviewComment = vo.getCoiReviewCommentDto();
			vo.getCoiReviewCommentDto().setCommentedByPersonId(AuthenticatedUser.getLoginPersonId());
			DisclComment disclComment = DisclComment.builder()
					.commentId(coiReviewComment.getCommentId())
					.componentTypeCode(TYPE_DISCLOSURE_DETAIL_COMMENT)
					.componentReferenceId(coiReviewComment.getDisclosureId())
					.componentReferenceNumber(coiReviewComment.getCoiSubSectionsId())	//	SFI Id/ Project id
					.commentType(coiReviewComment.getCoiSectionsTypeCode())	//	mapped with coi_sections_type
					.componentSubReferenceId(coiReviewComment.getComponentSubRefId()) //	SFIs of Projects
					.commentPersonId(coiReviewComment.getCommentedByPersonId())
					.documentOwnerPersonId(vo.getDocumentOwnerPersonId())
					.isPrivate(vo.getCoiReviewCommentDto().getIsPrivate())
					.parentCommentId(coiReviewComment.getCoiParentCommentId())
					.comment(coiReviewComment.getComment())
					.updateUser(AuthenticatedUser.getLoginUserName())
					.build();
			conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
			coiReviewComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(coiReviewComment.getUpdateUser()));
		    vo.setCoiReviewCommentDto(coiReviewComment);
		    List<CoiReviewCommentTag> coiReviewCommentTag = addTagPerson(coiReviewComment.getCoiReviewCommentTag(), disclComment.getCommentId(), coiReviewComment.getCoiReviewId());
		    coiReviewComment.setCoiReviewCommentTag(coiReviewCommentTag);
		    COIFileRequestDto request = COIFileRequestDto.builder()
		    								.componentReferenceId(coiReviewComment.getDisclosureId())
		    								.componentReferenceNumber(coiReviewComment.getDisclosureId().toString())
		    								.attaStatusCode(null)
		    								.attaTypeCode(null)
		    								.commentId(disclComment.getCommentId())
		    								.componentTypeCode(null)
		    								.file(null)
		    								.documentOwnerPersonId(null)
		    								.description(null)
		    								.build();
		    addReviewAttachment(files, request);
			conflictOfInterestDao.updateDisclosureUpdateDetails(coiReviewComment.getDisclosureId());
		} catch (Exception e) {
			throw new ApplicationException("error in saveOrUpdateCoiReviewComments", e, Constants.JAVA_ERROR);
		}
		return new ResponseEntity<>(vo.getCoiReviewCommentDto(), HttpStatus.OK);
	}

	private void addReviewAttachment(MultipartFile[] files, COIFileRequestDto request) {
		try {
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					request.setFile(files[i]);
					coiFileAttachmentService.saveFileAttachment(request);
				}
			}
		} catch (Exception e) {
			throw new ApplicationException("error in addReviewAttachment", e, Constants.JAVA_ERROR);
		}
	}

	private List<CoiReviewCommentTag> addTagPerson(List<CoiReviewCommentTag> coiReviewCommentTags, Integer coiReviewCommentId,  Integer coiReviewId) {
		try {
			coiReviewCommentTags.forEach(coiReviewCommentTag -> {
				if (coiReviewCommentTag.getCoiReviewCommentTagsId() == null) {
					coiReviewCommentTag.setCoiReviewCommentId(coiReviewCommentId);
					coiReviewCommentTag.setCoiReviewId(coiReviewId);
					conflictOfInterestDao.saveOrUpdateCoiReviewCommentTag(coiReviewCommentTag);
				}
			});
		} catch (Exception e) {
			throw new ApplicationException("error in addTagPerson", e, Constants.JAVA_ERROR);
		}
		return coiReviewCommentTags;
	}

	@Override
	public ResponseEntity<Object> loadCoiReviewComments(ConflictOfInterestVO vo){
		if (vo.getPersonId() != null) {
			vo.setTagGroupId(commonDao.getAdminGroupIdsBasedOnPersonId(vo.getPersonId()));
		}
		conflictOfInterestDao.loadCoiReviewComments(vo);
		List<DisclComment> coiReviewComments = vo.getDisclComments();
		Map<Integer, List<DisclComment>> replyComments = new HashMap<>();
		coiReviewComments.forEach(reviewComments -> {
			Integer parentCommentId = reviewComments.getParentCommentId();
			if (parentCommentId != null) {
				replyComments.computeIfAbsent(parentCommentId, k -> new ArrayList<>()).add(reviewComments);
			}
			reviewComments.setDisclAttachments(coiFileAttachmentService.getDisclAttachByCommentId(reviewComments.getCommentId()));
			reviewComments.setUpdateUserFullName(personDao.getUserFullNameByUserName(reviewComments.getUpdateUser()));
			reviewComments.setCoiReviewCommentTag(conflictOfInterestDao.fetchCoiReviewCommentTag(reviewComments.getCommentId()));
			reviewComments.getCoiReviewCommentTag().forEach(reviewCommentTag -> {
				if (reviewCommentTag.getTagPersonId() != null) {
					reviewCommentTag.setTagPersonFullName(personDao.getPersonFullNameByPersonId(reviewCommentTag.getTagPersonId()));
				}
				if (reviewCommentTag.getTagGroupId() != null) {
					reviewCommentTag.setTagGroupName(conflictOfInterestDao.fetchadminGroupName(reviewCommentTag.getTagGroupId()));
				}
			});
		});
		coiReviewComments.stream().forEach(parentComment -> {
			Integer parentId = parentComment.getCommentId();
			if (parentId != null) {
				List<DisclComment> childComments = replyComments.get(parentId);
				if (childComments != null) {
					parentComment.setReply(childComments);
				}
			}
		});
		coiReviewComments.removeIf(comment -> comment.getParentCommentId() != null);
		return new ResponseEntity<>(vo.getDisclComments(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> completeReview(ConflictOfInterestVO vo){
		if (conflictOfInterestDao.isReviewStatus(vo.getCoiReview().getCoiReviewId(), Arrays.asList(Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED))) {
			return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		conflictOfInterestDao.startReview(Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED,
				vo.getCoiReview().getCoiReviewId(), vo.getCoiReview().getEndDate());
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
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(coiReview.getDisclosureId());
		DisclosureActionLogDto actionLogDto;
		if (conflictOfInterestDao.numberOfReviewNotOfStatus(coiReview.getDisclosureId(), Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED).equals(0)) {
			CoiDisclosure coiDisclosure = new CoiDisclosure();
			coiDisclosure.setDisclosureId(coiReview.getDisclosureId());
			coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
			coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED);
			coiDisclosure.setVersionStatus(Constants.COI_PENDING_STATUS);
			conflictOfInterestDao.completeDisclosureReview(coiDisclosure);
			coiReview.getCoiDisclosure().setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED);
			coiReview.getCoiDisclosure().setCoiReviewStatusType(conflictOfInterestDao.getReviewStatusByCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED));
		}
		try {
			String actionTypeCode;
			String reviewerName = "";
			if (coiReview.getAssigneePersonId() != null &&
					coiReview.getAssigneePersonId().equalsIgnoreCase(AuthenticatedUser.getLoginPersonId())) {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_COMPLETE_REVIEW;
				reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
			} else if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITH_REVIEWER;
				reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
			} else {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITHOUT_REVIEWER;
			}
			actionLogDto = DisclosureActionLogDto.builder()
					.actionTypeCode(actionTypeCode)
					.disclosureId(disclosure.getDisclosureId())
					.disclosureNumber(disclosure.getDisclosureNumber()).fcoiTypeCode(disclosure.getFcoiTypeCode())
					.reviewername(reviewerName)
					.reviewLocationType(coiReview.getReviewLocationType())
					.administratorName(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("completeReview : {}", e.getMessage());
		}
		conflictOfInterestDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
		return new ResponseEntity<>(coiReview, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> deleteReview(Integer coiReviewId){
		try {
			CoiReview coiReview = conflictOfInterestDao.loadCoiReview(coiReviewId);
			if (coiReview == null) {
				return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
			}
			conflictOfInterestDao.deleteReviewAssigneeHistory(coiReviewId);
			List<CoiReviewCommentAttachment> coiReviewCommentAttachments = conflictOfInterestDao.fetchReviewAttachmentByReviewId(coiReviewId);
			coiReviewCommentAttachments.forEach(coiReviewCommentAttachment -> {
				conflictOfInterestDao.deleteFileData(conflictOfInterestDao.getFileDataById(coiReviewCommentAttachment.getFileDataId()));
			});
			conflictOfInterestDao.deleteReviewTagByReviewId(coiReviewId);
			conflictOfInterestDao.deleteReviewCommentAttachment(coiReviewId);
			conflictOfInterestDao.deleteReviewComment(coiReview.getAssigneePersonId(),coiReview.getDisclosureId());
			conflictOfInterestDao.deleteReview(coiReviewId);
			conflictOfInterestDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
			CoiDisclosure coiDisclosure = new CoiDisclosure();
			if (conflictOfInterestDao.numberOfReviewNotOfStatus(coiReview.getDisclosureId(), Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED).equals(0)) {
				coiDisclosure.setDisclosureId(coiReview.getDisclosureId());
				coiDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
				coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED);
				coiDisclosure.setVersionStatus(Constants.COI_PENDING_STATUS);
				conflictOfInterestDao.completeDisclosureReview(coiDisclosure);
				coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED);
				coiDisclosure.setCoiReviewStatusType(conflictOfInterestDao.getReviewStatusByCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED));
			} else {
				coiDisclosure.setReviewStatusCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_ASSIGNED);
				coiDisclosure.setCoiReviewStatusType(conflictOfInterestDao.getReviewStatusByCode(Constants.COI_DISCLOSURE_REVIEWER_STATUS_ASSIGNED));
			}
			try {
				String actionTypeCode;
				String reviewerName = "";
				if (coiReview.getAssigneePersonId() != null ) {
					actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_REVIEW_REMOVED_WITH_REVIEWER;
					reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
				} else {
					actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_REVIEW_REMOVED_WITHOUT_REVIEWER;
				}
				DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder()
						.actionTypeCode(actionTypeCode)
						.disclosureId(coiReview.getDisclosureId())
						.disclosureNumber(coiReview.getCoiDisclosure().getDisclosureNumber())
						.fcoiTypeCode(coiReview.getCoiDisclosure().getFcoiTypeCode())
						.reviewername(reviewerName)
						.reviewLocationType(coiReview.getReviewLocationType())
						.administratorName(AuthenticatedUser.getLoginUserFullName())
						.build();
				actionLogService.saveDisclosureActionLog(actionLogDto);
			} catch (Exception e) {
				logger.error("saveOrUpdateCoiReview : {}", e.getMessage());
			}
			return new ResponseEntity<>(coiDisclosure, HttpStatus.OK);
		} catch(Exception e) {
			throw new ApplicationException("deleteCoiReview",e, Constants.JAVA_ERROR);
		}
	}
	
	@Override
	public String deleteReviewComment(Integer coiReviewCommentId){
		try {
			conflictOfInterestDao.loadCoiReviewCommentsByParentId(coiReviewCommentId).stream().forEach(comment -> {
				conflictOfInterestDao.deleteReviewCommentByCommentId(comment);
			});
			DisclComment coiReviewComment = conflictOfInterestDao.loadCoiReviewCommentById(coiReviewCommentId);
			conflictOfInterestDao.deleteReviewTagByCommentId(coiReviewCommentId);
			conflictOfInterestDao.loadDisclAttachmentByCommentId(coiReviewCommentId).stream().forEach(attachment -> {
				COIFileRequestDto request = COIFileRequestDto.builder().attachmentId(attachment.getAttachmentId())
						.fileDataId(attachment.getFileDataId()).build();
				coiFileAttachmentService.deleteDisclAttachment(request);
			});
			conflictOfInterestDao.deleteReviewCommentByCommentId(coiReviewCommentId);
			conflictOfInterestDao.updateDisclosureUpdateDetails(coiReviewComment.getComponentReferenceId());
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
		return completeReview(disclosureId, disclosureNumber, false);
	}

	@Override
	public CoiDisclEntProjDetails updateProjectConflictStatus(CoiDisclEntProjDetails disclEntProjDetails){
		conflictOfInterestDao.addReviewerStatus(disclEntProjDetails);
		CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
//		coiConflictHistory.setComment(disclEntProjDetails.getComment().getComments());
//		coiConflictHistory.setCoiDetStatusCode(disclEntProjDetails.getCoiReviewerStatusCode());
		coiConflictHistory.setDisclosureDetailsId(disclEntProjDetails.getDisclosureDetailsId());
		conflictOfInterestDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
		conflictOfInterestDao.updateDisclosureUpdateDetails(disclEntProjDetails.getDisclosureId());
		return conflictOfInterestDao.getProjectRelationship(disclEntProjDetails.getDisclosureDetailsId());
	} 

	@Override
	public List<CoiConflictHistory> getCoiConflictHistory(Integer disclosureDetailsId){
		DisclComment disclComment = conflictOfInterestDao.getDisclEntProjRelationComment(disclosureDetailsId);
		CoiDisclEntProjDetails coiDisclEntProjDetails = conflictOfInterestDao.getProjectRelationship(disclosureDetailsId);
		CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
		coiConflictHistory.setComment(disclComment.getComment());
		coiConflictHistory.setConflictStatusCode(coiDisclEntProjDetails.getProjectConflictStatusCode());
		coiConflictHistory.setUpdateTimestamp(coiDisclEntProjDetails.getUpdateTimestamp());
		coiConflictHistory.setUpdateUser(coiDisclEntProjDetails.getUpdateUser());
		List<CoiConflictHistory> coiConflictHistoryList = conflictOfInterestDao.getCoiConflictHistory(disclosureDetailsId);
		coiConflictHistoryList.add(0, coiConflictHistory);
		coiConflictHistoryList.forEach(conflictHistory -> {
			conflictHistory.setUpdateUserFullName(personDao.getUserFullNameByUserName(conflictHistory.getUpdateUser()));
			conflictHistory.setConflictStatusDescription(conflictOfInterestDao.getCoiConflictStatusByStatusCode(conflictHistory.getConflictStatusCode()));
		});
		return coiConflictHistoryList;
	}

	@Override
	public String loadProposalsForDisclosure(String searchString) {
		List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE,
				AuthenticatedUser.getLoginPersonId(), null, searchString);
		return commonDao.convertObjectToJSON(proposalDetails);
	}

	@Override
	public String loadAwardsForDisclosure(String searchString) {
		List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
				AuthenticatedUser.getLoginPersonId(), null, searchString);
		return commonDao.convertObjectToJSON(awardDetails);
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
			disclComment.setDocumentOwnerPersonId(AuthenticatedUser.getLoginPersonId());
			disclComment.setIsPrivate(false);
			disclComment.setComponentReferenceId(entityProjectRelation.getDisclosureDetailsId());
			disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
			disclComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
			conflictOfInterestDao.updateDisclosureUpdateDetails(entityProjectRelation.getDisclosureId());
			vo.setCoiDisclEntProjDetail(entityProjectRelation);
		} catch (Exception e) {
			logger.error("saveSingleEntityProjectRelation : {}", e.getMessage());
			throw new ApplicationException("Failed to save Entity Project Relation", e, Constants.JAVA_ERROR);
		}
		return vo;
	}
	
	@Override
	public ResponseEntity<Object> saveOrUpdateCoiEntity(ConflictOfInterestVO vo) {
		CoiEntity coiEntity = vo.getCoiEntity();
		coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
		if (coiEntity.getEntityId() == null) { // on creation
			coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
			coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
			coiEntity.setIsActive(true); // Y
			coiEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
			coiEntity.setVersionNumber(Constants.COI_INITIAL_VERSION_NUMBER);
			coiEntity.setEntityNumber(conflictOfInterestDao.generateMaxCoiEntityNumber());
			if (coiEntity.getRiskCategoryCode() == null) {
				coiEntity.setRiskCategoryCode(RISK_CAT_CODE_LOW);
				coiEntity.setEntityRiskCategory(conflictOfInterestDao.getEntityRiskDetails(RISK_CAT_CODE_LOW));
			}
			conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_CREATE_ACTION_LOG_CODE, coiEntity, null);
		} else { // on update or patch checks its a major change or not
			Integer entityId = coiEntity.getEntityId();
			coiEntity.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
			coiEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
			if (coiEntity.isMajorVersion() && conflictOfInterestDao.checkEntityAdded(entityId, null)) { // checks the entity is linked to a SFI or not
				coiEntity.setIsActive(true); // N
				conflictOfInterestDao.archiveEntity(entityId);
				coiEntity.setEntityId(null);
				coiEntity.setVersionNumber(conflictOfInterestDao.getMaxEntityVersionNumber(coiEntity.getEntityNumber()) + 1);
				coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
				coiEntity.setCreateTimestamp(commonDao.getCurrentTimestamp());
				conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
				conflictOfInterestDao.syncEntityWithPersonEntity(coiEntity.getEntityId(), coiEntity.getEntityNumber(), null);
			} else {
				conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
			}
			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_MODIFY_ACTION_LOG_CODE, coiEntity, coiEntity.getRevisionReason());
		}
		return new ResponseEntity<>(coiEntity, HttpStatus.OK);
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
		conflictOfInterestVO.setOpaDisclosure(opaDao.getActiveAndPendingOpaDisclosure(personId));
		return new ResponseEntity<>(conflictOfInterestVO, HttpStatus.OK);
	}

	@Override
	public String getCOIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		if(!vo.getFilterType().equalsIgnoreCase(FILTER_TYPE_OPA)) {
			dashBoardProfile = conflictOfInterestDao.getCOIDashboard(vo);
		}
		if ((vo.getFilterType().equalsIgnoreCase(FILTER_TYPE_ALL) || vo.getFilterType().equalsIgnoreCase(FILTER_TYPE_OPA)) && (!vo.getTabName().equalsIgnoreCase(TAB_TYPE_TRAVEL_DISCLOSURES))) {
			OPADashboardRequestDto opaDashboardRequestDto = new OPADashboardRequestDto();
			opaDashboardRequestDto.setFetchAllRecords(true);
			opaDashboardRequestDto.setTabType(TAB_TYPE_MY_DASHBOARD);
			if (vo.getTabName().equalsIgnoreCase(TAB_TYPE_IN_PROGRESS_DISCLOSURES)) {
				opaDashboardRequestDto.setDispositionStatusCodes(Arrays.asList(Constants.OPA_DISPOSITION_STATUS_PENDING));
			} else if (vo.getTabName().equalsIgnoreCase(TAB_TYPE_APPROVED_DISCLOSURES)) {
				opaDashboardRequestDto.setDispositionStatusCodes(Arrays.asList(Constants.OPA_DISPOSITION_STATUS_COMPLETED));
			}
			OPADashboardResponseDto opaDashboardResponseDto = opaDao.getOPADashboard(opaDashboardRequestDto);
			dashBoardProfile.setOpaDashboardDto(opaDashboardResponseDto.getData());
		}
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
		OPADashboardRequestDto opaDashboardRequestDto = new OPADashboardRequestDto();
		ResultSet rset;
		opaDashboardRequestDto.setTabType(TAB_TYPE_MY_DASHBOARD);
		vo.setTabName("IN_PROGRESS_DISCLOSURES");
		Integer inProgressDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		vo.setTabName("APPROVED_DISCLOSURES");
		Integer approvedDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		vo.setTabName("TRAVEL_DISCLOSURES");
		Integer travelDisclosureCount = conflictOfInterestDao.getCOIDashboardCount(vo);
		conflictOfInterestVO.setTravelDisclosureCount(travelDisclosureCount);
		vo.setTabName("DISCLOSURE_HISTORY");
		vo.setFilterType("ALL");
		Integer disclosureHistoryCount = conflictOfInterestDao.getDisclosureHistoryCount(vo);
		conflictOfInterestVO.setDisclosureHistoryCount(disclosureHistoryCount);
		opaDashboardRequestDto.setDispositionStatusCodes(Arrays.asList(Constants.OPA_DISPOSITION_STATUS_PENDING));
		try {
			rset = opaDao.getOPADashboardResultSet(opaDashboardRequestDto, true);
			while (rset.next()) {
				inProgressDisclosureCount += rset.getInt(1);
			}
			opaDashboardRequestDto.setDispositionStatusCodes(Arrays.asList(Constants.OPA_DISPOSITION_STATUS_COMPLETED));
			rset = opaDao.getOPADashboardResultSet(opaDashboardRequestDto, true);
			while (rset.next()) {
				approvedDisclosureCount += rset.getInt(1);
			}
		} catch (SQLException e) {
			logger.error("Exception on getOPADashboard {}", e.getMessage());
            throw new ApplicationException("Unable to fetch opa dashboard details", e, Constants.DB_PROC_ERROR);	
		}
		conflictOfInterestVO.setInProgressDisclosureCount(inProgressDisclosureCount);
		conflictOfInterestVO.setApprovedDisclosureCount(approvedDisclosureCount);
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
	public ResponseEntity<Object> getCoiEntityDetails(Integer personEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiEntity(conflictOfInterestDao.getCoiEntityByPersonEntityId(personEntityId));
		vo.getCoiEntity().setUpdatedUserFullName(personDao.getUserFullNameByUserName(vo.getCoiEntity().getUpdateUser()));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getPersonEntityDetails(Integer personEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityId);
		PersonEntity personEntityObj = new PersonEntity();
		BeanUtils.copyProperties(personEntity, personEntityObj, "coiEntity");
		personEntityObj.setUpdateUserFullName(personDao.getUserFullNameByUserName(personEntityObj.getUpdateUser()));
		vo.setPersonEntity(personEntityObj);
		List<PersonEntityRelationship> PersonEntityRelationships = conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityId(personEntityId);
		PersonEntityRelationships.forEach(PersonEntityRelationship -> {
			conflictOfInterestDao.getValidPersonEntityRelTypeByTypeCode(PersonEntityRelationship.getValidPersonEntityRelTypeCode());
		});
		vo.setPersonEntityRelationships(PersonEntityRelationships);
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Object> getValidPersonRelationshipLookUp() {
		return new ResponseEntity<>(conflictOfInterestDao.fetchAllValidPersonEntityRelTypes(), HttpStatus.OK);
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
		return new ResponseEntity<>(conflictOfInterestDao.getRelationshipDetails(vo), HttpStatus.OK);
	}
	
	private void setAllTravelDisclosureStatus(CoiTravelDisclosure coiTravelDisclosure, Integer entityId) {
		coiTravelDisclosure.setTravelStatusCode(Constants.TRAVEL_STATUS_CODE);
		coiTravelDisclosure.setReviewStatusCode(coiTravelDisclosure.getReviewStatusCode() != null ?
				coiTravelDisclosure.getReviewStatusCode() : Constants.TRAVEL_REVIEW_STATUS_CODE_PENDING);
		CoiTravelReviewStatusType coiTravelReviewStatusType =
				conflictOfInterestDao.getTravelReviewStatusDetails(coiTravelDisclosure.getReviewStatusCode() != null ?
						coiTravelDisclosure.getReviewStatusCode() : Constants.TRAVEL_REVIEW_STATUS_CODE_PENDING);
		coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
		coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
		coiTravelDisclosure.setDocumentStatusCode(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		CoiTravelDocumentStatusType coiTravelDocumentStatusType =
				conflictOfInterestDao.getDocumentStatusDetails(coiTravelDisclosure.getDocumentStatusCode() != null ?
						coiTravelDisclosure.getDocumentStatusCode() : Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
		coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
		coiTravelDisclosure.setVersionStatus(Constants.TRAVEL_VERSION_STATUS_PENDING);
		coiTravelDisclosure.setDisclosureStatusCode(TRAVEL_DISCLOSURE_STATUS_NO_CONFLICT);
		CoiTravelDisclosureStatusType coiTravelDisclosureStatusType = conflictOfInterestDao.getTravelDisclosureStatusDetails(TRAVEL_DISCLOSURE_STATUS_NO_CONFLICT);
		coiTravelDisclosure.setCoiTravelDisclosureStatusTypeDetalis(coiTravelDisclosureStatusType);
		CoiEntity coiEntity = conflictOfInterestDao.getCoiEntityDetailsById(entityId);
		coiTravelDisclosure.setRiskCategoryCode(coiEntity.getRiskCategoryCode());
	}

	private void addEntryToTraveller(CoiTravelDisclosure coiTravelDisclosure, ConflictOfInterestVO vo) {
		List<String> travellerTypeCodeList = new ArrayList<>();
		if (vo.getTravelDisclosureId() != null) {
			conflictOfInterestDao.deleteEntriesFromTraveller(vo.getTravelDisclosureId());
		}
		vo.getTravellerTypeCode().forEach(typeCode -> {
			CoiTravelDisclosureTraveler coiTravelDisclosureTraveller = new CoiTravelDisclosureTraveler();
			coiTravelDisclosureTraveller.setTravelTravelerId(vo.getTravelTravellerId());
			coiTravelDisclosureTraveller.setTravelDisclosureId(coiTravelDisclosure.getTravelDisclosureId());
			coiTravelDisclosureTraveller.setUpdateUser(AuthenticatedUser.getLoginUserName());
			coiTravelDisclosureTraveller.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			coiTravelDisclosureTraveller.setTravelerTypeCode(typeCode);
			conflictOfInterestDao.saveOrUpdateCoiTravelDisclosureTraveller(coiTravelDisclosureTraveller);
			travellerTypeCodeList.add(typeCode);;
		});
		coiTravelDisclosure.setCoiTravellerTypeCodeList(travellerTypeCodeList);
	}

	@Override
	public ResponseEntity<Object> createCoiTravelDisclosure(ConflictOfInterestVO vo) {
		CoiTravelDisclosure coiTravelDisclosure =
				vo.getTravelDisclosureId() != null ? conflictOfInterestDao.loadTravelDisclosure(vo.getTravelDisclosureId()) : new CoiTravelDisclosure();
		coiTravelDisclosure.setVersionNumber(Constants.DEFAULT_TRAVEL_VERSION_NUMBER);
		CoiEntity entityDetails = conflictOfInterestDao.getEntityDetails(vo.getEntityId());
		coiTravelDisclosure.setEntityDetails(entityDetails);
		coiTravelDisclosure.setEntityId(vo.getEntityId());
		coiTravelDisclosure.setEntityNumber(vo.getEntityNumber());
		coiTravelDisclosure.setTravelTitle(vo.getTravelTitle());
		coiTravelDisclosure.setTravelstate(vo.getTravelState());
		coiTravelDisclosure.setDestinationCity(vo.getDestinationCity());
		coiTravelDisclosure.setPurposeOfTheTrip(vo.getPurposeOfTheTrip());
		coiTravelDisclosure.setRelationshipToYourResearch(vo.getRelationshipToYourResearch());
		coiTravelDisclosure.setTravelStartDate(vo.getTravelStartDate());
		coiTravelDisclosure.setTravelEndDate(vo.getTravelEndDate());
		coiTravelDisclosure.setIsSponsoredTravel(vo.getIsSponsoredTravel());
		coiTravelDisclosure.setTravelAmount(vo.getTravelAmount());
		coiTravelDisclosure.setDestinationCountry(vo.getDestinationCountry());
		coiTravelDisclosure.setNoOfDays(vo.getNoOfDays());
		coiTravelDisclosure.setIsInterNationalTravel(vo.getIsInternationalTravel());
		coiTravelDisclosure.setTravelNumber(conflictOfInterestDao.generateMaxTravelNumber());
		coiTravelDisclosure.setPersonId(vo.getPersonId());
		coiTravelDisclosure.setDescription(vo.getDescription());
		coiTravelDisclosure.setCreateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		setAllTravelDisclosureStatus(coiTravelDisclosure, vo.getEntityId());
		coiTravelDisclosure.setPersonFullName(personDao.getPersonFullNameByPersonId(coiTravelDisclosure.getPersonId()));
		setUnitDetails(coiTravelDisclosure, vo);
		if (vo.getPersonId() != null && vo.getEntityId() != null) {
			addEntryToPersonEntity(coiTravelDisclosure, vo);
		}
		coiTravelDisclosure.setIsInterNationalTravel(vo.getIsInternationalTravel());
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		coiTravelDisclosure.setCoiEntity(entityDetails);
		addEntryToTraveller(coiTravelDisclosure, vo);
		List<CoiTravelDisclosureTraveler> entries = conflictOfInterestDao.getEntriesFromTravellerTable(coiTravelDisclosure.getTravelDisclosureId());
		Map<String, String> travellerTypeCodeList = getTravellerTypeWithDescription(entries);
		coiTravelDisclosure.setTravellerTypeCodeList(travellerTypeCodeList);
		if (coiTravelDisclosure.getAdminGroupId() != null) {
			coiTravelDisclosure.setAdminGroupName(commonDao.getAdminGroupByGroupId(coiTravelDisclosure.getAdminGroupId()).getAdminGroupName());
		}
		if (coiTravelDisclosure.getAdminPersonId() != null) {
			coiTravelDisclosure.setAdminPersonName(personDao.getPersonFullNameByPersonId(coiTravelDisclosure.getAdminPersonId()));
		}
		if (coiTravelDisclosure.getRiskCategoryCode() != null) {
			CoiRiskCategory coiRiskCategory = conflictOfInterestDao.getRiskCategoryStatusByCode(coiTravelDisclosure.getRiskCategoryCode());
			coiTravelDisclosure.setRiskLevel(coiRiskCategory.getDescription());
		}
		try {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_CREATED)
					.travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId()).travelNumber(coiTravelDisclosure.getTravelNumber())
					.comment(vo.getDescription()).reporter(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("createTravelDisclosure : {}", e.getMessage());
		}
		return new ResponseEntity<>(coiTravelDisclosure, HttpStatus.OK);
	}

	private void addEntryToPersonEntity(CoiTravelDisclosure coiTravelDisclosure, ConflictOfInterestVO vo) {
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
				personEntity.setVersionStatus(Constants.TRAVEL_VERSION_STATUS_PENDING);
				personEntity.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				personEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
				personEntity.setCreateTimestamp(commonDao.getCurrentTimestamp());
				personEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
			}
	}

	private void setUnitDetails(CoiTravelDisclosure coiTravelDisclosure, ConflictOfInterestVO vo) {
		try {
			Unit unitDetails = conflictOfInterestDao.getUnitFromUnitNumber(vo.getHomeUnit());
			if (unitDetails != null) {
				coiTravelDisclosure.setTravellerHomeUnit(vo.getHomeUnit());
				coiTravelDisclosure.setTravellerUnitDetails(unitDetails);
			}
		} catch (NoResultException e) {
			coiTravelDisclosure.setTravellerHomeUnit("000001");
			coiTravelDisclosure.setTravellerUnitDetails(conflictOfInterestDao.getUnitFromUnitNumber("000001"));
		}
	}
	
	private void setAllStatusForAfterAssignAdminAction(CoiTravelDisclosure coiTravelDisclosure, CoiAssignTravelDisclosureAdminDto dto) {
		coiTravelDisclosure.setReviewStatusCode(Constants.TRAVEL_REVIEW_STATUS_CODE_INPROGRESS);
		CoiTravelReviewStatusType coiTravelReviewStatusType =
				conflictOfInterestDao.getTravelReviewStatusDetails(Constants.TRAVEL_REVIEW_STATUS_CODE_INPROGRESS);
		coiTravelDisclosure.setReviewStatusCode(coiTravelReviewStatusType.getReviewStatusCode());
		dto.setReviewStatus(coiTravelReviewStatusType.getDescription());
		dto.setReviewStatusCode(coiTravelReviewStatusType.getReviewStatusCode());
		dto.setVersionStatus(Constants.TRAVEL_VERSION_STATUS_PENDING);
		coiTravelDisclosure.setDocumentStatusCode(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		CoiTravelDocumentStatusType coiTravelDocumentStatusType =
				conflictOfInterestDao.getDocumentStatusDetails(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
		dto.setDocumentStatus(coiTravelDocumentStatusType.getDescription());
		dto.setDocumentStatusCode(coiTravelDocumentStatusType.getDocumentStatusCode());
		if (coiTravelDisclosure.getDisclosureStatusCode() != null) {
			CoiTravelDisclosureStatusType coiTravelDisclosureStatusType =
					conflictOfInterestDao.getTravelDisclosureStatusDetails(coiTravelDisclosure.getDisclosureStatusCode());
			dto.setDisclosureStatusCode(coiTravelDisclosureStatusType.getDisclosureStatusCode());
			dto.setDisclosureStatus(coiTravelDisclosureStatusType.getDescription());
		}
	}

	public ResponseEntity<Object> assignTravelDisclosureAdmin(CoiAssignTravelDisclosureAdminDto dto) {
		if (conflictOfInterestDao.isAdminPersonOrGroupAddedInTravel(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getTravelDisclosureId())) {
			return new ResponseEntity<>("Admin already assigned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		try {
			saveTravelDisclosureAssignAdminActionLog(dto.getAdminPersonId(), dto.getTravelDisclosureId());
		} catch (Exception e) {
			logger.error("assignDisclosureAdmin : {}", e.getMessage());
		}
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(dto.getTravelDisclosureId());
		conflictOfInterestDao.assignTravelDisclosureAdmin(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getTravelDisclosureId());
		if (dto.getAdminGroupId() != null) {
			coiTravelDisclosure.setAdminGroupId(dto.getAdminGroupId());
			coiTravelDisclosure.setAdminGroupName(commonDao.getAdminGroupByGroupId(dto.getAdminGroupId()).getAdminGroupName());
			dto.setAdminGroupName(coiTravelDisclosure.getAdminGroupName());
		} else {
			coiTravelDisclosure.setAdminGroupId(null);
			coiTravelDisclosure.setAdminGroupName(null);
			dto.setAdminGroupName(null);
			dto.setAdminGroupId(null);
		}
		if (dto.getAdminPersonId() != null) {
			coiTravelDisclosure.setAdminPersonId(dto.getAdminPersonId());
			coiTravelDisclosure.setAdminPersonName(personDao.getPersonFullNameByPersonId(dto.getAdminPersonId()));
			dto.setAdminPersonName(coiTravelDisclosure.getAdminPersonName());
		}
		setAllStatusForAfterAssignAdminAction(coiTravelDisclosure, dto);
		dto.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

	public void saveTravelDisclosureAssignAdminActionLog(String adminPersonId, Integer travelDisclosureId) {
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureId);
		String oldAdminPerson = coiTravelDisclosure.getAdminPersonId() != null
				? personDao.getPersonFullNameByPersonId(coiTravelDisclosure.getAdminPersonId())
				: null;
		String newAdminPerson = personDao.getPersonFullNameByPersonId(adminPersonId);
		if (oldAdminPerson != null) {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_REASSIGN_ADMIN)
	                .travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId())
	                .travelNumber(coiTravelDisclosure.getTravelNumber())
	                .oldAdmin(oldAdminPerson)
	                .newAdmin(newAdminPerson)
	                .coiAdmin(AuthenticatedUser.getLoginUserFullName())
	                .build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		}
		else {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_ASSIGN_ADMIN)
	                .travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId())
	                .travelNumber(coiTravelDisclosure.getTravelNumber())
	                .newAdmin(newAdminPerson)
	                .coiAdmin(AuthenticatedUser.getLoginUserFullName())
	                .build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		}
	}
	
	private Map<String, String> getTravellerTypeWithDescription(List<CoiTravelDisclosureTraveler> entries) {
		if (entries != null && !entries.isEmpty()) {
			List<String> travelerTypeCode = new ArrayList<>();
			entries.forEach(entry -> {
				travelerTypeCode.add(entry.getTravelerTypeCode());
			});
			List<CoiTravelerType> coiTravellerType = conflictOfInterestDao.getEntriesFromTravellerTypeTable(travelerTypeCode);
			return coiTravellerType.stream().collect(Collectors.toMap(x -> x.getTravelerTypeCode(), x -> x.getDescription()));
		}
		return null;
	}
	
	private void setAdminDetailsToDtoOnLoad(CoiTravelDisclosureDto dto, CoiTravelDisclosure coiTravelDisclosure, Integer travelDisclosureId) {
		if (coiTravelDisclosure.getAdminGroupId() != null) {
			coiTravelDisclosure.setAdminGroupName(commonDao.getAdminGroupByGroupId(coiTravelDisclosure.getAdminGroupId()).getAdminGroupName());
			dto.setAdminGroupName(coiTravelDisclosure.getAdminGroupName());
			dto.setAdminGroupId(coiTravelDisclosure.getAdminGroupId());
		}
		if (coiTravelDisclosure.getAdminPersonId() != null) {
			coiTravelDisclosure.setAdminPersonName(personDao.getPersonFullNameByPersonId(coiTravelDisclosure.getAdminPersonId()));
			dto.setAdminPersonId(coiTravelDisclosure.getAdminPersonId());
			dto.setAdminPersonName(coiTravelDisclosure.getAdminPersonName());
		}
		dto.setTravelDisclosureId(travelDisclosureId);
		dto.setTravelNumber(coiTravelDisclosure.getTravelNumber());
	}
	
	private void setAllStatusToDtoOnLoad(CoiTravelDisclosureDto dto, CoiTravelDisclosure coiTravelDisclosure) {
		CoiTravelReviewStatusType coiTravelReviewStatusType =
				conflictOfInterestDao.getTravelReviewStatusDetails(coiTravelDisclosure.getReviewStatusCode());
		dto.setReviewStatusCode(coiTravelReviewStatusType.getReviewStatusCode());
		dto.setReviewStatus(coiTravelReviewStatusType.getDescription());
		dto.setVersionStatus(coiTravelDisclosure.getVersionStatus());
		CoiTravelDocumentStatusType coiTravelDocumentStatusType =
				conflictOfInterestDao.getDocumentStatusDetails(coiTravelDisclosure.getDocumentStatusCode());
		dto.setDocumentStatus(coiTravelDocumentStatusType.getDescription());
		dto.setDocumentStatusCode(coiTravelDocumentStatusType.getDocumentStatusCode());
		if (coiTravelDisclosure.getDisclosureStatusCode() != null) {
			CoiTravelDisclosureStatusType coiTravelDisclosureStatusType =
					conflictOfInterestDao.getTravelDisclosureStatusDetails(coiTravelDisclosure.getDisclosureStatusCode());
			dto.setDisclosureStatusCode(coiTravelDisclosureStatusType.getDisclosureStatusCode());
			dto.setDisclosureStatus(coiTravelDisclosureStatusType.getDescription());
		}
	}
	
	private Date getExpirationDate() {
		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, 1);
		cal.add(Calendar.DAY_OF_MONTH, -1);
		return cal.getTime();
	}

	@Override
	public ResponseEntity<Object> loadTravelDisclosure(Integer travelDisclosureId) {
		CoiTravelDisclosureDto dto = new CoiTravelDisclosureDto();
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureId);
		setAdminDetailsToDtoOnLoad(dto, coiTravelDisclosure, travelDisclosureId);
		List<CoiTravelDisclosureTraveler> entries = conflictOfInterestDao.getEntriesFromTravellerTable(coiTravelDisclosure.getTravelDisclosureId());
		Map<String, String> travellerTypeCodeList = getTravellerTypeWithDescription(entries);
		CoiEntity entityDetails = conflictOfInterestDao.getEntityDetails(coiTravelDisclosure.getEntityId());
		dto.setEntityId(entityDetails.getEntityId());
		dto.setEntityTypeCode(entityDetails.getEntityTypeCode());
		dto.setEntityEmail(entityDetails.getEmailAddress());
		dto.setEntityAddress(entityDetails.getAddress());
		dto.setEntityIsActive(entityDetails.getIsActive());
		dto.setEntityRiskCategory(entityDetails.getEntityRiskCategory());
		dto.setRiskLevel(coiTravelDisclosure.getCoiRiskCategory() != null ? coiTravelDisclosure.getCoiRiskCategory().getDescription() : null);
		dto.setRiskCategoryCode(coiTravelDisclosure.getRiskCategoryCode());
		EntityType entityTypeDetails = conflictOfInterestDao.getEntityTypeDetails(entityDetails.getEntityTypeCode());
		dto.setEntityType(entityTypeDetails.getDescription());
		dto.setEntityNumber(entityDetails.getEntityNumber());
		dto.setTravelEntityName(entityDetails.getEntityName());
		Country countryDetails = conflictOfInterestDao.getCountryDetailsByCountryCode(entityDetails.getCountryCode());
		dto.setCountryCode(countryDetails.getCountryCode());
		dto.setCountry(countryDetails.getCountryName());
		dto.setTravellerTypeCodeList(travellerTypeCodeList);
		dto.setIsInterNationalTravel(coiTravelDisclosure.getIsInterNationalTravel());
		setAllStatusToDtoOnLoad(dto, coiTravelDisclosure);
		dto.setTravelSubmissionDate(coiTravelDisclosure.getTravelSubmissionDate());
		dto.setTravelTitle(coiTravelDisclosure.getTravelTitle());
		dto.setPurposeOfTheTrip(coiTravelDisclosure.getPurposeOfTheTrip());
		dto.setTravelAmount(coiTravelDisclosure.getTravelAmount());
		dto.setTravelStartDate(coiTravelDisclosure.getTravelStartDate());
		dto.setTravelEndDate(coiTravelDisclosure.getTravelEndDate());
		dto.setDestinationCity(coiTravelDisclosure.getDestinationCity());
		dto.setDestinationCountry(coiTravelDisclosure.getDestinationCountry());
		dto.setTravelState(coiTravelDisclosure.getTravelstate());
		dto.setRelationshipToYourResearch(coiTravelDisclosure.getRelationshipToYourResearch());
		dto.setAcknowledgeBy(coiTravelDisclosure.getAcknowledgeBy());
		dto.setAcknowledgeAt(coiTravelDisclosure.getAcknowledgeAt());
		dto.setCreateUser(coiTravelDisclosure.getCreateUser());
		dto.setCreateTimestamp(coiTravelDisclosure.getCreateTimestamp());
		dto.setUpdateUser(coiTravelDisclosure.getUpdateUser());
		dto.setUpdateTimestamp(coiTravelDisclosure.getUpdateTimestamp());
		Unit unitDetails = conflictOfInterestDao.getUnitFromUnitNumber(coiTravelDisclosure.getTravellerHomeUnit());
		dto.setHomeUnitNumber(unitDetails.getUnitNumber());
		dto.setHomeUnitName(unitDetails.getUnitName());
		dto.setTravellerHomeUnit(coiTravelDisclosure.getTravellerHomeUnit());
		dto.setTravelSubmissionDate(coiTravelDisclosure.getTravelSubmissionDate());
		dto.setPersonId(coiTravelDisclosure.getPersonId());
		dto.setPersonFullName(personDao.getPersonFullNameByPersonId(dto.getPersonId()));
		dto.setCertifiedAt(coiTravelDisclosure.getCertifiedAt());
		dto.setCertifiedBy(coiTravelDisclosure.getCertifiedBy());
		dto.setDescription(coiTravelDisclosure.getDescription());
		dto.setExpirationDate(coiTravelDisclosure.getExpirationDate());
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

	/** On Certifying travel disclosure, certifying person id and certified date is saving to database */
	@Override
	public ResponseEntity<Object> certifyTravelDisclosure(ConflictOfInterestVO vo) {
		Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
		CoiTravelDisclosureCertifyDto travelCertifyDto = new CoiTravelDisclosureCertifyDto();
		String personId = AuthenticatedUser.getLoginPersonId() != null ? AuthenticatedUser.getLoginPersonId() : vo.getPersonId();
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(vo.getTravelDisclosureId());
		coiTravelDisclosure.setCertifiedBy(personId);
		coiTravelDisclosure.setCertifiedAt(currentTimestamp);
		travelCertifyDto.setCertifiedAt(coiTravelDisclosure.getCertifiedAt());
		travelCertifyDto.setCertifiedBy(personId);
		travelCertifyDto.setUpdateTimestamp(currentTimestamp);
		conflictOfInterestDao.certifyTravelDisclosure(coiTravelDisclosure);
		return new ResponseEntity<>(travelCertifyDto, HttpStatus.OK);
	}
	
	/** If any travel disclosure is returned and it is having any admins or admin groups are assigned,
	 * then the review status should be changed to 'Review in Progress'. Otherwise it should be 'Submitted' */
	private void setTravelReviewStatusWhileSubmit(CoiTravelDisclosure coiTravelDisclosure) {
		if (coiTravelDisclosure.getReviewStatusCode().equalsIgnoreCase(Constants.TRAVEL_REVIEW_STATUS_CODE_RETURNED_TO_PI) &&
				(coiTravelDisclosure.getAdminPersonId() != null || coiTravelDisclosure.getAdminGroupId() != null)) {
			coiTravelDisclosure.setReviewStatusCode(Constants.TRAVEL_REVIEW_STATUS_CODE_INPROGRESS);
			CoiTravelReviewStatusType coiTravelReviewStatusType =
					conflictOfInterestDao.getTravelReviewStatusDetails(Constants.TRAVEL_REVIEW_STATUS_CODE_INPROGRESS);
			coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
		} else {
			coiTravelDisclosure.setReviewStatusCode(Constants.TRAVEL_REVIEW_STATUS_CODE_SUBMITTED);
			CoiTravelReviewStatusType coiTravelReviewStatusType =
					conflictOfInterestDao.getTravelReviewStatusDetails(Constants.TRAVEL_REVIEW_STATUS_CODE_SUBMITTED);
			coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
		}
	}


	/** On Submitting travel disclosure, Review Status -> Submitted, Document Status -> Draft and Version Status -> PENDING */
	@Override
	public ResponseEntity<Object> submitTravelDisclosure(ConflictOfInterestVO vo) {
		Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(vo.getTravelDisclosureId());
		if (coiTravelDisclosure.getReviewStatusCode().equalsIgnoreCase(Constants.TRAVEL_REVIEW_STATUS_CODE_SUBMITTED) ||
			coiTravelDisclosure.getReviewStatusCode().equalsIgnoreCase(Constants.TRAVEL_REVIEW_STATUS_CODE_INPROGRESS)) {
			return new ResponseEntity<>("Travel Disclosure already submitted", HttpStatus.METHOD_NOT_ALLOWED);
		}
		coiTravelDisclosure.setTravelSubmissionDate(commonDao.getCurrentTimestamp());
		setTravelReviewStatusWhileSubmit(coiTravelDisclosure);
		coiTravelDisclosure.setDocumentStatusCode(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		CoiTravelDocumentStatusType coiTravelDocumentStatusType =
				conflictOfInterestDao.getDocumentStatusDetails(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
		coiTravelDisclosure.setVersionStatus(Constants.TRAVEL_VERSION_STATUS_PENDING);
		String personId = AuthenticatedUser.getLoginPersonId() != null ? AuthenticatedUser.getLoginPersonId() : vo.getPersonId();
		coiTravelDisclosure.setCertifiedBy(personId);
		coiTravelDisclosure.setCertifiedAt(currentTimestamp);
		coiTravelDisclosure.setCertifiedAt(coiTravelDisclosure.getCertifiedAt());
		coiTravelDisclosure.setCertifiedBy(personId);
		coiTravelDisclosure.setUpdateTimestamp(currentTimestamp);
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setExpirationDate(getExpirationDate());
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		CoiTravelDisclosure coiTravelDosclosureObject = conflictOfInterestDao.loadTravelDisclosure(coiTravelDisclosure.getTravelDisclosureId());
		try {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_SUBMITTED)
					.travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId()).travelNumber(coiTravelDisclosure.getTravelNumber())
					.reporter(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("SubmitDisclosure : {}", e.getMessage());
		}
		return new ResponseEntity<>(coiTravelDosclosureObject, HttpStatus.OK);
	}

	/** On withdrawing travel disclosure, Review Status -> Withdrawn, Document Status -> Draft and Version Status -> PENDING */
	@Override
	public ResponseEntity<Object> withdrawTravelDisclosure(Integer travelDisclosureId, String description) {
		Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureId);
		if (coiTravelDisclosure.getReviewStatusCode().equalsIgnoreCase(Constants.TRAVEL_REVIEW_STATUS_CODE_SUBMITTED)) {
			coiTravelDisclosure.setReviewStatusCode(Constants.TRAVEL_REVIEW_STATUS_CODE_WITHDRAWN);
			CoiTravelReviewStatusType coiTravelReviewStatusType =
					conflictOfInterestDao.getTravelReviewStatusDetails(Constants.TRAVEL_REVIEW_STATUS_CODE_WITHDRAWN);
			coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
			coiTravelDisclosure.setDocumentStatusCode(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
			CoiTravelDocumentStatusType coiTravelDocumentStatusType =
					conflictOfInterestDao.getDocumentStatusDetails(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
			coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
			coiTravelDisclosure.setVersionStatus(Constants.TRAVEL_VERSION_STATUS_PENDING);
			coiTravelDisclosure.setCertifiedBy("");
			coiTravelDisclosure.setCertifiedAt(null);
			coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
			coiTravelDisclosure.setUpdateTimestamp(currentTimestamp);
			conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
			try {
				TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_WITHDRAWN)
						.travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId()).travelNumber(coiTravelDisclosure.getTravelNumber())
						.reporter(AuthenticatedUser.getLoginUserFullName()).comment(description).build();
				actionLogService.saveTravelDisclosureActionLog(actionLogDto);
			} catch (Exception e) {
				logger.error("withdrawDisclosure : {}", e.getMessage());
			}
			return new ResponseEntity<>(setDtoForAdminActions("WITHDRAW", coiTravelDisclosure, Constants.TRAVEL_VERSION_STATUS_PENDING,
					coiTravelDocumentStatusType, coiTravelReviewStatusType, currentTimestamp), HttpStatus.OK);
		} else {
			return new ResponseEntity<>("Travel Disclosure already withdrawn",HttpStatus.METHOD_NOT_ALLOWED);
		}
	}

	/** On approving travel disclosure, Review Status -> Approved, Document Status -> Approved and Version Status -> ACTIVE */
	@Override
	public ResponseEntity<Object> approveTravelDisclosure(Integer travelDisclosureId, String description) {
		Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureId);
		if (coiTravelDisclosure.getReviewStatusCode().equalsIgnoreCase(Constants.TRAVEL_REVIEW_STATUS_CODE_APPROVED)) {
			return new ResponseEntity<>("Travel Disclosure already approved", HttpStatus.METHOD_NOT_ALLOWED);
		}
		coiTravelDisclosure.setAcknowledgeAt(currentTimestamp);
		coiTravelDisclosure.setAcknowledgeBy(AuthenticatedUser.getLoginPersonId() != null ? AuthenticatedUser.getLoginPersonId() : coiTravelDisclosure.getAcknowledgeBy());
		coiTravelDisclosure.setReviewStatusCode(Constants.TRAVEL_REVIEW_STATUS_CODE_APPROVED);
		CoiTravelReviewStatusType coiTravelReviewStatusType =
				conflictOfInterestDao.getTravelReviewStatusDetails(Constants.TRAVEL_REVIEW_STATUS_CODE_APPROVED);
		coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
		coiTravelDisclosure.setDocumentStatusCode(Constants.TRAVEL_DOCUMENT_STATUS_CODE_APPROVED);
		CoiTravelDocumentStatusType coiTravelDocumentStatusType =
				conflictOfInterestDao.getDocumentStatusDetails(Constants.TRAVEL_DOCUMENT_STATUS_CODE_APPROVED);
		coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
		coiTravelDisclosure.setVersionStatus(Constants.TRAVE_VERSION_STATUS_ACTIVE);
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setUpdateTimestamp(currentTimestamp);
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		try {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_APPROVED)
					.travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId()).travelNumber(coiTravelDisclosure.getTravelNumber())
					.comment(description)
					.build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("approveTravelDisclosure : {}", e.getMessage());
		}
		return new ResponseEntity<>(setDtoForAdminActions("APPROVE", coiTravelDisclosure, Constants.TRAVE_VERSION_STATUS_ACTIVE,
				coiTravelDocumentStatusType, coiTravelReviewStatusType, currentTimestamp), HttpStatus.OK);
	}

	/** On returning travel disclosure, Review Status -> Returned, Document Status -> Draft and Version Status -> PENDING */
	@Override
	public ResponseEntity<Object> returnTravelDisclosure(Integer travelDisclosureId, String description) {
		Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureId);
		if (coiTravelDisclosure.getReviewStatusCode().equalsIgnoreCase(Constants.TRAVEL_REVIEW_STATUS_CODE_RETURNED_TO_PI)) {
			return new ResponseEntity<>("Disclosure already returned",HttpStatus.METHOD_NOT_ALLOWED);
		}
		coiTravelDisclosure.setReviewStatusCode(Constants.TRAVEL_REVIEW_STATUS_CODE_RETURNED_TO_PI);
		CoiTravelReviewStatusType coiTravelReviewStatusType =
				conflictOfInterestDao.getTravelReviewStatusDetails(Constants.TRAVEL_REVIEW_STATUS_CODE_RETURNED_TO_PI);
		coiTravelDisclosure.setCoiTravelReviewStatusTypeDetails(coiTravelReviewStatusType);
		coiTravelDisclosure.setDocumentStatusCode(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		CoiTravelDocumentStatusType coiTravelDocumentStatusType =
				conflictOfInterestDao.getDocumentStatusDetails(Constants.TRAVEL_DOCUMENT_STATUS_CODE_DRAFT);
		coiTravelDisclosure.setCoiDocumentStatusTypeDetalis(coiTravelDocumentStatusType);
		coiTravelDisclosure.setVersionStatus(Constants.TRAVEL_VERSION_STATUS_PENDING);
		coiTravelDisclosure.setCertifiedAt(null);
		coiTravelDisclosure.setCertifiedBy("");
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setUpdateTimestamp(currentTimestamp);
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		try {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_RETURNED)
					.travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId()).travelNumber(coiTravelDisclosure.getTravelNumber())
					.comment(description).administratorName(AuthenticatedUser.getLoginUserFullName()).build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("returnTravelDisclosure : {}", e.getMessage());
		}
		return new ResponseEntity<>(setDtoForAdminActions("RETURN", coiTravelDisclosure, Constants.TRAVEL_VERSION_STATUS_PENDING,
				coiTravelDocumentStatusType, coiTravelReviewStatusType, currentTimestamp), HttpStatus.OK);
	}
	
	private CoiTravelDisclosureActionsDto setDtoForAdminActions(String actionType, CoiTravelDisclosure coiTravelDisclosure,
																String versionStatusCode, CoiTravelDocumentStatusType coiTravelDocumentStatusType,
																CoiTravelReviewStatusType coiTravelReviewStatusType, Timestamp currentTimestamp) {
		CoiTravelDisclosureActionsDto actionDto = new CoiTravelDisclosureActionsDto();
		if (actionType.equalsIgnoreCase("APPROVE")) {
			actionDto.setAcknowledgeAt(currentTimestamp);
			actionDto.setAcknowledgeBy(AuthenticatedUser.getLoginPersonId() != null ?
					AuthenticatedUser.getLoginPersonId() : coiTravelDisclosure.getAcknowledgeBy());
		}
		if (actionType.equalsIgnoreCase("RETURN") || actionType.equalsIgnoreCase("WITHDRAW")) {
			actionDto.setCertifiedAt(null);
			actionDto.setCertifiedBy("");
		}
		actionDto.setDocumentStatusCode(coiTravelDocumentStatusType.getDocumentStatusCode());
		actionDto.setDocumentStatus(coiTravelDocumentStatusType.getDescription());
		actionDto.setReviewStatusCode(coiTravelReviewStatusType.getReviewStatusCode());
		actionDto.setReviewStatus(coiTravelReviewStatusType.getDescription());
		actionDto.setVersionStatus(versionStatusCode);
		actionDto.setDescription(coiTravelDisclosure.getDescription());
		actionDto.setUpdateTimestamp(currentTimestamp);
		return actionDto;
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
	public ResponseEntity<Object> checkEntityAdded(Integer entityId) {
		PersonEntity personEntity = conflictOfInterestDao.fetchPersonEntityById(entityId, AuthenticatedUser.getLoginPersonId());
		if (personEntity != null) {
			PersonEntityDto personEntityDto = new PersonEntityDto();
			BeanUtils.copyProperties(personEntity, personEntityDto);
			CoiEntity coiEntityObj = conflictOfInterestDao.getEntityDetails(personEntity.getEntityId());
			personEntityDto.setEntityType(coiEntityObj.getEntityType());
			personEntityDto.setCountry(coiEntityObj.getCountry());
			personEntityDto.setPersonFullName(personDao.getPersonFullNameByPersonId(personEntity.getPersonId()));
			personEntityDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(personEntity.getUpdateUser()));
			List<PersonEntityRelationship> PersonEntityRelationships = conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityId(personEntity.getPersonEntityId());
			PersonEntityRelationships.forEach(PersonEntityRelationship -> {
				conflictOfInterestDao.getValidPersonEntityRelTypeByTypeCode(PersonEntityRelationship.getValidPersonEntityRelTypeCode());
			});
			personEntityDto.setPersonEntityRelationships(PersonEntityRelationships);
			return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
		}
		return new ResponseEntity<>("Person Entity not found", HttpStatus.NO_CONTENT);
	}

	@Override
	public ResponseEntity<Object> validateDisclosure(Integer moduleCode, String moduleItemId) {
		Map<String, Object> validatedObject = conflictOfInterestDao.validateProjectDisclosure(AuthenticatedUser.getLoginPersonId(), moduleCode, moduleItemId);
		if (validatedObject.get("pendingProject") != null) {
			CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure((Integer) validatedObject.get("pendingProject"));
			CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
			BeanUtils.copyProperties(disclosure, coiDisclosureDto);
			coiDisclosureDto.setHomeUnitName(disclosure.getUnit() != null ? disclosure.getUnit().getUnitName() : null);
			coiDisclosureDto.setReviewStatus(disclosure.getCoiReviewStatusType() != null ? disclosure.getCoiReviewStatusType().getDescription() : null);
			coiDisclosureDto.setDispositionStatus(disclosure.getCoiDispositionStatusType() != null ? disclosure.getCoiDispositionStatusType().getDescription() : null);
			coiDisclosureDto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
			coiDisclosureDto.setCreateUserFullName(personDao.getUserFullNameByUserName(disclosure.getCreateUser()));
			coiDisclosureDto.setDisclosurePersonFullName(personDao.getPersonFullNameByPersonId(disclosure.getPersonId()));
			validatedObject.replace("pendingProject", coiDisclosureDto);
		}
		if (validatedObject.get("fcoiProject") != null) {
			CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure((Integer) validatedObject.get("fcoiProject"));
			CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
			BeanUtils.copyProperties(disclosure, coiDisclosureDto);
			coiDisclosureDto.setHomeUnitName(disclosure.getUnit() != null ? disclosure.getUnit().getUnitName() : null);
			coiDisclosureDto.setReviewStatus(disclosure.getCoiReviewStatusType() != null ? disclosure.getCoiReviewStatusType().getDescription() : null);
			coiDisclosureDto.setDispositionStatus(disclosure.getCoiDispositionStatusType() != null ? disclosure.getCoiDispositionStatusType().getDescription() : null);
			coiDisclosureDto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
			coiDisclosureDto.setCreateUserFullName(personDao.getUserFullNameByUserName(disclosure.getCreateUser()));
			coiDisclosureDto.setDisclosurePersonFullName(personDao.getPersonFullNameByPersonId(disclosure.getPersonId()));
			validatedObject.replace("fcoiProject", coiDisclosureDto);
		}
		return new ResponseEntity<>(validatedObject, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> assignDisclosureAdmin(CoiDisclosureDto dto) {
		if (conflictOfInterestDao.isAdminPersonOrGroupAdded(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getDisclosureId())) {
			return new ResponseEntity<>("Admin already assigned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		try {
			saveAssignAdminActionLog(dto.getAdminPersonId(), dto.getDisclosureId());
		} catch (Exception e) {
			logger.error("assignDisclosureAdmin : {}", e.getMessage());
		}
		conflictOfInterestDao.assignDisclosureAdmin(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getDisclosureId());
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(dto.getDisclosureId());
		if (disclosure.getReviewStatusCode().equalsIgnoreCase(SUBMITTED_FOR_REVIEW)) {
			conflictOfInterestDao.updateReviewStatus(dto.getDisclosureId(), DISCLOSURE_REVIEW_IN_PROGRESS);
			dto.setReviewStatusCode(DISCLOSURE_REVIEW_IN_PROGRESS);
			dto.setReviewStatus(REVIEW_IN_PROGRESS);
		}
		else{
			dto.setReviewStatusCode(disclosure.getReviewStatusCode());
			dto.setReviewStatus(disclosure.getCoiReviewStatusType().getDescription());
		}
		dto.setAdminGroupName(dto.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(dto.getAdminGroupId()).getAdminGroupName() : null);
		dto.setAdminPersonName(personDao.getPersonFullNameByPersonId(dto.getAdminPersonId()));
		dto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
		dto.setConflictStatusCode(disclosure.getConflictStatusCode());
		dto.setDispositionStatusCode(disclosure.getDispositionStatusCode());
		dto.setDispositionStatus(disclosure.getCoiDispositionStatusType().getDescription());
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

	public void saveAssignAdminActionLog(String adminPersonId, Integer disclosureId) {
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
		String oldAdminPerson = disclosure.getAdminPersonId() != null
				? personDao.getPersonFullNameByPersonId(disclosure.getAdminPersonId())
				: null;
		String newAdminPerson = personDao.getPersonFullNameByPersonId(adminPersonId);
		if (oldAdminPerson != null) {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_REASSIGN_ADMIN)
	                .disclosureId(disclosure.getDisclosureId())
	                .disclosureNumber(disclosure.getDisclosureNumber())
	                .oldAdmin(oldAdminPerson)
	                .coiAdmin(AuthenticatedUser.getLoginUserFullName())
	                .newAdmin(newAdminPerson).build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		}
		else {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_ASSIGN_ADMIN)
	                .disclosureId(disclosure.getDisclosureId())
	                .disclosureNumber(disclosure.getDisclosureNumber())
	                .coiAdmin(AuthenticatedUser.getLoginUserFullName())
	                .newAdmin(newAdminPerson).build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		}
	}

	@Override
	public ResponseEntity<Object> validateConflicts(Integer disclosureId) {
		CoiConflictStatusTypeDto statusCode = conflictOfInterestDao.validateConflicts(disclosureId);
		return new ResponseEntity<>(statusCode, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> evaluateValidation(Integer disclosureId) {
		List <COIValidateDto>coiValidateDtoList = new ArrayList<>();
		String personId = conflictOfInterestDao.loadDisclosure(disclosureId).getPersonId();
		coiValidateDtoList = conflictOfInterestDao.evaluateValidation(disclosureId, personId);
		return new ResponseEntity<>(coiValidateDtoList, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getProjConflictStatusType() {
		return new ResponseEntity<>(conflictOfInterestDao.getProjConflictStatusTypes(),HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> updateProjectRelationship(ConflictOfInterestVO vo) {
		if(conflictOfInterestDao.isDisclEntProjConflictAdded(vo.getConflictStatusCode(),vo.getDisclosureDetailsId())) {
			return new ResponseEntity<>("Conflict already updated", HttpStatus.METHOD_NOT_ALLOWED);
		}
		ProjectRelationshipResponseDto projectRelationshipResponseDto = new ProjectRelationshipResponseDto();
		saveOrUpdateCoiConflictHistory(vo);
		saveOrUpdateDisclComment(vo);
		conflictOfInterestDao.updateCoiDisclEntProjDetails(vo.getConflictStatusCode(),vo.getDisclosureDetailsId());
		projectRelationshipResponseDto.setCoiConflictHistoryList(getCoiConflictHistory(vo.getDisclosureDetailsId()));
		projectRelationshipResponseDto.setCoiConflictStatusTypeDto(conflictOfInterestDao.validateConflicts(vo.getDisclosureId()));
		return new ResponseEntity<>(projectRelationshipResponseDto,HttpStatus.OK);
	}

	private void saveOrUpdateDisclComment(ConflictOfInterestVO vo) {
		DisclComment disclComment = conflictOfInterestDao.getDisclEntProjRelationComment(vo.getDisclosureDetailsId());
		disclComment.setComment(vo.getComment());
		conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
	}

	@Override
	public ResponseEntity<Object> activateOrInactivateEntity(CoiEntityDto coiEntityDto) {
		if (conflictOfInterestDao.isEntityActiveOrNot(null, coiEntityDto.getEntityNumber(), coiEntityDto.getIsActive(), Constants.COI_ACTIVE_STATUS)) {
			if (coiEntityDto.getIsActive()) {
				return new ResponseEntity<>(" Entity already activated", HttpStatus.METHOD_NOT_ALLOWED);
			} else {
				return new ResponseEntity<>(" Entity already inactivated", HttpStatus.METHOD_NOT_ALLOWED);
			}
		}
		CoiEntity coiEntityObj = conflictOfInterestDao.getEntityDetails(coiEntityDto.getEntityId());
		if (conflictOfInterestDao.checkEntityAdded(coiEntityDto.getEntityId(), null)) { // checks the entity is linked to a SFI or not
			CoiEntity coiEntity = new CoiEntity();
			BeanUtils.copyProperties(coiEntityObj, coiEntity);
			coiEntity.setIsActive(coiEntityDto.getIsActive());
			conflictOfInterestDao.archiveEntity(coiEntityDto.getEntityId());
			coiEntity.setEntityId(null);
			coiEntity.setVersionNumber(conflictOfInterestDao.getMaxEntityVersionNumber(coiEntity.getEntityNumber()) + 1);
			coiEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
			coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
			coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
			coiEntity.setRevisionReason(coiEntityDto.getRevisionReason());
			conflictOfInterestDao.saveOrUpdateCoiEntity(coiEntity);
			coiEntityDto.setEntityId(coiEntity.getEntityId());
		} else {
			conflictOfInterestDao.activateOrInactivateEntity(coiEntityDto);
		}
		if (Boolean.TRUE.equals(coiEntityDto.getIsActive())) {
			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_ACTIVATE_ACTION_LOG_CODE, coiEntityObj, coiEntityDto.getRevisionReason());
		} else {
			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_INACTIVATE_ACTION_LOG_CODE, coiEntityObj, coiEntityDto.getRevisionReason());
		}
		return new ResponseEntity<>(coiEntityDto, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> activateOrInactivatePersonEntity(PersonEntityDto personEntityDto) {
		if(conflictOfInterestDao.isPersonEntityActiveOrNot(null, personEntityDto.getPersonEntityNumber(), personEntityDto.getIsRelationshipActive(), Constants.COI_ACTIVE_STATUS)) {
			if (personEntityDto.getIsRelationshipActive())
				return new ResponseEntity<>("SFI already activated", HttpStatus.METHOD_NOT_ALLOWED);
			else
				return new ResponseEntity<>("SFI already inactivated", HttpStatus.METHOD_NOT_ALLOWED);
		}
		PersonEntity personEntityObj = conflictOfInterestDao.getPersonEntityDetailsById(personEntityDto.getPersonEntityId());
		if (conflictOfInterestDao.checkPersonEntityAdded(personEntityDto.getPersonEntityId())) {
			PersonEntity draftVersion = conflictOfInterestDao.getPersonEntityByNumberAndStatus(personEntityDto.getPersonEntityNumber(), Constants.COI_PENDING_STATUS);
			if (draftVersion != null) {
				conflictOfInterestDao.activateOrInactivatePersonEntity(personEntityDto);
				conflictOfInterestDao.patchPersonEntityVersionStatus(personEntityDto.getPersonEntityId(), Constants.COI_ARCHIVE_STATUS);
				conflictOfInterestDao.syncProjectWithDisclosure(null,
						null, personEntityDto.getPersonEntityId(), null, null, Constants.TYPE_INACTIVATE_SFI);
				personEntityDto.setPersonEntityId(draftVersion.getPersonEntityId());
				personEntityDto.setVersionStatus(Constants.COI_ARCHIVE_STATUS);
				personEntityDto.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				personEntityDto.setEntityName(draftVersion.getCoiEntity().getEntityName());
				personEntityDto.setPersonEntityNumber(draftVersion.getPersonEntityNumber());
				personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_ACTIVATED);
				actionLogService.savePersonEntityActionLog(personEntityDto);
				return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
			}
			conflictOfInterestDao.patchPersonEntityVersionStatus(personEntityDto.getPersonEntityId(), Constants.COI_ARCHIVE_STATUS);
			PersonEntity personEntity = new PersonEntity();
			BeanUtils.copyProperties(personEntityObj, personEntity);
			personEntity.setRevisionReason(personEntityDto.getRevisionReason());
			personEntity.setIsRelationshipActive(personEntityDto.getIsRelationshipActive());
			personEntity.setPersonEntityId(null);
			personEntity.setVersionNumber(conflictOfInterestDao.getMaxPersonEntityVersionNumber(personEntityObj.getPersonEntityNumber()) + 1);
			personEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
			personEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
			personEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
			personEntity.setCreateTimestamp(commonDao.getCurrentTimestamp());
			personEntity.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			conflictOfInterestDao.saveOrUpdateSFI(personEntity);
			conflictOfInterestDao.syncProjectWithDisclosure(null,
						null, personEntityObj.getPersonEntityId(), null, null, Constants.TYPE_INACTIVATE_SFI);
			conflictOfInterestDao.getCoiFinancialEntityDetails(personEntityObj.getPersonEntityId()).forEach(personEntityRelationship -> {
				PersonEntityRelationship relationship = new PersonEntityRelationship();
				BeanUtils.copyProperties(personEntityRelationship, relationship);
				relationship.setPersonEntityRelId(null);
				relationship.setPersonEntityId(personEntity.getPersonEntityId());
				relationship.setUpdateUser(AuthenticatedUser.getLoginUserName());
				relationship.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(relationship);
			});
			copyPersonEntityQuestionnaireData(personEntityObj, personEntity);
			personEntityDto.setPersonEntityId(personEntity.getPersonEntityId());
			personEntityDto.setVersionStatus(personEntity.getVersionStatus());
			personEntityDto.setUpdateTimestamp(personEntity.getUpdateTimestamp());
			personEntityDto.setPersonEntityNumber(personEntity.getPersonEntityNumber());
		} else {
			personEntityDto.setUpdateTimestamp(conflictOfInterestDao.activateOrInactivatePersonEntity(personEntityDto));
			if (Boolean.TRUE.equals(personEntityDto.getIsRelationshipActive())) {
				personEntityDto.setVersionStatus(Constants.COI_ACTIVE_STATUS);
				conflictOfInterestDao.syncProjectWithDisclosure(null,
						null, personEntityDto.getPersonEntityId(), null, null, Constants.TYPE_SFI);
			} else {
				personEntityDto.setVersionStatus(Constants.COI_ACTIVE_STATUS);
				conflictOfInterestDao.syncProjectWithDisclosure(null,
						null, personEntityDto.getPersonEntityId(), null, null, Constants.TYPE_INACTIVATE_SFI);
			}
		}
		personEntityDto.setEntityName(personEntityObj.getCoiEntity().getEntityName());
		personEntityDto.setPersonEntityNumber(personEntityObj.getPersonEntityNumber());
		personEntityDto.setActionTypeCode(Boolean.TRUE.equals(personEntityDto.getIsRelationshipActive()) ?
				Constants.COI_PERSON_ENTITY_ACTION_LOG_ACTIVATED : Constants.COI_PERSON_ENTITY_ACTION_LOG_INACTIVATED);
		actionLogService.savePersonEntityActionLog(personEntityDto);
		return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
	}

	/**
	 * Copying old version's of person entity relationship questionnaire answers to newer version
	 * @param personEntityOld
	 * @param personEntity
	 */
	private void copyPersonEntityQuestionnaireData(PersonEntity personEntityOld, PersonEntity personEntity) {
		List<Integer> submoduleCodes = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setActionPersonId(AuthenticatedUser.getLoginPersonId());
		questionnaireDataBus.setActionUserId(AuthenticatedUser.getLoginUserName());
		questionnaireDataBus.setModuleItemCode(Constants.COI_MODULE_CODE);
		questionnaireDataBus.setModuleItemKey(personEntityOld.getPersonEntityId().toString());
		submoduleCodes.add(Constants.COI_SFI_SUBMODULE_CODE);
		questionnaireDataBus.getModuleSubItemCodes().addAll(submoduleCodes);
		questionnaireDataBus.setModuleSubItemKey("0");
		questionnaireDataBus.setCopyModuleItemKey(personEntity.getPersonEntityId().toString());
		questionnaireService.copyQuestionnaireForVersion(questionnaireDataBus);
	}

	private void saveOrUpdateCoiConflictHistory(ConflictOfInterestVO vo) {
		CoiConflictHistory coiConflictHistory =  new CoiConflictHistory();
		CoiDisclEntProjDetails coiDisclEntProjDetails = conflictOfInterestDao.getProjectRelationship(vo.getDisclosureDetailsId());
		DisclComment disclComment = conflictOfInterestDao.getDisclEntProjRelationComment(vo.getDisclosureDetailsId());
		coiConflictHistory.setConflictStatusCode(conflictOfInterestDao.getProjectConflictStatusCode(vo.getDisclosureDetailsId()));
		coiConflictHistory.setComment(disclComment.getComment());
		coiConflictHistory.setDisclosureId(vo.getDisclosureId());
		coiConflictHistory.setDisclosureDetailsId(vo.getDisclosureDetailsId());
		coiConflictHistory.setUpdateUser(coiDisclEntProjDetails.getUpdateUser());
		coiConflictHistory.setUpdateTimestamp(coiDisclEntProjDetails.getUpdateTimestamp());
		conflictOfInterestDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
		conflictOfInterestDao.updateDisclosureUpdateDetails(vo.getDisclosureId());
	}

	@Override
	public ResponseEntity<Object> deletePersonEntity(Integer personEntityId) {
		if (conflictOfInterestDao.getPersonEntityDetailsById(personEntityId) == null) {
			return new ResponseEntity<>("SFI already deleted", HttpStatus.METHOD_NOT_ALLOWED);
		}
		conflictOfInterestDao.getRelationshipDetails(personEntityId).forEach(relationship ->
			deletePerEntQuestAnsRelationship(relationship.getPersonEntityRelId(), personEntityId, relationship.getValidPersonEntityRelTypeCode())
		);
		actionLogDao.deletePersonEntityActionLog(personEntityId);
		conflictOfInterestDao.deletePersonEntity(personEntityId);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	/**
	 * This method is used to delete  a person entity relationship and its questionnaire answers
	 * @param personEntityRelId
	 * @param personEntityId
	 * @param relationshipTypeCode
	 */
	private void deletePerEntQuestAnsRelationship(Integer personEntityRelId, Integer personEntityId, Integer relationshipTypeCode) {
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setModuleItemCode(Constants.COI_MODULE_CODE);
		questionnaireDataBus.setModuleSubItemCode(Constants.COI_SFI_SUBMODULE_CODE);
		questionnaireDataBus.setModuleItemKey(personEntityId.toString());
		questionnaireDataBus.setModuleSubItemKey(relationshipTypeCode.toString());
		questionnaireService.deleteAllQuestionAnswers(questionnaireDataBus);
		conflictOfInterestDao.deletePersonEntityRelationship(personEntityRelId);
	}

	@Override
	public ResponseEntity<Object> fetchAllRelationshipTypes() {
		return new ResponseEntity<>(conflictOfInterestDao.fetchAllRelationshipTypes(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> approveEntity(EntityRelationship entityRelationship) {
		if(conflictOfInterestDao.isEntityApproved(entityRelationship.getEntityId())) {
			return new ResponseEntity<>("Entity already approved", HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiEntityDto coiEntityDto = new CoiEntityDto();
		coiEntityDto.setEntityId(entityRelationship.getEntityId());
		coiEntityDto.setUpdateTimestamp(conflictOfInterestDao.approveEntity(entityRelationship.getEntityId()));
		if (entityRelationship.getEntityRelTypeCode() != 1) { //  entityRelTypeCode = 1 (new)
			entityRelationship.setUpdateUser(AuthenticatedUser.getLoginUserName());
			entityRelationship.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			conflictOfInterestDao.saveOrUpdateEntityRelationship(entityRelationship);
		}
		coiEntityDto.setEntityStatusCode(Constants.COI_ENTITY_STATUS_VERIFIED);
		coiEntityDto.setUpdatedUserFullName(personDao.getUserFullNameByUserName(AuthenticatedUser.getLoginUserFullName()));
		CoiEntity coiEntity = conflictOfInterestDao.getCoiEntityDetailsById(coiEntityDto.getEntityId());
		CoiEntity coiEntityCopy = new CoiEntity();
		BeanUtils.copyProperties(coiEntity, coiEntityCopy);
		coiEntityCopy.setUpdatedUserFullName(personDao.getUserFullNameByUserName(coiEntity.getUpdateUser()));
		actionLogService.saveEntityActionLog(Constants.COI_ENTITY_VERIFY_ACTION_LOG_CODE, coiEntityCopy, null);
		return new ResponseEntity<>(coiEntityDto, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getDisclosureHistory(CoiDashboardVO dashboardVO) {
		DisclosureHistoryResponse disclosureHistoryResponse = new DisclosureHistoryResponse();
		if (dashboardVO.getFilterType().equalsIgnoreCase(FILTER_TYPE_ALL) || dashboardVO.getFilterType().equalsIgnoreCase(FILTER_TYPE_OPA)) {
			OPADashboardRequestDto opaDashboardRequestDto = new OPADashboardRequestDto();
			opaDashboardRequestDto.setTabType(TAB_TYPE_MY_DASHBOARD);
			opaDashboardRequestDto.setFetchAllRecords(true);
			OPADashboardResponseDto opaDashboardResponseDto = opaDao.getOPADashboard(opaDashboardRequestDto);
			disclosureHistoryResponse.setOpaDashboardDtos(opaDashboardResponseDto.getData());
		}
		if(!dashboardVO.getFilterType().equalsIgnoreCase(FILTER_TYPE_OPA)) {
			disclosureHistoryResponse.setDisclosureHistoryDtos(conflictOfInterestDao.getDisclosureHistory(dashboardVO));
		}
		return new ResponseEntity<>(disclosureHistoryResponse, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> modifyRisk(CoiEntityDto entityDto) {
		if (conflictOfInterestDao.isEntityRiskAdded(entityDto)) {
			return  new ResponseEntity<>("Risk already added", HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiEntity entity = conflictOfInterestDao.getEntityDetails(entityDto.getEntityId());
		EntityRiskCategory riskCategory = conflictOfInterestDao.getEntityRiskDetails(entityDto.getRiskCategoryCode());
		CoiEntity entityCopy = new CoiEntity();
		BeanUtils.copyProperties(entity, entityCopy);
		entityCopy.setNewRiskCategory(riskCategory);
		entityDto.setUpdateTimestamp(conflictOfInterestDao.updateEntityRiskCategory(entityDto));
		entityCopy.setUpdatedUserFullName(personDao.getUserFullNameByUserName(AuthenticatedUser.getLoginUserName()));
		actionLogService.saveEntityActionLog(Constants.COI_ENTITY_MODIFY_RISK_ACTION_LOG_CODE, entityCopy, entityDto.getRevisionReason());
		return new ResponseEntity<>(entityDto, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchEntityRiskHistory(Integer entityId) {
		return new ResponseEntity<>(actionLogService.fetchEntityActionLog(entityId, Arrays.asList(Constants.COI_ENTITY_MODIFY_RISK_ACTION_LOG_CODE,
				Constants.COI_ENTITY_RISK_ADD_ACTION_LOG_CODE)), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchEntityHistory(CoiEntityDto coiEntityDto) {
		return new ResponseEntity<>(actionLogService.fetchAllEntityActionLog(coiEntityDto), HttpStatus.OK);
	}

	@Override
	public List<CoiTravelHistoryDto> loadTravelDisclosureHistory(String personId, Integer entityNumber) {
		List<CoiTravelHistoryDto> travelHistories = new ArrayList<>();
		List<CoiTravelDisclosure> historyList = conflictOfInterestDao.loadTravelDisclosureHistory(personId, entityNumber);
		historyList.forEach(history -> {
			CoiTravelHistoryDto travelHistoryDto = new CoiTravelHistoryDto();
			travelHistoryDto.setTravelDisclosureId(history.getTravelDisclosureId());
			List<CoiTravelDisclosureTraveler> entries = conflictOfInterestDao.getEntriesFromTravellerTable(history.getTravelDisclosureId());
			Map<String, String> travellerTypeCodeList = getTravellerTypeWithDescription(entries);
			travelHistoryDto.setTravelEntityName(history.getCoiEntity().getEntityName());
			travelHistoryDto.setTravellerTypeCodeList(travellerTypeCodeList);
			travelHistoryDto.setEntityType(history.getCoiEntity().getEntityType().getDescription());
			travelHistoryDto.setDestinationCountry(history.getDestinationCountry());
			travelHistoryDto.setTravelTitle(history.getTravelTitle());
			travelHistoryDto.setPurposeOfTheTrip(history.getPurposeOfTheTrip());
			travelHistoryDto.setDestinationCity(history.getDestinationCity());
			travelHistoryDto.setDestinationState(history.getTravelstate());
			travelHistoryDto.setTravelAmount(history.getTravelAmount());
			travelHistoryDto.setTravelStartDate(history.getTravelStartDate());
			travelHistoryDto.setTravelEndDate(history.getTravelEndDate());
			travelHistories.add(travelHistoryDto);
		});
		return travelHistories;
	}

	@Override
	public ResponseEntity<Object> updatePersonEntity(PersonEntityDto personEntityDto) {
		personEntityDto.setUpdateTimestamp(conflictOfInterestDao.updatePersonEntity(personEntityDto));
		return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> deletePersonEntityRelationship(Integer personEntityRelId, Integer personEntityId) {
		PersonEntityRelationship relationship = conflictOfInterestDao.getRelationshipDetailsById(personEntityRelId);
		if (relationship == null) {
			return new ResponseEntity<>("Already deleted",HttpStatus.METHOD_NOT_ALLOWED);
		}
		deletePerEntQuestAnsRelationship(personEntityRelId, personEntityId, relationship.getValidPersonEntityRelTypeCode());
		conflictOfInterestDao.updatePersonEntityUpdateDetails(relationship.getPersonEntityId());
		PersonEntityDto personEntityDto = new PersonEntityDto();
		personEntityDto.setPersonEntityId(relationship.getPersonEntityId());
		personEntityDto.setPersonEntityNumber(relationship.getPersonEntity().getPersonEntityNumber());
		personEntityDto.setRelationshipName(relationship.getValidPersonEntityRelType().getDescription());
		personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_REL_REMOVED);
		actionLogService.savePersonEntityActionLog(personEntityDto);
		return new ResponseEntity<>(commonDao.getCurrentTimestamp(), HttpStatus.OK);
	}


	@Override
	public ResponseEntity<Object> modifyPersonEntity(Integer personEntityId) {
		PersonEntityDto personEntityDto = new PersonEntityDto();
		PersonEntity personEntityObj = conflictOfInterestDao.getPersonEntityDetailsById(personEntityId);
		if (personEntityObj != null  && personEntityObj.getIsRelationshipActive() != null && !personEntityObj.getIsRelationshipActive()) {
			personEntityDto.setPersonEntityId(personEntityId);
			return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
		}
		PersonEntity draftVersion = conflictOfInterestDao.getPersonEntityByNumberAndStatus(personEntityObj.getPersonEntityNumber(),
				Constants.COI_PENDING_STATUS);
		if (draftVersion != null) {
			personEntityDto.setPersonEntityId(draftVersion.getPersonEntityId());
			return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
		}
		if (conflictOfInterestDao.checkPersonEntityAdded(personEntityId)) {
			PersonEntity personEntity = new PersonEntity();
			Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
			String loginUsername = AuthenticatedUser.getLoginUserName();
			BeanUtils.copyProperties(personEntityObj, personEntity);
			personEntity.setPersonEntityId(null);
			personEntity.setVersionNumber(conflictOfInterestDao.getMaxPersonEntityVersionNumber(personEntityObj.getPersonEntityNumber()) + 1);
			personEntity.setVersionStatus(Constants.COI_PENDING_STATUS);
			personEntity.setUpdateUser(loginUsername);
			personEntity.setCreateUser(loginUsername);
			personEntity.setCreateTimestamp(currentTimestamp);
			personEntity.setUpdateTimestamp(currentTimestamp);
			personEntity.setEntityId(conflictOfInterestDao.getMaxEntityId(personEntityObj.getEntityNumber()));
			conflictOfInterestDao.saveOrUpdateSFI(personEntity);
			conflictOfInterestDao.getCoiFinancialEntityDetails(personEntityObj.getPersonEntityId()).forEach(personEntityRelationship -> {
				PersonEntityRelationship relationship = new PersonEntityRelationship();
				BeanUtils.copyProperties(personEntityRelationship, relationship);
				relationship.setPersonEntityRelId(null);
				relationship.setPersonEntityId(personEntity.getPersonEntityId());
				relationship.setUpdateUser(loginUsername);
				relationship.setUpdateTimestamp(currentTimestamp);
				conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(relationship);
			});
			copyPersonEntityQuestionnaireData(personEntityObj, personEntity);
			personEntityDto.setPersonEntityId(personEntity.getPersonEntityId());
		} else {
			personEntityObj.setVersionStatus(Constants.COI_PENDING_STATUS);
			personEntityObj.setEntityId(conflictOfInterestDao.getMaxEntityId(personEntityObj.getEntityNumber()));
			conflictOfInterestDao.saveOrUpdateSFI(personEntityObj);
			conflictOfInterestDao.syncEntityWithPersonEntity(personEntityObj.getEntityId(), personEntityObj.getEntityNumber(), personEntityId);
			conflictOfInterestDao.syncProjectWithDisclosure(null,
					null, personEntityId, null, null, Constants.TYPE_INACTIVATE_SFI);
			personEntityDto.setPersonEntityId(personEntityId);
		}
		personEntityDto.setPersonEntityNumber(personEntityObj.getPersonEntityNumber());
		personEntityDto.setEntityName(personEntityObj.getCoiEntity().getEntityName());
		personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_MODIFIED);
		actionLogService.savePersonEntityActionLog(personEntityDto);
		return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> finalizePersonEntity(PersonEntityDto personEntityDto) {
		if (conflictOfInterestDao.isPersonEntityActiveOrNot(personEntityDto.getPersonEntityId(), null, true,  Constants.COI_ACTIVE_STATUS)) {
			return new ResponseEntity<>("SFI already activated", HttpStatus.METHOD_NOT_ALLOWED);
		}
		personEntityDto.setVersionStatus(Constants.COI_ACTIVE_STATUS);
		personEntityDto.setIsRelationshipActive(true);
		conflictOfInterestDao.activateOrInactivatePersonEntity(personEntityDto);
		personEntityDto.setVersionStatus(Constants.COI_ACTIVE_STATUS);
		conflictOfInterestDao.syncProjectWithDisclosure(null,
				null, personEntityDto.getPersonEntityId(), null, null, Constants.TYPE_FINALIZE_SFI);
		PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityDto.getPersonEntityId());
		personEntityDto.setEntityName(personEntity.getCoiEntity().getEntityName());
		personEntityDto.setPersonEntityNumber(personEntity.getPersonEntityNumber());
		personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_ACTIVATED);
		actionLogService.savePersonEntityActionLog(personEntityDto);
		return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
	}

	@Override
    public ResponseEntity<Object> withdrawDisclosure(Integer disclosureId, String description) {
        CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
        if ((!SUBMITTED_FOR_REVIEW.equalsIgnoreCase(disclosure.getReviewStatusCode()))
                || (disclosure.getAdminPersonId() != null) || (disclosure.getAdminGroupId() != null)) {
            return new ResponseEntity<>("Disclosure already withdrawn", HttpStatus.METHOD_NOT_ALLOWED);
        }
        disclosure.setCertificationText(null);
        disclosure.setCertifiedAt(null);
        disclosure.setCertifiedBy(null);
        disclosure.setExpirationDate(null);
        disclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
        disclosure.setReviewStatusCode(REVIEW_STATUS_WITHDRAWN);
        disclosure = conflictOfInterestDao.saveOrUpdateCoiDisclosure(disclosure);
		conflictOfInterestDao.syncProjectWithDisclosure(disclosure.getDisclosureId(),
				disclosure.getDisclosureNumber(),null, null, null, Constants.TYPE_RESYNC_SFI);
        WithdrawDisclosureDto withdrawDisclosureDto = WithdrawDisclosureDto.builder()
                .certifiedAt(null)
                .expirationDate(null)
                .updateTimestamp(commonDao.getCurrentTimestamp())
                .reviewStatusCode(disclosure.getReviewStatusCode())
                .reviewStatusDescription(conflictOfInterestDao.getReviewStatusByCode(REVIEW_STATUS_WITHDRAWN).getDescription())
                .build();
		try {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_WITHDRAWN)
					.disclosureId(disclosure.getDisclosureId()).disclosureNumber(disclosure.getDisclosureNumber())
					.fcoiTypeCode(disclosure.getFcoiTypeCode()).revisionComment(description)
	                .reporter(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("saveSingleEntityProjectRelation : {}", e.getMessage());
		}
        return new ResponseEntity<>(withdrawDisclosureDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> returnDisclosure(Integer disclosureId, String description) {
        CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
		if (disclosure.getReviewStatusCode().equalsIgnoreCase(REVIEW_STATUS_RETURNED)) {
			return new ResponseEntity<>("Disclosure already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
        disclosure.setCertificationText(null);
        disclosure.setCertifiedAt(null);
        disclosure.setCertifiedBy(null);
        disclosure.setExpirationDate(null);
        disclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
        disclosure.setReviewStatusCode(REVIEW_STATUS_RETURNED);
        disclosure = conflictOfInterestDao.saveOrUpdateCoiDisclosure(disclosure);
		conflictOfInterestDao.syncProjectWithDisclosure(disclosure.getDisclosureId(),
				disclosure.getDisclosureNumber(),null, null, null, Constants.TYPE_RESYNC_SFI);
		try {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_RETURNED)
					.disclosureId(disclosure.getDisclosureId()).disclosureNumber(disclosure.getDisclosureNumber())
					.fcoiTypeCode(disclosure.getFcoiTypeCode()).revisionComment(description)
					.administratorName(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
		} catch (Exception e) {
			logger.error("returnDisclosure : {}", e.getMessage());
		}
        return new ResponseEntity<>(HttpStatus.OK);
    }

	@Override
	public ResponseEntity<Object> getTravelConflictStatusType() {
		return new ResponseEntity<>(conflictOfInterestDao.getTravelConflictStatusType(),HttpStatus.OK);
	}
	
	private void setTravelActionLogWhileDisclosureStatusChange(ConflictOfInterestVO vo, CoiTravelDisclosure coiTravelDisclosure) {
		String oldDisclosureStatus = coiTravelDisclosure.getDisclosureStatusCode() != null ?
				conflictOfInterestDao.getTravelDisclosureStatusDetails(coiTravelDisclosure.getDisclosureStatusCode()).getDescription() : null;
		String newDisclosureStatus = conflictOfInterestDao.getTravelDisclosureStatusDetails(vo.getDisclosureStatusCode()).getDescription();
		if (oldDisclosureStatus != null) {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_DISCLOSURE_STATUS_CHANGED)
	                .travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId())
	                .travelNumber(coiTravelDisclosure.getTravelNumber())
	                .oldDisclosureStatus(oldDisclosureStatus)
	                .newDisclosureStatus(newDisclosureStatus).build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		}
		else {
			TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder().actionTypeCode(ACTION_LOG_DISCLOSURE_STATUS_CREATED)
	                .travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId())
	                .travelNumber(coiTravelDisclosure.getTravelNumber())
	                .newDisclosureStatus(newDisclosureStatus).build();
			actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		}
	}

	@Override
	public ResponseEntity<Object> manageTravelConflict(ConflictOfInterestVO vo) {
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(vo.getTravelDisclosureId());
		if(coiTravelDisclosure.getDisclosureStatusCode()==null) {
			saveTravelDisclConflictStatus(coiTravelDisclosure, vo.getDisclosureStatusCode());
			saveTravelDisclConflictComment(vo);
		}
		else {
			setTravelActionLogWhileDisclosureStatusChange(vo, coiTravelDisclosure);
			DisclComment disclComment = conflictOfInterestDao.getTravelConflictComment(vo.getTravelDisclosureId());
			saveTravelConflictHistory(coiTravelDisclosure, disclComment);
			updateTravelDisclConflictComment(disclComment, vo.getDescription(), vo.getTravelDisclosureId());
			updateTravelDisclConflictStatus(coiTravelDisclosure, vo.getDisclosureStatusCode());
		}
		return new ResponseEntity<>(getCoiTravelConflictHistory(vo.getTravelDisclosureId()),HttpStatus.OK);
	}

	private void updateTravelDisclConflictStatus(CoiTravelDisclosure coiTravelDisclosure, String disclosureStatusCode) {
		coiTravelDisclosure.setDisclosureStatusCode(disclosureStatusCode);
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
	}

	private void updateTravelDisclConflictComment(DisclComment disclComment, String description, Integer travelDisclosureId) {
		if (disclComment.getComment() == null) {
			disclComment = DisclComment.builder().comment(description).componentTypeCode(TRAVEL_DISCLOSURE_CONFLICT_COMMENT).commentType(TRAVEL_DISCLOSURE_CONFLICT_COMMENT)
					.commentPersonId(AuthenticatedUser.getLoginPersonId())
					.documentOwnerPersonId(AuthenticatedUser.getLoginPersonId()).isPrivate(false)
					.componentReferenceId(travelDisclosureId).build();
		} else {
			disclComment.setComment(description);
		}
		disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
		conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
	}

	private void saveTravelConflictHistory(CoiTravelDisclosure coiTravelDisclosure, DisclComment disclComment) {
		CoiTravelConflictHistory coiTravelConflictHistory = new CoiTravelConflictHistory();
		coiTravelConflictHistory.setComment(disclComment.getComment());
		coiTravelConflictHistory.setConflictStatusCode(coiTravelDisclosure.getDisclosureStatusCode());
		coiTravelConflictHistory.setTravelDisclosureId(coiTravelDisclosure.getTravelDisclosureId());
		coiTravelConflictHistory.setUpdateTimestamp(disclComment.getUpdateTimestamp());
		coiTravelConflictHistory.setUpdateUser(disclComment.getUpdateUser());
		conflictOfInterestDao.saveOrUpdateCoiTravelConflictHistory(coiTravelConflictHistory);
	}

	private void saveTravelDisclConflictComment(ConflictOfInterestVO vo) {
		DisclComment disclComment = new DisclComment();
		disclComment.setComment(vo.getDescription());
		disclComment.setComponentTypeCode("2");		//Travel disclosure conflict comment
		disclComment.setCommentType("2");		//Travel disclosure conflict comment
		disclComment.setCommentPersonId(AuthenticatedUser.getLoginPersonId());
		disclComment.setDocumentOwnerPersonId(vo.getPersonId());
		disclComment.setIsPrivate(false);
		disclComment.setComponentReferenceId(vo.getTravelDisclosureId());
		disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
		conflictOfInterestDao.saveOrUpdateDisclComment(disclComment);
	}

	private void saveTravelDisclConflictStatus(CoiTravelDisclosure coiTravelDisclosure, String disclosureStatusCode) {
		coiTravelDisclosure.setDisclosureStatusCode(disclosureStatusCode);
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
	}

	@Override
	public List<CoiTravelConflictHistory> getCoiTravelConflictHistory(Integer travelDisclosureId) {
		CoiTravelConflictHistory coiTravelConflictHistory = new CoiTravelConflictHistory();
		List<CoiTravelConflictHistory> coiTravelConflictHistoryList = conflictOfInterestDao.getCoiTravelConflictHistory(travelDisclosureId);
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureId);
		if(coiTravelDisclosure.getDisclosureStatusCode()!=null) {
			DisclComment disclComment = conflictOfInterestDao.getTravelConflictComment(travelDisclosureId);
			coiTravelConflictHistory.setTravelDisclosureId(travelDisclosureId);
			coiTravelConflictHistory.setComment(disclComment.getComment());
			coiTravelConflictHistory.setConflictStatusCode(coiTravelDisclosure.getDisclosureStatusCode());
			coiTravelConflictHistory.setUpdateTimestamp(disclComment.getUpdateTimestamp());
			coiTravelConflictHistory.setUpdateUser(disclComment.getUpdateUser());
			coiTravelConflictHistoryList.add(0, coiTravelConflictHistory);
		}
		if (!coiTravelConflictHistoryList.isEmpty()) {
		    coiTravelConflictHistoryList.remove(coiTravelConflictHistoryList.size() - 1);
		}
		coiTravelConflictHistoryList.forEach(conflictHistory -> {
			conflictHistory.setUpdateUserFullName(personDao.getUserFullNameByUserName(conflictHistory.getUpdateUser()));
			conflictHistory.setConflictStatusDescription(conflictOfInterestDao.getCoiTravelConflictStatusByStatusCode(conflictHistory.getConflictStatusCode()));
		});
		return coiTravelConflictHistoryList;
	}

	@Override
	public ResponseEntity<Object> modifyDisclosureRisk(CoiDisclosureDto disclosureDto) {
		if (conflictOfInterestDao.isDisclosureRiskAdded(disclosureDto)) {
			return new ResponseEntity<>("Risk is already updated", HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(disclosureDto.getDisclosureId());
		CoiRiskCategory risk = conflictOfInterestDao.getRiskCategoryStatusByCode(disclosureDto.getRiskCategoryCode());
		disclosureDto.setUpdateTimestamp(conflictOfInterestDao.updateDisclosureRiskCategory(disclosureDto));
		DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().disclosureId(disclosure.getDisclosureId())
				.disclosureNumber(disclosure.getDisclosureNumber()).riskCategory(disclosure.getCoiRiskCategory().getDescription())
				.riskCategoryCode(disclosure.getRiskCategoryCode()).newRiskCategory(risk.getDescription())
				.newRiskCategoryCode(risk.getRiskCategoryCode()).actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_RISK)
				.administratorName(AuthenticatedUser.getLoginUserFullName())
				.fcoiTypeCode(disclosure.getFcoiTypeCode()).revisionComment(disclosureDto.getRevisionComment()).build();
		actionLogService.saveDisclosureActionLog(actionLogDto);
		return new ResponseEntity<>(disclosureDto, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchAllDisclosureRisk() {
		return new ResponseEntity<>(conflictOfInterestDao.fetchDisclosureRiskCategory(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchDisclosureHistory(DisclosureActionLogDto actionLogDto) {
		actionLogDto.setActionTypeCodes(Arrays.asList(Constants.COI_DISCLOSURE_ACTION_LOG_ADD_RISK, Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_RISK));
		return new ResponseEntity<>(actionLogService.fetchDisclosureActionLog(actionLogDto), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getCoiSectionsTypeCode(ConflictOfInterestVO vo) {
		CoiSectionTypeDto coiSectionTypeDto = CoiSectionTypeDto.builder()
				.coiSectionsTypeList(getSectionTypeList())
				.personEntities(getPersonEntityList(vo))
				.projectList(getProjectDetailList(vo.getPersonId(), vo.getDisclosureId()))
				.questionnaireDataBus(getQuestionnaireList(vo.getDisclosureId()))
				.build();
		return new ResponseEntity<>(coiSectionTypeDto,HttpStatus.OK);
	}

	private QuestionnaireDataBus getQuestionnaireList(Integer disclosureId) {
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setModuleItemCode(Integer.parseInt("8"));
		questionnaireDataBus.setModuleItemKey(disclosureId.toString());
		questionnaireDataBus.setModuleSubItemKey("0");
		questionnaireDataBus.setModuleSubItemCode(0);
		questionnaireDataBus.setActionPersonId(AuthenticatedUser.getLoginPersonId());
		questionnaireDataBus.setQuestionnaireMode("ANSWERED");
		questionnaireDataBus = questionnaireService.getApplicableQuestionnaire(questionnaireDataBus);
		return questionnaireDataBus;
	}

	private List<PersonEntity> getPersonEntityList(ConflictOfInterestVO vo) {
		return conflictOfInterestDao.getSFIOfDisclosure(vo);
	}

	private List<DisclosureDetailDto> getProjectDetailList(String personId, Integer disclosureId) {
		List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
				personId, disclosureId, null);
		List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, personId,
				disclosureId, null);
		List<DisclosureDetailDto> projectList = Stream.concat(proposalDetails.stream(), awardDetails.stream())
		        .collect(Collectors.toList());
		projectList.stream().forEach(project -> {
			List<CoiDisclEntProjDetailsDto> disclosureDetails = new ArrayList<>();
			conflictOfInterestDao.getProjectRelationshipByParam(project.getModuleCode(), project.getModuleItemId(),personId,
					disclosureId).forEach(disclosureDetail -> {
				CoiDisclEntProjDetailsDto coiDisclEntProjDetails = new CoiDisclEntProjDetailsDto();
				BeanUtils.copyProperties(disclosureDetail, coiDisclEntProjDetails, "coiDisclosure", "coiEntity", "personEntity");
				if (disclosureDetail.getCoiEntity() != null) {
					CoiEntityDto coiEntityDto = new CoiEntityDto();
					BeanUtils.copyProperties(disclosureDetail.getCoiEntity(), coiEntityDto, "entityStatus", "entityType", "coiProjConflictStatusType");
					coiDisclEntProjDetails.setCoiEntity(coiEntityDto);
				}
				disclosureDetails.add(coiDisclEntProjDetails);
			});
			project.setCoiDisclEntProjDetails(disclosureDetails);
		});
		return projectList;
	}

	private List<CoiSectionsType> getSectionTypeList() {
		return conflictOfInterestDao.getCoiSectionsTypeCode();
	}

	@Override
	public ResponseEntity<Object> modifyTravelDisclosureRisk(CoiTravelDisclosureDto travelDisclosureDto) {
		CoiTravelDisclosure traveldisclosure = conflictOfInterestDao.loadTravelDisclosure(travelDisclosureDto.getTravelDisclosureId());
		CoiRiskCategory risk = conflictOfInterestDao.getRiskCategoryStatusByCode(travelDisclosureDto.getRiskCategoryCode());
		travelDisclosureDto.setUpdateTimestamp(conflictOfInterestDao.updateTravelDisclosureRiskCategory(travelDisclosureDto));
		TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder()
				.travelDisclosureId(traveldisclosure.getTravelDisclosureId())
				.travelNumber(traveldisclosure.getTravelNumber())
				.riskCategory(traveldisclosure.getCoiRiskCategory() != null ? traveldisclosure.getCoiRiskCategory().getDescription() : null)
				.riskCategoryCode(traveldisclosure.getRiskCategoryCode()).newRiskCategory(risk.getDescription())
				.newRiskCategoryCode(risk.getRiskCategoryCode()).actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_RISK)
				.administratorName(AuthenticatedUser.getLoginUserFullName())
				.comment(travelDisclosureDto.getComment()).build();
		actionLogService.saveTravelDisclosureActionLog(actionLogDto);
		traveldisclosure.setRiskCategoryCode(risk.getRiskCategoryCode());
		traveldisclosure.setRiskLevel(risk.getDescription());
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(traveldisclosure);
		return new ResponseEntity<>(traveldisclosure, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchTravelDisclosureHistory(TravelDisclosureActionLogDto actionLogDto) {
		return new ResponseEntity<>(actionLogService.fetchTravelDisclosureActionLog(actionLogDto), HttpStatus.OK);
	}

	@Override
	public String deleteReviewCommentTag(Integer coiReviewCommentTagId) {
		conflictOfInterestDao.deleteReviewTagByCommentTagId(coiReviewCommentTagId);
		return commonDao.convertObjectToJSON(DELETE_MSG);
	}

	@Override
	public ResponseEntity<Object> getSFILatestVersion(Integer personEntityNumber) {
		return new ResponseEntity<>(conflictOfInterestDao.getSFILatestVersion(personEntityNumber), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> loadDisclAttachTypes() {
		return new ResponseEntity<>(conflictOfInterestDao.loadDisclAttachTypes(), HttpStatus.OK);
	}
	
	@Override
	public List<Inbox> fetchAllActiolListEntriesForBanners(NotificationBannerDto notifyBannerDto) {
		return conflictOfInterestDao.fetchAllActiolListEntriesForBanners(notifyBannerDto);
	}

	@Override
	public List<Notes> fetchAllNotesForPerson(String personId) {
		return conflictOfInterestDao.fetchAllNotesForPerson(personId);
	}

	@Override
	public ResponseEntity<Object> saveOrUpdatePersonNote(NotesDto coiNotesdto) {
		Notes notes = coiNotesdto.getNoteId() == null ? new Notes() :
						 conflictOfInterestDao.loadCoiNotesForNoteId(coiNotesdto.getNoteId());
		notes.setPersonId(coiNotesdto.getPersonId());
		notes.setContent(coiNotesdto.getContent());
		notes.setUpdateUser(AuthenticatedUser.getLoginUserName());
		notes.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		conflictOfInterestDao.saveOrUpdatePersonNote(notes);
		return new ResponseEntity<>(notes, HttpStatus.OK);
	}

	@Override
	public Notes getNoteDetailsForNoteId(Integer noteId) {
		return conflictOfInterestDao.loadCoiNotesForNoteId(noteId);
	}
	
	@Override
	public ResponseEntity<Object> saveOrUpdateAttachments(MultipartFile[] files, String formDataJSON) {
		List<Attachments> attachmentsList = new ArrayList<>();
		AttachmentsDto dto = new AttachmentsDto();
		ObjectMapper mapper = new ObjectMapper();
		try {
			dto = mapper.readValue(formDataJSON, AttachmentsDto.class);
			dto.getNewAttachments().forEach(ele -> {
				int count = 0;
				AttachmentsDto request = AttachmentsDto.builder()
						.personId(AuthenticatedUser.getLoginPersonId())
						.attaTypeCode(ele.getAttaTypeCode())
						.fileName(ele.getFileName())
						.mimeType(ele.getMimeType())
						.description(ele.getDescription())
						.createUser(AuthenticatedUser.getLoginUserName())
						.createTimestamp(commonDao.getCurrentTimestamp())
						.updateUser(AuthenticatedUser.getLoginUserName())
						.updateTimestamp(commonDao.getCurrentTimestamp())
						.build();
				DisclAttaType disclosureAttachmentType = conflictOfInterestDao.getDisclosureAttachmentForTypeCode(ele.getAttaTypeCode());
				Attachments attachment = addAttachments(files[count], request, AuthenticatedUser.getLoginPersonId());
				attachment.setDisclAttaTypeDetails(disclosureAttachmentType);
				attachmentsList.add(attachment);
				count++;
			});
		} catch (JsonProcessingException e) {
			throw new ApplicationException("error in addTagPerson", e, Constants.JAVA_ERROR);
		}
		return new ResponseEntity<>(attachmentsList, HttpStatus.OK);
	}

	private Attachments addAttachments(MultipartFile file, AttachmentsDto request, String personId) {
		try {
			Attachments attachment = null;
			if (file != null) {
				request.setFile(file);
				attachment = coiFileAttachmentService.saveAttachment(request, personId);
			}
			return attachment;
		} catch (Exception e) {
			throw new ApplicationException("error in addAttachments", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public ResponseEntity<Object> deleteNote(Integer noteId) {
		conflictOfInterestDao.deleteNote(noteId);
		return null;
	}

	@Override
	public List<Attachments> loadAllAttachmentsForPerson(String personId) {
		List<Attachments> attachmentsList = conflictOfInterestDao.loadAllAttachmentsForPerson(personId);
		return attachmentsList;
	}

	@Override
	public ResponseEntity<Object> getEntityWithRelationShipInfo(CommonRequestDto requestDto) {
		requestDto.setId(AuthenticatedUser.getLoginPersonId());
		return new ResponseEntity<>(conflictOfInterestDao.getEntityWithRelationShipInfo(requestDto), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getSFIRelationshipDetails() {
		return new ResponseEntity<>(conflictOfInterestDao.getRelatedEntityInfo(null, AuthenticatedUser.getLoginPersonId(), true), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> completeDisclosureReviews(Map<Integer, Integer> disclosureIdNumberMap) {
		List<Integer> notAllowedDisclosureIds = new ArrayList<>();
		disclosureIdNumberMap.forEach((disclosureId, disclosureNumber) -> {
		    ResponseEntity<Object> result = completeDisclosureReviews(disclosureId, disclosureNumber);
		    if (result.getStatusCode() == HttpStatus.METHOD_NOT_ALLOWED) {
		        notAllowedDisclosureIds.add(disclosureId);
		    }
		});
		return !notAllowedDisclosureIds.isEmpty() ? new ResponseEntity<>(commonDao.convertObjectToJSON(notAllowedDisclosureIds), HttpStatus.OK) 
				: new ResponseEntity<>(commonDao.convertObjectToJSON("Approved successfully") , HttpStatus.OK);
	}

	private ResponseEntity<Object> completeDisclosureReviews(Integer opaDisclosureId, Integer opaDisclosureNumber) {
		return completeReview(opaDisclosureId, opaDisclosureNumber, true);
	}

	private ResponseEntity<Object> completeReview(Integer disclosureId, Integer disclosureNumber, boolean isBatch) {
		if (conflictOfInterestDao.isDisclosureInStatuses(disclosureId, APPROVED, REVIEW_STATUS_COMPLETE, Constants.COI_ACTIVE_STATUS)) {
			return  new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		if (conflictOfInterestDao.numberOfReviewNotOfStatus(disclosureId, Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED).equals(0)) {
			CoiDisclosure coiDisclosure = new CoiDisclosure();
			coiDisclosure.setDisclosureId(disclosureId);
			coiDisclosure.setDispositionStatusCode(APPROVED);
			coiDisclosure.setReviewStatusCode(REVIEW_STATUS_COMPLETE);
			coiDisclosure.setVersionStatus(Constants.COI_ACTIVE_STATUS);
			conflictOfInterestDao.completeDisclosureReview(coiDisclosure);
			CoiDisclosure disclosure = conflictOfInterestDao.loadDisclosure(disclosureId);
			if (disclosure.getFcoiTypeCode().equals("1")) {
				conflictOfInterestDao.archiveDisclosureOldVersions(disclosureId, disclosureNumber);
			}
			try {
				DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder()
						.actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_REVIEW_COMPLETED).disclosureId(disclosure.getDisclosureId())
						.disclosureNumber(disclosure.getDisclosureNumber()).fcoiTypeCode(disclosure.getFcoiTypeCode())
						.administratorName(AuthenticatedUser.getLoginUserFullName())
						.build();
				actionLogService.saveDisclosureActionLog(actionLogDto);
			} catch (Exception e) {
				logger.error("completeDisclosureReview : {}", e.getMessage());
			}
			return isBatch ? new ResponseEntity<>("Approved successfully", HttpStatus.OK) : new ResponseEntity<>(loadDisclosure(disclosureId), HttpStatus.OK);
//			return new ResponseEntity<>(loadDisclosure(disclosureId), HttpStatus.OK);
		}
		return new ResponseEntity<>("REVIEW_STATUS_NOT_COMPLETE", HttpStatus.OK);
	}

}
