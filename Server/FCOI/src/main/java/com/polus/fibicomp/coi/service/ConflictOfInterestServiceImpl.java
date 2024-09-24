
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

import javax.persistence.NoResultException;
import javax.validation.Valid;

import com.polus.core.constants.CoreConstants;
import com.polus.fibicomp.fcoiDisclosure.service.FCOIDisclProjectService;
import com.polus.fibicomp.fcoiDisclosure.service.FcoiDisclosureService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.inbox.pojo.Inbox;
import com.polus.core.messageq.config.MessageQServiceRouter;
import com.polus.core.messageq.vo.MessageQVO;
import com.polus.core.messageq.vo.MessagingQueueProperties;
import com.polus.core.notification.pojo.NotificationRecipient;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.person.pojo.Person;
import com.polus.core.pojo.Country;
import com.polus.core.pojo.FileType;
import com.polus.core.pojo.Unit;
import com.polus.core.questionnaire.dto.QuestionnaireDataBus;
import com.polus.core.questionnaire.service.QuestionnaireService;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.CoiAssignTravelDisclosureAdminDto;
import com.polus.fibicomp.coi.dto.CoiDisclEntProjDetailsDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.CoiSectionTypeDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureActionsDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureCertifyDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiTravelHistoryDto;
import com.polus.fibicomp.coi.dto.CommonRequestDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.dto.DisclosureHistoryResponse;
import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.dto.NotesDto;
import com.polus.fibicomp.coi.dto.NotificationBannerDto;
import com.polus.fibicomp.coi.dto.NotificationDto;
import com.polus.fibicomp.coi.dto.PersonAttachmentDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.WithdrawDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.pojo.Attachments;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.CoiTravelConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureTraveler;
import com.polus.fibicomp.coi.pojo.CoiTravelDocumentStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelReviewStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelerType;
import com.polus.fibicomp.coi.pojo.DisclAttaType;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.pojo.Notes;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.repository.ActionLogDao;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.coi.vo.DashBoardProfile;
import com.polus.fibicomp.constants.ActionTypes;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.constants.StaticPlaceholders;
import com.polus.fibicomp.fcoiDisclosure.dao.FcoiDisclosureDao;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclosure;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiProjectType;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiRiskCategory;
import com.polus.fibicomp.globalentity.pojo.Entity;
import com.polus.fibicomp.opa.dao.OPADao;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPADashboardResponseDto;
import com.polus.fibicomp.reviewcomments.dao.ReviewCommentDao;
import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import com.polus.fibicomp.reviewcomments.service.ReviewCommentService;


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

	@Autowired
	private ReviewCommentDao reviewCommentDao;

	@Autowired
	private ReviewCommentService reviewCommentService;

	@Autowired
	private PersonEntityService personEntityService;

	@Autowired
	private MessageQServiceRouter messageQServiceRouter;

	@Autowired
	private MessagingQueueProperties messagingQueueProperties;

	@Autowired
	private FcoiDisclosureDao fcoiDisclosureDao;

	@Autowired
	private FcoiDisclosureService fcoiDisclosureService;

	@Autowired
	private FCOIDisclProjectService projectService;

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
	private static final String TAB_TYPE_CONSULTING_DISCLOSURES = "CONSULTING_DISCLOSURES";
	private static final String TAB_TYPE_MY_DASHBOARD = "MY_DASHBOARD";
	private static final String TAB_TYPE_IN_PROGRESS_DISCLOSURES = "IN_PROGRESS_DISCLOSURES";
	private static final String TAB_TYPE_APPROVED_DISCLOSURES = "APPROVED_DISCLOSURES";
	private static final String FCOI_DISCLOSURE = "FCOI_DISCLOSURE";
	private static final String PROJECT_DISCLOSURE = "PROJECT_DISCLOSURE";

	private static final String PROJECT_NOTIFY = "PROJECT_NOTIFY";
	private static final String SINGLE_PERSON = "S";
	private static final String NOTIFICATION_RECIPIENTS = "NOTIFICATION_RECIPIENTS";
	private static final String ALL_PERSON = "A";

	private static final String PROJECT_ID = "PROJECT_ID";

	private static final String PROJECT_TITLE = "PROJECT_TITLE";

	private static final String DISCLOSURE_STATUS = "DISCLOSURE_STATUS";

	private static final String REPORTER_NAME = "REPORTER_NAME";

	@Override
	public List<Entity> searchEntity(ConflictOfInterestVO vo) {
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
					disclosure.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(disclosure.getUpdatedBy()));
				});
			}
		}
		return new ResponseEntity<>(disclosures, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> saveOrUpdateCoiReview(ConflictOfInterestVO vo){
		fcoiDisclosureService.checkDispositionStatusIsVoid(vo.getCoiReview().getDisclosureId());
		String actionTypeCode = null;
		CoiReview coiReview = vo.getCoiReview();
		if (coiReview.getCoiReviewId() == null && conflictOfInterestDao.isReviewAdded(coiReview)) {
			return new ResponseEntity<>(commonDao.convertObjectToJSON("Review already added"), HttpStatus.INTERNAL_SERVER_ERROR);
		} else if (coiReview.getCoiReviewId() != null) {
			if (conflictOfInterestDao.isReviewStatusChanged(coiReview)) {
				return new ResponseEntity<>(commonDao.convertObjectToJSON("Review status changed"), HttpStatus.METHOD_NOT_ALLOWED);
			}
			if (!coiReview.getReviewStatusTypeCode().equalsIgnoreCase("2") && conflictOfInterestDao.isReviewPresent(coiReview)) {
				return new ResponseEntity<>(commonDao.convertObjectToJSON("Review already added"), HttpStatus.INTERNAL_SERVER_ERROR);
			}
		}
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		Map<String, String> actionTypes = new HashMap<>();
		if (coiReview.getCoiReviewId() == null) {
			if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITH_REVIEWER;
				actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_REVIEWER_ASSIGN);
				actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_REVIEWER_ASSIGN);
			} else {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITHOUT_REVIEWER;
				actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_LOCATION_ASSIGN);
				actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_LOCATION_ASSIGN);
			}
		}
		else {
			if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITH_REVIEWER;
				actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_REVIEWER_UPDATE);
				actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_REVIEWER_UPDATE);
			} else {
				actionTypeCode = Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITHOUT_REVIEWER;
				actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_LOCATION_UPDATE);
				actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_LOCATION_UPDATE);
			}
		}
		String assigneePersonId = coiReview.getCoiReviewId() != null ? conflictOfInterestDao.loadCoiReviewAssigneePersonName(coiReview.getCoiReviewId()) : null;
		String assigneePersonName = assigneePersonId != null ? personDao.getPersonFullNameByPersonId(assigneePersonId) : null;
		CoiReview coiReviewObj = conflictOfInterestDao.saveOrUpdateCoiReview(vo.getCoiReview());
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
		CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(coiReview.getDisclosureId());
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
		fcoiDisclosureDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
		/*Need clarification*/
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(CREATE_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		//Publishing to queue
		Map<String, String> additionalDetails = new HashMap<>();
		additionalDetails.put(StaticPlaceholders.REVIEWER_REVIEW_STATUS, coiReview.getReviewerStatusType().getDescription());
		processCoiMessageToQ(getDisclosureActionType(disclosure.getFcoiTypeCode(), actionTypes), disclosure.getDisclosureId(), coiReviewObj.getCoiReviewId(), additionalDetails);
		/*Need clarification*/
		coiReview.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
		coiReview.setUpdateTimestamp(disclosure.getUpdateTimestamp());
		return new ResponseEntity<>(coiReview, HttpStatus.OK);
	}

	@Override
	public List<CoiReview> getCoiReview(CoiDisclosureDto disclosureDto){
		if (!disclosureDto.getDispositionStatusCode().equals(Constants.COI_DISCL_DISPOSITION_STATUS_VOID) &&
				fcoiDisclosureDao.isDisclDispositionInStatus(Constants.COI_DISCL_DISPOSITION_STATUS_VOID, disclosureDto.getDisclosureId())) {
			throw new ApplicationException("Disclosure is in void status!",CoreConstants.JAVA_ERROR, HttpStatus.METHOD_NOT_ALLOWED);
		}
		List<CoiReview> coiReviews = conflictOfInterestDao.getCoiReview(disclosureDto.getDisclosureId());
		coiReviews.forEach(coiReview -> {
			coiReview.setAssigneePersonName(personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId()));
		});
		return conflictOfInterestDao.getCoiReview(disclosureDto.getDisclosureId());
	}

	@Override
	public ResponseEntity<Object> startReview(ConflictOfInterestVO vo){
		fcoiDisclosureService.checkDispositionStatusIsVoid(vo.getCoiReview().getDisclosureId());
		CoiReviewAssigneeHistory coiReviewAssigneeHistory = new CoiReviewAssigneeHistory();
		if (conflictOfInterestDao.isReviewStatus(vo.getCoiReview().getCoiReviewId(),
				Arrays.asList(Constants.COI_REVIEWER_REVIEW_STATUS_START, Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED))) {
			return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(vo.getCoiReview().getDisclosureId());
		if (disclosure.getReviewStatusCode().equalsIgnoreCase(REVIEW_STATUS_RETURNED)) {
			return new ResponseEntity<>("Disclosure already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		conflictOfInterestDao.startReview(DISCLOSURE_REVIEW_IN_PROGRESS,vo.getCoiReview().getCoiReviewId(), null);
		CoiReview coiReview = conflictOfInterestDao.loadCoiReview(vo.getCoiReview().getCoiReviewId());
		coiReviewAssigneeHistory.setAdminGroupId(coiReview.getAdminGroupId());
		coiReviewAssigneeHistory.setAssigneePersonId(coiReview.getAssigneePersonId());
		coiReviewAssigneeHistory.setAssigneeType(coiReview.getAdminGroupId() != null ? "G" :"P");
		coiReviewAssigneeHistory.setCoiReviewId(coiReview.getCoiReviewId());
		coiReviewAssigneeHistory.setCoiReviewActivityId(START_ACTIVIVITY);
		conflictOfInterestDao.saveOrUpdateCoiReviewAssigneeHistory(coiReviewAssigneeHistory);
		try {
			String actionTypeCode;
			String reviewerName = "";
			if (coiReview.getAssigneePersonId() != null &&
					coiReview.getAssigneePersonId().equalsIgnoreCase(AuthenticatedUser.getLoginPersonId())) {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_START_REVIEW;
				reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
				coiReview.setAssigneePersonName(reviewerName);
			} else if (coiReview.getAssigneePersonId() != null) {
				actionTypeCode = Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITH_REVIEWER;
				reviewerName = personDao.getPersonFullNameByPersonId(coiReview.getAssigneePersonId());
				coiReview.setAssigneePersonName(reviewerName);
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
		Timestamp updateTimestamp = fcoiDisclosureDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
		coiReview.setUpdateTimestamp(updateTimestamp);
		coiReview.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiReview.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
		return new ResponseEntity<>(coiReview, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> completeReview(ConflictOfInterestVO vo){
		if (conflictOfInterestDao.isReviewStatus(vo.getCoiReview().getCoiReviewId(), Arrays.asList(Constants.COI_REVIEWER_REVIEW_STATUS_COMPLETED))) {
			return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiDisclosure disclObj = fcoiDisclosureDao.loadDisclosure(vo.getCoiReview().getDisclosureId());
		if (disclObj.getReviewStatusCode().equalsIgnoreCase(REVIEW_STATUS_RETURNED)) {
			return new ResponseEntity<>("Disclosure already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		fcoiDisclosureService.checkDispositionStatusIsVoid(disclObj.getDispositionStatusCode());
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
		CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(coiReview.getDisclosureId());
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
		Timestamp updateTimestamp = fcoiDisclosureDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
		coiReview.setUpdateTimestamp(updateTimestamp);
		coiReview.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiReview.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
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
		return new ResponseEntity<>(coiReview, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> deleteReview(Integer coiReviewId){
		try {
			CoiReview coiReview = conflictOfInterestDao.loadCoiReview(coiReviewId);
			if (coiReview == null) {
				return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
			}
			fcoiDisclosureService.checkDispositionStatusIsVoid(coiReview.getDisclosureId());
			conflictOfInterestDao.deleteReviewAssigneeHistory(coiReviewId);

			reviewCommentDao.fetchReviewComments(ReviewCommentsDto.builder()
					.componentTypeCode(Constants.COI_DISCL_REVIEW_COMPONENT_TYPE)
					.moduleCode(Constants.COI_MODULE_CODE)
					.subModuleItemKey(coiReviewId)
					.moduleItemKey(coiReview.getDisclosureId()).build()).forEach(reviewComment -> {
				reviewCommentService.deleteReviewComment(reviewComment.getCommentId());
			});
			conflictOfInterestDao.deleteReview(coiReviewId);
			Timestamp updateTimestamp = fcoiDisclosureDao.updateDisclosureUpdateDetails(coiReview.getDisclosureId());
			CoiDisclosure coiDisclosure = new CoiDisclosure();
			coiDisclosure.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
			coiDisclosure.setUpdateTimestamp(updateTimestamp);
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
	public ResponseEntity<Object> completeDisclosureReview(Integer disclosureId, Integer disclosureNumber){
		fcoiDisclosureService.checkDispositionStatusIsVoid(disclosureId);
		return completeReview(disclosureId, disclosureNumber, false);
	}

	@Override
	public List<CoiConflictHistory> getCoiConflictHistory(Integer disclosureDetailsId){
		List<CoiConflictHistory> coiConflictHistoryList = conflictOfInterestDao.getCoiConflictHistory(disclosureDetailsId);
		coiConflictHistoryList.forEach(conflictHistory -> {
			conflictHistory.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(conflictHistory.getUpdatedBy()));
			conflictHistory.setConflictStatusDescription(conflictOfInterestDao.getCoiConflictStatusByStatusCode(conflictHistory.getConflictStatusCode()));
		});
		return coiConflictHistoryList;
	}

	@Override
	public String loadProposalsForDisclosure(String searchString) {
		List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE,
				AuthenticatedUser.getLoginPersonId(), searchString, null);
		return commonDao.convertObjectToJSON(proposalDetails);
	}

	@Override
	public String loadAwardsForDisclosure(String searchString) {
		List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
				AuthenticatedUser.getLoginPersonId(), searchString, null);
		return commonDao.convertObjectToJSON(awardDetails);
	}

	@Override
	public String loadDisclosureHistory(ConflictOfInterestVO vo) {
		List<CoiDisclosure> coiDisclosures = conflictOfInterestDao.getCoiDisclosuresByDisclosureNumber(vo.getDisclosureNumber());
		if (coiDisclosures != null && !coiDisclosures.isEmpty()) {
			Set<String> userIds = coiDisclosures.stream().map(CoiDisclosure::getUpdatedBy).collect(Collectors.toSet());
			if (!userIds.isEmpty()) {
				List<Person> personDetails = commonDao.getPersonDetailByPersonId(new ArrayList<>(userIds));
				Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(Person::getPersonId,
						person -> person.getFullName()));
				coiDisclosures.stream().filter(item -> item.getUpdatedBy() != null).filter(item ->
						collect.containsKey(item.getUpdatedBy().toUpperCase())).forEach(item ->
						item.setUpdateUserFullName(collect.get(item.getUpdatedBy().toUpperCase())));
			}
			vo.setPerson(coiDisclosures.get(0).getPersonId() != null ? personDao.getPersonDetailById(coiDisclosures.get(0).getPersonId()) : null);
			vo.setCoiDisclosures(coiDisclosures);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<Object> saveOrUpdateEntity(ConflictOfInterestVO vo) {
		Entity coiEntity = vo.getCoiEntity();
//		coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
//		if (coiEntity.getEntityId() == null) { // on creation
//			coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
//			coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
//			coiEntity.setIsActive(true); // Y
//			coiEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
//			coiEntity.setVersionNumber(Constants.COI_INITIAL_VERSION_NUMBER);
//			coiEntity.setEntityNumber(conflictOfInterestDao.generateMaxEntityNumber());
//			if (coiEntity.getRiskCategoryCode() == null) {
//				coiEntity.setRiskCategoryCode(RISK_CAT_CODE_LOW);
//				coiEntity.setEntityRiskCategory(conflictOfInterestDao.getEntityRiskDetails(RISK_CAT_CODE_LOW));
//			}
//			conflictOfInterestDao.saveOrUpdateEntity(coiEntity);
//			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_CREATE_ACTION_LOG_CODE, coiEntity, null);
//		} else { // on update or patch checks its a major change or not
//			Integer entityId = coiEntity.getEntityId();
//			coiEntity.setUpdateTimestamp(commonDao.getCurrentTimestamp());
//			coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
//			coiEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
//			if (coiEntity.isMajorVersion() && conflictOfInterestDao.checkEntityAdded(entityId, null)) { // checks the entity is linked to a SFI or not
//				coiEntity.setIsActive(true); // N
//				conflictOfInterestDao.archiveEntity(entityId);
//				coiEntity.setEntityId(null);
//				coiEntity.setVersionNumber(conflictOfInterestDao.getMaxEntityVersionNumber(coiEntity.getEntityNumber()) + 1);
//				coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
//				coiEntity.setCreateTimestamp(commonDao.getCurrentTimestamp());
//				conflictOfInterestDao.saveOrUpdateEntity(coiEntity);
//				conflictOfInterestDao.syncEntityWithPersonEntity(coiEntity.getEntityId(), coiEntity.getEntityNumber(), null);
//			} else {
//				conflictOfInterestDao.saveOrUpdateEntity(coiEntity);
//			}
//			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_MODIFY_ACTION_LOG_CODE, coiEntity, coiEntity.getRevisionReason());
//		}
		return new ResponseEntity<>(coiEntity, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getEntityDetails(Integer coiEntityId) {
		ConflictOfInterestVO vo = new ConflictOfInterestVO();
		vo.setCoiEntity(conflictOfInterestDao.getEntityDetailsById(coiEntityId));
//		vo.getEntity().setUpdatedUserFullName(personDao.getPersonFullNameByPersonId(vo.getEntity().getUpdatedBy()));
//		vo.getEntity().setCreateUserFullName(personDao.getPersonFullNameByPersonId(vo.getEntity().getCreatedBy()));
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
		if ((vo.getFilterType().equalsIgnoreCase(FILTER_TYPE_ALL) || vo.getFilterType().equalsIgnoreCase(FILTER_TYPE_OPA))
				&& (!(vo.getTabName().equalsIgnoreCase(TAB_TYPE_TRAVEL_DISCLOSURES) || vo.getTabName().equalsIgnoreCase(TAB_TYPE_CONSULTING_DISCLOSURES)))) {
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
		conflictOfInterestVO.setTravelDisclosureCount(conflictOfInterestDao.getCOIDashboardCount(vo));
		vo.setTabName("CONSULTING_DISCLOSURES");
		conflictOfInterestVO.setConsultDisclCount(conflictOfInterestDao.getCOIDashboardCount(vo));
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
		vo.setEntityList(conflictOfInterestDao.getAllEntityList(vo));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> setEntityStatus(ConflictOfInterestVO vo) {
		conflictOfInterestDao.setEntityStatus(vo);
		return new ResponseEntity<>(vo, HttpStatus.OK);
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
		vo.setCoiEntity(conflictOfInterestDao.getEntityByPersonEntityId(personEntityId));
//		vo.getEntity().setUpdatedUserFullName(personDao.getUserFullNameByUserName(vo.getEntity().getUpdateUser()));
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getValidPersonRelationshipLookUp() {
		return new ResponseEntity<>(conflictOfInterestDao.fetchAllValidPersonEntityRelTypes(), HttpStatus.OK);
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
		Entity coiEntity = conflictOfInterestDao.getEntityDetailsById(entityId);
//		coiTravelDisclosure.setRiskCategoryCode(coiEntity.getRiskCategoryCode());
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
		Entity entityDetails = conflictOfInterestDao.getEntityDetails(vo.getEntityId());
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
		coiTravelDisclosure.setPerson(personDao.getPersonDetailById(AuthenticatedUser.getLoginPersonId()));
		coiTravelDisclosure.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(AuthenticatedUser.getLoginPersonId()));
		coiTravelDisclosure.setPersonNotesCount(conflictOfInterestDao.personNotesCount(AuthenticatedUser.getLoginPersonId()));
		coiTravelDisclosure.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(AuthenticatedUser.getLoginPersonId()).build()));
		setAllTravelDisclosureStatus(coiTravelDisclosure, vo.getEntityId());
		coiTravelDisclosure.setPersonFullName(personDao.getPersonFullNameByPersonId(coiTravelDisclosure.getPersonId()));
		setUnitDetails(coiTravelDisclosure, vo);
		if (vo.getEntityId() != null) {
			addEntryToPersonEntity(coiTravelDisclosure, vo);
		}
		coiTravelDisclosure.setIsInterNationalTravel(vo.getIsInternationalTravel());
		coiTravelDisclosure.setUpdateUser(AuthenticatedUser.getLoginUserName());
		coiTravelDisclosure.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
		updateTravelDisclConflictComment(null, null, coiTravelDisclosure.getTravelDisclosureId());
		coiTravelDisclosure.setEntity(entityDetails);
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
		if (vo.getTravelDisclosureId() == null) {
			try {
				TravelDisclosureActionLogDto actionLogDto = TravelDisclosureActionLogDto.builder()
						.actionTypeCode(ACTION_LOG_CREATED)
						.travelDisclosureId(coiTravelDisclosure.getTravelDisclosureId())
						.travelNumber(coiTravelDisclosure.getTravelNumber()).comment(vo.getDescription())
						.reporter(AuthenticatedUser.getLoginUserFullName()).build();
				actionLogService.saveTravelDisclosureActionLog(actionLogDto);
			} catch (Exception e) {
				logger.error("createTravelDisclosure : {}", e.getMessage());
			}
		}
		return new ResponseEntity<>(coiTravelDisclosure, HttpStatus.OK);
	}

	private void addEntryToPersonEntity(CoiTravelDisclosure coiTravelDisclosure, ConflictOfInterestVO vo) {
			Integer personEntityId;
			personEntityId = conflictOfInterestDao.fetchMaxPersonEntityId(vo.getPersonId(), vo.getEntityId());
			if (personEntityId != null) {
				coiTravelDisclosure.setPersonEntityId(personEntityId);
				try {
					PersonEntityRelationship personEntityRelationship = new PersonEntityRelationship();
					personEntityRelationship.setPersonEntityId(personEntityId);
					personEntityRelationship.setValidPersonEntityRelTypeCodes(Arrays.asList(Constants.TRAVEL_SELF_RELATIONSHIP));
					personEntityService.saveOrUpdatePersonEntityRelationship(personEntityRelationship);
				} catch (Exception e) {
					logger.error("Exception on saveOrUpdatePersonEntityRelationship from create travel", e.getMessage());
				}
			} else {
				PersonEntity personEntity = new PersonEntity();
				personEntity.setEntityId(vo.getEntityId());
				personEntity.setEntityNumber(vo.getEntityNumber());
				personEntity.setInvolvementStartDate(coiTravelDisclosure.getTravelStartDate());
				personEntity.setInstituteResourceInvolvement("Relationship with Entity");
				personEntity.setStudentInvolvement("Relationship with Entity");
				personEntity.setStaffInvolvement("Relationship with Entity");
				personEntity.setValidPersonEntityRelTypeCodes(Arrays.asList(Constants.TRAVEL_SELF_RELATIONSHIP));
				ResponseEntity<Object> response = personEntityService.createPersonEntity(personEntity);
				personEntity = (PersonEntity)response.getBody();
				coiTravelDisclosure.setPersonEntityId(personEntity.getPersonEntityId());
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
		if ((dto.getActionType().equals("R") && conflictOfInterestDao.isSameAdminPersonOrGroupAddedInTravel(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getTravelDisclosureId()))
				|| (dto.getActionType().equals("A") && conflictOfInterestDao.isAdminPersonOrGroupAddedInTravel(dto.getTravelDisclosureId()))) {
			return new ResponseEntity<>("Admin already assigned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		CoiTravelDisclosure coiTravelDisclosure = conflictOfInterestDao.loadTravelDisclosure(dto.getTravelDisclosureId());
		if ((dto.getActionType().equals("R"))
				&& (coiTravelDisclosure.getReviewStatusCode().equals(Constants.TRAVEL_REVIEW_STATUS_CODE_RETURNED_TO_PI) || coiTravelDisclosure.getReviewStatusCode().equals(Constants.TRAVEL_REVIEW_STATUS_CODE_APPROVED))) {
			return new ResponseEntity<>("Reassign admin not allowed", HttpStatus.METHOD_NOT_ALLOWED);
		}
		if (dto.getActionType().equals("A") && !coiTravelDisclosure.getReviewStatusCode().equals(Constants.TRAVEL_REVIEW_STATUS_CODE_SUBMITTED)) {
			return new ResponseEntity<>("Assign admin not allowed", HttpStatus.METHOD_NOT_ALLOWED);
		}
		try {
			saveTravelDisclosureAssignAdminActionLog(dto.getAdminPersonId(), dto.getTravelDisclosureId());
		} catch (Exception e) {
			logger.error("assignDisclosureAdmin : {}", e.getMessage());
		}
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
		Entity entityDetails = conflictOfInterestDao.getEntityDetails(coiTravelDisclosure.getEntityId());
		dto.setEntityId(entityDetails.getEntityId());
		dto.setEntityTypeCode(entityDetails.getEntityStatusTypeCode());
		dto.setEntityEmail(entityDetails.getCertifiedEmail());
		dto.setEntityAddress(entityDetails.getPrimaryAddressLine1());
		dto.setEntityIsActive(entityDetails.getIsActive());
//		dto.setEntityRiskCategory(entityDetails.getEntityRiskCategory());
		dto.setRiskLevel(coiTravelDisclosure.getCoiRiskCategory() != null ? coiTravelDisclosure.getCoiRiskCategory().getDescription() : null);
		dto.setRiskCategoryCode(coiTravelDisclosure.getRiskCategoryCode());
//		EntityType entityTypeDetails = conflictOfInterestDao.getEntityTypeDetails(entityDetails.getEntityTypeCode());
//		dto.setEntityType(entityTypeDetails.getDescription());
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
		Person personDetails = personDao.getPersonDetailById(dto.getPersonId());
		dto.setPersonFullName(personDetails.getFullName());
		dto.setPersonPrimaryTitle(personDetails.getPrimaryTitle());
		dto.setPersonEmail(personDetails.getEmailAddress());
		dto.setCertifiedAt(coiTravelDisclosure.getCertifiedAt());
		dto.setCertifiedBy(coiTravelDisclosure.getCertifiedBy());
		dto.setDescription(coiTravelDisclosure.getDescription());
		dto.setExpirationDate(coiTravelDisclosure.getExpirationDate());
		dto.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(dto.getPersonId()));
		dto.setPersonNotesCount(conflictOfInterestDao.personNotesCount(dto.getPersonId()));
		dto.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(dto.getPersonId()).build()));
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
			logger.error("submitTravelDisclosure : {}", e.getMessage());
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
		PersonEntity personEntity = conflictOfInterestDao.fetchPersonEntityByEntityId(entityId, AuthenticatedUser.getLoginPersonId());
		if (personEntity != null) {
			PersonEntityDto personEntityDto = new PersonEntityDto();
			BeanUtils.copyProperties(personEntity, personEntityDto);
			Entity coiEntityObj = conflictOfInterestDao.getEntityDetails(personEntity.getEntityId());
			personEntityDto.setEntityOwnershipType(coiEntityObj.getEntityOwnershipType());
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
	public ResponseEntity<Object> activateOrInactivateEntity(CoiEntityDto coiCoiEntityDto) {
//		if (conflictOfInterestDao.isEntityActiveOrNot(null, coiCoiEntityDto.getEntityNumber(), coiCoiEntityDto.getIsActive(), Constants.COI_ACTIVE_STATUS)) {
//			if (coiCoiEntityDto.getIsActive()) {
//				return new ResponseEntity<>(" Entity already activated", HttpStatus.METHOD_NOT_ALLOWED);
//			} else {
//				return new ResponseEntity<>(" Entity already inactivated", HttpStatus.METHOD_NOT_ALLOWED);
//			}
//		}
//		Entity coiEntityObj = conflictOfInterestDao.getEntityDetails(coiCoiEntityDto.getEntityId());
//		if (conflictOfInterestDao.checkEntityAdded(coiCoiEntityDto.getEntityId(), null)) { // checks the entity is linked to a SFI or not
//			Entity coiEntity = new Entity();
//			BeanUtils.copyProperties(coiEntityObj, coiEntity);
//			coiEntity.setIsActive(coiCoiEntityDto.getIsActive());
//			conflictOfInterestDao.archiveEntity(coiCoiEntityDto.getEntityId());
//			coiEntity.setEntityId(null);
//			coiEntity.setVersionNumber(conflictOfInterestDao.getMaxEntityVersionNumber(coiEntity.getEntityNumber()) + 1);
//			coiEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS);
//			coiEntity.setUpdateUser(AuthenticatedUser.getLoginUserName());
//			coiEntity.setCreateUser(AuthenticatedUser.getLoginUserName());
//			coiEntity.setRevisionReason(coiCoiEntityDto.getRevisionReason());
//			conflictOfInterestDao.saveOrUpdateEntity(coiEntity);
//			coiCoiEntityDto.setEntityId(coiEntity.getEntityId());
//		} else {
//			conflictOfInterestDao.activateOrInactivateEntity(coiCoiEntityDto);
//		}
//		if (Boolean.TRUE.equals(coiCoiEntityDto.getIsActive())) {
//			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_ACTIVATE_ACTION_LOG_CODE, coiEntityObj, coiCoiEntityDto.getRevisionReason());
//		} else {
//			actionLogService.saveEntityActionLog(Constants.COI_ENTITY_INACTIVATE_ACTION_LOG_CODE, coiEntityObj, coiCoiEntityDto.getRevisionReason());
//		}
//		return new ResponseEntity<>(coiCoiEntityDto, HttpStatus.OK);
		return null;
	}

	@Override
	public ResponseEntity<Object> fetchAllRelationshipTypes() {
		return new ResponseEntity<>(conflictOfInterestDao.fetchAllRelationshipTypes(), HttpStatus.OK);
	}

	private DisclComment getDisclProjectConflictComment(Integer moduleItemKey, Integer submoduleItemKey) {
		List<DisclComment> reviewComment = reviewCommentDao.fetchReviewComments(ReviewCommentsDto.builder()
				.componentTypeCode(Constants.COI_DISCL_CONFLICT_RELATION_COMPONENT_TYPE)
				.moduleCode(Constants.COI_MODULE_CODE)
				.subModuleItemKey(submoduleItemKey)
				.moduleItemKey(moduleItemKey).build());
		return reviewComment != null && !reviewComment.isEmpty()? reviewComment.get(0) : null;
	}

	@Override
	public ResponseEntity<Object> approveEntity(EntityRelationship entityRelationship) {
//		if(conflictOfInterestDao.isEntityApproved(entityRelationship.getEntityId())) {
//			return new ResponseEntity<>("Entity already approved", HttpStatus.METHOD_NOT_ALLOWED);
//		}
//		CoiEntityDto coiCoiEntityDto = new CoiEntityDto();
//		coiCoiEntityDto.setEntityId(entityRelationship.getEntityId());
//		coiCoiEntityDto.setUpdateTimestamp(conflictOfInterestDao.approveEntity(entityRelationship.getEntityId()));
//		if (entityRelationship.getEntityRelTypeCode() != 1) { //  entityRelTypeCode = 1 (new)
//			entityRelationship.setUpdateUser(AuthenticatedUser.getLoginUserName());
//			entityRelationship.setUpdateTimestamp(commonDao.getCurrentTimestamp());
//			conflictOfInterestDao.saveOrUpdateEntityRelationship(entityRelationship);
//		}
//		coiCoiEntityDto.setEntityStatusCode(Constants.COI_ENTITY_STATUS_VERIFIED);
//		coiCoiEntityDto.setUpdatedUserFullName(personDao.getUserFullNameByUserName(AuthenticatedUser.getLoginUserFullName()));
//		Entity coiEntity = conflictOfInterestDao.getEntityDetailsById(coiCoiEntityDto.getEntityId());
//		Entity coiEntityCopy = new Entity();
//		BeanUtils.copyProperties(coiEntity, coiEntityCopy);
//		coiEntityCopy.setUpdatedUserFullName(personDao.getUserFullNameByUserName(coiEntity.getUpdateUser()));
//		actionLogService.saveEntityActionLog(Constants.COI_ENTITY_VERIFY_ACTION_LOG_CODE, coiEntityCopy, null);
//		return new ResponseEntity<>(coiCoiEntityDto, HttpStatus.OK);
		return null;
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
//		if (conflictOfInterestDao.isEntityRiskAdded(entityDto)) {
//			return  new ResponseEntity<>("Risk already added", HttpStatus.METHOD_NOT_ALLOWED);
//		}
//		Entity entity = conflictOfInterestDao.getEntityDetails(entityDto.getEntityId());
//		EntityRiskCategory riskCategory = conflictOfInterestDao.getEntityRiskDetails(entityDto.getRiskCategoryCode());
//		Entity entityCopy = new Entity();
//		BeanUtils.copyProperties(entity, entityCopy);
//		entityCopy.setNewRiskCategory(riskCategory);
//		entityDto.setUpdateTimestamp(conflictOfInterestDao.updateEntityRiskCategory(entityDto));
//		entityCopy.setUpdatedUserFullName(personDao.getUserFullNameByUserName(AuthenticatedUser.getLoginUserName()));
//		actionLogService.saveEntityActionLog(Constants.COI_ENTITY_MODIFY_RISK_ACTION_LOG_CODE, entityCopy, entityDto.getRevisionReason());
//		return new ResponseEntity<>(entityDto, HttpStatus.OK);
		return null;
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
			travelHistoryDto.setTravelEntityName(history.getEntity().getEntityName());
			travelHistoryDto.setTravellerTypeCodeList(travellerTypeCodeList);
			travelHistoryDto.setEntityType(history.getEntity().getEntityStatusType().getDescription());
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
    public ResponseEntity<Object> withdrawDisclosure(Integer disclosureId, String description) {
        CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(disclosureId);
		fcoiDisclosureService.checkDispositionStatusIsVoid(disclosure.getDispositionStatusCode());
		if ((!SUBMITTED_FOR_REVIEW.equalsIgnoreCase(disclosure.getReviewStatusCode()))
                || (disclosure.getAdminPersonId() != null) || (disclosure.getAdminGroupId() != null)) {
            return new ResponseEntity<>("Disclosure already withdrawn", HttpStatus.METHOD_NOT_ALLOWED);
        }
		Map<String, String> additionalDetails = new HashMap<>();
		additionalDetails.put(StaticPlaceholders.CERTIFICATION_DATE, disclosure.getCertifiedAt().toString());
		additionalDetails.put(StaticPlaceholders.WITHDRAWAL_REASON, description);
        disclosure.setCertificationText(null);
        disclosure.setCertifiedAt(null);
        disclosure.setCertifiedBy(null);
        disclosure.setExpirationDate(null);
        disclosure.setUpdatedBy(AuthenticatedUser.getLoginPersonId());
        disclosure.setReviewStatusCode(REVIEW_STATUS_WITHDRAWN);
		disclosure.setSyncNeeded(true);
        disclosure = fcoiDisclosureDao.saveOrUpdateCoiDisclosure(disclosure);
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
			Map<String, String> actionTypes = new HashMap<>();
			actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_WITHDRAW);
			actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_WITHDRAW);
			processCoiMessageToQ(getDisclosureActionType(disclosure.getFcoiTypeCode(), actionTypes), disclosure.getDisclosureId(), null, additionalDetails);
		} catch (Exception e) {
			logger.error("Exception on withdrawDisclosure : {}", e.getMessage());
		}
        return new ResponseEntity<>(withdrawDisclosureDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> returnDisclosure(Integer disclosureId, String description) {
        CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(disclosureId);
		if (disclosure.getReviewStatusCode().equalsIgnoreCase(REVIEW_STATUS_RETURNED)) {
			return new ResponseEntity<>("Disclosure already returned", HttpStatus.METHOD_NOT_ALLOWED);
		}
		Map<String, String> additionalDetails = new HashMap<>();
		additionalDetails.put(StaticPlaceholders.CERTIFICATION_DATE, disclosure.getCertifiedAt().toString());
        disclosure.setCertificationText(null);
        disclosure.setCertifiedAt(null);
        disclosure.setCertifiedBy(null);
        disclosure.setExpirationDate(null);
        disclosure.setUpdatedBy(AuthenticatedUser.getLoginPersonId());
        disclosure.setReviewStatusCode(REVIEW_STATUS_RETURNED);
		disclosure.setSyncNeeded(true);
        disclosure = fcoiDisclosureDao.saveOrUpdateCoiDisclosure(disclosure);
		try {
			DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_RETURNED)
					.disclosureId(disclosure.getDisclosureId()).disclosureNumber(disclosure.getDisclosureNumber())
					.fcoiTypeCode(disclosure.getFcoiTypeCode()).revisionComment(description)
					.administratorName(AuthenticatedUser.getLoginUserFullName())
					.build();
			actionLogService.saveDisclosureActionLog(actionLogDto);
			Map<String, String> actionTypes = new HashMap<>();
			actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_RETURN);
			actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_RETURN);
			Person adminDetails = personDao.getPersonDetailById(AuthenticatedUser.getLoginPersonId());
			additionalDetails.put(StaticPlaceholders.ADMINISTRATOR_NAME, adminDetails.getFirstName());
			additionalDetails.put(StaticPlaceholders.RETURN_REASON, description);
			additionalDetails.put(StaticPlaceholders.DISCLOSURE_STATUS,disclosure.getConflictStatusCode() != null? disclosure.getCoiConflictStatusType().getDescription() : null);
			processCoiMessageToQ(getDisclosureActionType(disclosure.getFcoiTypeCode(), actionTypes), disclosure.getDisclosureId(), null, additionalDetails);
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
			DisclComment disclComment = getTravelConflictComment(vo.getTravelDisclosureId());
			saveTravelConflictHistory(coiTravelDisclosure, disclComment);
			updateTravelDisclConflictComment(disclComment, vo.getDescription(), vo.getTravelDisclosureId());
			updateTravelDisclConflictStatus(coiTravelDisclosure, vo.getDisclosureStatusCode());
		}
		return new ResponseEntity<>(getCoiTravelConflictHistory(vo.getTravelDisclosureId()),HttpStatus.OK);
	}

	private DisclComment getTravelConflictComment(Integer travelDisclosureId) {
		List<DisclComment> resultData = reviewCommentDao.fetchReviewComments(ReviewCommentsDto.builder()
				.componentTypeCode(Constants.COI_TRAVEL_DISCL_CONFLICT_RELATION_COMPONENT_TYPE)
				.moduleCode(Constants.TRAVEL_MODULE_CODE)
				.moduleItemKey(travelDisclosureId).build());
		return resultData != null && !resultData.isEmpty() ? resultData.get(0) : null;
	}

	private void updateTravelDisclConflictStatus(CoiTravelDisclosure coiTravelDisclosure, String disclosureStatusCode) {
		coiTravelDisclosure.setDisclosureStatusCode(disclosureStatusCode);
		conflictOfInterestDao.saveOrUpdateCoiTravelDisclosure(coiTravelDisclosure);
	}

	private void updateTravelDisclConflictComment(DisclComment disclComment, String description, Integer travelDisclosureId) {
		if (disclComment == null) {
			disclComment = DisclComment.builder().comment(description)
					.componentTypeCode(TRAVEL_DISCLOSURE_CONFLICT_COMMENT)
//					.commentTypeCode(TRAVEL_DISCLOSURE_CONFLICT_COMMENT) //TODO need recheck on
					.commentPersonId(AuthenticatedUser.getLoginPersonId())
					.documentOwnerPersonId(AuthenticatedUser.getLoginPersonId()).isPrivate(false)
					.moduleItemKey(travelDisclosureId).moduleCode(Constants.TRAVEL_MODULE_CODE)
					.build();
		} else {
			disclComment.setComment(description);
		}
		disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
		reviewCommentDao.saveObject(disclComment);
	}

	private void saveTravelConflictHistory(CoiTravelDisclosure coiTravelDisclosure, DisclComment disclComment) {
		CoiTravelConflictHistory coiTravelConflictHistory = new CoiTravelConflictHistory();
		if (disclComment != null && disclComment.getComment() != null) {
			coiTravelConflictHistory.setComment(disclComment.getComment());
		}
		coiTravelConflictHistory.setUpdateTimestamp(disclComment.getUpdateTimestamp());
		coiTravelConflictHistory.setUpdateUser(disclComment.getUpdateUser());
		coiTravelConflictHistory.setConflictStatusCode(coiTravelDisclosure.getDisclosureStatusCode());
		coiTravelConflictHistory.setTravelDisclosureId(coiTravelDisclosure.getTravelDisclosureId());
		conflictOfInterestDao.saveOrUpdateCoiTravelConflictHistory(coiTravelConflictHistory);
	}

	private void saveTravelDisclConflictComment(ConflictOfInterestVO vo) {
		DisclComment disclComment = new DisclComment();
		disclComment.setComment(vo.getDescription());
		disclComment.setComponentTypeCode(Constants.COI_TRAVEL_DISCL_CONFLICT_RELATION_COMPONENT_TYPE);		//Travel disclosure conflict comment
		disclComment.setCommentPersonId(AuthenticatedUser.getLoginPersonId());
		disclComment.setDocumentOwnerPersonId(vo.getPersonId());
		disclComment.setIsPrivate(false);
		disclComment.setModuleItemKey(vo.getTravelDisclosureId());
		disclComment.setModuleItemNumber(String.valueOf(vo.getTravelNumber()));
		disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
		reviewCommentDao.saveObject(disclComment);
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
		DisclComment disclComment = getTravelConflictComment(travelDisclosureId);
		if(coiTravelDisclosure.getDisclosureStatusCode()!=null) {
			if (disclComment != null) {
				coiTravelConflictHistory.setTravelDisclosureId(travelDisclosureId);
				coiTravelConflictHistory.setComment(disclComment.getComment());
				coiTravelConflictHistory.setConflictStatusCode(coiTravelDisclosure.getDisclosureStatusCode());
				coiTravelConflictHistory.setUpdateTimestamp(disclComment.getUpdateTimestamp());
				coiTravelConflictHistory.setUpdateUser(disclComment.getUpdateUser());
			}
			coiTravelConflictHistoryList.add(0, coiTravelConflictHistory);
		}

		coiTravelConflictHistoryList.forEach(conflictHistory -> {
			conflictHistory.setUpdateUserFullName(personDao.getUserFullNameByUserName(conflictHistory.getUpdateUser()));
			conflictHistory.setConflictStatusDescription(conflictOfInterestDao.getCoiTravelConflictStatusByStatusCode(conflictHistory.getConflictStatusCode()));
		});
		return coiTravelConflictHistoryList;
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

	private List<DisclosureProjectDto> getProjectDetailList(String personId, Integer disclosureId) {
//		List<DisclosureDetailDto> awardDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.AWARD_MODULE_CODE,
//				personId, disclosureId, null, null);
//		List<DisclosureDetailDto> proposalDetails = conflictOfInterestDao.getProjectsBasedOnParams(Constants.DEV_PROPOSAL_MODULE_CODE, personId,
//				disclosureId, null, null);
//		List<DisclosureDetailDto> projectList = Stream.concat(proposalDetails.stream(), awardDetails.stream())
//				.collect(Collectors.toList());

		List<DisclosureProjectDto> projectList = fcoiDisclosureService.getDisclProjectsByDispStatus(disclosureId);

		projectList.stream().forEach(project -> {
			List<CoiDisclEntProjDetailsDto> disclosureDetails = new ArrayList<>();
			fcoiDisclosureDao.getProjectRelationshipByParam(project.getModuleCode(), Integer.valueOf(project.getProjectId()),personId,
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
		PersonAttachmentDto dto = new PersonAttachmentDto();
		ObjectMapper mapper = new ObjectMapper();
		try {
			dto = mapper.readValue(formDataJSON, PersonAttachmentDto.class);
			dto.getNewAttachments().forEach(ele -> {
				int count = 0;
				PersonAttachmentDto request = PersonAttachmentDto.builder()
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

	private Attachments addAttachments(MultipartFile file, PersonAttachmentDto request, String personId) {
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
	public List<PersonAttachmentDto> loadAllAttachmentsForPerson(String personId) {
		List<Attachments> attachments = conflictOfInterestDao.loadAllAttachmentsForPerson(personId);
		List<PersonAttachmentDto> attachmentsDto = new ArrayList<>();
	    attachments.forEach(attachment -> {
	        PersonAttachmentDto dto = PersonAttachmentDto.builder()
	            .attachmentId(attachment.getAttachmentId())
	            .personId(attachment.getPersonId())
	            .attaTypeCode(attachment.getAttaTypeCode())
	            .fileName(attachment.getFileName())
	            .mimeType(attachment.getMimeType())
	            .description(attachment.getDescription())
	            .createUser(attachment.getCreateUser())
	            .createTimestamp(attachment.getCreateTimestamp())
	            .updateUser(attachment.getUpdateUser())
	            .updateTimestamp(attachment.getUpdateTimestamp())
	            .attachmentNumber(attachment.getAttachmentNumber())
	            .versionNumber(attachment.getVersionNumber())
	            .updateUserFullame(personDao.getPersonFullNameByPersonId(attachment.getPersonId()))
	            .attachmentTypeDescription(attachment.getDisclAttaTypeDetails().getDescription())
	            .build();
	        attachmentsDto.add(dto);
	    });
	    return attachmentsDto;
	}


	@Override
	public ResponseEntity<Object> getEntityWithRelationShipInfo(CommonRequestDto requestDto) {
		requestDto.setId(AuthenticatedUser.getLoginPersonId());
		return new ResponseEntity<>(conflictOfInterestDao.getEntityWithRelationShipInfo(requestDto), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getSFIRelationshipDetails() {
		return new ResponseEntity<>(conflictOfInterestDao.getPersonEntities(null, AuthenticatedUser.getLoginPersonId(), true), HttpStatus.OK);
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
			CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(disclosureId);
			disclosure.setAdminPersonName(AuthenticatedUser.getLoginUserFullName());
			disclosure.setAdminGroupName(coiDisclosure.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(coiDisclosure.getAdminGroupId()).getAdminGroupName() : null);
			disclosure.setPersonAttachmentsCount(conflictOfInterestDao.personAttachmentsCount(AuthenticatedUser.getLoginPersonId()));
			disclosure.setPersonNotesCount(conflictOfInterestDao.personNotesCount(AuthenticatedUser.getLoginPersonId()));
			disclosure.setPersonEntitiesCount(conflictOfInterestDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(AuthenticatedUser.getLoginPersonId()).build()));
			if (disclosure.getFcoiTypeCode().equals("1") || disclosure.getFcoiTypeCode().equals("3")) {
				conflictOfInterestDao.archiveDisclosureOldVersions(disclosureId, disclosureNumber);
			}
			fcoiDisclosureDao.generateProjectSnapshot(disclosureId, disclosure.getPersonId());
			try {
				DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder()
						.actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_REVIEW_COMPLETED).disclosureId(disclosure.getDisclosureId())
						.disclosureNumber(disclosure.getDisclosureNumber()).fcoiTypeCode(disclosure.getFcoiTypeCode())
						.administratorName(AuthenticatedUser.getLoginUserFullName())
						.build();
				actionLogService.saveDisclosureActionLog(actionLogDto);
				Map<String, String> actionTypes = new HashMap<>();
				actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_COMPLETE);
				actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_COMPLETE);
				Map<String, String> additionalDetails = new HashMap<>();
				processCoiMessageToQ(getDisclosureActionType(disclosure.getFcoiTypeCode(), actionTypes), disclosure.getDisclosureId(), null, additionalDetails);
			} catch (Exception e) {
				logger.error("completeDisclosureReview : {}", e.getMessage());
			}
			return isBatch ? new ResponseEntity<>("Approved successfully", HttpStatus.OK) : new ResponseEntity<>(disclosure, HttpStatus.OK);
//			return new ResponseEntity<>(loadDisclosure(disclosureId), HttpStatus.OK);
		}
		return new ResponseEntity<>("REVIEW_STATUS_NOT_COMPLETE", HttpStatus.OK);
	}

//	@Override
//	public ResponseEntity<Object> checkDisclosureRiskStatus(CoiDisclosureDto disclosureDto) {
//		if (Boolean.TRUE.equals(conflictOfInterestDao.isDisclosureRiskStatusModified(disclosureDto.getRiskCategoryCode(), disclosureDto.getDisclosureId()))) {
//			return  new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
//		}
//		return new ResponseEntity<>(HttpStatus.OK);
//	}

	@Override
	public ResponseEntity<Object> checkEntityRiskStatus(CoiEntityDto entityDto) {
		if (Boolean.TRUE.equals(conflictOfInterestDao.isEntityRiskStatusModified(entityDto.getRiskCategoryCode(), entityDto.getEntityId()))) {
			return  new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> checkTravelDisclosureRiskStatus(CoiTravelDisclosureDto travelDisclosureDto) {
		if (Boolean.TRUE.equals(conflictOfInterestDao.isTravelDisclosureRiskStatusModified(travelDisclosureDto.getRiskCategoryCode(), travelDisclosureDto.getTravelDisclosureId()))) {
			return  new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}

    //Defining action type based on disclosure type code
	@Override
    public String getDisclosureActionType(String fcoiType, Map<String, String> actionTypes) {
        String actionType;
        if (fcoiType.equals(Constants.DISCLOSURE_TYPE_CODE_FCOI) ||
                fcoiType.equals(Constants.DISCLOSURE_TYPE_CODE_REVISION)) {
            actionType = actionTypes.get(FCOI_DISCLOSURE);
        } else {
            actionType = actionTypes.get(PROJECT_DISCLOSURE);;
        }
        return actionType;
    }

    //Setting up the basic details for publishing message to queue
	@Override
    public void processCoiMessageToQ(String actionType, Integer moduleItemKey, Integer moduleSubItemKey, Map<String, String> additionDetails) {
        MessageQVO messageQVO = new MessageQVO();
        messageQVO.setActionType(actionType);
        messageQVO.setModuleCode(Constants.COI_MODULE_CODE);
        messageQVO.setSubModuleCode(Constants.COI_SUBMODULE_CODE);
        messageQVO.setPublishedUserName(AuthenticatedUser.getLoginUserName());
        messageQVO.setPublishedTimestamp(commonDao.getCurrentTimestamp());
        messageQVO.setOrginalModuleItemKey(moduleItemKey);
        messageQVO.setSubModuleItemKey(moduleSubItemKey);
        messageQVO.setSourceExchange(messagingQueueProperties.getQueues().get("exchange"));
        messageQVO.setSourceQueueName(messagingQueueProperties.getQueues().get("coi"));
        messageQVO.setAdditionalDetails(additionDetails);
        messageQServiceRouter.getMessagingQueueServiceBean().publishMessageToQueue(messageQVO);
    }

	@Override
	public ResponseEntity<Object> projectPersonNotify(NotificationDto notificationDto) {
		Map<String, String> additionalDetails = new HashMap<>();
		CoiDisclosure disclosure = fcoiDisclosureDao.loadDisclosure(notificationDto.getDisclosureId());
		String recipientPersonIds = notificationDto.getRecipients().stream()
				.map(NotificationRecipient::getRecipientPersonId).map(String::valueOf).collect(Collectors.joining(","));
		additionalDetails.put(NOTIFICATION_RECIPIENTS, recipientPersonIds);
		List<DisclosureDetailDto> projectDetails = conflictOfInterestDao.getProjectsBasedOnParams(
				notificationDto.getProjectTypeCode(), null, null, notificationDto.getProjectId());
		DisclosureDetailDto disclProjectDetail = projectDetails.get(0);
		additionalDetails.put("notificationTypeId", notificationDto.getNotificationTypeId());
		additionalDetails.put("PROJECT_ID", disclProjectDetail.getModuleItemKey());
		additionalDetails.put("PROJECT_TITLE", disclProjectDetail.getTitle());
		additionalDetails.put("DISCLOSURE_STATUS", disclosure.getCoiReviewStatusType().getDescription());
		additionalDetails.put("REPORTER_NAME", personDao.getPersonFullNameByPersonId(disclosure.getPersonId()));
		processCoiMessageToQ(ActionTypes.PROJECT_NOTIFY, 0, null, additionalDetails);
		return new ResponseEntity<>("Notification send successfully", HttpStatus.OK);
	}

	@Override
	public Map<String, List<FileType>> fetchRequiredParams() {
		List<FileType> fileTypes = conflictOfInterestDao.getAllFileTypes();
		Map<String, List<FileType>> response = new HashMap<>();
	    response.put("fileTypes", fileTypes);
	    return response;
	}

}
