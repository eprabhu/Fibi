package com.polus.fibicomp.fcoiDisclosure.service;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import com.polus.core.constants.CoreConstants;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.questionnaire.dto.QuestionnaireDataBus;
import com.polus.core.questionnaire.service.QuestionnaireService;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.*;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;
import com.polus.fibicomp.constants.ActionTypes;
import com.polus.fibicomp.constants.StaticPlaceholders;
import com.polus.fibicomp.fcoiDisclosure.dto.ProjectEntityRequestDto;
import com.polus.fibicomp.fcoiDisclosure.dto.SFIJsonDetailsDto;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjectEntityRel;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjects;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclosure;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiRiskCategory;
import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fcoiDisclosure.dao.FcoiDisclosureDao;
import com.polus.fibicomp.reviewcomments.dao.ReviewCommentDao;
import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

@Transactional
@Service
public class FcoiDisclosureServiceImpl implements FcoiDisclosureService {

    protected static Logger logger = LogManager.getLogger(FcoiDisclosureServiceImpl.class.getName());

    private static final String DISPOSITION_STATUS_PENDING = "1";
    private static final String REVIEW_STATUS_PENDING = "1";
    private static final String PROJECT_DISCLOSURE_TYPE_CODE = "2";
    private static final String DISCLOSURE_REVIEW_IN_PROGRESS = "3";
    private static final String SUBMITTED_FOR_REVIEW = "2";
    private static final String REVIEW_STATUS_RETURNED = "5";
    private static final String RISK_CATEGORY_LOW = "3";
    private static final String RISK_CATEGORY_LOW_DESCRIPTION = "Low";
    private static final String DISPOSITION_STATUS_TYPE_CODE = "1";
    private static final String FCOI_DISCLOSURE = "FCOI_DISCLOSURE";
    private static final String PROJECT_DISCLOSURE = "PROJECT_DISCLOSURE";
    private static final String REVIEW_IN_PROGRESS = "Review in progress";

    @Autowired
    private FcoiDisclosureDao disclosureDao;

    @Autowired
    private ActionLogService actionLogService;

    @Autowired
    private CommonDao commonDao;

    @Autowired
    private ConflictOfInterestDao coiDao;

    @Autowired
    private PersonDao personDao;

    @Autowired
    private ReviewCommentDao reviewCommentDao;

    @Autowired
    private QuestionnaireService questionnaireService;

    @Autowired
    private ConflictOfInterestService coiService;

    @Override
    public ResponseEntity<Object> createDisclosure(CoiDisclosureDto vo) throws JsonProcessingException {
        Map<String, Object> validatedObject = disclosureDao.validateProjectDisclosure(vo.getPersonId(), vo.getModuleCode(), vo.getModuleItemKey());
        Integer disclosureId = !validatedObject.isEmpty() ?
                validatedObject.get("projectDisclosure") != null ?
                        (Integer) validatedObject.get("projectDisclosure") :
                        validatedObject.get("fcoiDisclosure") != null ?
                                (Integer) validatedObject.get("fcoiDisclosure") :
                                null :
                null;
        if (disclosureId != null && disclosureId != 0) {
            CoiDisclosure disclosure = disclosureDao.loadDisclosure(disclosureId);
            CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
            BeanUtils.copyProperties(disclosure, coiDisclosureDto);
            coiDisclosureDto.setHomeUnitName(disclosure.getUnit() != null ? disclosure.getUnit().getUnitName() : null);
            coiDisclosureDto.setReviewStatus(disclosure.getCoiReviewStatusType() != null ? disclosure.getCoiReviewStatusType().getDescription() : null);
            coiDisclosureDto.setDispositionStatus(disclosure.getCoiDispositionStatusType() != null ? disclosure.getCoiDispositionStatusType().getDescription() : null);
            coiDisclosureDto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
            coiDisclosureDto.setCreateUserFullName(personDao.getPersonFullNameByPersonId(disclosure.getCreatedBy()));
            coiDisclosureDto.setDisclosurePersonFullName(disclosure.getPerson().getFullName());
            return new ResponseEntity<>(coiDisclosureDto, HttpStatus.METHOD_NOT_ALLOWED);
        } else if (vo.getFcoiTypeCode().equals("3") && disclosureDao.isMasterDisclosurePresent(vo.getPersonId())) {
            return new ResponseEntity<>("Could not create master disclosure ", HttpStatus.METHOD_NOT_ALLOWED);

        }
        CoiDisclosure coiDisclosure = CoiDisclosure.builder()
                .fcoiTypeCode(vo.getFcoiTypeCode())
                .coiProjectTypeCode(vo.getCoiProjectTypeCode())
                .createdBy(vo.getPersonId())
                .personId(vo.getPersonId())
                .disclosureNumber(disclosureDao.generateMaxDisclosureNumber())
                .versionNumber(1).versionStatus(Constants.COI_PENDING_STATUS)
                .homeUnit(vo.getHomeUnit())
                .dispositionStatusCode(DISPOSITION_STATUS_PENDING)
                .reviewStatusCode(REVIEW_STATUS_PENDING)
                .updatedBy(AuthenticatedUser.getLoginPersonId())
                .build();
        disclosureDao.saveOrUpdateCoiDisclosure(coiDisclosure);
        List<CoiDisclProjects> disclosureProjects = new ArrayList<>();
        String loginPersonId = AuthenticatedUser.getLoginPersonId();
        if (vo.getFcoiTypeCode().equals(Constants.PROJECT_DISCL_FCOI_TYPE_CODE)) {
            CoiDisclProjects coiDisclProject = CoiDisclProjects.builder().
                    disclosureId(coiDisclosure.getDisclosureId())
                    .disclosureNumber(coiDisclosure.getDisclosureNumber())
                    .moduleCode(vo.getModuleCode())
                    .moduleItemKey(vo.getModuleItemKey())
                    .updateTimestamp(commonDao.getCurrentTimestamp())
                    .updatedBy(AuthenticatedUser.getLoginPersonId())
                    .build();
            disclosureDao.saveOrUpdateCoiDisclProjects(coiDisclProject);
            disclosureProjects.add(coiDisclProject);
        } else {
            disclosureProjects = disclosureDao.syncFcoiDisclosureProjects(coiDisclosure.getDisclosureId(),
                    coiDisclosure.getDisclosureNumber(), loginPersonId);
        }
        List<SFIJsonDetailsDto> sfiDetails = disclosureDao.getPersonEntitiesByPersonId(loginPersonId);

        if (disclosureProjects.isEmpty()) {
            throw new ApplicationException("No project(s) found/synced", Constants.JAVA_ERROR);
        }
        String sfiJsonString = convertListToJson(sfiDetails);

        ExecutorService executorService = Executors.newWorkStealingPool(4);
        disclosureProjects.forEach(disclosureProject ->
                executorService.submit(() -> {
                    logger.info("syncFcoiDisclProjectsAndEntities is executing for project id {} on thread {}", disclosureProject.getCoiDisclProjectId(), Thread.currentThread().getName());
                    disclosureDao.syncFcoiDisclProjectsAndEntities(coiDisclosure.getDisclosureId(),
                            coiDisclosure.getDisclosureNumber(), disclosureProject.getCoiDisclProjectId(),
                            disclosureProject.getModuleCode(), disclosureProject.getModuleItemKey(),
                            sfiJsonString, loginPersonId);
                }));

        vo.setDisclosureId(coiDisclosure.getDisclosureId());

        try {
//            DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_CREATED)
//                    .disclosureId(coiDisclosure.getDisclosureId()).disclosureNumber(coiDisclosure.getDisclosureNumber())
//                    .fcoiTypeCode(coiDisclosure.getFcoiTypeCode()).revisionComment(coiDisclosure.getRevisionComment())
//                    .reporter(AuthenticatedUser.getLoginUserFullName())
//                    .build();
//            actionLogService.saveDisclosureActionLog(actionLogDto);
        } catch (
                Exception e) {
            logger.error("createDisclosure : {}", e.getMessage());
        }
        return new ResponseEntity<>(vo, HttpStatus.OK);
    }

    public String convertListToJson(List<SFIJsonDetailsDto> dtoList) throws JsonProcessingException {
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.writeValueAsString(dtoList);
    }

    @Override
    public ResponseEntity<Object> loadDisclosure(Integer disclosureId) {
        CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
        ConflictOfInterestVO vo = new ConflictOfInterestVO();
        CoiDisclosure coiDisclosure = disclosureDao.loadDisclosure(disclosureId);
        BeanUtils.copyProperties(coiDisclosure, coiDisclosureDto, "countryOfCitizenshipDetails", "countryDetails", "currency");
        if (Objects.equals(coiDisclosure.getFcoiTypeCode(), PROJECT_DISCLOSURE_TYPE_CODE)) {
            List<DisclosureProjectDto> disclProjects = disclosureDao.getDisclosureProjects(disclosureId);
            vo.setProjectDetail(disclProjects.get(0));
        }
        coiDisclosureDto.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosure.getUpdatedBy()));
        coiDisclosureDto.setAdminGroupName(coiDisclosure.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(coiDisclosure.getAdminGroupId()).getAdminGroupName() : null);
        coiDisclosureDto.setAdminPersonName(personDao.getPersonFullNameByPersonId(coiDisclosure.getAdminPersonId()));
        coiDisclosureDto.setCoiSectionsTypes(disclosureDao.fetchCoiSections());
        coiDisclosureDto.setPersonEntitiesCount(coiDao.getSFIOfDisclosureCount(ConflictOfInterestVO.builder().personId(coiDisclosure.getPersonId()).build()));
        coiDisclosureDto.setPersonAttachmentsCount(coiDao.personAttachmentsCount(coiDisclosure.getPersonId()));
        coiDisclosureDto.setPersonNotesCount(coiDao.personNotesCount(coiDisclosure.getPersonId()));
        if (coiDisclosure.getSyncNeeded()) {
            disclosureDao.syncFCOIDisclosure(coiDisclosure.getDisclosureId(), coiDisclosure.getDisclosureNumber());
        }
        vo.setCoiDisclosure(coiDisclosureDto);
        return new ResponseEntity<>(vo, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> certifyDisclosure(CoiDisclosureDto coiDisclosureDto) {
        CoiDisclosure coiDisclosureObj = disclosureDao.loadDisclosure(coiDisclosureDto.getDisclosureId());
        if (coiDisclosureObj.getReviewStatusCode().equals(DISCLOSURE_REVIEW_IN_PROGRESS) || coiDisclosureObj.getReviewStatusCode().equals(SUBMITTED_FOR_REVIEW) || coiDisclosureObj.getReviewStatusCode().equals(Constants.COI_DISCLOSURE_REVIEWER_STATUS_COMPLETED) || coiDisclosureObj.getReviewStatusCode().equals(Constants.COI_DISCLOSURE_REVIEWER_STATUS_ASSIGNED)) {
            return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
        }
        setDisclosureReviewStatusCode(coiDisclosureDto, coiDisclosureObj);
        coiDisclosureDto.setDispositionStatusCode(DISPOSITION_STATUS_PENDING);
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.YEAR, 1);
        cal.add(Calendar.DAY_OF_MONTH, -1);
        coiDisclosureDto.setExpirationDate(cal.getTime());
        disclosureDao.certifyDisclosure(coiDisclosureDto);
        disclosureDao.validateConflicts(coiDisclosureDto.getDisclosureId());
        CoiRiskCategory riskCategory = null;
        boolean isReturned = true;
        if (coiDisclosureObj.getReviewStatusCode().equals(REVIEW_STATUS_PENDING)) {
            riskCategory = disclosureDao.syncDisclosureRisk(coiDisclosureObj.getDisclosureId(), coiDisclosureObj.getDisclosureNumber());
            isReturned = false;
        }
        if (riskCategory == null) {
            coiDisclosureDto.setRiskCategoryCode(RISK_CATEGORY_LOW);
            disclosureDao.updateDisclosureRiskCategory(coiDisclosureDto);
        }
        String reporterFullName = AuthenticatedUser.getLoginUserFullName();
        ExecutorService executorService = Executors.newSingleThreadExecutor();
        executorService.submit(() -> {
            try {
                saveConflictHistory(coiDisclosureDto.getDisclosureId(), reporterFullName);
            } catch (Exception e) {
                logger.error("Error in saveConflictHistory: {}", e.getMessage());
            }
        });
        executorService.shutdown();
        coiDisclosureDto.setCreateUserFullName(personDao.getPersonFullNameByPersonId(coiDisclosureObj.getCreatedBy()));
        coiDisclosureDto.setUpdateUserFullName(personDao.getPersonFullNameByPersonId(AuthenticatedUser.getLoginPersonId()));
        try {
            Map<String, String> actionTypes = new HashMap<>();
            if (isReturned) {
                actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_RESUBMIT);
                actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_RESUBMIT);
            } else {
                actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_SUBMIT);
                actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_SUBMIT);
            }
            Map<String, String> additionalDetails = new HashMap<>();
            additionalDetails.put(StaticPlaceholders.DISCLOSURE_STATUS, riskCategory != null ? riskCategory.getDescription() : RISK_CATEGORY_LOW_DESCRIPTION);
            coiService.processCoiMessageToQ(coiService.getDisclosureActionType(coiDisclosureObj.getFcoiTypeCode(), actionTypes), coiDisclosureObj.getDisclosureId(), null, additionalDetails);

            DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_SUBMITTED).disclosureId(coiDisclosureObj.getDisclosureId()).disclosureNumber(coiDisclosureObj.getDisclosureNumber()).riskCategory(riskCategory != null ? riskCategory.getDescription() : RISK_CATEGORY_LOW_DESCRIPTION).fcoiTypeCode(coiDisclosureObj.getFcoiTypeCode()).reporter(AuthenticatedUser.getLoginUserFullName()).build();
            actionLogService.saveDisclosureActionLog(actionLogDto);
        } catch (Exception e) {
            logger.error("certifyDisclosure : {}", e.getMessage());
        }
        return new ResponseEntity<>(coiDisclosureObj, HttpStatus.OK);
    }

    private void setDisclosureReviewStatusCode(CoiDisclosureDto coiDisclosure, CoiDisclosure coiDisclosureObj) {
        String reviewStatusCode = coiDisclosureObj.getReviewStatusCode();
        if (reviewStatusCode.equals(REVIEW_STATUS_RETURNED)) {
            if (Boolean.TRUE.equals(disclosureDao.isReviewerAssigned(coiDisclosureObj.getDisclosureId()))) {
                if (Boolean.TRUE.equals(disclosureDao.isReviewerReviewCompleted(coiDisclosureObj.getDisclosureId()))) {
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

    private void saveConflictHistory(Integer disclosureId, String reporterFullName) {
        List<CoiDisclProjectEntityRel> projEntityRelationships = disclosureDao.getProjEntityRelationshipsByDisclId(disclosureId);
        ConflictOfInterestVO vo = new ConflictOfInterestVO();
        vo.setDisclosureId(disclosureId);
        vo.setReporterFullName(reporterFullName);
        projEntityRelationships.forEach(projEntityRel -> {
            vo.setCoiDisclEntProjDetail(projEntityRel);
            saveOrUpdateCoiConflictHistory(vo, Constants.COI_DISCLOSURE_ACTION_LOG_ADD_CONFLICT_STATUS);
        });
    }

    private void saveOrUpdateCoiConflictHistory(ConflictOfInterestVO vo, String actionTypeCode) {
        CoiConflictHistory coiConflictHistory = new CoiConflictHistory();
        CoiDisclProjectEntityRel coiDisclEntProjDetails = vo.getCoiDisclEntProjDetail();
        String existingConflictStatus = disclosureDao.getLatestConflHisStatusCodeByProEntRelId(coiDisclEntProjDetails.getCoiDisclProjectEntityRelId());
        if (!coiDisclEntProjDetails.getProjectConflictStatusCode().equals(existingConflictStatus)) {
            DisclComment disclComment = getDisclProjectConflictComment(vo.getDisclosureId(), coiDisclEntProjDetails.getCoiDisclProjectEntityRelId());
            coiConflictHistory.setConflictStatusCode(coiDisclEntProjDetails.getProjectConflictStatusCode());
            coiConflictHistory.setComment(disclComment.getComment());
            coiConflictHistory.setDisclosureId(vo.getDisclosureId());
            coiConflictHistory.setCoiDisclProjectEntityRelId(coiDisclEntProjDetails.getCoiDisclProjectEntityRelId());
            coiConflictHistory.setUpdatedBy(coiDisclEntProjDetails.getUpdatedBy());
            coiConflictHistory.setUpdateTimestamp(coiDisclEntProjDetails.getUpdateTimestamp());
            DisclosureActionLogDto actionLogDto = new DisclosureActionLogDto();
//            List<CoiConflictHistory> coiConflictHistoryList = conflictOfInterestDao.getCoiConflictHistory(vo.getDisclosureDetailsId());
//            actionLogDto.setActionTypeCode(coiConflictHistoryList.isEmpty()
//                    ? Constants.COI_DISCLOSURE_ACTION_LOG_ADD_CONFLICT_STATUS
//                    : Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_CONFLICT_STATUS);
            actionLogDto.setActionTypeCode(actionTypeCode);
            actionLogDto.setNewConflictStatus(coiDisclEntProjDetails.getCoiProjConflictStatusType().getDescription());
            if (Constants.COI_DISCLOSURE_ACTION_LOG_ADD_CONFLICT_STATUS.equalsIgnoreCase(actionTypeCode)) {
                actionLogDto.setReporter(personDao.getUserFullNameByUserName(disclComment.getUpdateUser()));
            } else {
                actionLogDto.setConflictStatus(existingConflictStatus);
                actionLogDto.setAdministratorName(vo.getReporterFullName() == null ? AuthenticatedUser.getLoginUserFullName() : vo.getReporterFullName());
            }
            coiConflictHistory.setMessage(actionLogService.getFormattedMessageByActionType(actionLogDto));
            disclosureDao.saveOrUpdateCoiConflictHistory(coiConflictHistory);
        }
    }

    private DisclComment getDisclProjectConflictComment(Integer moduleItemKey, Integer submoduleItemKey) {
        List<DisclComment> reviewComment = reviewCommentDao.fetchReviewComments(ReviewCommentsDto.builder().componentTypeCode(Constants.COI_DISCL_CONFLICT_RELATION_COMPONENT_TYPE).moduleCode(Constants.COI_MODULE_CODE).subModuleItemKey(submoduleItemKey).moduleItemKey(moduleItemKey).build());
        return reviewComment != null && !reviewComment.isEmpty() ? reviewComment.get(0) : null;
    }

    @Override
    public ResponseEntity<Object> modifyDisclosureRisk(CoiDisclosureDto disclosureDto) {
        if (disclosureDao.isDisclosureRiskAdded(disclosureDto)) {
            return new ResponseEntity<>("Risk is already updated", HttpStatus.METHOD_NOT_ALLOWED);
        }
        CoiDisclosure disclosure = disclosureDao.loadDisclosure(disclosureDto.getDisclosureId());
        CoiRiskCategory risk = disclosureDao.getRiskCategoryStatusByCode(disclosureDto.getRiskCategoryCode());
        disclosureDto.setUpdateTimestamp(disclosureDao.updateDisclosureRiskCategory(disclosureDto));
        DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().disclosureId(disclosure.getDisclosureId()).disclosureNumber(disclosure.getDisclosureNumber()).riskCategory(disclosure.getCoiRiskCategory().getDescription()).riskCategoryCode(disclosure.getRiskCategoryCode()).newRiskCategory(risk.getDescription()).newRiskCategoryCode(risk.getRiskCategoryCode()).actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_RISK).administratorName(AuthenticatedUser.getLoginUserFullName()).fcoiTypeCode(disclosure.getFcoiTypeCode()).revisionComment(disclosureDto.getRevisionComment()).build();
        actionLogService.saveDisclosureActionLog(actionLogDto);
        return new ResponseEntity<>(disclosureDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> fetchAllDisclosureRisk() {
        return new ResponseEntity<>(disclosureDao.fetchDisclosureRiskCategory(), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> fetchDisclosureHistory(DisclosureActionLogDto actionLogDto) {
        actionLogDto.setActionTypeCodes(Arrays.asList(Constants.COI_DISCLOSURE_ACTION_LOG_ADD_RISK, Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_RISK));
        return new ResponseEntity<>(actionLogService.fetchDisclosureActionLog(actionLogDto), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> checkDisclosureRiskStatus(CoiDisclosureDto disclosureDto) {
        if (Boolean.TRUE.equals(disclosureDao.isDisclosureRiskStatusModified(disclosureDto.getRiskCategoryCode(), disclosureDto.getDisclosureId()))) {
            return new ResponseEntity<>(HttpStatus.METHOD_NOT_ALLOWED);
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getDisclosureProjects(Integer disclosureId) {
//        conflictOfInterestDao.syncProjectWithDisclosure(disclosureId,
//                null, null, null, null, Constants.TYPE_SYNC_SFI_WITH_DISCLOSURE_PROJECTS);
        return new ResponseEntity<>(disclosureDao.getDisclosureProjects(disclosureId), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getDisclosureLookups() {
        ConflictOfInterestVO vo = new ConflictOfInterestVO();
        vo.setCoiConflictStatusTypes(disclosureDao.getCoiConflictStatusTypes());
        vo.setCoiProjConflictStatusTypes(disclosureDao.getProjConflictStatusTypes());
        return new ResponseEntity<>(vo, HttpStatus.OK);
    }

    @Override
    public List<DisclosureProjectDto> getDisclProjectEntityRelations(ProjectEntityRequestDto vo) {
        if (disclosureDao.isProjectSFISyncNeeded(vo.getDisclosureId())) {
            disclosureDao.syncFCOIDisclosure(vo.getDisclosureId(), vo.getDisclosureNumber());
        }
        CompletableFuture<List<CoiDisclEntProjDetailsDto>> disclosureDetailsFuture =
                CompletableFuture.supplyAsync(() -> disclosureDao.getDisclEntProjDetails(vo.getDisclosureId()));
        CompletableFuture<List<DisclosureProjectDto>> projectsFuture =
                CompletableFuture.supplyAsync(() -> disclosureDao.getDisclosureProjects(vo.getDisclosureId()));
        CompletableFuture<List<PersonEntityRelationshipDto>> personEntityRelationshipFuture =
                CompletableFuture.supplyAsync(() -> coiDao.getPersonEntities(vo.getDisclosureId(), vo.getPersonId(), null));
        CompletableFuture<Void> allOf = CompletableFuture.allOf(
                disclosureDetailsFuture, projectsFuture, personEntityRelationshipFuture
        );
        List<DisclosureProjectDto> disclProjects;
        try {
            allOf.get();
            List<CoiDisclEntProjDetailsDto> disclProjEntRelations = disclosureDetailsFuture.get();
            disclProjects = projectsFuture.get();
            List<PersonEntityRelationshipDto> disclPersonEntities = personEntityRelationshipFuture.get();
            for (CoiDisclEntProjDetailsDto projEntityDetail : disclProjEntRelations) {
                Optional<PersonEntityRelationshipDto> matchingProject = disclPersonEntities.stream()
                        .filter(personEntity -> personEntity.getPersonEntityId().equals(projEntityDetail.getPersonEntityId()))
                        .findFirst();
                matchingProject.ifPresent(projEntityDetail::setPersonEntity);
            }
            Map<Integer, List<CoiDisclEntProjDetailsDto>> disclEntityRelations = disclProjEntRelations.stream()
                    .collect(Collectors.groupingBy(CoiDisclEntProjDetailsDto::getCoiDisclProjectId));
            disclProjects.parallelStream().forEach(disclosureProject -> {
                List<CoiDisclEntProjDetailsDto> disclEntProjDetails = disclEntityRelations.get(disclosureProject.getCoiDisclProjectId());
                Map<Integer, Long> conflictCount = getRelationConflictCount(disclEntProjDetails);
                if (!conflictCount.containsKey(0)) {
                    disclosureProject.setConflictCompleted(true);
                }
                conflictCount.remove(0);
                disclosureProject.setConflictCount(conflictCount);
                disclosureProject.setCoiDisclEntProjDetails(disclEntProjDetails);
            });
        } catch (Exception e) {
            e.printStackTrace();
            throw new ApplicationException("Unable to fetch data", e, CoreConstants.JAVA_ERROR);
        }
        return disclProjects;
    }

    @Override
    public List<PersonEntityRelationshipDto> getDisclosureEntityRelations(ProjectEntityRequestDto vo) {
        CompletableFuture<List<CoiDisclEntProjDetailsDto>> disclosureDetailsFuture =
                CompletableFuture.supplyAsync(() -> disclosureDao.getDisclEntProjDetails(vo.getDisclosureId()));
        CompletableFuture<List<DisclosureProjectDto>> projectsFuture =
                CompletableFuture.supplyAsync(() -> disclosureDao.getDisclosureProjects(vo.getDisclosureId()));
        CompletableFuture<List<PersonEntityRelationshipDto>> personEntityRelationshipFuture =
                CompletableFuture.supplyAsync(() -> coiDao.getPersonEntities(vo.getDisclosureId(), vo.getPersonId(), null));
        CompletableFuture<Void> allOf = CompletableFuture.allOf(
                disclosureDetailsFuture, projectsFuture, personEntityRelationshipFuture
        );
        List<PersonEntityRelationshipDto> disclPersonEntities;
        try {
            allOf.get();
            List<CoiDisclEntProjDetailsDto> disclProjEntRelations = disclosureDetailsFuture.get();
            List<DisclosureProjectDto> disclProjects = projectsFuture.get();
            disclPersonEntities = personEntityRelationshipFuture.get();
            for (CoiDisclEntProjDetailsDto projEntityDetail : disclProjEntRelations) {
                Optional<DisclosureProjectDto> matchingProject = disclProjects.stream()
                        .filter(project -> project.getCoiDisclProjectId().equals(projEntityDetail.getCoiDisclProjectId()))
                        .findFirst();
                matchingProject.ifPresent(projEntityDetail::setProject);
            }
            Map<Integer, List<CoiDisclEntProjDetailsDto>> disclEntityRelations = disclProjEntRelations.stream()
                    .collect(Collectors.groupingBy(CoiDisclEntProjDetailsDto::getPersonEntityId));
            disclPersonEntities.parallelStream().forEach(personEntityRelationshipDto -> {
                List<CoiDisclEntProjDetailsDto> disclEntProjDetails = disclEntityRelations.get(personEntityRelationshipDto.getPersonEntityId());

                Map<Integer, Long> conflictCount = getRelationConflictCount(disclEntProjDetails);
                if (!conflictCount.containsKey(0)) {
                    personEntityRelationshipDto.setConflictCompleted(true);
                }
                conflictCount.remove(0);
                personEntityRelationshipDto.setConflictCount(conflictCount);
                personEntityRelationshipDto.setProjEntRelations(disclEntProjDetails);
            });
        } catch (Exception e) {
            throw new ApplicationException("Unable to fetch data", e, CoreConstants.JAVA_ERROR);
        }
        return disclPersonEntities;
    }

    private static Map<Integer, Long> getRelationConflictCount(List<CoiDisclEntProjDetailsDto> disclEntProjDetails) {
        if (disclEntProjDetails == null) {
            return Collections.EMPTY_MAP;
        }
        Map<Integer, Long> conflictCount = disclEntProjDetails.stream().collect(Collectors.groupingBy(projectConflictStatus -> {
            if (projectConflictStatus.getProjectConflictStatusCode() != null
                    && Integer.parseInt(projectConflictStatus.getProjectConflictStatusCode()) >= 100
                    && Integer.parseInt(projectConflictStatus.getProjectConflictStatusCode()) < 200) {
                return 1;
            } else if (projectConflictStatus.getProjectConflictStatusCode() != null
                    && Integer.parseInt(projectConflictStatus.getProjectConflictStatusCode()) >= 200
                    && Integer.parseInt(projectConflictStatus.getProjectConflictStatusCode()) < 300) {
                return 2;
            } else if (projectConflictStatus.getProjectConflictStatusCode() != null
                    && Integer.parseInt(projectConflictStatus.getProjectConflictStatusCode()) >= 300
                    && Integer.parseInt(projectConflictStatus.getProjectConflictStatusCode()) < 400) {
                return 3;
            } else return 0;
        }, Collectors.counting()));
        return conflictCount;
    }

    @Override
    public ResponseEntity<Object> saveDisclosureConflict(ProjectEntityRequestDto vo) {
        disclosureDao.saveOrUpdateCoiDisclEntProjDetails(vo);
        List<ProjectEntityRequestDto> projectEntityRequestDtos = new ArrayList<>();
        if (vo.getApplyAll()) {
            disclosureDao.fetchDisclProjectEntityRelIds(vo).forEach(disclProjectEntityRelId -> {
                vo.setCoiDisclProjectEntityRelId(disclProjectEntityRelId);
                vo.setCommentId(saveDisclProjRelationComment(vo).getCommentId());
                projectEntityRequestDtos.add(vo);
            });

        } else {
            vo.setCommentId(saveDisclProjRelationComment(vo).getCommentId());
            projectEntityRequestDtos.add(vo);
        }
        return new ResponseEntity<>(projectEntityRequestDtos, HttpStatus.OK);
    }

    private DisclComment saveDisclProjRelationComment(ProjectEntityRequestDto entityProjectRelation) {
        DisclComment disclComment = new DisclComment();
        disclComment.setCommentId(entityProjectRelation.getCommentId());
        disclComment.setComment(entityProjectRelation.getComment());
        disclComment.setComponentTypeCode(Constants.COI_DISCL_CONFLICT_RELATION_COMPONENT_TYPE);        //Disclosure detail comment
//		disclComment.setCommentTypeCode(Constants.COI_DISCL_CONFLICT_RELATION_COMMENT_TYPE);		//TODO Disclosure detail comment
        disclComment.setCommentPersonId(AuthenticatedUser.getLoginPersonId());
        disclComment.setDocumentOwnerPersonId(AuthenticatedUser.getLoginPersonId());
        disclComment.setIsPrivate(false);
        disclComment.setModuleItemKey(entityProjectRelation.getDisclosureId());
        disclComment.setModuleItemNumber(String.valueOf(entityProjectRelation.getDisclosureNumber()));
        disclComment.setSubModuleItemKey(entityProjectRelation.getCoiDisclProjectEntityRelId());
        disclComment.setModuleCode(Constants.COI_MODULE_CODE);
        disclComment.setUpdateUser(AuthenticatedUser.getLoginUserName());
        disclComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        disclosureDao.saveOrUpdateDisclComment(disclComment);
        return disclComment;
    }

    @Override
    public ResponseEntity<Object> reviseDisclosure(ConflictOfInterestVO vo) {
        CoiDisclosure fcoiDisclosure = disclosureDao.isFCOIDisclosureExists(AuthenticatedUser.getLoginPersonId(), "1", Constants.COI_PENDING_STATUS);
        if (fcoiDisclosure != null) {
            CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
            BeanUtils.copyProperties(fcoiDisclosure, coiDisclosureDto);
            coiDisclosureDto.setHomeUnitName(fcoiDisclosure.getUnit() != null ? fcoiDisclosure.getUnit().getUnitName() : null);
            coiDisclosureDto.setReviewStatus(fcoiDisclosure.getCoiReviewStatusType() != null ? fcoiDisclosure.getCoiReviewStatusType().getDescription() : null);
            coiDisclosureDto.setDispositionStatus(fcoiDisclosure.getCoiDispositionStatusType() != null ? fcoiDisclosure.getCoiDispositionStatusType().getDescription() : null);
            coiDisclosureDto.setConflictStatus(fcoiDisclosure.getCoiConflictStatusType() != null ? fcoiDisclosure.getCoiConflictStatusType().getDescription() : null);
            coiDisclosureDto.setCreateUserFullName(personDao.getPersonFullNameByPersonId(fcoiDisclosure.getCreatedBy()));
            coiDisclosureDto.setDisclosurePersonFullName(fcoiDisclosure.getPerson().getFullName());
            return new ResponseEntity<>(coiDisclosureDto, HttpStatus.METHOD_NOT_ALLOWED);
        }
        CoiDisclosure disclosure = disclosureDao.loadDisclosure(vo.getDisclosureId());
        if (!disclosure.getReviewStatusCode().equals("4")) {  // review status code 4 -> completed
            throw new ApplicationException("You are attempting to revise a pending version of disclosure. You can only have one revision at a time.", Constants.JAVA_ERROR);
        }
        CoiDisclosure copyDisclosure = new CoiDisclosure();
        copyDisclosure.setRevisionComment(vo.getRevisionComment());
        copyDisclosure.setHomeUnit(vo.getHomeUnit());
        copyDisclosure(disclosure, copyDisclosure);
        vo.setDisclosureId(copyDisclosure.getDisclosureId());
        copyDisclosureQuestionnaireData(disclosure, copyDisclosure);
        List<CoiDisclProjects> disclosureProjects = disclosureDao.syncFcoiDisclosureProjects(copyDisclosure.getDisclosureId(),
                copyDisclosure.getDisclosureNumber(), copyDisclosure.getPersonId());
        List<SFIJsonDetailsDto> sfiDetails =
                disclosureDao.getPersonEntitiesByPersonId(copyDisclosure.getPersonId());

        if (disclosureProjects.isEmpty()) {
            throw new ApplicationException("No project(s) found/synced", Constants.JAVA_ERROR);
        }
        try {
            String sfiJsonString = convertListToJson(sfiDetails);
            ExecutorService executorService = Executors.newWorkStealingPool(4);
            disclosureProjects.forEach(disclosureProject ->
                    executorService.submit(() -> {
                        logger.info("syncFcoiDisclProjectsAndEntities is executing for project id {} on thread {}", disclosureProject.getCoiDisclProjectId(), Thread.currentThread().getName());
                        disclosureDao.syncFcoiDisclProjectsAndEntities(copyDisclosure.getDisclosureId(),
                                copyDisclosure.getDisclosureNumber(), disclosureProject.getCoiDisclProjectId(),
                                disclosureProject.getModuleCode(), disclosureProject.getModuleItemKey(),
                                sfiJsonString, copyDisclosure.getPersonId());
                    }));
        } catch (Exception e) {
            logger.info("Unable to sync SFIs : {}", e.getMessage());
        }
        DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_REVISED).disclosureId(copyDisclosure.getDisclosureId()).disclosureNumber(copyDisclosure.getDisclosureNumber()).fcoiTypeCode(copyDisclosure.getFcoiTypeCode()).revisionComment(copyDisclosure.getRevisionComment()).reporter(AuthenticatedUser.getLoginUserFullName()).build();
        actionLogService.saveDisclosureActionLog(actionLogDto);
        return new ResponseEntity<>(vo, HttpStatus.OK);
    }

    private CoiDisclosure copyDisclosure(CoiDisclosure disclosure, CoiDisclosure copyDisclosure) {
        copyDisclosure.setFcoiTypeCode(disclosure.getFcoiTypeCode());
        copyDisclosure.setDispositionStatusCode(DISPOSITION_STATUS_TYPE_CODE);
        copyDisclosure.setReviewStatusCode(REVIEW_STATUS_PENDING);
        copyDisclosure.setVersionStatus(Constants.COI_PENDING_STATUS);
        copyDisclosure.setVersionNumber(disclosure.getVersionNumber() + 1);
        copyDisclosure.setPersonId(AuthenticatedUser.getLoginPersonId());
        copyDisclosure.setDisclosureNumber(disclosure.getDisclosureNumber());
        copyDisclosure.setCreatedBy(AuthenticatedUser.getLoginUserName());
        copyDisclosure.setUpdatedBy(AuthenticatedUser.getLoginUserName());
        return disclosureDao.saveOrUpdateCoiDisclosure(copyDisclosure);
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
        questionnaireService.copyQuestionnaireForVersion(questionnaireDataBus, false);
    }

    @Override
    public boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo) {
        return disclosureDao.evaluateDisclosureQuestionnaire(vo.getModuleCode(), vo.getSubmoduleCode(), vo.getModuleItemId());
    }

    @Override
    public ResponseEntity<Object> updateProjectRelationship(ConflictOfInterestVO vo) {
        if (disclosureDao.isDisclEntProjConflictAdded(vo.getConflictStatusCode(), vo.getCoiDisclProjectEntityRelId())) {
            return new ResponseEntity<>("Conflict already updated", HttpStatus.METHOD_NOT_ALLOWED);
        }
        ProjectRelationshipResponseDto projectRelationshipResponseDto = new ProjectRelationshipResponseDto();
        saveOrUpdateDisclComment(vo);
        disclosureDao.updateCoiDisclEntProjDetails(vo.getConflictStatusCode(), vo.getCoiDisclProjectEntityRelId());
        disclosureDao.updateDisclosureUpdateDetails(vo.getDisclosureId());
        vo.setCoiDisclEntProjDetail(disclosureDao.getCoiDisclProjectEntityRelById(vo.getCoiDisclProjectEntityRelId()));
        saveOrUpdateCoiConflictHistory(vo, Constants.COI_DISCLOSURE_ACTION_LOG_MODIFY_CONFLICT_STATUS);
        projectRelationshipResponseDto.setCoiConflictHistoryList(coiService.getCoiConflictHistory(vo.getCoiDisclProjectEntityRelId()));
        projectRelationshipResponseDto.setCoiConflictStatusTypeDto(disclosureDao.validateConflicts(vo.getDisclosureId()));
        return new ResponseEntity<>(projectRelationshipResponseDto, HttpStatus.OK);
    }

    private void saveOrUpdateDisclComment(ConflictOfInterestVO vo) {
        DisclComment disclComment = getDisclProjectConflictComment(vo.getDisclosureId(), vo.getCoiDisclProjectEntityRelId());
        disclComment.setComment(vo.getComment());
        reviewCommentDao.saveObject(disclComment);
    }

    @Override
    public ResponseEntity<Object> validateConflicts(Integer disclosureId) {
        CoiConflictStatusTypeDto statusCode = disclosureDao.validateConflicts(disclosureId);
        return new ResponseEntity<>(statusCode, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> validateDisclosure(Integer moduleCode, String moduleItemId) {
        Map<String, Object> validatedObject = disclosureDao.validateProjectDisclosure(AuthenticatedUser.getLoginPersonId(), moduleCode, moduleItemId);
        CoiDisclosureDto coiDisclosureDto = new CoiDisclosureDto();
        if (validatedObject.get("projectDisclosure") != null) {
            CoiDisclosure disclosure = disclosureDao.loadDisclosure((Integer) validatedObject.get("projectDisclosure"));
            BeanUtils.copyProperties(disclosure, coiDisclosureDto);
            coiDisclosureDto.setHomeUnitName(disclosure.getUnit() != null ? disclosure.getUnit().getUnitName() : null);
            coiDisclosureDto.setReviewStatus(disclosure.getCoiReviewStatusType() != null ? disclosure.getCoiReviewStatusType().getDescription() : null);
            coiDisclosureDto.setDispositionStatus(disclosure.getCoiDispositionStatusType() != null ? disclosure.getCoiDispositionStatusType().getDescription() : null);
            coiDisclosureDto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
            coiDisclosureDto.setCreateUserFullName(personDao.getPersonFullNameByPersonId(disclosure.getCreatedBy()));
            coiDisclosureDto.setDisclosurePersonFullName(personDao.getPersonFullNameByPersonId(disclosure.getPersonId()));
            validatedObject.replace("projectDisclosure", coiDisclosureDto);
        }
        if (validatedObject.get("fcoiDisclosure") != null) {
            CoiDisclosure disclosure = disclosureDao.loadDisclosure((Integer) validatedObject.get("fcoiDisclosure"));
            BeanUtils.copyProperties(disclosure, coiDisclosureDto);
            coiDisclosureDto.setHomeUnitName(disclosure.getUnit() != null ? disclosure.getUnit().getUnitName() : null);
            coiDisclosureDto.setReviewStatus(disclosure.getCoiReviewStatusType() != null ? disclosure.getCoiReviewStatusType().getDescription() : null);
            coiDisclosureDto.setDispositionStatus(disclosure.getCoiDispositionStatusType() != null ? disclosure.getCoiDispositionStatusType().getDescription() : null);
            coiDisclosureDto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
            coiDisclosureDto.setCreateUserFullName(personDao.getPersonFullNameByPersonId(disclosure.getCreatedBy()));
            coiDisclosureDto.setDisclosurePersonFullName(personDao.getPersonFullNameByPersonId(disclosure.getPersonId()));
            validatedObject.replace("fcoiDisclosure", coiDisclosureDto);
        }
        return new ResponseEntity<>(validatedObject, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> assignDisclosureAdmin(CoiDisclosureDto dto) {
        if ((dto.getActionType().equals("R") && (disclosureDao.isSameAdminPersonOrGroupAdded(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getDisclosureId())))
                || (dto.getActionType().equals("A") && disclosureDao.isAdminPersonOrGroupAdded(dto.getDisclosureId()))) {
            return new ResponseEntity<>("Admin already assigned", HttpStatus.METHOD_NOT_ALLOWED);
        }
        CoiDisclosure disclosure = disclosureDao.loadDisclosure(dto.getDisclosureId());
        if ((dto.getActionType().equals("R"))
                && (disclosure.getReviewStatusCode().equals(Constants.COI_DISCLOSURE_STATUS_RETURN) || disclosure.getReviewStatusCode().equals(Constants.COI_DISCLOSURE_STATUS_COMPLETED))) {
            return new ResponseEntity<>("Reassign admin not allowed", HttpStatus.METHOD_NOT_ALLOWED);
        }
        if (dto.getActionType().equals("A") && !disclosure.getReviewStatusCode().equals(Constants.COI_DISCLOSURE_STATUS_SUBMITTED)) {
            return new ResponseEntity<>("Assign admin not allowed", HttpStatus.METHOD_NOT_ALLOWED);
        }
        try {
            saveAssignAdminActionLog(dto.getAdminPersonId(), dto.getDisclosureId(), disclosure.getDisclosureNumber(), disclosure.getAdminPersonId());
        } catch (Exception e) {
            logger.error("assignDisclosureAdmin : {}", e.getMessage());
        }
        dto.setUpdateTimestamp(disclosureDao.assignDisclosureAdmin(dto.getAdminGroupId(), dto.getAdminPersonId(), dto.getDisclosureId()));
        if (disclosure.getReviewStatusCode().equalsIgnoreCase(SUBMITTED_FOR_REVIEW)) {
            coiDao.updateReviewStatus(dto.getDisclosureId(), DISCLOSURE_REVIEW_IN_PROGRESS);
            dto.setReviewStatusCode(DISCLOSURE_REVIEW_IN_PROGRESS);
            dto.setReviewStatus(REVIEW_IN_PROGRESS);
        } else {
            dto.setReviewStatusCode(disclosure.getReviewStatusCode());
            dto.setReviewStatus(disclosure.getCoiReviewStatusType().getDescription());
        }
        Map<String, String> actionTypes = new HashMap<>();
        Map<String, String> additionalDetails = new HashMap<>();
        if (dto.getActionType().equals("A")) {
            actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_ASSIGN_ADMIN);
            actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_ASSIGN_ADMIN);
        } else {
            actionTypes.put(FCOI_DISCLOSURE, ActionTypes.FCOI_REASSIGN_ADMIN);
            actionTypes.put(PROJECT_DISCLOSURE, ActionTypes.PROJECT_REASSIGN_ADMIN);
            additionalDetails.put(StaticPlaceholders.NOTIFICATION_RECIPIENTS, disclosure.getAdminPersonId());
            additionalDetails.put(StaticPlaceholders.ADMINISTRATOR_NAME, personDao.getPersonFullNameByPersonId(disclosure.getAdminPersonId()));
        }
        additionalDetails.put(StaticPlaceholders.ADMIN_ASSIGNED_BY, personDao.getPersonFullNameByPersonId(AuthenticatedUser.getLoginPersonId()));
        additionalDetails.put(StaticPlaceholders.ADMIN_ASSIGNED_TO, personDao.getPersonFullNameByPersonId(dto.getAdminPersonId()));
        additionalDetails.put(StaticPlaceholders.CERTIFICATION_DATE, disclosure.getCertifiedAt().toString());
        additionalDetails.put(StaticPlaceholders.DISCLOSURE_STATUS, disclosure.getConflictStatusCode() != null ?
                disclosure.getCoiConflictStatusType().getDescription() : RISK_CATEGORY_LOW_DESCRIPTION);
        coiService.processCoiMessageToQ(coiService.getDisclosureActionType(disclosure.getFcoiTypeCode(), actionTypes), disclosure.getDisclosureId(), null, additionalDetails);

        dto.setAdminGroupName(dto.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(dto.getAdminGroupId()).getAdminGroupName() : null);
        dto.setAdminPersonName(personDao.getPersonFullNameByPersonId(dto.getAdminPersonId()));
        dto.setConflictStatus(disclosure.getCoiConflictStatusType() != null ? disclosure.getCoiConflictStatusType().getDescription() : null);
        dto.setConflictStatusCode(disclosure.getConflictStatusCode());
        dto.setDispositionStatusCode(disclosure.getDispositionStatusCode());
        dto.setDispositionStatus(disclosure.getCoiDispositionStatusType().getDescription());
        return new ResponseEntity<>(dto, HttpStatus.OK);
    }

    public void saveAssignAdminActionLog(String adminPersonId, Integer disclosureId, Integer disclosureNumber, String oldAdminPersonId) {

        String oldAdminPerson = oldAdminPersonId != null ? personDao.getPersonFullNameByPersonId(oldAdminPersonId) : null;
        String newAdminPerson = personDao.getPersonFullNameByPersonId(adminPersonId);
        if (oldAdminPerson != null) {
            DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_REASSIGN_ADMIN)
                    .disclosureId(disclosureId)
                    .disclosureNumber(disclosureNumber)
                    .oldAdmin(oldAdminPerson)
                    .coiAdmin(AuthenticatedUser.getLoginUserFullName())
                    .newAdmin(newAdminPerson).build();
            actionLogService.saveDisclosureActionLog(actionLogDto);
        } else {
            DisclosureActionLogDto actionLogDto = DisclosureActionLogDto.builder().actionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_ASSIGN_ADMIN)
                    .disclosureId(disclosureId)
                    .disclosureNumber(disclosureNumber)
                    .coiAdmin(AuthenticatedUser.getLoginUserFullName())
                    .newAdmin(newAdminPerson).build();
            actionLogService.saveDisclosureActionLog(actionLogDto);
        }
    }

    @Override
    public void syncFCOIDisclosure(CoiDisclosureDto coiDisclosureDto) {
        disclosureDao.syncFCOIDisclosure(coiDisclosureDto.getDisclosureId(),
                coiDisclosureDto.getDisclosureNumber());
    }

    @Override
    public ResponseEntity<Object> evaluateValidation(Integer disclosureId, Integer disclosureNumber) {
        disclosureDao.syncFCOIDisclosure(disclosureId, disclosureNumber);
        List<COIValidateDto> coiValidateDtoList = disclosureDao.evaluateValidation(disclosureId, AuthenticatedUser.getLoginPersonId());
        return new ResponseEntity<>(coiValidateDtoList, HttpStatus.OK);
    }

    @Override
    public void updateFcoiDisclSyncNeedStatus(DisclosureProjectDto projectDto) {
        disclosureDao.updateFcoiDisclSyncNeedStatus(projectDto);
    }

}
