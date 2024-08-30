package com.polus.fibicomp.coi.service;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.questionnaire.dto.QuestionnaireDataBus;
import com.polus.core.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.PersonEntityRelationshipDto;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.pojo.ValidPersonEntityRelType;
import com.polus.fibicomp.coi.repository.ActionLogDao;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fcoiDisclosure.dao.FcoiDisclosureDao;
import com.polus.core.security.AuthenticatedUser;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@Transactional
public class PersonEntityServiceImpl implements PersonEntityService {

    @Autowired
    @Qualifier(value = "conflictOfInterestDao")
    private ConflictOfInterestDao conflictOfInterestDao;

    @Autowired
    private ActionLogService actionLogService;

    @Autowired
    private PersonDao personDao;

    @Autowired
    private CommonDao commonDao;

    @Autowired
    private QuestionnaireService questionnaireService;

    @Autowired
    private ActionLogDao actionLogDao;

    @Autowired
    private FcoiDisclosureDao fcoiDisclosureDao;

    private static final String IS_FORM_COMPLETED = "isFormCompleted";

    @Override
    public ResponseEntity<Object> createPersonEntity(PersonEntity personEntity) {
        String loginUserName = AuthenticatedUser.getLoginUserName();
        String loginPersonId = AuthenticatedUser.getLoginPersonId();
        List<PersonEntity> persEntityObj = conflictOfInterestDao.fetchPersonEntityByEntityNum(personEntity.getEntityNumber(), loginPersonId);
        if (persEntityObj != null && !persEntityObj.isEmpty()) {
            return new ResponseEntity<>(persEntityObj.get(0), HttpStatus.METHOD_NOT_ALLOWED);
        }
        personEntity.setVersionNumber(Constants.COI_INITIAL_VERSION_NUMBER);
        personEntity.setPersonEntityNumber(conflictOfInterestDao.getMaxPersonEntityNumber() + 1);
        personEntity.setVersionStatus(Constants.COI_ACTIVE_STATUS); //By default SFI will be in ACTIVE status
        personEntity.setPersonId(AuthenticatedUser.getLoginPersonId());
        personEntity.setUpdateUser(loginUserName);
        personEntity.setCreateUser(loginUserName);
        conflictOfInterestDao.saveOrUpdatePersonEntity(personEntity);
        for (Integer relTypeCode : personEntity.getValidPersonEntityRelTypeCodes()) {
            PersonEntityRelationship personEntityRelation = new PersonEntityRelationship();
            personEntityRelation.setPersonEntityId(personEntity.getPersonEntityId());
            personEntityRelation.setValidPersonEntityRelTypeCode(relTypeCode);
            personEntityRelation.setUpdateUser(loginUserName);
            conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(personEntityRelation);
        }
        ResponseEntity<Map<String, Object>> responseData = updatePersonEntityCompleteFlag(personEntity.getPersonEntityId());
        personEntity.setIsFormCompleted((Boolean) responseData.getBody().get(IS_FORM_COMPLETED));
        fcoiDisclosureDao.updateDisclosureSyncNeededByPerEntId(personEntity.getPersonEntityId(), true);
        PersonEntityDto personEntityDto = new PersonEntityDto();
        personEntityDto.setPersonEntityId(personEntity.getPersonEntityId());
        personEntityDto.setPersonEntityNumber(personEntity.getPersonEntityNumber());
        personEntityDto.setEntityName(conflictOfInterestDao.getEntityDetails(personEntity.getEntityId()).getEntityName());
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
        PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityRelationship.getPersonEntityId());
        Integer personEntityId = personEntity.getPersonEntityId();
        Integer versionId = conflictOfInterestDao.getPersonEntityIdOfNonArchiveVersion(personEntity.getPersonEntityNumber());
        if (!versionId.equals(personEntityId)) {
            personEntityId = versionId;
        }
        personEntityId = copyPersonEntity(personEntity, personEntity.getVersionStatus()).getPersonEntityId();
        fcoiDisclosureDao.updateDisclosureSyncNeededByPerEntId(personEntity.getPersonEntityId(), true);
        List<PersonEntityRelationship> personEntityRelationshipList = new ArrayList<>();
        List<String> relationshipNames = new ArrayList<>();
        List<ValidPersonEntityRelType> validRelTypes = conflictOfInterestDao.getValidPersonEntityRelTypeByTypeCodes(personEntityRelationship.getValidPersonEntityRelTypeCodes());
        for (ValidPersonEntityRelType relType : validRelTypes) {
            PersonEntityRelationship personEntityRelation = new PersonEntityRelationship();
            personEntityRelation.setPersonEntityId(personEntityId);
            personEntityRelation.setValidPersonEntityRelTypeCode(relType.getValidPersonEntityRelTypeCode());
            personEntityRelation.setValidPersonEntityRelType(relType);
            personEntityRelation.setUpdateUser(AuthenticatedUser.getLoginUserName());
            conflictOfInterestDao.saveOrUpdatePersonEntityRelationship(personEntityRelation);
            relationshipNames.add(relType.getDescription());
            personEntityRelationshipList.add(personEntityRelation);
        }
        PersonEntityDto personEntityDto = new PersonEntityDto();
        personEntityDto.setPersonEntityId(personEntityId);
        personEntityDto.setPersonEntityNumber(personEntity.getPersonEntityNumber());
        personEntityDto.setEntityName(personEntity.getCoiEntity().getEntityName());
        personEntityDto.setRelationshipName(String.join(", ", relationshipNames));
        personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_REL_ADDED);
        actionLogService.savePersonEntityActionLog(personEntityDto);
        ResponseEntity<Map<String, Object>> response = updatePersonEntityCompleteFlag(personEntityId);
        personEntityDto.setIsFormCompleted((Boolean) response.getBody().get(IS_FORM_COMPLETED));
        personEntityDto.setPersonEntityRelationships(personEntityRelationshipList);
        personEntityDto.setUpdateTimestamp(conflictOfInterestDao.updatePersonEntityUpdateDetails(personEntityId));
        return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
    }

    private boolean checkFormCompleted(Integer validPersonEntityRelTypeCode, Integer personEntityId, boolean isFormCompleted) {
        QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
        questionnaireDataBus.setModuleItemCode(Constants.COI_MODULE_CODE);
        questionnaireDataBus.setModuleSubItemCode(Constants.COI_SFI_SUBMODULE_CODE);
        questionnaireDataBus.setModuleSubItemKey(String.valueOf(validPersonEntityRelTypeCode));
        if (personEntityId != null)
            questionnaireDataBus.setModuleItemKey(String.valueOf(personEntityId));
        questionnaireDataBus.setQuestionnaireMode(Constants.ACTIVE_ANSWERED_UNANSWERED);
        QuestionnaireDataBus questData = questionnaireService.getApplicableQuestionnaire(questionnaireDataBus);
        if (questData.getApplicableQuestionnaire() != null && !questData.getApplicableQuestionnaire().isEmpty()) {
            isFormCompleted = questData.getApplicableQuestionnaire().stream()
                    .allMatch(map -> map.containsKey("QUESTIONNAIRE_COMPLETED_FLAG") && map.get("QUESTIONNAIRE_COMPLETED_FLAG") != null &&
                            map.get("QUESTIONNAIRE_COMPLETED_FLAG").equals("Y"));

        }
        return isFormCompleted;
    }

    private PersonEntity copyPersonEntity(PersonEntity personEntityObj, String versionStatus) {
        PersonEntity personEntity = new PersonEntity();
        Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
        String loginUsername = AuthenticatedUser.getLoginUserName();
        BeanUtils.copyProperties(personEntityObj, personEntity);
        personEntity.setPersonEntityId(null);
        personEntity.setVersionNumber(conflictOfInterestDao.getMaxPersonEntityVersionNumber(personEntityObj.getPersonEntityNumber()) + 1);
        personEntity.setVersionStatus(versionStatus);
        personEntity.setUpdateUser(loginUsername);
        personEntity.setCreateUser(loginUsername);
        personEntity.setCreateTimestamp(currentTimestamp);
        personEntity.setUpdateTimestamp(currentTimestamp);
        personEntity.setEntityId(conflictOfInterestDao.getMaxEntityId(personEntityObj.getEntityNumber()));
        conflictOfInterestDao.saveOrUpdatePersonEntity(personEntity);
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
        conflictOfInterestDao.updatePersonEntityVersionStatus(personEntityObj.getPersonEntityId(), Constants.COI_ARCHIVE_STATUS);
        return personEntity;
    }

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
        questionnaireService.copyQuestionnaireForVersion(questionnaireDataBus, false);
    }

    @Override
    public ResponseEntity<Object> getPersonEntityDetails(Integer personEntityId) {
        ConflictOfInterestVO vo = new ConflictOfInterestVO();
        PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityId);
        PersonEntity personEntityObj = new PersonEntity();
        BeanUtils.copyProperties(personEntity, personEntityObj, "coiEntity", "person");
        personEntityObj.setUpdateUserFullName(personDao.getUserFullNameByUserName(personEntityObj.getUpdateUser()));
        vo.setPersonEntity(personEntityObj);
        List<PersonEntityRelationship> personEntityRelationships = conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityId(personEntityId);
        personEntityRelationships.forEach(PersonEntityRelationship -> conflictOfInterestDao.getValidPersonEntityRelTypeByTypeCode(PersonEntityRelationship.getValidPersonEntityRelTypeCode()));
        vo.setPersonEntityRelationships(personEntityRelationships);
        return new ResponseEntity<>(vo, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getPersonEntityDashboard(CoiDashboardVO vo) {
        return new ResponseEntity<>(conflictOfInterestDao.getPersonEntityDashboard(vo), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> updatePersonEntity(PersonEntityDto personEntityDto) {
//        Integer versionId = conflictOfInterestDao.getPersonEntityIdOfNonArchiveVersion(personEntityDto.getPersonEntityNumber());
//        if (versionId != personEntityDto.getPersonEntityId()) {
//            personEntityDto.setPersonEntityId(versionId);
//        }
//        if (conflictOfInterestDao.checkPersonEntityAdded(personEntityDto.getPersonEntityId())) {
//            PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityDto.getPersonEntityId());
//            personEntityDto.setPersonEntityId(copyPersonEntity(personEntity, personEntity.getVersionStatus()).getPersonEntityId());
//        }
    	personEntityDto.setUpdateTimestamp(conflictOfInterestDao.updatePersonEntity(personEntityDto));
    	personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_MODIFIED);
        actionLogService.savePersonEntityActionLog(personEntityDto);
        return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> deletePersonEntityRelationship(Integer personEntityRelId, Integer personEntityId) {
        PersonEntityRelationship relationship = conflictOfInterestDao.getRelationshipDetailsById(personEntityRelId);
        if (relationship == null) {
            return new ResponseEntity<>("Already deleted", HttpStatus.METHOD_NOT_ALLOWED);
        }
//        Integer versionId = conflictOfInterestDao.getPersonEntityIdOfNonArchiveVersion(relationship.getPersonEntity().getPersonEntityNumber());
//        if (versionId != personEntityId) {
//            personEntityId = versionId;
//        }
//        if (conflictOfInterestDao.checkPersonEntityAdded(personEntityId)) {
//            PersonEntity personEntity = conflictOfInterestDao.getPersonEntityDetailsById(personEntityId);
//            personEntityId = copyPersonEntity(personEntity, personEntity.getVersionStatus()).getPersonEntityId();
//        }
        deletePerEntQuestAnsRelationship(personEntityRelId, personEntityId, relationship.getValidPersonEntityRelTypeCode());
        Timestamp updateTimestamp = conflictOfInterestDao.updatePersonEntityUpdateDetails(personEntityId);
        ResponseEntity<Map<String, Object>> isFormCompleted = updatePersonEntityCompleteFlag(personEntityId);
        PersonEntityDto personEntityDto = new PersonEntityDto();
        personEntityDto.setIsFormCompleted((Boolean) isFormCompleted.getBody().get(IS_FORM_COMPLETED));
        personEntityDto.setPersonEntityId(personEntityId);
        personEntityDto.setPersonEntityNumber(relationship.getPersonEntity().getPersonEntityNumber());
        personEntityDto.setRelationshipName(relationship.getValidPersonEntityRelType().getDescription());
        personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_REL_REMOVED);
        personEntityDto.setUpdateTimestamp(updateTimestamp);
        actionLogService.savePersonEntityActionLog(personEntityDto);
        fcoiDisclosureDao.updateDisclosureSyncNeededByPerEntId(personEntityId, true);
        return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
    }

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
    public ResponseEntity<Object> activateOrInactivatePersonEntity(PersonEntityDto personEntityDto) {
        if (conflictOfInterestDao.isPersonEntityActiveOrNot(null, personEntityDto.getPersonEntityNumber(), personEntityDto.getVersionStatus())) {
            if (personEntityDto.getVersionStatus().equals(Constants.COI_ACTIVE_STATUS))
                return new ResponseEntity<>("SFI already activated", HttpStatus.METHOD_NOT_ALLOWED);
            else
                return new ResponseEntity<>("SFI already inactivated", HttpStatus.METHOD_NOT_ALLOWED);
        }
        PersonEntity personEntityObj = conflictOfInterestDao.getPersonEntityDetailsById(personEntityDto.getPersonEntityId());
        PersonEntity personEntity = copyPersonEntity(personEntityObj, personEntityDto.getVersionStatus());
        personEntityDto.setPersonEntityId(personEntity.getPersonEntityId());
        fcoiDisclosureDao.updateDisclosureSyncNeededByPerEntId(personEntityObj.getPersonEntityId(), true);
        personEntityDto.setIsFormCompleted(personEntityObj.getIsFormCompleted());
        personEntityDto.setUpdateTimestamp(conflictOfInterestDao.updatePersonEntityVersionStatus(personEntityDto.getPersonEntityId(), personEntityDto.getVersionStatus()));
        personEntityDto.setEntityName(personEntityObj.getCoiEntity().getEntityName());
        personEntityDto.setPersonEntityNumber(personEntityObj.getPersonEntityNumber());
        personEntityDto.setActionTypeCode(personEntityDto.getVersionStatus().equals(Constants.COI_ACTIVE_STATUS) ?
                Constants.COI_PERSON_ENTITY_ACTION_LOG_ACTIVATED : Constants.COI_PERSON_ENTITY_ACTION_LOG_INACTIVATED);
        actionLogService.savePersonEntityActionLog(personEntityDto);
        return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
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

    @Override
    public ResponseEntity<Object> getPersonEntityRelationship(ConflictOfInterestVO vo) {
        List<PersonEntityRelationship> personEntityRelationships = new ArrayList<>();
        conflictOfInterestDao.getRelationshipDetails(vo).forEach(personEntityRelationship -> {
            PersonEntityRelationship copyRelationship = new PersonEntityRelationship();
            BeanUtils.copyProperties(personEntityRelationship, copyRelationship, "personEntity");
            personEntityRelationships.add(copyRelationship);
        });
        return new ResponseEntity<>(personEntityRelationships, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getSFILatestVersion(Integer personEntityNumber) {
        return new ResponseEntity<>(conflictOfInterestDao.getSFILatestVersion(personEntityNumber), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getAllPersonEntityVersions(Integer personEntityNumber) {
        return new ResponseEntity<>(conflictOfInterestDao.fetchAllPersonEntityVersions(personEntityNumber), HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Map<String, Object>> updatePersonEntityCompleteFlag(Integer personEntityId) {
        boolean isFormCompleted = true;
        List<PersonEntityRelationship> personEntityRelationships = conflictOfInterestDao.getPersonEntityRelationshipByPersonEntityId(personEntityId);
        if (personEntityRelationships == null || personEntityRelationships.isEmpty()) {
            isFormCompleted = false;
        }
        for (PersonEntityRelationship personEntityRelationship : personEntityRelationships) {
            if (isFormCompleted)
                isFormCompleted = checkFormCompleted(personEntityRelationship.getValidPersonEntityRelTypeCode(), personEntityId, isFormCompleted);
            else break;
        }
        conflictOfInterestDao.updatePersonEntityCompleteFag(personEntityId, isFormCompleted);
        if (isFormCompleted) {
            PersonEntity personEntityObj = conflictOfInterestDao.getPersonEntityDetailsById(personEntityId);
            PersonEntityDto personEntityDto = new PersonEntityDto();
            personEntityDto.setPersonEntityId(personEntityId);
            personEntityDto.setPersonEntityNumber(personEntityObj.getPersonEntityNumber());
            personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_FORM_COMPLETED);
            actionLogService.savePersonEntityActionLog(personEntityDto);
        }
        Map<String, Object> response = new HashMap<>();
        response.put("isFormCompleted", isFormCompleted);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getSFIOfDisclosure(ConflictOfInterestVO vo) {
    	Map<String, Object> responseData = new HashMap<>();
		List<PersonEntity> personEntities  = conflictOfInterestDao.getSFIOfDisclosure(vo);
//		Integer disclosureId = vo.getDisclosureId() != null ? vo.getDisclosureId() : null;
//		String personId = disclosureId == null ? vo.getPersonId() : null;
//		List<PersonEntityRelationshipDto> personEntityRelationshipDto = conflictOfInterestDao.getPersonEntities(disclosureId, personId, null);
		personEntities.forEach(personEntity -> {personEntity.setValidPersonEntityRelTypes(conflictOfInterestDao.getValidPersonEntityRelTypes(personEntity.getPersonEntityId()));
//												personEntity.setPersonEntityRelationshipDto(personEntityRelationshipDto
//														.stream()
//											            .filter(dto -> personEntity.getPersonEntityId().equals(dto.getPersonEntityId()))
//											            .findFirst()
//											            .orElse(null));
        });
		if(vo.getFilterType().equalsIgnoreCase("Financial")) {
			responseData.put("isProjectPresent", conflictOfInterestDao.isProjectPresent(vo));
			personEntities.forEach(personEntity -> personEntity.setSfiCompleted(fcoiDisclosureDao.isSFICompletedForDisclosure(personEntity.getPersonEntityId(), vo.getDisclosureId())));
			personEntities.forEach(personEntity -> personEntity.setDisclosureStatusCount(conflictOfInterestDao.disclosureStatusCountBySFI(personEntity.getPersonEntityId(), vo.getDisclosureId())));
		}
		responseData.put("personEntities", personEntities);
		responseData.put("count", conflictOfInterestDao.getSFIOfDisclosureCount(vo));
		return new ResponseEntity<>(responseData, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getSFIDetails(Integer coiFinancialEntityId) {
        ConflictOfInterestVO vo = new ConflictOfInterestVO();
        vo.setPersonEntityRelationships(conflictOfInterestDao.getCoiFinancialEntityDetails(coiFinancialEntityId));
        vo.setPersonEntity(conflictOfInterestDao.getSFIDetails(coiFinancialEntityId));
        return new ResponseEntity<>(vo, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> modifyPersonEntity(PersonEntityDto personEntityDto) {
        Integer personEntityId = personEntityDto.getPersonEntityId();
        PersonEntity personEntityObj = conflictOfInterestDao.getPersonEntityDetailsById(personEntityId);
        if (personEntityObj != null && personEntityObj.getVersionStatus().equals(Constants.COI_INACTIVE_STATUS)) {
        	personEntityDto.setPersonEntityId(personEntityId);
            return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
        }
        Integer latestPerEntVersionId = conflictOfInterestDao.getPersonEntityIdOfNonArchiveVersion(personEntityObj.getPersonEntityNumber());
        if (!latestPerEntVersionId.equals(personEntityId)) {
        	personEntityDto.setPersonEntityId(latestPerEntVersionId);
            return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
        }
        if (conflictOfInterestDao.checkPersonEntityAdded(personEntityId)) {
            PersonEntity copiedPerEntVersion = copyPersonEntity(personEntityObj, personEntityObj.getVersionStatus());
            personEntityDto.setPersonEntityId(copiedPerEntVersion.getPersonEntityId());
        } else {
//            personEntityObj.setEntityId(conflictOfInterestDao.getMaxEntityId(personEntityObj.getEntityNumber()));
        }
        fcoiDisclosureDao.updateDisclosureSyncNeededByPerEntId(personEntityObj.getPersonEntityId(), true);
        personEntityDto.setPersonEntityNumber(personEntityObj.getPersonEntityNumber());
        personEntityDto.setEntityName(personEntityObj.getCoiEntity().getEntityName());
        personEntityDto.setActionTypeCode(Constants.COI_PERSON_ENTITY_ACTION_LOG_MODIFIED);
        actionLogService.savePersonEntityActionLog(personEntityDto);
        return new ResponseEntity<>(personEntityDto, HttpStatus.OK);
    }
}
