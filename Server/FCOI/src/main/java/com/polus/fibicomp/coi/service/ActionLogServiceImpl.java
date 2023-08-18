package com.polus.fibicomp.coi.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.EntityActionLogDto;
import com.polus.fibicomp.coi.dto.HistoryDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.DisclosureActionType;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionType;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;
import com.polus.fibicomp.coi.repository.ActionLogRepositoryCustom;
import com.polus.fibicomp.coi.repository.DisclosureActionLogRepository;
import com.polus.fibicomp.coi.repository.DisclosureActionTypeRepository;
import com.polus.fibicomp.coi.repository.TravelDisclosureActionLogRepository;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.security.AuthenticatedUser;

@Service
@Transactional
public class ActionLogServiceImpl implements ActionLogService {

//    @Autowired
//    private EntityActionLogRepository entityActionLogRepository;
//
//    @Autowired
//    private EntityActionTypeRepository entityActionTypeRepository;

    @Autowired
    private ActionLogRepositoryCustom actionLogRepositoryCustom;

    @Autowired
    private DisclosureActionLogRepository disclosureActionLogRepository;

    @Autowired
    private DisclosureActionTypeRepository disclosureActionTypeRepository;

    @Autowired
	private CommonDao commonDao;

    @Autowired
	@Qualifier(value = "conflictOfInterestDao")
	private ConflictOfInterestDao conflictOfInterestDao;

    @Autowired
	private PersonDao personDao;

    @Autowired
    private TravelDisclosureActionLogRepository travelDisclosureActionLogRepository;

    private static final String DISCLOSURE_TYPE_FCOI = "FCOI";
	private static final String DISCLOSURE_TYPE_PROJECT = "Project";
	private static final String DISCLOSURE_TYPE_TRAVEL = "Travel";
	private static final String TYPE_CODE_FCOI = "1";
	private static final String TYPE_CODE_PROPOSAL = "2";
	private static final String TYPE_CODE_AWARD = "3";

    @Override
    public void saveEntityActionLog(String actionLogTypeCode, CoiEntity coiEntity, String comment) {
        EntityActionType entityActionType = actionLogRepositoryCustom.getEntityActionType(actionLogTypeCode);
        if (entityActionType != null) {
            String message = buildEntityLogMessage(entityActionType.getMessage(), coiEntity);
            EntityActionLog actionLog = EntityActionLog.builder().actionTypeCode(actionLogTypeCode)
                    .entityId(coiEntity.getEntityId())
                    .entityNumber(coiEntity.getEntityNumber())
                    .description(message)
                    .comment(comment)
                    .updateTimestamp(commonDao.getCurrentTimestamp())
                    .updateUser(AuthenticatedUser.getLoginUserName()).build();
            actionLogRepositoryCustom.saveObject(actionLog);
        }
    }

    private String buildEntityLogMessage(String message, CoiEntity coiEntity) {
        Map<String, String> placeholdersAndValues = new HashMap<>();
        placeholdersAndValues.put("{ENTITY_NAME}", coiEntity.getEntityName());
        placeholdersAndValues.put("{PERSON_NAME}", coiEntity.getCreateUserFullName());
        placeholdersAndValues.put("{ADMIN_NAME}", coiEntity.getUpdatedUserFullName());
        placeholdersAndValues.put("{UPDATE_TIMESTAMP}", coiEntity.getUpdateTimestamp().toString());
        placeholdersAndValues.put("{RISK}", coiEntity.getEntityRiskCategory() != null ? coiEntity.getEntityRiskCategory().getDescription() : "");
        if (coiEntity.getNewRiskCategory() != null) {
            placeholdersAndValues.put("{NEW_RISK}", coiEntity.getNewRiskCategory().getDescription());
        }
        return renderPlaceholders(message, placeholdersAndValues);
    }

    private String renderPlaceholders(String message, Map<String, String> replacementParameters) {
        if (replacementParameters != null) {
            for (String key : replacementParameters.keySet()) {
                message = StringUtils.replace(message, key, replacementParameters.get(key));
            }
        }
        return message;
    }

	@Override
	public void saveDisclosureActionLog(DisclosureActionLogDto actionLogDto) {
//		Optional<DisclosureActionType> disclosureActionType = disclosureActionTypeRepository.findById(actionLogDto.getActionTypeCode());
		DisclosureActionType disclosureActionType = conflictOfInterestDao.fetchDisclosureActionTypeById(actionLogDto.getActionTypeCode());
//		if (disclosureActionType.isPresent()) {
//            String message = buildDisclosureLogMessage(actionLogDto,disclosureActionType.get().getMessage());
		String message = buildDisclosureLogMessage(actionLogDto, disclosureActionType.getMessage());
		DisclosureActionLog actionLog = DisclosureActionLog.builder().actionTypeCode(actionLogDto.getActionTypeCode())
				.disclosureId(actionLogDto.getDisclosureId()).disclosureNumber(actionLogDto.getDisclosureNumber())
				.description(message).comment(actionLogDto.getRevisionComment())
				.updateTimestamp(commonDao.getCurrentTimestamp()).updateUser(AuthenticatedUser.getLoginUserName())
				.build();
		conflictOfInterestDao.saveOrUpdateDisclosureActionLog(actionLog);
//            disclosureActionLogRepository.save(actionLog);
//        }
	}

	private String buildDisclosureLogMessage(DisclosureActionLogDto actionLogDto, String message) {
        Map<String, String> placeholdersAndValues = new HashMap<>();
        if(TYPE_CODE_FCOI.equals(actionLogDto.getFcoiTypeCode())) {
			message = message.replace("{FCOI /Project /Travel}", DISCLOSURE_TYPE_FCOI);
		}
		else if(TYPE_CODE_PROPOSAL.equals(actionLogDto.getFcoiTypeCode())||TYPE_CODE_AWARD.equals(actionLogDto.getFcoiTypeCode())){
			message = message.replace("{FCOI /Project /Travel}", DISCLOSURE_TYPE_PROJECT);
		}
        if(actionLogDto.getOldAdmin()!=null) {
        	placeholdersAndValues.put("{ADMIN_ONE}", actionLogDto.getOldAdmin());
            placeholdersAndValues.put("{ADMIN_TWO}", actionLogDto.getNewAdmin());
        }
        else if(actionLogDto.getNewAdmin()!=null) {
        	placeholdersAndValues.put("{ADMIN_ONE}", actionLogDto.getNewAdmin());
        }
        if(actionLogDto.getReviewername()!=null) {
        	placeholdersAndValues.put("{Reviewer Name}", actionLogDto.getReviewername());
        }
        if (actionLogDto.getRiskCategory() != null) {
            placeholdersAndValues.put("{LOW}", actionLogDto.getRiskCategory());
            placeholdersAndValues.put("{HIGH}", actionLogDto.getNewRiskCategory());
        }
        return renderPlaceholders(message, placeholdersAndValues);
    }

	@Override
	public ResponseEntity<Object> getDisclosureHistoryById(Integer disclosureId) {
		List<DisclosureActionLog> disclsouretActionLogs = actionLogRepositoryCustom.fetchDisclosureActionLogsBasedOnDisclosureId(disclosureId);
		List<HistoryDto> disclosureHistories = new ArrayList<>();
		disclsouretActionLogs.forEach(disclsouretActionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(disclsouretActionLog.getUpdateTimestamp());
			if (disclsouretActionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(disclsouretActionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(disclsouretActionLog.getActionTypeCode());
			historyDto.setMessage(disclsouretActionLog.getDescription());
			disclosureHistories.add(historyDto);
		});
		return new ResponseEntity<>(disclosureHistories, HttpStatus.OK);
	}

	@Override
	public void saveTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto) {
		DisclosureActionType disclosureActionType = conflictOfInterestDao.fetchDisclosureActionTypeById(actionLogDto.getActionTypeCode());
		String message = buildTravelDisclosureLogMessage(actionLogDto, disclosureActionType.getMessage());
		TravelDisclosureActionLog actionLog = TravelDisclosureActionLog.builder().actionTypeCode(actionLogDto.getActionTypeCode())
				.travelDisclosureId(actionLogDto.getTravelDisclosureId()).travelNumber(actionLogDto.getTravelNumber())
				.description(message)
				.updateTimestamp(commonDao.getCurrentTimestamp()).updateUser(AuthenticatedUser.getLoginUserName())
				.comment(actionLogDto.getComment())
				.build();
		conflictOfInterestDao.saveOrUpdateTravelDisclosureActionLog(actionLog);
	}

	private String buildTravelDisclosureLogMessage(TravelDisclosureActionLogDto actionLogDto, String message) {
		Map<String, String> placeholdersAndValues = new HashMap<>();
		message = message.replace("{FCOI /Project /Travel}", DISCLOSURE_TYPE_TRAVEL);
		if (actionLogDto.getOldAdmin() != null) {
			placeholdersAndValues.put("{ADMIN_ONE}", actionLogDto.getOldAdmin());
			placeholdersAndValues.put("{ADMIN_TWO}", actionLogDto.getNewAdmin());
		} else if (actionLogDto.getNewAdmin() != null) {
			placeholdersAndValues.put("{ADMIN_ONE}", actionLogDto.getNewAdmin());
		}
		if (actionLogDto.getNewRiskCategory() != null) {
			placeholdersAndValues.put("{LOW}", actionLogDto.getRiskCategory());
			placeholdersAndValues.put("{HIGH}", actionLogDto.getNewRiskCategory());
		}
		if (actionLogDto.getActionTypeCode().equals(Constants.COI_DISCLOSURE_ACTION_LOG_ADD_RISK)) {
			placeholdersAndValues.put("{LOW}", actionLogDto.getRiskCategory());
		}
		return renderPlaceholders(message, placeholdersAndValues);
	}

    @Override
    public List<EntityActionLogDto> fetchEntityActionLog(Integer entityId, List<String> actionLogCodes) {
        List<EntityActionLogDto> entityLogs = new ArrayList<>();
        actionLogRepositoryCustom.fetchEntityActionLog(entityId, actionLogCodes).forEach(entityActionLog -> {
            EntityActionLogDto entityActionLogDto = new EntityActionLogDto();
            BeanUtils.copyProperties(entityActionLog, entityActionLogDto);
            entityActionLogDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(entityActionLog.getUpdateUser()));
            entityLogs.add(entityActionLogDto);
        });
        return entityLogs;
    }

    @Override
    public List<EntityActionLogDto> fetchAllEntityActionLog(CoiEntityDto coiEntityDto) {
        List<EntityActionLogDto> entityLogs = new ArrayList<>();
        actionLogRepositoryCustom.fetchAllEntityActionLog(coiEntityDto).forEach(entityActionLog -> {
            EntityActionLogDto entityActionLogDto = new EntityActionLogDto();
            BeanUtils.copyProperties(entityActionLog, entityActionLogDto);
            entityActionLogDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(entityActionLog.getUpdateUser()));
            entityLogs.add(entityActionLogDto);
        });
        return entityLogs;
    }

    @Override
    public List<DisclosureActionLog> fetchDisclosureActionLog(DisclosureActionLogDto actionLogDto) {
        List<DisclosureActionLog> actionLogList = new ArrayList<>();
        actionLogRepositoryCustom.fetchDisclosureActionLog(actionLogDto).forEach(actionLog ->  {
            DisclosureActionLog disclosureActionLog = new DisclosureActionLog();
            BeanUtils.copyProperties(actionLog, disclosureActionLog, "disclosure", "disclosureActionType");
            disclosureActionLog.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
            actionLogList.add(disclosureActionLog);
        });
        return actionLogList;
    }

	@Override
	public ResponseEntity<Object> getTravelDisclosureHistoryById(Integer travelDisclosureId) {
		List<TravelDisclosureActionLog> travelDisclosureActionLogs = actionLogRepositoryCustom.fetchTravelDisclosureActionLogsBasedOnId(travelDisclosureId);
		List<HistoryDto> travelDisclosureHistories = new ArrayList<>();
		travelDisclosureActionLogs.forEach(actionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(actionLog.getUpdateTimestamp());
			if (actionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(actionLog.getActionTypeCode());
			historyDto.setMessage(actionLog.getDescription());
			travelDisclosureHistories.add(historyDto);
		});
		return new ResponseEntity<>(travelDisclosureHistories, HttpStatus.OK);
	}

	@Override
	public List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto) {
		List<TravelDisclosureActionLog> actionLogList = new ArrayList<>();
		actionLogRepositoryCustom.fetchTravelDisclosureActionLog(actionLogDto).forEach(actionLog -> {
			TravelDisclosureActionLog travelDisclosureActionLog = new TravelDisclosureActionLog();
			BeanUtils.copyProperties(actionLog, travelDisclosureActionLog, "coiTravelDisclosure", "disclosureActionType");
			travelDisclosureActionLog.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			actionLogList.add(travelDisclosureActionLog);
		});
		actionLogDto.setActionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_ADD_RISK);
		actionLogRepositoryCustom.fetchTravelDisclosureActionLog(actionLogDto).forEach(actionLog -> {
			TravelDisclosureActionLog travelDisclosureActionLog = new TravelDisclosureActionLog();
			BeanUtils.copyProperties(actionLog, travelDisclosureActionLog, "coiTravelDisclosure", "disclosureActionType");
			actionLogList.add(travelDisclosureActionLog);
		});
		return actionLogList;
	}

}
