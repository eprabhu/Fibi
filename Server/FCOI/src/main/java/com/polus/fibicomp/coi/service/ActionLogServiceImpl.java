package com.polus.fibicomp.coi.service;

import java.util.ArrayList;
import java.util.Arrays;
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

import com.polus.core.common.dao.CommonDao;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.HistoryDto;
import com.polus.fibicomp.coi.dto.PersonEntityActionLogDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.DisclosureActionType;
import com.polus.fibicomp.coi.pojo.PersonEntityActionLog;
import com.polus.fibicomp.coi.pojo.PersonEntityActionType;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;
import com.polus.fibicomp.coi.repository.ActionLogDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.disclosures.consultingdisclosure.dto.ConsultDisclCommonDto;
import com.polus.fibicomp.disclosures.consultingdisclosure.pojo.ConsultingDisclActionLog;
import com.polus.fibicomp.disclosures.consultingdisclosure.pojo.ConsultingDisclActionLogType;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.pojo.OPAActionLog;
import com.polus.fibicomp.opa.pojo.OPAActionLogType;;

@Service
@Transactional
public class ActionLogServiceImpl implements ActionLogService {

    @Autowired
    private ActionLogDao actionLogDao;

    @Autowired
	private CommonDao commonDao;

    @Autowired
	@Qualifier(value = "conflictOfInterestDao")
	private ConflictOfInterestDao conflictOfInterestDao;

    @Autowired
	private PersonDao personDao;
 
	private static final String DISCLOSURE_TYPE_TRAVEL = "Travel";

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
		DisclosureActionLog actionLog = DisclosureActionLog.builder().actionTypeCode(actionLogDto.getActionTypeCode())
				.disclosureId(actionLogDto.getDisclosureId()).disclosureNumber(actionLogDto.getDisclosureNumber())
				.description(getFormattedMessageByActionType(actionLogDto)).comment(actionLogDto.getRevisionComment())
				.updateTimestamp(commonDao.getCurrentTimestamp()).updateUser(AuthenticatedUser.getLoginUserName())
				.build();
		conflictOfInterestDao.saveOrUpdateDisclosureActionLog(actionLog);
	}

	@Override
	public String getFormattedMessageByActionType(DisclosureActionLogDto actionLogDto) {
		DisclosureActionType disclosureActionType = conflictOfInterestDao.fetchDisclosureActionTypeById(actionLogDto.getActionTypeCode());
		return buildDisclosureLogMessage(actionLogDto, disclosureActionType.getMessage());
	}

	private String buildDisclosureLogMessage(DisclosureActionLogDto actionLogDto, String message) {
        Map<String, String> placeholdersAndValues = new HashMap<>(); 
        if(actionLogDto.getFcoiTypeDescription() != null) {
		message = message.replace("{FCOI /Project /Travel}", actionLogDto.getFcoiTypeDescription());
        }
        if(actionLogDto.getOldAdmin()!=null) {
        	placeholdersAndValues.put("{ADMIN_ONE}", actionLogDto.getOldAdmin());
            placeholdersAndValues.put("{ADMIN_TWO}", actionLogDto.getNewAdmin());
        }
        else if(actionLogDto.getNewAdmin()!=null) {
        	placeholdersAndValues.put("{ADMIN_ONE}", actionLogDto.getNewAdmin());
        }
        if(actionLogDto.getReviewername()!=null) {
        	placeholdersAndValues.put("{REVIEWER_NAME}", actionLogDto.getReviewername());
        }
        if (actionLogDto.getConflictStatus() != null || actionLogDto.getNewConflictStatus() != null) {
            placeholdersAndValues.put("{OLD}", actionLogDto.getConflictStatus());
            placeholdersAndValues.put("{NEW}", actionLogDto.getNewConflictStatus());
        }
        if (actionLogDto.getRiskCategory() != null) {
            placeholdersAndValues.put("{LOW}", actionLogDto.getRiskCategory());
            placeholdersAndValues.put("{HIGH}", actionLogDto.getNewRiskCategory());
        }
        if (actionLogDto.getAdministratorName() != null) {
            placeholdersAndValues.put("{ADMIN_NAME}", actionLogDto.getAdministratorName());
        }
        if (actionLogDto.getOldReviewer() != null) {
            placeholdersAndValues.put("{REVIEWER_ONE}", actionLogDto.getOldReviewer());
            placeholdersAndValues.put("{REVIEWER_TWO}", actionLogDto.getNewReviewer());
        }
        if(actionLogDto.getReporter()!=null) {
        	placeholdersAndValues.put("{REPORTER}", actionLogDto.getReporter());
        }
        if(actionLogDto.getCoiAdmin()!=null) {
        	placeholdersAndValues.put("{COI_ADMIN}", actionLogDto.getCoiAdmin());
        }
		if (actionLogDto.getReviewLocationType() != null) {
			placeholdersAndValues.put("{LOCATION}", actionLogDto.getReviewLocationType().getDescription());
		}
		if (actionLogDto.getReviewerStatusType() != null) {
			placeholdersAndValues.put("{REVIEW_STATUS}", actionLogDto.getReviewerStatusType().getDescription());
		}
        return renderPlaceholders(message, placeholdersAndValues);
    }

	@Override
	public ResponseEntity<Object> getDisclosureHistoryById(Integer disclosureId) {
		List<String> reviewActionTypeCodes = Arrays.asList(Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITH_REVIEWER, Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITHOUT_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_START_REVIEW, Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITHOUT_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITH_REVIEWER,Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_COMPLETE_REVIEW,
				Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITHOUT_REVIEWER,Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITH_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITHOUT_REVIEWER,Constants.COI_DISCLOSURE_ACTION_LOG_REVIEW_REMOVED_WITH_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_REVIEW_REMOVED_WITHOUT_REVIEWER, Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITH_REVIEWER,
				Constants.COI_DIS_ACTION_LOG_DISCLOSURE_SYNCED);
		List<DisclosureActionLog> disclsouretActionLogs = actionLogDao.fetchDisclosureActionLogsBasedOnDisclosureId(disclosureId, reviewActionTypeCodes);
		List<HistoryDto> disclosureHistories = new ArrayList<>();
		disclsouretActionLogs.forEach(disclsouretActionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(disclsouretActionLog.getUpdateTimestamp());
			if (disclsouretActionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(disclsouretActionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(disclsouretActionLog.getActionTypeCode());
			historyDto.setMessage(disclsouretActionLog.getDescription());
			historyDto.setComment(disclsouretActionLog.getComment());
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
		if (actionLogDto.getOldDisclosureStatus() != null) {
			placeholdersAndValues.put("{STATUS_ONE}", actionLogDto.getOldDisclosureStatus());
			placeholdersAndValues.put("{STATUS_TWO}", actionLogDto.getNewDisclosureStatus());
		} else if (actionLogDto.getNewAdmin() != null) {
			placeholdersAndValues.put("{STATUS_ONE}", actionLogDto.getNewDisclosureStatus());
		}
		if(actionLogDto.getReporter()!=null) {
        	placeholdersAndValues.put("{REPORTER}", actionLogDto.getReporter());
        }
		if (actionLogDto.getAdministratorName() != null) {
            placeholdersAndValues.put("{ADMIN_NAME}", actionLogDto.getAdministratorName());
        }
		if(actionLogDto.getCoiAdmin()!=null) {
        	placeholdersAndValues.put("{COI_ADMIN}", actionLogDto.getCoiAdmin());
        }
		return renderPlaceholders(message, placeholdersAndValues);
	}

    @Override
    public List<DisclosureActionLog> fetchDisclosureActionLog(DisclosureActionLogDto actionLogDto) {
        List<DisclosureActionLog> actionLogList = new ArrayList<>();
        actionLogDao.fetchDisclosureActionLog(actionLogDto).forEach(actionLog ->  {
            DisclosureActionLog disclosureActionLog = new DisclosureActionLog();
            BeanUtils.copyProperties(actionLog, disclosureActionLog, "disclosure", "disclosureActionType");
            disclosureActionLog.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
            actionLogList.add(disclosureActionLog);
        });
        return actionLogList;
    }

	@Override
	public ResponseEntity<Object> getTravelDisclosureHistoryById(Integer travelDisclosureId) {
		List<TravelDisclosureActionLog> travelDisclosureActionLogs = actionLogDao.fetchTravelDisclosureActionLogsBasedOnId(travelDisclosureId);
		List<HistoryDto> travelDisclosureHistories = new ArrayList<>();
		travelDisclosureActionLogs.forEach(actionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(actionLog.getUpdateTimestamp());
			if (actionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(actionLog.getActionTypeCode());
			historyDto.setMessage(actionLog.getDescription());
			historyDto.setComment(actionLog.getComment());
			travelDisclosureHistories.add(historyDto);
		});
		return new ResponseEntity<>(travelDisclosureHistories, HttpStatus.OK);
	}

	@Override
	public List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto) {
		List<TravelDisclosureActionLog> actionLogList = new ArrayList<>();
		actionLogDao.fetchTravelDisclosureActionLog(actionLogDto).forEach(actionLog -> {
			TravelDisclosureActionLog travelDisclosureActionLog = new TravelDisclosureActionLog();
			BeanUtils.copyProperties(actionLog, travelDisclosureActionLog, "coiTravelDisclosure", "disclosureActionType");
			travelDisclosureActionLog.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			actionLogList.add(travelDisclosureActionLog);
		});
		actionLogDto.setActionTypeCode(Constants.COI_DISCLOSURE_ACTION_LOG_ADD_RISK);
		actionLogDao.fetchTravelDisclosureActionLog(actionLogDto).forEach(actionLog -> {
			TravelDisclosureActionLog travelDisclosureActionLog = new TravelDisclosureActionLog();
			BeanUtils.copyProperties(actionLog, travelDisclosureActionLog, "coiTravelDisclosure", "disclosureActionType");
			actionLogList.add(travelDisclosureActionLog);
		});
		return actionLogList;
	}

	@Override
	public ResponseEntity<Object> getReviewHistoryById(Integer disclosureId) {
		List<String> actionTypeCodes = Arrays.asList(Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITH_REVIEWER, Constants.COI_DIS_ACTION_LOG_CREATED_REVIEW_WITHOUT_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_START_REVIEW, Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITHOUT_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITH_REVIEWER, Constants.COI_DISCLOSURE_ACTION_LOG_REVIEWER_COMPLETE_REVIEW,
				Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_START_REVIEW_WITHOUT_REVIEWER, Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITH_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITHOUT_REVIEWER, Constants.COI_DISCLOSURE_ACTION_LOG_REVIEW_REMOVED_WITH_REVIEWER,
				Constants.COI_DISCLOSURE_ACTION_LOG_REVIEW_REMOVED_WITHOUT_REVIEWER, Constants.COI_DIS_ACTION_LOG_MODIFIED_REVIEW_WITH_REVIEWER);
		List<DisclosureActionLog> disclsouretActionLogs = actionLogDao.fetchReviewActionLogs(disclosureId, actionTypeCodes);
		List<HistoryDto> disclosureHistories = new ArrayList<>();
		disclsouretActionLogs.forEach(disclsouretActionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(disclsouretActionLog.getUpdateTimestamp());
			if (disclsouretActionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(disclsouretActionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(disclsouretActionLog.getActionTypeCode());
			historyDto.setMessage(disclsouretActionLog.getDescription());
			historyDto.setComment(disclsouretActionLog.getComment());
			disclosureHistories.add(historyDto);
		});
		return new ResponseEntity<>(disclosureHistories, HttpStatus.OK);
	}

	@Override
	public void saveOPAActionLog(String actionLogTypeCode, OPACommonDto opaCommonDto) {
		OPAActionLogType opaActionLogType = actionLogDao.getOPAActionType(actionLogTypeCode);
		if (opaActionLogType != null) {
			String message = buildOPALogMessage(opaActionLogType.getMessage(), opaCommonDto);
			OPAActionLog opaActionLog = OPAActionLog.builder()
					.actionTypeCode(actionLogTypeCode)
					.comment(opaCommonDto.getComment())
					.description(message)
					.opaDisclosureId(opaCommonDto.getOpaDisclosureId())
					.opaDisclosureNumber(opaCommonDto.getOpaDisclosureNumber())
					.updateTimestamp(commonDao.getCurrentTimestamp()).updateUser(AuthenticatedUser.getLoginUserName())
					.build();
			actionLogDao.saveObject(opaActionLog);
		}
	}

	private String buildOPALogMessage(String message, OPACommonDto commonDto) {
		Map<String, String> placeholdersAndValues = new HashMap<>();
		placeholdersAndValues.put("{REPORTER}", commonDto.getUpdateUserFullName());
		placeholdersAndValues.put("{ADMIN_NAME}", commonDto.getUpdateUserFullName());
		placeholdersAndValues.put("{ASSIGNED_ADMIN}", commonDto.getAdminPersonName());
		placeholdersAndValues.put("{REASSIGNED_ADMIN}", commonDto.getReassignedAdminPersonName());
		if (commonDto.getReviewerFullName() != null) {
			placeholdersAndValues.put("{REVIEWER_NAME}", commonDto.getReviewerFullName());
		}
		if (commonDto.getReviewLocationType() != null) {
			placeholdersAndValues.put("{LOCATION}", commonDto.getReviewLocationType());
		}
		if (commonDto.getReviewStatusType() != null) {
			placeholdersAndValues.put("{REVIEW_STATUS}", commonDto.getReviewStatusType());
		}
		return renderPlaceholders(message, placeholdersAndValues);
	}

	@Override
	public ResponseEntity<Object> getOpaDisclosureHistoryById(Integer opaDisclosureId) {
		List<String> actionTypeCodes = Arrays.asList(Constants.OPA_DIS_ACTION_LOG_MODIFIED_REVIEW_WITH_REVIEWER, Constants.OPA_DIS_ACTION_LOG_MODIFIED_REVIEW_WITHOUT_REVIEWER,
				Constants.OPA_DIS_ACTION_LOG_CREATED_REVIEW_WITHOUT_REVIEWER, Constants.OPA_DIS_ACTION_LOG_CREATED_REVIEW_WITH_REVIEWER,
				Constants.OPA_DIS_ACTION_LOG_ADMIN_START_REVIEW_WITH_REVIEWER, Constants.OPA_DIS_ACTION_LOG_ADMIN_START_REVIEW_WITHOUT_REVIEWER,
				Constants.OPA_DIS_ACTION_LOG_ADMIN_START_REVIEW_BY_REVIEWER, Constants.OPA_DIS_ACTION_LOG_ADMIN_COMPLETE_REVIEW_BY_REVIEWER,
				Constants.OPA_DIS_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITH_REVIEWER, Constants.OPA_DIS_ACTION_LOG_ADMIN_COMPLETE_REVIEW_WITHOUT_REVIEWER,
				Constants.OPA_DIS_ACTION_LOG_ADMIN_REMOVED_REVIEW_WITH_REVIEWER, Constants.OPA_DIS_ACTION_LOG_ADMIN_REMOVED_REVIEW_WITHOUT_REVIEWER);
		List<OPAActionLog> opaActionLogs = actionLogDao.fetchOpaDisclosureActionLogsBasedOnId(opaDisclosureId, actionTypeCodes, false);
		List<HistoryDto> opaDisclosureHistories = new ArrayList<>();
		opaActionLogs.forEach(actionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(actionLog.getUpdateTimestamp());
			if (actionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(actionLog.getActionTypeCode());
			historyDto.setMessage(actionLog.getDescription());
			historyDto.setComment(actionLog.getComment());
			opaDisclosureHistories.add(historyDto);
		});
		return new ResponseEntity<>(opaDisclosureHistories, HttpStatus.OK);
	}

	@Override
	public void savePersonEntityActionLog(PersonEntityDto personEntityDto) {
		PersonEntityActionType personEntityActionType = actionLogDao.getPersonEntityActionType(personEntityDto.getActionTypeCode());
		if (personEntityActionType != null) {
			String message = buildPersonEntityLogMessage(personEntityActionType.getMessage(), personEntityDto);
			PersonEntityActionLog actionLog = PersonEntityActionLog.builder().actionTypeCode(personEntityDto.getActionTypeCode())
					.personEntityId(personEntityDto.getPersonEntityId())
					.personEntityNumber(personEntityDto.getPersonEntityNumber())
					.description(message)
					.comment(personEntityDto.getRevisionReason())
					.updateTimestamp(commonDao.getCurrentTimestamp())
					.updateUser(AuthenticatedUser.getLoginUserName()).build();
			actionLogDao.saveObject(actionLog);
		}
	}

	private String buildPersonEntityLogMessage(String message, PersonEntityDto personEntityDto) {
		Map<String, String> placeholdersAndValues = new HashMap<>();
		placeholdersAndValues.put("{ENTITY_NAME}", personEntityDto.getEntityName());
		placeholdersAndValues.put("{RELATIONSHIP_NAME}", personEntityDto.getRelationshipName());
		return renderPlaceholders(message, placeholdersAndValues);
	}

	@Override
	public ResponseEntity<Object> getAllPersonEntityActionLog(PersonEntityDto personEntityDto) {
		List<PersonEntityActionLogDto> actionLogs = new ArrayList<>();
		actionLogDao.fetchPersonEntityActionLog(personEntityDto).forEach(actionLog -> {
			PersonEntityActionLogDto entityActionLogDto = new PersonEntityActionLogDto();
			BeanUtils.copyProperties(actionLog, entityActionLogDto);
			entityActionLogDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			actionLogs.add(entityActionLogDto);
		});
		return new ResponseEntity<>(actionLogs, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> getConsultingDisclosureHistoryById(Integer disclosureId) {
		List<ConsultingDisclActionLog> consultingDisclActionLogs = actionLogDao.fetchConsultDisclActionLogsBasedOnId(disclosureId);
		List<HistoryDto> consultDisclHistories = new ArrayList<>();
		consultingDisclActionLogs.forEach(actionLog -> {
			HistoryDto historyDto = new HistoryDto();
			historyDto.setUpdateTimestamp(actionLog.getUpdateTimestamp());
			if (actionLog.getUpdateUser() != null) {
				historyDto.setUpdateUserFullName(personDao.getUserFullNameByUserName(actionLog.getUpdateUser()));
			}
			historyDto.setActionTypeCode(actionLog.getActionTypeCode());
			historyDto.setMessage(actionLog.getDescription());
			historyDto.setComment(actionLog.getComment());
			consultDisclHistories.add(historyDto);
		});
		return new ResponseEntity<>(consultDisclHistories, HttpStatus.OK);
	}

	@Override
	public void saveConsultingDisclActionLog(String actionLogTypeCode, ConsultDisclCommonDto consultDisclCommonDto) {
		ConsultingDisclActionLogType consultDisclActionLogType = actionLogDao.getConsultDisclActionType(actionLogTypeCode);
		if (consultDisclActionLogType != null) {
			String message = buildConsultDisclLogMessage(consultDisclActionLogType.getMessage(), consultDisclCommonDto);
			ConsultingDisclActionLog consultingDisclActionLog = ConsultingDisclActionLog.builder()
					.actionTypeCode(actionLogTypeCode)
					.comment(consultDisclCommonDto.getComment())
					.description(message)
					.disclosureId(consultDisclCommonDto.getDisclosureId())
					.updateTimestamp(commonDao.getCurrentTimestamp()).updateUser(AuthenticatedUser.getLoginUserName())
					.build();
			actionLogDao.saveObject(consultingDisclActionLog);
		}
	}

	private String buildConsultDisclLogMessage(String message, ConsultDisclCommonDto consultDisclCommonDto) {
		Map<String, String> placeholdersAndValues = new HashMap<>();
		placeholdersAndValues.put("{REPORTER}", consultDisclCommonDto.getUpdateUserFullName());
		placeholdersAndValues.put("{ADMIN_NAME}", consultDisclCommonDto.getUpdateUserFullName());
		placeholdersAndValues.put("{ASSIGNED_ADMIN}", consultDisclCommonDto.getAdminPersonName());
		placeholdersAndValues.put("{REASSIGNED_ADMIN}", consultDisclCommonDto.getReassignedAdminPersonName());
		return renderPlaceholders(message, placeholdersAndValues);
	}

}
