package com.polus.fibicomp.globalentity.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.person.dao.PersonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.dto.EntityActionLogDto;
import com.polus.fibicomp.globalentity.dao.EntityActionLogDao;
import com.polus.fibicomp.globalentity.dto.ActionLogRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntityActionLog;
import com.polus.fibicomp.globalentity.pojo.EntityActionType;;

@Service
@Transactional
public class EntityActionLogServiceImpl implements EntityActionLogService {

    @Autowired
    private EntityActionLogDao actionLogDao;

    @Autowired
	private CommonDao commonDao;

    @Autowired
	@Qualifier(value = "conflictOfInterestDao")
	private ConflictOfInterestDao conflictOfInterestDao;

    @Autowired
	private PersonDao personDao;

    @Override
    public void saveEntityActionLog(String actionLogTypeCode, ActionLogRequestDTO dto, String comment) {
        EntityActionType entityActionType = actionLogDao.getEntityActionType(actionLogTypeCode);
        if (entityActionType != null) {
            String message = buildEntityLogMessage(entityActionType.getMessage(), dto);
            EntityActionLog actionLog = EntityActionLog.builder().actionTypeCode(actionLogTypeCode)
                    .entityId(dto.getEntityId())
                    .entityNumber(dto.getEntityId())
                    .description(message)
                    .comment(comment)
                    .updateTimestamp(commonDao.getCurrentTimestamp())
                    .updateUser(AuthenticatedUser.getLoginUserName()).build();
            actionLogDao.saveObject(actionLog);
        }
    }

    private String buildEntityLogMessage(String message, ActionLogRequestDTO dto) {
        Map<String, String> placeholdersAndValues = new HashMap<>();
        placeholdersAndValues.put("{ENTITY_NAME}", dto.getEntityName());
        placeholdersAndValues.put("{ADMIN_NAME}", personDao.getPersonFullNameByPersonId(AuthenticatedUser.getLoginPersonId()));
        placeholdersAndValues.put("{DUNS_NUMBER}", dto.getDunsNumber());
        placeholdersAndValues.put("{TAB_NAME}", dto.getTabName());
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
    public List<EntityActionLogDto> fetchAllEntityActionLog(Integer entityId) {
        List<EntityActionLogDto> entityLogs = new ArrayList<>();
        actionLogDao.fetchAllEntityActionLog(entityId).forEach(entityActionLog -> {
            EntityActionLogDto entityActionLogDto = new EntityActionLogDto();
            BeanUtils.copyProperties(entityActionLog, entityActionLogDto);
            entityLogs.add(entityActionLogDto);
        });
        return entityLogs;
    }

}
