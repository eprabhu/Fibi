package com.polus.fibicomp.coi.repository;

import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionType;

import java.util.List;
import java.util.Optional;

public interface ActionLogRepositoryCustom {

    /**
     * This method is used to fetch Entity Action log by @params
     * @param entityId
     * @param actionTypeCode
     * @return
     */
    List<EntityActionLog> fetchEntityActionLog(Integer entityId, String actionTypeCode);

    public List<DisclosureActionLog> fetchDisclosureActionLogsBasedOnDisclosureId(Integer disclosureId);


    void saveObject(Object e);

    /**
     *
     * @param actionLogTypeCode
     * @return
     */
    EntityActionType getEntityActionType(String actionLogTypeCode);

    /**
     *
     * @param coiEntityDto
     * @return
     */
    List<EntityActionLog> fetchAllEntityActionLog(CoiEntityDto coiEntityDto);

    /**
     * This method is used to fetch disclosure action log
     * @param actionLogDto
     * @return
     */
    List<DisclosureActionLog> fetchDisclosureActionLog(DisclosureActionLogDto actionLogDto);
}
