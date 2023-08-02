package com.polus.fibicomp.coi.repository;

import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionLog;

import java.util.List;

public interface ActionLogRepositoryCustom {

    /**
     * This method is used to fetch Entity Action log by @params
     * @param entityId
     * @param actionTypeCode
     * @return
     */
    List<EntityActionLog> fetchEntityActionLog(Integer entityId, String actionTypeCode);

    public List<DisclosureActionLog> fetchDisclosureActionLogsBasedOnDisclosureId(Integer disclosureId);

}
