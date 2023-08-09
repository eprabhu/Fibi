package com.polus.fibicomp.coi.repository;

import java.util.List;

import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;

public interface ActionLogRepositoryCustom {

    /**
     * This method is used to fetch Entity Action log by @params
     * @param entityId
     * @param actionTypeCode
     * @return
     */
    List<EntityActionLog> fetchEntityActionLog(Integer entityId, String actionTypeCode);

    public List<DisclosureActionLog> fetchDisclosureActionLogsBasedOnDisclosureId(Integer disclosureId);
    
    List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(Integer travelDisclosureId, String actionTypeCode);

}
