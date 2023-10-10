package com.polus.fibicomp.coi.repository;

import java.util.List;

import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionType;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;
import com.polus.fibicomp.opa.pojo.OPAActionLogType;

public interface ActionLogRepositoryCustom {

    /**
     * This method is used to fetch Entity Action log by @params
     * @param entityId
     * @param actionLogCodes
     * @return
     */
    List<EntityActionLog> fetchEntityActionLog(Integer entityId, List<String> actionLogCodes);

    public List<DisclosureActionLog> fetchDisclosureActionLogsBasedOnDisclosureId(Integer disclosureId, List<String> reviewActionTypeCodes);

    List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(Integer travelDisclosureId, String actionTypeCode);

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

    public List<TravelDisclosureActionLog> fetchTravelDisclosureActionLogsBasedOnId(Integer travelDisclosureId);

    List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto);

    /**
     * This method is used to fetch review action log
     * @param disclosureId
     * @param actionTypeCodes
     * @return
     */
	List<DisclosureActionLog> fetchReviewActionLogs(Integer disclosureId, List<String> actionTypeCodes);

    /**
     * This method is used to fetch OPA Action Log Type
     * @param actionLogTypeCode
     * @return OPAActionLogType
     */
    OPAActionLogType getOPAActionType(String actionLogTypeCode);
}
