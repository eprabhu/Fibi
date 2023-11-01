package com.polus.fibicomp.coi.repository;

import java.util.List;

import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionType;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;
import com.polus.fibicomp.opa.pojo.OPAActionLog;
import com.polus.fibicomp.opa.pojo.OPAActionLogType;
import com.polus.fibicomp.coi.pojo.PersonEntityActionType;
import com.polus.fibicomp.coi.pojo.PersonEntityActionLog;

public interface ActionLogDao {

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
     * This method is used to get PersonEntityActionType by actionLogTypeCode
     * @param actionLogTypeCode String
     * @return PersonEntityActionType
     */
    PersonEntityActionType getPersonEntityActionType(String actionLogTypeCode);

    /**
     * This method is fetches all the Person Entity action logs of current version and of previous versions
     * @param personEntityDto
     * @return List<PersonEntityActionLog>
     */
    List<PersonEntityActionLog> fetchPersonEntityActionLog(PersonEntityDto personEntityDto);

    /**
     * This method deletes person entity action log by person entity id
     * @param personEntityId
     */
    void deletePersonEntityActionLog(Integer personEntityId);

    /**
     * This method is used to fetch OPA Action Log Type
     * @param actionLogTypeCode
     * @return OPAActionLogType
     */
    OPAActionLogType getOPAActionType(String actionLogTypeCode);

    /**
     * This method is used to fetch OPA disclosure action logs based on id and isStatusIn
     * @param opaDisclosureId
     * @param actionTypeCodes
     * @param isStatusIn
     * @return
     */
    List<OPAActionLog> fetchOpaDisclosureActionLogsBasedOnId(Integer opaDisclosureId, List<String> actionTypeCodes, boolean isStatusIn);
}
