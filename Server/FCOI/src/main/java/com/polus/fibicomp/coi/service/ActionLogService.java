package com.polus.fibicomp.coi.service;

import com.polus.fibicomp.coi.dto.EntityActionLogDto;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.EntityActionLog;

import java.util.List;

public interface ActionLogService {

    /**
     * This method is used to build and save Entity Action Log
     * @param actionLogTypeCode
     * @param coiEntity
     * @param comment
     */
    void saveEntityActionLog(String actionLogTypeCode, CoiEntity coiEntity, String comment);

    /**
     *
     * @param actionLogDto
     */
	void saveDisclosureActionLog(DisclosureActionLogDto actionLogDto);

    /**
     *
     * @param disclosureId
     * @return
     */
	ResponseEntity<Object> getDisclosureHistoryById(Integer disclosureId);

	void saveTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto);

    /**
     * This method is used to fetch entity action log by following @params
     * @param entityId
     * @param actionLogCode
     * @return
     */
    List<EntityActionLogDto> fetchEntityActionLog(Integer entityId, String actionLogCode);

    /**
     * This method is used to fetch all entity action log based on entity number
     * @param coiEntityDto
     * @return
     */
    List<EntityActionLogDto> fetchAllEntityActionLog(CoiEntityDto coiEntityDto);

    /**
     * This method is used to fetch disclosure action log
     * @param actionLogDto
     * @return
     */
    List<DisclosureActionLog> fetchDisclosureActionLog(DisclosureActionLogDto actionLogDto);
	
	ResponseEntity<Object> getTravelDisclosureHistoryById(Integer travelDisclosureId);
}
