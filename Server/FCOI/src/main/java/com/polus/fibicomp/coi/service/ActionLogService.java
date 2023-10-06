package com.polus.fibicomp.coi.service;

import java.util.List;

import com.polus.fibicomp.opa.dto.OPACommonDto;
import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.EntityActionLogDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;

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
     * @param actionLogCodes
     * @return
     */
    List<EntityActionLogDto> fetchEntityActionLog(Integer entityId, List<String> actionLogCodes);

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

	List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto);

	/**
     * This method is used to fetch review history
     * @param disclosureId
     * @return
     */
	ResponseEntity<Object> getReviewHistoryById(Integer disclosureId);

    /**
     * This method is used to save OPA Action log
     * @param actionLogTypeCode
     * @param opaCommonDto
     */
    void saveOPAActionLog(String actionLogTypeCode, OPACommonDto opaCommonDto);
}
