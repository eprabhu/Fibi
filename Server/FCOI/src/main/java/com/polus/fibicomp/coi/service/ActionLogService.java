package com.polus.fibicomp.coi.service;

import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;

public interface ActionLogService {

    /**
     * This method is used to build and save Entity Action Log
     * @param actionLogTypeCode
     * @param coiEntity
     * @param comment
     */
    void saveEntityActionLog(String actionLogTypeCode, CoiEntity coiEntity, String comment);

	void saveDisclsoureActionLog(DisclosureActionLogDto actionLogDto);

	ResponseEntity<Object> getDisclosureHistoryById(Integer disclosureId);
	
	void saveTravelDisclosureActionLog(TravelDisclosureActionLogDto actionLogDto);

}
