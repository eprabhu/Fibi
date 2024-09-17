package com.polus.fibicomp.globalentity.service;

import java.util.List;

import com.polus.fibicomp.coi.dto.EntityActionLogDto;
import com.polus.fibicomp.globalentity.dto.ActionLogRequestDTO;

public interface EntityActionLogService {

    /**
     * This method is used to build and save Entity Action Log
     * @param actionLogTypeCode
     * @param coiEntity
     * @param comment
     */
    void saveEntityActionLog(String actionLogTypeCode, ActionLogRequestDTO dto, String comment);

    /**
     * This method is used to fetch all entity action log based on entity number
     * @param coiEntityDto
     * @return
     */
    List<EntityActionLogDto> fetchAllEntityActionLog(Integer entityId);

}
