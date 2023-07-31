package com.polus.fibicomp.coi.service;

import com.polus.fibicomp.coi.pojo.CoiEntity;

public interface ActionLogService {

    /**
     * This method is used to build and save Entity Action Log
     * @param actionLogTypeCode
     * @param coiEntity
     * @param comment
     */
    void saveEntityActionLog(String actionLogTypeCode, CoiEntity coiEntity, String comment);
}
