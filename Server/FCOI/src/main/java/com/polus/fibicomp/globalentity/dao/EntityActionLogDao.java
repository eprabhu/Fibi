package com.polus.fibicomp.globalentity.dao;

import java.util.List;

import com.polus.fibicomp.globalentity.pojo.EntityActionLog;
import com.polus.fibicomp.globalentity.pojo.EntityActionType;

public interface EntityActionLogDao {

	void saveEntityActionLog(EntityActionLog entityActionLog);

    /**
     * @param actionLogTypeCode
     * @return
     */
    EntityActionType getEntityActionType(String actionLogTypeCode);

    /**
     * @param entityId
     * @return
     */
    List<EntityActionLog> fetchAllEntityActionLog(Integer entityId);

}
