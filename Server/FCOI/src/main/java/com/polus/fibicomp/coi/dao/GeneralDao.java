package com.polus.fibicomp.coi.dao;

import java.util.List;

/**
 * General Dao
 */
public interface GeneralDao {

    List<String> fetchAllCoiRights(String personId);

    /**
     * This method is used to check a person is in reviewer table
     *
     * @param personId
     * @return
     */
    boolean isPersonInReviewer(String personId);
}
