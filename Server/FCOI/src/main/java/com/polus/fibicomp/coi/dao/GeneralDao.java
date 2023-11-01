package com.polus.fibicomp.coi.dao;

import java.util.List;

/**
 * General Dao
 */
public interface GeneralDao {

    /**
     * This method is used to check a person is in reviewer table
     *
     * @param personId
     * @return
     */
    boolean isPersonInReviewer(String personId);

	/**
     * This method is used to fetch rights
     *
     * @return List<String> rights
     */
	List<String> fetchAllCoiOpaRights(String loginPersonId);

    /**
     * This method checks the person is assigned to a OPA review
     * @param loginPersonId
     * @return
     */
    boolean isPersonInOPAReviewer(String loginPersonId);
}
