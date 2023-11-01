package com.polus.fibicomp.coi.service;

import org.springframework.http.ResponseEntity;

public interface GeneralService {

    /**
     * This method is used to fetch Admin groups and Persons
     * @param moduleCode
     * @return List<Object>
     */
    ResponseEntity<Object> fetchAdminGroupsAndPersons(Integer moduleCode);

    /**
     * This method is used to fetch rights
     *
     * @return List<String> rights
     */
	ResponseEntity<Object> fetchAllCoiOpaRights();

}
