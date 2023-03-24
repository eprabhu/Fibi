package com.polus.fibicomp.adminportal.service;

import com.polus.fibicomp.adminportal.dto.SponsorHierarchyDto;
import com.polus.fibicomp.common.dto.ResponseData;
import com.polus.fibicomp.search.pojo.SearchResult;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * SponsorHierarchy Service
 *
 * @createdBy Ajin
 * @date 24 Nov 2021
 */

@Service
public interface SponsorHierarchyService {

    /**
     * Create Sponsor Hierarchy
     *
     * @param sponsorHierarchy
     * @return
     */
    ResponseEntity<ResponseData> createSponsorHierarchy(SponsorHierarchyDto sponsorHierarchy);

    /**
     * Get Sponsor Hierarchy either by root sponsor hierarchy or by group name or sponsor name
     *
     * @param searchResult
     * @return
     */
    ResponseEntity<ResponseData> getSponsorHierarchy(SearchResult searchResult);

    /**
     * Update Sponsor Hierarchy
     *
     * @param sponsorHierarchyList
     * @return
     */
    ResponseEntity<ResponseData> updateSponsorHierarchy(SponsorHierarchyDto sponsorHierarchyList);

    /**
     * Delete Sponsor Hierarchy
     *
     * @param sponsorGroupId
     * @return
     */
    ResponseEntity<ResponseData> deleteSponsorHierarchy(Integer sponsorGroupId);

    /**
     * Get All Not Added Sponsors In Sponsor Hierarchy Table
     *
     * @param rootGroupId
     * @param searchWord
     * @return
     */
    ResponseEntity<Object> getNotAddedSponsorsInSH(Integer rootGroupId, String searchWord);

    /**
     * Get Sponsor Hierarchies
     *
     * @return
     */
    ResponseEntity<ResponseData> getSponsorHierarchies();

    /**
     * Get All Sponsor hierarchy groups
     *
     * @param searchWord
     * @return
     */
    ResponseEntity<Object> getAllSponsorHierarchyGroups(String searchWord);
}
