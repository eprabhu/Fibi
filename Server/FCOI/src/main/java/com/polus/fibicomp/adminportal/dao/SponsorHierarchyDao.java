package com.polus.fibicomp.adminportal.dao;

import com.polus.fibicomp.adminportal.pojo.SponsorHierarchy;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;

/**
 * SponsorHierarchy Repository
 *
 * @author Ajin
 * @since 24 Nov 2021
 */
@Service
public interface SponsorHierarchyDao {

    /**
     * Save or Update Sponsor Hierarchy
     *
     * @param sponHiearachy Sponsor Hierarchy Entity class
     */
    void saveSponsorHierarchy(SponsorHierarchy sponHiearachy);

    /**
     * Delete Sponsor Hierarchy by primary key
     *
     * @param sponsorGroupId primary key of Sponsor Hierarchy entity class
     */
    void deleteSponsorHierarchyById(Integer sponsorGroupId);

    /**
     * Get All Sponsor Hierarchies From Root By Root Group ID Null
     *
     * @return list of SponsorHierarchy objects if exists on db
     */
    List<SponsorHierarchy> getAllSponsorHierarchyFromRoot();

    /**
     * Find Sponsor Hierarchy By PK Sponsor Group ID
     *
     * @param sponsorGroupId Sponsor Group ID
     * @return SponsorHierarchy if entity found with sponsorGroupId
     */
    SponsorHierarchy findSponsorHierarchyBySponsorGroupIdOrderByOrderNumberDesc(Integer sponsorGroupId);

    /**
     * Update single field SponsorHierarchy
     *
     * @param sponsorGroupName Sponsor Group Name
     * @param orderNumber      Order Number
     * @param updateTimestamp  Update Timestamp
     * @param updateUser       Updated by User
     * @param sponsorGroupId   Sponsor Group ID
     */
    void updateSponsorHierarchy(String sponsorGroupName, Integer orderNumber,
                                Timestamp updateTimestamp, String updateUser,
                                Integer sponsorGroupId);

    /**
     * Finds All Not Added Sponsors In A Sponsor Hierarchy
     *
     * @param sponsorGroupId Sponsor Group ID
     * @return sponsorCode, sponsorName, sponsor type description, acronym based on sponsorGroupId and searchWord
     */
    List<Object> findAllSponsorsNotInSponsorHierarchy(Integer sponsorGroupId, String searchWord);


    /**
     * Get All Distinct Sponsor Group Name By Search Word
     *
     * @param searchWord Search Word
     * @return Sponsor Group Name and Sponsor Group ID if found search word on sponsorGroupName
     */
    List<Object> findAllDistinctSponsorGroupNameBySearchWord(String searchWord);

    /**
     * Get All Distinct Sponsor Group Name
     *
     * @return distinct sponsorGroupName
     */
    List<Object> findAllDistinctSponsorGroupName();
}
