package com.polus.fibicomp.fcoiDisclosure.dao;

import com.polus.fibicomp.coi.dto.*;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.fcoiDisclosure.dto.ProjectEntityRequestDto;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiConflictStatusType;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclosureFcoiType;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.fcoiDisclosure.dto.SFIJsonDetailsDto;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjectEntityRel;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjects;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiProjConflictStatusType;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiRiskCategory;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

public interface FcoiDisclosureDao {

    /**
     * This method is used for save disclosure details
     * @param coiDisclosure
     * @return
     */
    public CoiDisclosure saveOrUpdateCoiDisclosure(CoiDisclosure coiDisclosure);

    /**
     * This method is used to load disclosure
     *
     * @param disclosureId
     * @return
     */
    CoiDisclosure loadDisclosure(Integer disclosureId);

    /**
     * This method is used to modify disclosure risk
     *
     * @param coiDisclosureDto
     * @return
     */
    boolean isDisclosureRiskAdded(CoiDisclosureDto coiDisclosureDto);

    /**
     * This method is used to get RiskCategory by category code
     *
     * @param riskCategoryCode
     * @return
     */
    CoiRiskCategory getRiskCategoryStatusByCode(String riskCategoryCode);

    /**
     * This method updates disclosure risk category
     *
     * @param coiDisclosureDto
     * @return
     */
    Timestamp updateDisclosureRiskCategory(CoiDisclosureDto coiDisclosureDto);

    /**
     * This method fetches all disclosure risk categories
     *
     * @return
     */
    List<CoiRiskCategory> fetchDisclosureRiskCategory();

    /**
     * This method is used to Check if the risk status of the disclosure has been modified
     *
     * @return
     */
    Boolean isDisclosureRiskStatusModified(String riskCategoryCode, Integer disclosureId);

    /**
     * This method returns disclosure projects
     *
     * @param disclosureId
     * @return
     */
    List<DisclosureProjectDto> getDisclosureProjects(Integer disclosureId);

    /**
     * This method used to fetch all disclosure conflict Status Types
     *
     * @param
     * @return list of Coi Disclosure Detail Statuses
     */
    List<CoiConflictStatusType> getCoiConflictStatusTypes();

    /**
     * This method used to fetch all project conflict Status Types
     *
     * @return
     */
    List<CoiProjConflictStatusType> getProjConflictStatusTypes();

    /**
     * This method is used to check master disclosure exists or not
     * @param personId
     * @return
     */
    boolean isMasterDisclosurePresent(String personId);

    /**
     * This method is used to generate Disclosure number
     * @return
     */
    Integer generateMaxDisclosureNumber();

    /**
     * This method is used to fetch FCOI Disclosure Type
     * @param coiTypeCode
     * @return
     */
    CoiDisclosureFcoiType getCoiDisclosureFcoiTypeByCode(String coiTypeCode);

    /**
     * Fcoi disclosure section types
     * @return
     */
    List<CoiSectionsType> fetchCoiSections();

    /**
     * This method is used to Check if the disclosure have reviewers assigned
     * @return
     */
    Boolean isReviewerAssigned(Integer disclosureId);

    /**
     * This method is used to Check if the reviewers in the disclosure have completed their reviews
     * @return
     */
    Boolean isReviewerReviewCompleted(Integer disclosureId);

    /**
     * This method is used for certify disclosure
     * @param coiDisclosure
     */
    void certifyDisclosure(CoiDisclosureDto coiDisclosure);

    /**
     * This method is used to validate conflicts and update
     *
     * @param disclosureId disclosureId
     * @return CoiConflictStatusType
     */
    CoiConflictStatusTypeDto validateConflicts(Integer disclosureId);

    /**
     * This method is used to sync disclosure risk
     * @param disclosureId
     * @param disclosureNumber
     * @return CoiRiskCategory
     */
    CoiRiskCategory syncDisclosureRisk(Integer disclosureId, Integer disclosureNumber);

    /**
     * This method fetches the project entity relation ids
     * @param disclosureId
     * @return
     */
    List<CoiDisclProjectEntityRel> getProjEntityRelationshipsByDisclId(Integer disclosureId);

    /**
     * Fetch Latest ConflictHistory conflict status code by project entity rel id
     * @param coiDisclProjectEntityRelId
     * @return
     */
    String getLatestConflHisStatusCodeByProEntRelId(Integer coiDisclProjectEntityRelId);

    /**
     * This method used to save conflict history
     * @param coiConflictHistory
     */
    void saveOrUpdateCoiConflictHistory(CoiConflictHistory coiConflictHistory);

    /**
     * This method used to update conflict status against entity & project
     * @param entityProjectRelation
     */
    void saveOrUpdateCoiDisclEntProjDetails(ProjectEntityRequestDto entityProjectRelation);

    /**
     * Fetch DisclProjectEntityRelIds
     * @param entityProjectRelation
     * @return
     */
    List<Integer> fetchDisclProjectEntityRelIds(ProjectEntityRequestDto entityProjectRelation);

    /**
     * This method is used to check if SFI is completed for a disclosure
     * @param personEntityId
     * @param disclosureId
     */
    Boolean isSFICompletedForDisclosure(Integer personEntityId, Integer disclosureId);

    /**
     *  This method checks the conflict is marked against project
     * @param
     * @return check if SFI Completed For Project
     */
    Boolean checkIsSFICompletedForProject(Integer moduleCode, Integer moduleItemId, Integer disclosureId);

    /**
     * This method is used to update disclosure header update details
     * @param disclosureId
     */
    Timestamp updateDisclosureUpdateDetails(Integer disclosureId);

    /**
     * This method fetches disclosure entity vs project relations
     * @param disclosureId
     * @return
     */
    List<CoiDisclEntProjDetailsDto> getDisclEntProjDetails(Integer disclosureId);

    /**
     * This method is used to check FCOI disclosure is exists or not
     * @param personId
     * @param versionStatus
     * @param fcoiTypeCode
     * @return
     */
    CoiDisclosure isFCOIDisclosureExists(String personId, String fcoiTypeCode, String versionStatus);

    /**
     * This method is used for evaluate DisclosureQuestionnaire
     * @param moduleCode
     * @param submoduleCode
     * @param moduleItemKey
     * @return Boolean value
     */
    boolean evaluateDisclosureQuestionnaire(Integer moduleCode, Integer submoduleCode, String moduleItemKey);

    /**
     * This
     * @param projectConflictStatusCode
     * @param disclosureDetailsId
     * @return
     */
    boolean isDisclEntProjConflictAdded(String projectConflictStatusCode, Integer disclosureDetailsId);

    /**
     * This method fetches CoiDisclProjectEntityRel by id
     * @param coiDisclProjectEntityRelId
     * @return
     */
    CoiDisclProjectEntityRel getCoiDisclProjectEntityRelById(Integer coiDisclProjectEntityRelId);

    /**
     * This method used to update conflict status against project
     * @param conflictStatusCode
     * @param coiDisclProjectEntityRelId
     * @return Timestamp
     */
    Timestamp updateCoiDisclEntProjDetails(String conflictStatusCode, Integer coiDisclProjectEntityRelId);

    /**
     *
     * @param disclosureId
     * @param
     * @return list of Coi Disclosure Details
     */
    List<CoiDisclProjectEntityRel> getProjectRelationshipByParam(Integer moduleCode, Integer moduleItemId, String loginPersonId, Integer disclosureId);

    /**
     * This method return count of SFIs in a disclosure
     * @param disclosureId
     * @return
     */
    Integer getNumberOfSFIBasedOnDisclosureId(Integer disclosureId);

    /**
     * This method is used to validate
     * 1) If selected project expired date passed
     * 2) Is part of any pending project disclosure
     * 3) If the selected project is part of any active/ pending  FCOi disclosure
     *
     * @param personId
     * @param moduleCode
     * @param moduleItemKey
     * @return Map of validated values
     */
    Map<String, Object> validateProjectDisclosure(String personId, Integer moduleCode, String moduleItemKey);

    /**
     * This method  used for convertJsonStringToListMap
     * @param jsonString
     * @return
     */
    List<Map<Object, Object>> convertJsonStringToListMap(String jsonString);

    /**
     * This method is used to save or update coiDisclProjects
     * @param coiDisclProjects
     */
    void saveOrUpdateCoiDisclProjects(CoiDisclProjects coiDisclProjects);


    List<CoiDisclProjects> syncFcoiDisclosureProjects(Integer disclosureId, Integer disclosureNumber, String loginPersonId);

    List<SFIJsonDetailsDto> getPersonEntitiesByPersonId(String personId);

    void syncFcoiDisclProjectsAndEntities(Integer disclosureId, Integer disclosureNumber,Integer coiDisclProjectId, Integer moduleCode,
                                          String moduleItemKey, String sfiJsonArray, String loginPersonId);

    /**
     * This method is used to Check if Admin is assigned
     * @param disclosureId
     * @return
     */
    boolean isAdminPersonOrGroupAdded(Integer disclosureId);

    /**
     * This method is used to check given admin person and group is added or not
     * @param adminGroupId
     * @param adminPersonId
     * @param disclosureId
     */
    boolean isSameAdminPersonOrGroupAdded(Integer adminGroupId, String adminPersonId, Integer disclosureId);

    /**
     *This method updates the assign admin/group and changes the disclosure status to 3 review in progress
     *
     * @param adminGroupId
     * @param adminPersonId
     * @param disclosureId
     * @return Update Timestamp
     */
    Timestamp assignDisclosureAdmin(Integer adminGroupId, String adminPersonId, Integer disclosureId);

    /**
     * This method is used to sync the projects/SFIs with disclosure
     * @param disclosureId
     * @param disclosureNumber
     */
    void syncFCOIDisclosure(Integer disclosureId, Integer disclosureNumber);

    /**
     * Disclosure validation before submission.
     * @param disclosureId
     * @param personId
     * @return
     */
    List<COIValidateDto> evaluateValidation(Integer disclosureId, String personId);

    /**
     * This method is used to check disclosure project sfi sync is needed
     * @param disclosureId
     * @return
     */
    boolean isProjectSFISyncNeeded(Integer disclosureId);

    /**
     * This method is used to update sync needed flag
     * @param disclosureId
     * @param syncNeeded
     */
    void updateDisclosureSyncNeeded(Integer disclosureId, boolean syncNeeded);

    /**
     * This method is used to update sync needed flag
     * @param personEntityId
     * @param syncNeeded
     */
    void updateDisclosureSyncNeededByPerEntId(Integer personEntityId, boolean syncNeeded);

    /**
     * This method is used to update the disclosures sync needed flag by certain condition
     * @param projectDto
     */
    void updateFcoiDisclSyncNeedStatus(DisclosureProjectDto projectDto);
}
