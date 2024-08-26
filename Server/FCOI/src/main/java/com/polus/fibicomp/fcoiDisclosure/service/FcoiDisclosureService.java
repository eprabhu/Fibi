package com.polus.fibicomp.fcoiDisclosure.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.dto.PersonEntityRelationshipDto;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.fcoiDisclosure.dto.ProjectEntityRequestDto;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface FcoiDisclosureService {

    /**
     * This method is used to create Disclosure
     * @param vo
     * @return created disclosure details, person details and number of sfi
     */
    ResponseEntity<Object> createDisclosure(CoiDisclosureDto vo) throws JsonProcessingException;

    /**
     * This method is used to get list of disclosure
     * @param disclosureId
     * @return
     */
    ResponseEntity<Object> loadDisclosure(Integer disclosureId);

    /**
     * This method is used to certifyDisclosure
     * @param coiDisclosure
     * @return vo
     */
    ResponseEntity<Object> certifyDisclosure(CoiDisclosureDto coiDisclosure);

    /**
     * This method is used to modify disclosure risk
     * @param disclosureDto
     * @return
     */
    ResponseEntity<Object> modifyDisclosureRisk(CoiDisclosureDto disclosureDto);

    /**
     * This method fetches all disclosure risks
     * @return
     */
    ResponseEntity<Object> fetchAllDisclosureRisk();

    /**
     * This method is used to fetch disclosure history
     * @param actionLogDto
     * @return
     */
    ResponseEntity<Object> fetchDisclosureHistory(DisclosureActionLogDto actionLogDto);

    /**
     * This method is used to Check if the risk status of the disclosure has been modified
     * @return
     */
    ResponseEntity<Object> checkDisclosureRiskStatus(CoiDisclosureDto disclosureDto);

    /**
     * This method returns projects of a disclosure
     * @param disclosureId
     * @return
     */
    ResponseEntity<Object> getDisclosureProjects(Integer disclosureId);

    /**
     * This method fetches disclosure lookups
     * @return
     */
    ResponseEntity<Object> getDisclosureLookups();

    /**
     * This method is used to fetch disclosure project entity relations
     * @param vo
     * @return
     */
    List<DisclosureProjectDto> getDisclProjectEntityRelations(ProjectEntityRequestDto vo);

    /**
     * This method is used to fetch disclosure project entity relations
     * @param vo
     * @return
     */
    List<PersonEntityRelationshipDto> getDisclosureEntityRelations(ProjectEntityRequestDto vo);

    /**
     * This method is used to save disclosure Relationship conflict details.
     * @return vo
     */
    ResponseEntity<Object> saveDisclosureConflict(ProjectEntityRequestDto vo);

    /**
     * This method is used to revise Coi disclosure
     * @return counts
     */
    ResponseEntity<Object> reviseDisclosure(ConflictOfInterestVO vo);

    /**
     * This method is used for evaluate DisclosureQuestionnaire
     * @param vo
     * @return boolean value
     */
    boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo);

    /**
     * This method updates conflicts
     * @param vo
     * @return
     */
    ResponseEntity<Object> updateProjectRelationship(ConflictOfInterestVO vo);

    /**
     * This method is used to validate conflicts and update
     * Validates
     * 1) If selected project expired date passed
     * 2) Is part of any pending project disclosure
     * 3) If the selected project is part of any active/ pending  FCOi disclosure
     *
     * @param disclosureId
     * @return
     */
    ResponseEntity<Object> validateConflicts(Integer disclosureId);

    /**
     * This method is used to validate to create a disclosure
     * @param disclosureDto
     * @return
     */
    ResponseEntity<Object> validateDisclosure(CoiDisclosureDto disclosureDto);

    /**
     * This method is used to update administrator
     * @param dto
     * @return
     */
    ResponseEntity<Object> assignDisclosureAdmin(CoiDisclosureDto dto);

    /**
     * This method syncs the projects with person entities(SFIs)
     * @param coiDisclosureDto
     */
    void syncFCOIDisclosure(CoiDisclosureDto coiDisclosureDto);

    /**
     * This method is used to evaluate validation conditions:
     * 1.If SFI has to be defined based on questionnaire evaluation.
     * 2.Is there any SFI's with relationship not defined.
     * 3.Is there any SFI in draft status
     */
    ResponseEntity<Object> evaluateValidation(Integer disclosureId, Integer disclosureNumber);

    /**
     * This method is used to update sync needed status from integration side
     * @param projectDto
     */
    void updateFcoiDisclSyncNeedStatus(DisclosureProjectDto projectDto);

    /**
     * This method is used to detach projects on certain conditions configured on procedure
     * @param projectDto
     */
    void detachFcoiDisclProject(DisclosureProjectDto projectDto);
}
