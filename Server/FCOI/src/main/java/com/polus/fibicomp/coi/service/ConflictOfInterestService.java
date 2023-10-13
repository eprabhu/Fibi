package com.polus.fibicomp.coi.service;


import java.util.List;

import javax.validation.Valid;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.coi.dto.CoiAssignTravelDisclosureAdminDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.NotesDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiTravelHistoryDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.dto.NotificationBannerDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiTravelConflictHistory;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.pojo.Notes;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.coi.pojo.Attachments;

@Transactional
@Service(value = "conflictOfInterestService")
public interface ConflictOfInterestService {

	/**
	 * This method is used to create Disclosure
	 * @param vo
	 * @return created disclosure details, person details and number of sfi
	 */
	ResponseEntity<Object> createDisclosure(ConflictOfInterestVO vo);

	/**
	 * This method is used to get list of disclosure
	 * @param disclosureId
	 * @return
	 */
	ResponseEntity<Object> loadDisclosure(Integer disclosureId);

	/**
	 * This method is used to get list of Disclosure Relations.
	 * @param vo
	 * @return A list of Disclosure Relations.
	 */
	String getDisclosureRelations(ConflictOfInterestVO vo);

	/**
	 * This method is used for get list of sfi of a person
	 * @param vo
	 * @return A list of sfi
	 */
	ResponseEntity<Object> getSFIOfDisclosure(ConflictOfInterestVO vo);

	/**
	 * This method is used for get list of entity table values(enpoint for entity)
	 * @param vo
	 * @return A list of entity
	 */
	List<CoiEntity> searchEntity(ConflictOfInterestVO vo);

	/**
	 * This method is used for get lookup table of sfi
	 * @return EntityStatus, EntityType, CoiFinancialEntityRelType
	 */
	ResponseEntity<Object> loadAddSFILookups();

	/**
	 * This method is used to get list of sfi details based on coiFinancialEntityId
	 * @param coiFinancialEntityId
	 * @return A list of sfi details
	 */
	ResponseEntity<Object> getSFIDetails(Integer coiFinancialEntityId);

	/**
	 * This method is used to create CoiFinancialEntityDetails
	 * @param personEntityRelationship
	 * @return COIFinancialEntityDetails
	 */
	List<PersonEntityRelationship> saveOrUpdatePersonEntityRelationship(PersonEntityRelationship personEntityRelationship);

	/**
	 * This method is used to create SFI
	 * @param vo
	 * @return vo
	 */
	ResponseEntity<Object> createSFI(ConflictOfInterestVO vo);

	/**
	 * This method is used to certifyDisclosure
	 * @param coiDisclosure
	 * @return vo
	 */
	ResponseEntity<Object> certifyDisclosure(CoiDisclosure coiDisclosure);

	/**
	 * This method is used to save disclosure Relationship details.
	 * @return vo
	 */
	ConflictOfInterestVO saveEntityProjectRelation(ConflictOfInterestVO vo);

	/**
	 * This method is used to get disclosure Relationship by proposalId.
	 * @return vo
	 */
	ResponseEntity<Object> getDisclosureProjectRelations(ConflictOfInterestVO vo);

	/**
	 * This method is used to get sfi relation all conflicts are completed or not.
	 * @return vo
	 */
	String checkSFICompleted(ConflictOfInterestVO vo);

	/**
	 * This method is used to revise Coi disclosure
	 * @return counts
	 */
	String reviseDisclosure(ConflictOfInterestVO vo);

	/**
	 * This method is used for evaluate DisclosureQuestionnaire
	 * @param vo
	 * @return boolean value
	 */
	boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo);

	/**
	 * This method is used to get disclosure details for SFI
	 * @param coiFinancialEntityId
	 * @return list of SFI details
	 */
	ResponseEntity<Object> getDisclosureDetailsForSFI(Integer coiFinancialEntityId);

	/**
	 * This method is used for save review details
	 * @param vo
	 * @return saved CoiReview object
	 */
	CoiReview saveOrUpdateCoiReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for get CoiReview based on disclosureId
	 * @param disclosureId
	 * @return List of CoiReview
	 */
	List<CoiReview> getCoiReview(Integer disclosureId);

	/**
	 * This method is used for Start review
	 * @param vo
	 * @return CoiReview
	 */
	CoiReview startReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for add comments
	 * @param files
	 * @param formDataJSON
	 * @return success message
	 */
	ResponseEntity<Object> saveOrUpdateCoiReviewComments(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used for get review comment details
	 * @param vo
	 * @return comment details
	 */
	ResponseEntity<Object> loadCoiReviewComments(ConflictOfInterestVO vo);

	/**
	 * This method is used for complete review
	 * @param vo
	 * @return CoiReview
	 */
	CoiReview completeReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for delete review
	 * @param coiReviewId
	 * @return
	 */
	ResponseEntity<Object> deleteReview(Integer coiReviewId);

	/**
	 * This method is used for delete comment
	 * @param coiReviewCommentId
	 * @return String
	 */
	String deleteReviewComment(Integer coiReviewCommentId);

	/**
	 * This method is used for download Attachment
	 * @param attachmentId
	 * @return
	 */
	ResponseEntity<byte[]> downloadCoiReviewAttachment(Integer attachmentId);

	/**
	 * This method is used for delete Attachment
	 * @param coiReviewCommentAttId
	 * @return String
	 */
	String deleteReviewAttachment(Integer coiReviewCommentAttId);

	/**
	 * This method is used for complete Disclosure
	 * @param disclosureId
	 * @param disclosureNumber
	 * @return Disclosure details
	 */
	ResponseEntity<Object> completeDisclosureReview(Integer disclosureId, Integer disclosureNumber);

	/**
	 * This method is used for update ProjectConflictStatus
	 * @param disclEntProjDetails
	 * @return CoiDisclosureOldDetails
	 */
	CoiDisclEntProjDetails updateProjectConflictStatus(CoiDisclEntProjDetails disclEntProjDetails);

	/**
	 * This method is used for get Project Conflict History
	 * @param coiConflictHistoryId
	 * @return list of CoiConflictHistory
	 */
	List<CoiConflictHistory> getCoiConflictHistory(Integer coiConflictHistoryId);

	/**
	 * This method is used to get proposals for Disclosure
	 * @param searchString
	 * @return list of proposals
	 */
	String loadProposalsForDisclosure(String searchString);

	String loadAwardsForDisclosure(String searchString);

	/**
	 * @param vo
	 * @return
	 */
	String loadDisclosureHistory(ConflictOfInterestVO vo);

	/**
	 * This method is ued to save a single entity project relation
	 *
	 * @param vo
	 * @return
	 */
	ConflictOfInterestVO saveSingleEntityProjectRelation(ConflictOfInterestVO vo);

	/**
	 * This method is used to create Entity
	 * @param vo
	 * @return vo
	 */
	ResponseEntity<Object> saveOrUpdateCoiEntity(ConflictOfInterestVO vo);

	/**
	 * This method is used to get entity details based on coiEntityId
	 * @param coiEntityId
	 * @return A list of entity details
	 */
	ResponseEntity<Object> getEntityDetails(Integer coiEntityId);

	ResponseEntity<Object> getActiveDisclosure();

	/**
	 * This method is used to get COI dasboard data .
	 * @param vo -
	 * @return A list of dashboard COI data.
	 */
	String getCOIDashboard(CoiDashboardVO vo);

	/**
	 * This method is used to get COI Admin dasboard data .
	 * @param vo -
	 * @return A list of dashboard COI data.
	 */
	String getCOIAdminDashboard(@Valid CoiDashboardVO vo);

	/**
	 * This method is used to get SFI dasboard data .
	 * @param vo -
	 * @return A list of dashboard SFI data.
	 */
	String getSFIDashboard(CoiDashboardVO vo);

	String getCOIDashboardCount(CoiDashboardVO vo);

	ResponseEntity<Object> getAllEntityList(ConflictOfInterestVO vo);

	ResponseEntity<Object> setEntityStatus(ConflictOfInterestVO vo);

	ResponseEntity<Object> getAllSystemEntityList(CoiDashboardVO vo);

	ResponseEntity<Object> createCoiTravelDisclosure(ConflictOfInterestVO vo);

	ResponseEntity<Object> getAllCoiTravelDisclosureList();

	ResponseEntity<Object> loadTravelDisclosure(Integer travelDisclosureId);

	ResponseEntity<Object> getCoiProjectTypes();

	ResponseEntity<Object> getPersonEntityDashboard(CoiDashboardVO vo);


	/**
	 *
	 * @param vo
	 * @return
	 */
	ResponseEntity<Object> getCOIReviewerDashboard(CoiDashboardVO vo);
	
	public ResponseEntity<Object> getCoiEntityDetails(Integer personEntityId);

	public ResponseEntity<Object> getPersonEntityDetails(Integer personEntityId);

	ResponseEntity<Object> getRelatioshipDetails(String tabName);

	ResponseEntity<Object> getPersonEntityRelationship(ConflictOfInterestVO vo);
	
	ResponseEntity<Object> loadTravellerTypesLookup();
	
	ResponseEntity<Object> loadTravelStatusTypesLookup();

	/**
	 * This method is used to check a enitity is added againt a person or not
	 *
	 * @param entityId Entity Id
	 * @return
	 */
	ResponseEntity<Object> checkEntityAdded(Integer entityId);

	/**
	 * Validate
	 * 1) If selected project expired date passed
	 * 2) Is part of any pending project disclosure
	 * 3) If the selected project is part of any active/ pending  FCOi disclosure
	 *
	 * @param moduleCode
	 * @param moduleItemId
	 * @return
	 */
    ResponseEntity<Object> validateDisclosure(Integer moduleCode, String moduleItemId);

	/**
	 * This method is used to assign admin group or admin person
	 *
	 * @param dto
	 * @return
	 */
	ResponseEntity<Object> assignDisclosureAdmin(CoiDisclosureDto dto);

	/**
	 * This method is used to validate conflicts and update
	 *
	 * @param disclosureId
	 * @return
	 */
    ResponseEntity<Object> validateConflicts(Integer disclosureId);
    
    /**
	 * This method is used to assign admin group or admin person for travel disclosures
	 *
	 * @param dto
	 * @return
	 */
	ResponseEntity<Object> assignTravelDisclosureAdmin(CoiAssignTravelDisclosureAdminDto dto);
	
	/**
	 * This method is used to certifyTravelDisclosure
	 * @param vo
	 * @return vo
	 */
	ResponseEntity<Object> submitTravelDisclosure(ConflictOfInterestVO vo);
	
	/**
	 * This method is used to certifyDisclosure
	 * @param vo
	 * @return vo
	 */
	ResponseEntity<Object> certifyTravelDisclosure(ConflictOfInterestVO vo);
	
	ResponseEntity<Object> withdrawTravelDisclosure(Integer travelDisclosureId, String description);
	
	ResponseEntity<Object> approveTravelDisclosure(Integer travelDisclosureId, String description);
	
	ResponseEntity<Object> returnTravelDisclosure(Integer travelDisclosureId, String description);

	/**
	 * This method is used to evaluate validation conditions:
	 * 1.If SFI has to be defined based on questionnaire evaluation.
	 * 2.Is there any SFI's with relationship not defined.
	 * 3.Is there any SFI in draft status
	 */
	ResponseEntity<Object> evaluateValidation(Integer disclosureId);

	ResponseEntity<Object> getProjConflictStatusType();

	/**
	 *This method is used to activate/inactive entity by checking the entity is used anywhere.
	 * If entity is linked on a SFI new version will be created
	 * @param coiEntityDto
	 * @return
	 */
	ResponseEntity<Object> activateOrInactivateEntity(CoiEntityDto coiEntityDto);

	/**
	 *This method is used to activate/inactive  person entity
	 * @param personEntityDto
	 * @return
	 */
	ResponseEntity<Object> activateOrInactivatePersonEntity(PersonEntityDto personEntityDto);

	ResponseEntity<Object> updateProjectRelationship(ConflictOfInterestVO vo);

	/**
	 * This method is used to delete Person entity
	 * @param personEntityId
	 * @return
	 */
	ResponseEntity<Object> deletePersonEntity(Integer personEntityId);

	/**
	 * This method is used to fetch all entity relationship types
	 * @return
	 */
	ResponseEntity<Object> fetchAllRelationshipTypes();

	/**
	 * This method is used to approve Entity
	 * @param entityRelationship
	 * @return
	 */
	ResponseEntity<Object> approveEntity(EntityRelationship entityRelationship);

	/**
	 * This method is used to fetch disclosure history
	 * @param dashboardVO
	 */
	ResponseEntity<Object> getDisclosureHistory(CoiDashboardVO dashboardVO);

	/**
	 * This method is used to modify risk
	 * @param entityDto
	 * @return
	 */
	ResponseEntity<Object> modifyRisk(CoiEntityDto entityDto);

	/**
	 * This method is used to fetch all risk history of an entity
	 * @param entityId
	 * @return
	 */
	ResponseEntity<Object> fetchEntityRiskHistory(Integer entityId);

	/**
	 * This method is used to fetch Entity Action Log
	 * @param coiEntityDto
	 * @return
	 */
	ResponseEntity<Object> fetchEntityHistory(CoiEntityDto coiEntityDto);

	List<CoiTravelHistoryDto> loadTravelDisclosureHistory(String personId, Integer entityNumber);

	/**
	 * This method is used to update person entity
	 * @param personEntityDto
	 * @return
	 */
	ResponseEntity<Object> updatePersonEntity(PersonEntityDto personEntityDto);

	/**
	 * This method is used to delete Person Entity Relationship
	 * @param personEntityRelId
	 * @param personEntityId
	 * @return
	 */
	ResponseEntity<Object> deletePersonEntityRelationship(Integer personEntityRelId, Integer personEntityId);

	/**
	 * This method is used to validate and modify a person entity based on following condition
	 * 1) if the current version of person entity is not used anywhere, makes this version to draft
	 * 2) if the current version of person entity is used anywhere, creates a new version in draft status
	 * @param personEntityId
	 * @return person entity
	 */
	ResponseEntity<Object> modifyPersonEntity(Integer personEntityId);

	/**
	 * This method is used to finalize Person Entity
	 * @param personEntityDto
	 * @return
	 */
	ResponseEntity<Object> finalizePersonEntity(PersonEntityDto personEntityDto);

	ResponseEntity<Object> withdrawDisclosure(Integer disclosureId, String description);

    ResponseEntity<Object> returnDisclosure(Integer disclosureId, String description);

	ResponseEntity<Object> getTravelConflictStatusType();

	ResponseEntity<Object> manageTravelConflict(ConflictOfInterestVO vo);

	List<CoiTravelConflictHistory> getCoiTravelConflictHistory(Integer travelDisclosureId);

	/**
	 * This method is used to modify disclosure risk
	 * @param disclosureDto
	 * @return
	 */
	ResponseEntity<Object> modifyDisclosureRisk(CoiDisclosureDto disclosureDto);

	/**
	 * This method is used to fetch all disclosure risk
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
	 * This method is used to fetch section type codes
	 * @param ConflictOfInterestVO
	 * @return
	 */
	ResponseEntity<Object> getCoiSectionsTypeCode(ConflictOfInterestVO vo);

	ResponseEntity<Object> modifyTravelDisclosureRisk(CoiTravelDisclosureDto travelDisclosureDto);

	ResponseEntity<Object> fetchTravelDisclosureHistory(TravelDisclosureActionLogDto actionLogDto);

	String deleteReviewCommentTag(Integer coiReviewCommentTagId);

	/**
	 * This service is used to fetch the latest active person entity
	 * @param personEntityNumber
	 * @return
	 */
    ResponseEntity<Object> getSFILatestVersion(Integer personEntityNumber);

    /**
	 * This method is used to fetch disclosure attachment types
	 * @return
	 */
    ResponseEntity<Object> loadDisclAttachTypes();
    
    List<Inbox> fetchAllActiolListEntriesForBanners(NotificationBannerDto notifyBannerDto);
    
    List<Notes> fetchAllNotesForPerson(String personId);
    
    ResponseEntity<Object> saveOrUpdatePersonNote(NotesDto dto);
    
    Notes getNoteDetailsForNoteId(Integer noteId);
    
    ResponseEntity<Object> deleteNote(Integer noteId);
    
    ResponseEntity<Object> saveOrUpdateAttachments(MultipartFile[] files, String formDataJSON);
    
    List<Attachments> loadAllAttachmentsForPerson(String personId);

}
