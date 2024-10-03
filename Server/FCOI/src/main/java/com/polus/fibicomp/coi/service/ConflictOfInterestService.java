package com.polus.fibicomp.coi.service;


import java.util.List;
import java.util.Map;

import javax.validation.Valid;

import com.polus.fibicomp.coi.clients.model.EmailNotificationDto;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.core.inbox.pojo.Inbox;
import com.polus.core.pojo.FileType;
import com.polus.fibicomp.coi.dto.CoiAssignTravelDisclosureAdminDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiTravelHistoryDto;
import com.polus.fibicomp.coi.dto.CommonRequestDto;
import com.polus.fibicomp.coi.dto.NotesDto;
import com.polus.fibicomp.coi.dto.NotificationBannerDto;
import com.polus.fibicomp.coi.dto.NotificationDto;
import com.polus.fibicomp.coi.dto.PersonAttachmentDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiTravelConflictHistory;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.pojo.Notes;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.globalentity.pojo.Entity;

@Transactional
@Service(value = "conflictOfInterestService")
public interface ConflictOfInterestService {

	/**
	 * This method is used for get list of entity table values(enpoint for entity)
	 * @param vo
	 * @return A list of entity
	 */
	List<Entity> searchEntity(ConflictOfInterestVO vo);

	/**
	 * This method is used for get lookup table of sfi
	 * @return EntityStatus, EntityType, CoiFinancialEntityRelType
	 */
	ResponseEntity<Object> loadAddSFILookups();

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
	ResponseEntity<Object> saveOrUpdateCoiReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for get CoiReview based on disclosureId
	 * @param disclosureDto
	 * @return List of CoiReview
	 */
	List<CoiReview> getCoiReview(CoiDisclosureDto disclosureDto);

	/**
	 * This method is used for Start review
	 * @param vo
	 * @return CoiReview
	 */
	ResponseEntity<Object> startReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for complete review
	 * @param vo
	 * @return CoiReview
	 */
	ResponseEntity<Object> completeReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for delete review
	 * @param coiReviewId
	 * @return
	 */
	ResponseEntity<Object> deleteReview(Integer coiReviewId);

	/**
	 * This method is used for complete Disclosure
	 * @param disclosureId
	 * @param disclosureNumber
	 * @return Disclosure details
	 */
	ResponseEntity<Object> completeDisclosureReview(Integer disclosureId, Integer disclosureNumber);

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
	 * This method is used to create Entity
	 * @param vo
	 * @return vo
	 */
	ResponseEntity<Object> saveOrUpdateEntity(ConflictOfInterestVO vo);

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

	String getCOIDashboardCount(CoiDashboardVO vo);

	ResponseEntity<Object> getAllEntityList(ConflictOfInterestVO vo);

	ResponseEntity<Object> setEntityStatus(ConflictOfInterestVO vo);

	ResponseEntity<Object> createCoiTravelDisclosure(ConflictOfInterestVO vo);

	ResponseEntity<Object> getAllCoiTravelDisclosureList();

	ResponseEntity<Object> loadTravelDisclosure(Integer travelDisclosureId);

	ResponseEntity<Object> getCoiProjectTypes();

	/**
	 *
	 * @param vo
	 * @return
	 */
	ResponseEntity<Object> getCOIReviewerDashboard(CoiDashboardVO vo);
	
	public ResponseEntity<Object> getCoiEntityDetails(Integer personEntityId);

	ResponseEntity<Object> getValidPersonRelationshipLookUp();

	ResponseEntity<Object> loadTravellerTypesLookup();
	
	ResponseEntity<Object> loadTravelStatusTypesLookup();

	/**
	 * This method is used to check a enitity is added againt a person or not
	 *
	 * @param entityNumber entityNumber
	 * @return
	 */
	ResponseEntity<Object> checkEntityAdded(Integer entityNumber);
    
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
	 *This method is used to activate/inactive entity by checking the entity is used anywhere.
	 * If entity is linked on a SFI new version will be created
	 * @param coiEntityDto
	 * @return
	 */
	ResponseEntity<Object> activateOrInactivateEntity(CoiEntityDto coiEntityDto);

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

	List<CoiTravelHistoryDto> loadTravelDisclosureHistory(String personId, Integer entityNumber);

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
//	ResponseEntity<Object> modifyDisclosureRisk(CoiDisclosureDto disclosureDto);

	/**
	 * This method is used to fetch all disclosure risk
	 * @return
	 */
//	ResponseEntity<Object> fetchAllDisclosureRisk();

	/**
	 * This method is used to fetch disclosure history
	 * @param actionLogDto
	 * @return
	 */
//	ResponseEntity<Object> fetchDisclosureHistory(DisclosureActionLogDto actionLogDto);

	/**
	 * This method is used to fetch section type codes
	 * @param conflictOfInterestVO
	 * @return
	 */
	ResponseEntity<Object> getCoiSectionsTypeCode(ConflictOfInterestVO conflictOfInterestVO);

	ResponseEntity<Object> modifyTravelDisclosureRisk(CoiTravelDisclosureDto travelDisclosureDto);

	ResponseEntity<Object> fetchTravelDisclosureHistory(TravelDisclosureActionLogDto actionLogDto);

	String deleteReviewCommentTag(Integer coiReviewCommentTagId);

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
    
    List<PersonAttachmentDto> loadAllAttachmentsForPerson(String personId);

	/**
	 * This method fetches all person entity with entity and relationship of a person
	 * @param requestDto
	 * @return
	 */
    ResponseEntity<Object> getEntityWithRelationShipInfo(CommonRequestDto requestDto);

    /**
	 * This method fetches SFI relationship details
	 * @return
	 */
	ResponseEntity<Object> getSFIRelationshipDetails();

	/**
	 * This method used to complete disclosure reviews
	 * @param disclosureIdNumberMap
	 * @return
	 */
	ResponseEntity<Object> completeDisclosureReviews(Map<Integer, Integer> disclosureIdNumberMap);

	/**
	 * This method is used to Check if the risk status of the disclosure has been modified
	 * @return
	 */
//	ResponseEntity<Object> checkDisclosureRiskStatus(CoiDisclosureDto disclosureDto);

	/**
	 * This method is used to Check if the risk status of the Entity has been modified
	 * @return
	 */
	ResponseEntity<Object> checkEntityRiskStatus(CoiEntityDto entityDto);

	/**
	 * This method is used to Check if the risk status of the Travel Disclosure has been modified
	 * @return
	 */
	ResponseEntity<Object> checkTravelDisclosureRiskStatus(CoiTravelDisclosureDto travelDisclosureDto);

	/**
	 * This method is used to build & push message to queue
	 * @param actionType
	 * @param moduleItemKey
	 * @param moduleSubItemKey
	 * @param additionDetails
	 */
	void processCoiMessageToQ(String actionType, Integer moduleItemKey, Integer moduleSubItemKey, Map<String, String> additionDetails);

	/**
	 * Defining action type based on disclosure type code
	 * @param fcoiType
	 * @param actionTypes
	 * @return
	 */
	String getDisclosureActionType(String fcoiType, Map<String, String> actionTypes);

	/**
	 * For notifying person
	 * @param notificationDto
	 * @return
	 */
	ResponseEntity<Object> projectPersonNotify(NotificationDto notificationDto);
	
	/**
	 * fetches the required parameters
	 * @return
	 */
	Map<String, List<FileType>> fetchRequiredParams();

	/**
	 * Fetch Email preview
	 * @param emailNotificationDto
	 * @return
	 */
	ResponseEntity<Object> getEmailPreview(EmailNotificationDto emailNotificationDto);

}
