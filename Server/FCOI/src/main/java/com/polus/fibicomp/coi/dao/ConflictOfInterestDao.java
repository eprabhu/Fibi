package com.polus.fibicomp.coi.dao;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dto.COIValidateDto;
import com.polus.fibicomp.coi.dto.CoiConflictStatusTypeDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.dto.DisclosureHistoryDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiConflictStatusType;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiDisclosureFcoiType;
import com.polus.fibicomp.coi.pojo.CoiDispositionStatusType;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiFileData;
import com.polus.fibicomp.coi.pojo.CoiProjConflictStatusType;
import com.polus.fibicomp.coi.pojo.CoiProjectAward;
import com.polus.fibicomp.coi.pojo.CoiProjectProposal;
import com.polus.fibicomp.coi.pojo.CoiProjectType;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewActivity;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiReviewStatusType;
import com.polus.fibicomp.coi.pojo.CoiRiskCategory;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.CoiTravelConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureTraveler;
import com.polus.fibicomp.coi.pojo.CoiTravelDocumentStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelReviewStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelerStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelerType;
import com.polus.fibicomp.coi.pojo.DisclComment;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.DisclosureActionType;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.pojo.EntityRelationshipType;
import com.polus.fibicomp.coi.pojo.EntityRiskCategory;
import com.polus.fibicomp.coi.pojo.EntityStatus;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelType;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;
import com.polus.fibicomp.coi.pojo.ValidPersonEntityRelType;
import com.polus.fibicomp.coi.pojo.EntityRelationshipType;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.Unit;

@Transactional
@Service
public interface ConflictOfInterestDao {

	/**
	 * This method is used for save disclosure details
	 * @param coiDisclosure
	 * @return
	 */
	public CoiDisclosure saveOrUpdateCoiDisclosure(CoiDisclosure coiDisclosure);

	/**
	 * This method is used for get disclosure details
	 * @param coiDisclosureId
	 * @return
	 */
	public CoiDisclosure loadDisclosure(Integer coiDisclosureId);

	/**
	 * This method is used for get number of sfi for a person
	 * @param personId
	 * @return
	 */
	public Integer numberOfSFI(String personId);

	/**
	 * This method is used for get max id of disclosure table
	 * @return
	 */
	public Integer generateMaxDisclosureId();

	/**
	 * This method is used for get sfi details using person id
	 * @param personId
	 * @return
	 */
	public List<PersonEntity> fetchCOIFinancialEntityByPersonId(String personId);

	/**
	 * This method is used for get entity details
	 * @param searchString
	 * @return
	 */
	public List<CoiEntity> searchEnitiy(String searchString);

	/**
	 * This method is used for get entity status(lookup)
	 * @return
	 */
	public List<EntityStatus> fetchEntityStatus();

	/**
	 * This method is used for get entity type(lookup)
	 * @return
	 */
	public List<EntityType> fetchEntityType();

	/**
	 * This method is used for get FinancialEntityRelType(lookup)
	 * @return
	 */
	public List<PersonEntityRelType> fetchPersonEntityRelType();

	/**
	 * This method is used for get sfi details of a person
	 * @param personId
	 * @return
	 */
	public List<PersonEntity> getSFIOfDisclosure(String personId);

	/**
	 * 
	 * @param 
	 * @return list of Coi Disclosure Detail Statuses
	 */
	List<CoiConflictStatusType> getCoiConflictStatusTypes();

	/**
	 * This method is used for get sfi details based on financialEntityId
	 * @param financialEntityId
	 * @return
	 */
	PersonEntity getSFIDetails(Integer financialEntityId);
	
	/**
	 * This method is used for get CoiFinancialEntityDetails based on coiFinancialEntityId
	 * @param coiFinancialEntityId
	 * @return list of CoiFinancialEntityDetails
	 */
	List<PersonEntityRelationship> getCoiFinancialEntityDetails(Integer coiFinancialEntityId);

	/**
	 * This method is used for save CoiFinancialEntityDetails
	 * @param personEntityRelationship
	 * @return saved information of coiFinancialEntity
	 */
	PersonEntityRelationship saveOrUpdatePersonEntityRelationship(PersonEntityRelationship personEntityRelationship);

	/**
	 * This method is used for save SFI details
	 * @param personEntity
	 * @return saved information of coiFinancialEntity
	 */
	PersonEntity saveOrUpdateCoiSFI(PersonEntity personEntity);

	/**
	 * This method is used for save coiEntity details
	 * @param coiEntity
	 * @return saved information of coiEntity
	 */
	CoiEntity saveOrUpdateCoiEntity(CoiEntity coiEntity);

	/**
	 * This method is used for get number of Disclosure of a person that are in CURRENT_DISCLOSURE type
	 * @return count of disclosures
	 */
	Integer getNumberOfDisclosure(String disclosureCategoryType);

	/**
	 * This method is used for certify disclosure
	 * @param coiDisclosure
	 */
	public void certifyDisclosure(CoiDisclosure coiDisclosure);
	
	/**
	 * This method is used for certify disclosure
	 * @param coiTravelDisclosure
	 */
	public void certifyTravelDisclosure(CoiTravelDisclosure coiTravelDisclosure);

	/**
	 * 
	 * @param disclosureId 
	 * @param 
	 * @return list of Coi Disclosure Details
	 */
	public List<CoiDisclEntProjDetails> getProjectRelationshipByParam(Integer moduleCode, Integer moduleItemId, String loginPersonId, Integer disclosureId);

	/**
	 * 
	 * @param 
	 * @return save or update of Coi Disclosure Details
	 */
	CoiDisclEntProjDetails saveOrUpdateCoiDisclEntProjDetails(CoiDisclEntProjDetails entityProjectRelation);

	/**
	 * 
	 * @param 
	 * @return check if SFI Completed For Project
	 */
	public Boolean checkIsSFICompletedForProject(Integer moduleCode, Integer moduleItemId, Integer disclosureId);

	/**
	 * 
	 * @param 
	 * @return get SFI Based On DisclosureId
	 */
	public List<PersonEntity> getSFIBasedOnDisclosureId(Integer disclosureId);

	/**
	 * This method is used for evaluate DisclosureQuestionnaire
	 * @param moduleCode
	 * @param submoduleCode
	 * @param moduleItemKey
	 * @return Boolean value
	 */
	public boolean evaluateDisclosureQuestionnaire(Integer moduleCode, Integer submoduleCode, Integer moduleItemKey);

	/**
	 * This method is used for save the evaluate DisclosureQuestionnaire value in disclosure table
	 * @param isDisclosureQuestionnaire
	 * @param disclosureId
	 */
	public void setDisclosureQuestionnaire(Boolean isDisclosureQuestionnaire, Integer disclosureId);

	/**
	 * This method is used for get Disclosure Ids based on coiFinancialEntityId
	 * @param coiFinancialEntityId
	 * @return list of Disclosure Ids
	 */
	public List<Integer> getDisclosureIdsByCOIFinancialEntityId(Integer coiFinancialEntityId);

	/**
	 * This method is used for get Disclosure Details based on coiFinancialEntityId and sequence statusCodes
	 * @param disclosureIds
	 * @param statusCodes
	 * @return list of Disclosures
	 */
	public List<CoiDisclosure> getActiveAndPendingCoiDisclosureDetailsByDisclosureIdsAndSequenceStatus(List<Integer> disclosureIds, List<String> statusCodes);

	/**
	 * This method is used for get ModuleItemKeys based on coiFinancialEntityId and module code
	 * @param coiFinancialEntityId
	 * @param moduleCode
	 * @return list of ModuleItemKeys
	 */
	public List<String> getModuleItemKeysByCOIFinancialEntityIdAndModuleCode(Integer coiFinancialEntityId, Integer moduleCode);

	/**
	 * This method is used for fetchCoiSections
	 * @return
	 */
	public List<CoiSectionsType> fetchCoiSections();

	/**
	 * This method is used for saveCoiReview
	 * @param coiReview
	 * @return
	 */
	public CoiReview saveOrUpdateCoiReview(CoiReview coiReview);

	/**
	 * This method is used for getCoiReview
	 * @param disclosureId
	 * @return
	 */
	public List<CoiReview> getCoiReview(Integer disclosureId);

	/**
	 * This method is used for update reviewStatusTypeCode in CoiReview
	 * @param reviewStatusTypeCode
	 * @param coiReviewId
	 */
	public void startReview(String reviewStatusTypeCode, Integer coiReviewId);

	/**
	 * This method is used for load CoiReview
	 * @param coiReviewId
	 * @return
	 */
	public CoiReview loadCoiReview(Integer coiReviewId);

	/**
	 * This method is used for save Comments
	 * @param coiReviewComment
	 * @return
	 */
	public CoiReviewComments saveOrUpdateCoiReviewComments(CoiReviewComments coiReviewComment);

	/**
	 * 
	 * @return
	 */
	public List<CoiReviewActivity> fetchCoiReviewActivity();

	/**
	 * 
	 * @param coiReviewCommentAttachment
	 * @return
	 */
	public CoiReviewCommentAttachment saveOrUpdateAttachment(CoiReviewCommentAttachment coiReviewCommentAttachment);

	/**
	 * 
	 * @param fileData
	 * @return
	 */
	public CoiFileData saveFileData(CoiFileData fileData);

	/**
	 * 
	 * @param reviewStatusTypeCode
	 * @return
	 */
	public CoiReviewStatusType getReviewStatus(String reviewStatusTypeCode);

	/**
	 * 
	 * @param coiReviewAssigneeHistory
	 * @return
	 */
	CoiReviewAssigneeHistory saveOrUpdateCoiReviewAssigneeHistory(CoiReviewAssigneeHistory coiReviewAssigneeHistory);

	/**
	 * This method is used for get DisclosureStatus By Code
	 * @param disclosureStatusCode
	 * @return COIDisclosureStatus
	 */
	CoiConflictStatusType getDisclosureStatusByCode(String disclosureStatusCode);

	/**
	 * This method is used for get Disposition Status By Code
	 * @param dispositionStatusTypeCode
	 * @return COIDispositionStatus
	 */
	CoiDispositionStatusType getDispositionStatusByCode(String dispositionStatusTypeCode);

	/**
	 * This method is used for get count SFIs of person in disclosure
	 * @param disclosureStatusCode
	 * @param personId
	 * @param disclosureId
	 * @return SFI count
	 */
	public Integer getSFICountBasedOnParams(String disclosureStatusCode, String personId, Integer disclosureId);

	/**
	 *  This method is used for fetch ReviewCommentAttachment 
	 * @param coiReviewCommentId
	 * @return list of CoiReviewCommentAttachment
	 */
	public List<CoiReviewCommentAttachment> fetchReviewCommentAttachment(Integer coiReviewCommentId);

	/**
	 * This method is used for deleteReviewCommentAttachment 
	 * @param coiReviewId
	 */
	public void deleteReviewCommentAttachment(Integer coiReviewId);

	/**
	 * This method is used for deleteReviewComment 
	 * @param coiReviewId
	 */
	public void deleteReviewComment(Integer coiReviewId);

	/**
	 * This method is used for deleteReview 
	 * @param coiReviewId
	 */
	public void deleteReview(Integer coiReviewId);

	/**
	 * This method is used for deleteReviewAttachmentByCommentId 
	 * @param coiReviewCommentId
	 */
	public void deleteReviewAttachmentByCommentId(Integer coiReviewCommentId);

	/**
	 * This method is used for deleteReviewCommentByCommentId 
	 * @param coiReviewCommentId
	 */
	public void deleteReviewCommentByCommentId(Integer coiReviewCommentId);

	/**
	 * This method is used for fetchAttachmentById 
	 * @param coiReviewCommentAttId
	 * @return
	 */
	public CoiReviewCommentAttachment fetchAttachmentById(Integer coiReviewCommentAttId);

	/**
	 * This method is used for deleteReviewAssigneeHistory 
	 * @param coiReviewId
	 */
	public void deleteReviewAssigneeHistory(Integer coiReviewId);

	/**
	 * This method is used for getFileDataById
	 * @param fileDataId
	 * @return CoiFileData
	 */
	public CoiFileData getFileDataById(String fileDataId);

	/**
	 * This method is used for deleteFileData
	 * @param fileData
	 */
	public void deleteFileData(CoiFileData fileData);

	/**
	 * This method is used for fetchReviewAttachmentByReviewId
	 * @param coiReviewId
	 * @return
	 */
	public List<CoiReviewCommentAttachment> fetchReviewAttachmentByReviewId(Integer coiReviewId);

	/**
	 * This method is used for fetchReviewAttachmentByCommentId
	 * @param coiReviewCommentId
	 * @return
	 */
	public List<CoiReviewCommentAttachment> fetchReviewAttachmentByCommentId(Integer coiReviewCommentId);

	/**
	 * This method is used for delete attachment
	 * @param coiReviewCommentAttId
	 * @return CoiReviewCommentAttachment
	 */
	public CoiReviewCommentAttachment deleteAttachment(Integer coiReviewCommentAttId);

	/**
	 * This method is used for load ReviewComments
	 * @param vo
	 * @return ConflictOfInterestVO
	 */
	public ConflictOfInterestVO loadCoiReviewComments(ConflictOfInterestVO vo);

	/**
	 * This method is used for save Tag details of a comment
	 * @param coiReviewCommentTag
	 * @return CoiReviewCommentTag
	 */
	public CoiReviewCommentTag saveOrUpdateCoiReviewCommentTag(CoiReviewCommentTag coiReviewCommentTag);

	/**
	 * This method is used for get ProjectRelationship/disclosureDetails
	 * @param disclosureDetailsId
	 * @return CoiDisclosureDetails
	 */
	CoiDisclEntProjDetails getProjectRelationship(Integer disclosureDetailsId);

	/**
	 * This method is used for get tags of a comment
	 * @param coiReviewCommentId
	 * @return list of CoiReviewCommentTag
	 */
	public List<CoiReviewCommentTag> fetchCoiReviewCommentTag(Integer coiReviewCommentId);

	/**
	 * This methd is used for get name of admin group based on groupId
	 * @param tagGroupId
	 * @return String(group name)
	 */
	public String fetchadminGroupName(Integer tagGroupId);

	/**
	 * This method is used for delete tag based on coiReviewCommentId
	 * @param coiReviewCommentId
	 */
	public void deleteReviewTagByCommentId(Integer coiReviewCommentId);

	/**
	 * This method is used for complete CoiDisclosure
	 * @param coiDisclosure
	 */
	public void completeDisclosureReview(CoiDisclosure coiDisclosure);

	/**
	 * This method is used for get number of incomplete reviews
	 * @param disclosureId
	 * @return Integer
	 */
	public Integer numberOfInCompleteReview(Integer disclosureId);

	/**
	 * This method is used for delete tag details based on coiReviewId
	 * @param coiReviewId
	 */
	public void deleteReviewTagByReviewId(Integer coiReviewId);

	/**
	 * This method is used for add Reviewer Status
	 * @param coiDisclosureDetails
	 */
	void addReviewerStatus(CoiDisclEntProjDetails coiDisclosureDetails);

	/**
	 * This method is used for saveOrUpdate ProjectConflictHistory
	 * @param coiConflictHistory
	 * @return
	 */
	public CoiConflictHistory saveOrUpdateCoiConflictHistory(CoiConflictHistory coiConflictHistory);

	/**
	 * This method is used for get history of project Conflicts
	 * @param coiConflictHistoryId
	 * @return list of CoiConflictHistory
	 */
	public List<CoiConflictHistory> getCoiConflictHistory(Integer coiConflictHistoryId);

	/**
	 * @param disclosureId
	 * @return
	 */
	public String getProposalIdLinkedInDisclosure(Integer disclosureId);

	/**
	 * @param disclosureCategoryTypeCode
	 * @return
	 */
//	public COIDisclosureCategoryType getDisclosureCategoryTypeByCode(String disclosureCategoryTypeCode);

	/**
	 * @param disclosureNumber
	 * @return
	 */
	List<CoiDisclosure> getCoiDisclosuresByDisclosureNumber(Integer disclosureNumber);

	/**
	 * This method is used for get count of comments
	 * @return count
	 */
	public Integer getReviewCommentsCount();

	/**
	 * @param disclosureId
	 * @param coiFinancialEntityId
	 */
	public void updateFinacialEntityInDisclosureRelation(Integer disclosureId, Integer coiFinancialEntityId);

	/**
	 *This method is used to get the count based on conflict status
	 *
	 * @param moduleCode module code
	 * @param moduleItemId module item key
	 * @param disclosureId disclosure id
	 * @return list of count objects
	 */
	List<Map<Object, Object>> disclosureStatusCount(Integer moduleCode, Integer moduleItemId, Integer disclosureId);

	/**
	 * This method is used to get Entity Details by Entity Id
	 * @return COIEntity
	 */
	public CoiEntity getCoiEntityDetailsById(Integer coiEntityId);

	public List<CoiDisclosure> getActiveDisclosure(String personId);

	public Integer getNumberOfSFIBasedOnDisclosureId(Integer disclosureId);

	/**
	 * @param vo
	 * @return list of coi
	 */
	public DashBoardProfile getCOIDashboard(CoiDashboardVO vo);

	public Integer getCOIDashboardCount(CoiDashboardVO vo);

	/**
	 * This method is used to get list of coi for admin
	 * @param vo
	 * @return list of coi
	 */
	public DashBoardProfile getCOIAdminDashboard(CoiDashboardVO vo);

	/**
	 * This method is used to get list of sfi
	 * @param vo
	 * @return list of sfi
	 */
	public DashBoardProfile getSFIDashboard(CoiDashboardVO vo);

	public List<CoiEntity> getAllEntityList(ConflictOfInterestVO vo);

	public void setEntityStatus(ConflictOfInterestVO vo);


	/**
	 *This method is used to get the project based in the given parameters
	 * @param moduleCode
	 * @param personId
	 * @param disclosureId
	 * @param searchString 
	 * @return
	 */
	List<DisclosureDetailDto> getProjectsBasedOnParams(Integer moduleCode, String personId, Integer disclosureId, String searchString);

	public List<CoiEntity> getAllSystemEntityList(CoiDashboardVO vo);

	public CoiTravelDisclosure saveOrUpdateCoiTravelDisclosure(CoiTravelDisclosure coiTravelDisclosure);

	public List<CoiTravelDisclosure> getAllCoiTravelDisclosureList(ConflictOfInterestVO vo);

	public CoiTravelDisclosure loadTravelDisclosure(Integer travelDisclosureId);

	public List<CoiProjectType> getCoiProjectTypes();

	DashBoardProfile getPersonEntityDashboard(CoiDashboardVO vo);

	public Integer generateMaxCoiEntityNumber();

	public PersonEntity saveOrUpdateSFI(PersonEntity personEntity);

	public Integer generateMaxDisclosureNumber();
	
	public CoiTravelDisclosureTraveler saveOrUpdateCoiTravelDisclosureTraveller(CoiTravelDisclosureTraveler coiTravelDisclosureTraveller);


	/**
	 *
	 * @param vo
	 * @return
	 */
	DashBoardProfile getCOIReviewerDashboard(CoiDashboardVO vo);
	
	public CoiProjectProposal saveOrUpdateCoiProjectProposal(CoiProjectProposal coiProjectProposal);

	public CoiProjectAward saveOrUpdateCoiProjectAward(CoiProjectAward coiProjectAward);

	CoiEntity getCoiEntityByPersonEntityId(Integer personEntityId);

	public PersonEntity getPersonEntityDetailsById(Integer personEntityId);

	public List<ValidPersonEntityRelType> getRelationshipDetails(String tabName);

	public List<PersonEntityRelationship> getRelationshipDetails(ConflictOfInterestVO vo);

	/**
	 * This method is used to get Relationship Details personEntityId
	 * @param personEntityId
	 * @return
	 */
	List<PersonEntityRelationship> getRelationshipDetails(Integer personEntityId);

	/**
	 * This method is used to get Relationship Details By personEntityRelId
	 * @param personEntityRelId
	 * @return
	 */
	PersonEntityRelationship getRelationshipDetailsById(Integer personEntityRelId);

	public CoiReviewStatusType getReviewStatusByCode(String reviewStatusPending);

	public CoiRiskCategory getRiskCategoryStatusByCode(String riskCategoryLow);

	public PersonEntityRelationship getPersonEntityRelationshipByPersonEntityRelId(Integer personEntityRelId);

	public ValidPersonEntityRelType getValidPersonEntityRelTypeByTypeCode(Integer validPersonEntityRelTypeCode);
	
	public Integer generateMaxTravelNumber();
	
	public List<CoiTravelerType> loadTravellerTypesLookup();
	
	public List<CoiTravelerStatusType> loadTravelStatusTypesLookup();
	
	public List<ValidPersonEntityRelType> getValidPersonEntityRelTypes(Integer personEntityId);

	public Integer getAllSystemEntityListCount(CoiDashboardVO vo);

	public List<CoiProjConflictStatusType> getProjConflictStatusTypes();

	/**
	 * This method is used to check the sfi is created with this entity
	 *
	 * @param enitityId
	 * @param personId
	 * @return
	 */
	boolean checkEntityAdded(Integer enitityId, String personId);

	/**
	 *
	 * @param disclosureId
	 * @param disclosureNumber
	 * @param personEntityId
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param type
	 */
	void syncProjectWithDisclosure(Integer disclosureId, Integer disclosureNumber, Integer personEntityId,
								   Integer moduleCode, String moduleItemKey, String type);

	public CoiDisclosureFcoiType getCoiDisclosureFcoiTypeByCode(String coiTypeCode);

	public List<PersonEntityRelationship> getPersonEntityRelationshipByPersonEntityId(Integer personEntityId);

	public Integer getNumberOfProposalsBasedOnDisclosureId(Integer disclosureId);

	public Integer getNumberOfAwardsBasedOnDisclosureId(Integer disclosureId);

	public boolean isSFIDefined(String personId);

	public boolean isRelationshipDefined(Integer disclosureId);

	public boolean isMasterDisclosurePresent(String personId);

	/**
	 *
	 * @param disclosureId
	 * @param disclosureNumber
	 */
	void archiveDisclosureOldVersions(Integer disclosureId, Integer disclosureNumber);
	
	public Integer fetchMaxPersonEntityId(String personId, Integer entityId);
	
	public Integer generateMaxPersonEntityId();

	public void saveOrUpdateDisclComment(DisclComment disclComment);

	public DisclComment getDisclEntProjRelationComment(Integer disclosureDetailsId);

	/**
	 * This method is used to validate
	 * 1) If selected project expired date passed
	 * 2) Is part of any pending project disclosure
	 * 3) If the selected project is part of any active/ pending  FCOi disclosure
	 *
	 * @param personId personId
	 * @param moduleCode moduleCode
	 * @param moduleItemKey moduleItemKey
	 * @return Map of validated values
	 */
	Map<String, Object> validateProjectDisclosure(String personId, Integer moduleCode, String moduleItemKey);

	/**
	 * This method is used for get sfi details by a person id or disclosure id
	 *
	 * @param vo ConflictOfInterestVO
	 * @return
	 */
	List<PersonEntity> getSFIOfDisclosure(ConflictOfInterestVO vo);

	/**
	 *This method updates the assign admin/group and changes the disclosure status to 3 review in progress
	 *
	 * @param adminGroupId
	 * @param adminPersonId
	 * @param disclosureId
	 */
    void assignDisclosureAdmin(Integer adminGroupId, String adminPersonId, Integer disclosureId);

    /**
	 * This method is used for updating review status of disclosure
	 *
	 * @param disclosureId
	 * @param disclosureReviewInProgress status code
	 */
	public void updateReviewStatus(Integer disclosureId, String disclosureReviewInProgress);

	public List<EntityRiskCategory> fetchEntityRiskCategory();

	 /**
	 *
	 * @param unitNumber
	 * This method is used to fetch the entire row(Unit details) from unit table against the input unit number
	 */
	public Unit getUnitFromUnitNumber(String unitNumber);

	/**
	 * This method is used to validate conflicts and update
	 *
	 * @param disclosureId disclosureId
	 * @return CoiConflictStatusType
	 */
	CoiConflictStatusTypeDto validateConflicts(Integer disclosureId);

	/**
	 * This method is used to load CoiConflictStatusType
	 *
	 * @param coiConflictStatusCode Coi Conflict Status Code
	 * @return CoiConflictStatusType
	 */
	CoiConflictStatusType loadCoiConflictStatusType(String coiConflictStatusCode);
	
	/**
	 * This method is used to fetch the review status object based on status code
	 */
	public CoiReviewStatusType getReviewStatusDetails(String reviewStatusCode);
	
	/**
	 * This method is used to fetch the travel disclosure status object based on status code
	 */
	public CoiTravelDisclosureStatusType getTravelDisclosureStatusDetails(String travelDisclosureStatusCode);

	/**
	 * This method is used to archive an entity
	 * @param entityId
	 */
	public void archiveEntity(Integer entityId);

	/**
	 * This method is used to get maximum of version number of coi entity
	 * @param entityNumber
	 * @return versionNumber
	 */
	public Integer getMaxEntityVersionNumber(Integer entityNumber);
	
	/**
	 *This method updates the assign admin/group and changes the disclosure status to 3 review in progress
	 *
	 * @param adminGroupId
	 * @param adminPersonId
	 * @param travelDisclosureId
	 */
    void assignTravelDisclosureAdmin(Integer adminGroupId, String adminPersonId, Integer travelDisclosureId);
    
    public List<CoiTravelDisclosureTraveler> getEntriesFromTravellerTable(Integer travelDisclosureId);
    
    public CoiEntity getEntityDetails(Integer entityId);
    
    public void deleteEntriesFromTraveller(Integer travelDisclosureId);
    
    public CoiTravelDocumentStatusType getDocumentStatusDetails(String documentStatusCode);
    
    public CoiTravelReviewStatusType getTravelReviewStatusDetails(String reviewStatusCode);
    
    public Country getCountryDetailsByCountryCode(String countryCode);
    
    public CoiTravelerType getEntryFromTravellerTypeTable(String travellerTypeCode);

	List<CoiTravelerType> getEntriesFromTravellerTypeTable(List<String> travellerTypeCode);

	public EntityType getEntityTypeDetails(String entityTypeCode);

	public EntityRiskCategory getEntityRiskDetails(String riskCategoryCode);

	public void updateCoiDisclEntProjDetails(String projectConflictStatusCode, Integer disclosureDetailsId);

	public String getProjectConflictStatusCode(Integer disclosureDetailsId);

	/**
	 * This method is used to delete Person entity
	 * @param personEntityId
	 */
	void deletePersonEntity(Integer personEntityId);

	public String getCoiConflictStatusByStatusCode(String conflictStatusCode);

	/**
	 * This method is used to activate/inactive entity
	 * @param coiEntityDto
	 */
	void activateOrInactivateEntity(CoiEntityDto coiEntityDto);

	/**
	 * This method is used to fetch SFIs of disclosure
	 * @param vo
	 * @return
	 */
	Integer getSFIOfDisclosureCount(ConflictOfInterestVO vo);

	/**
	 * This method is used to activate/inactive  person entity
	 * @param personEntityDto
	 * @return Timestamp
	 */
	Timestamp activateOrInactivatePersonEntity(PersonEntityDto personEntityDto);

	/**
	 * This method is used to change version status of person entity
	 * @param personEntityId
	 * @param versionStatus
	 */
	void patchPersonEntityVersionStatus(Integer personEntityId, String versionStatus);

	/**
	 * This method is used to get the max of version number
	 * @param personEntityNumber
	 * @return
	 */
	Integer getMaxPersonEntityVersionNumber(Integer personEntityNumber);

	/**
	 * This method is used to check the person entity is linked to disclosure/travel
	 * @param personEntityId
	 * @return
	 */
	boolean checkPersonEntityAdded(Integer personEntityId);

	/**
	 * This method is used to get Person Entity maximum Person Entity number
	 * @return
	 */
	Integer getMaxPersonEntityNumber();
	
	public List<COIValidateDto> evaluateValidation(Integer disclosureId, String personId);

	public String getConflictStatusUpdateUser(Integer disclosureDetailsId);
	

	/**
	 * This method is used to update disclosure header update details
	 * @param disclosureId
	 */
	void updateDisclosureUpdateDetails(Integer disclosureId);

	/**
	 * This method is to load CoiReviewComment By Id
	 * @param coiReviewCommentId
	 * @return
	 */
	CoiReviewComments loadCoiReviewCommentById(Integer coiReviewCommentId);

	/**
	 * This method is used to update PersonEntity header update details
	 * @param personEntityId
	 */
	void updatePersonEntityUpdateDetails(Integer personEntityId);

	/**
	 * This method is used to fetch all entity relationship types
	 * @return
	 */
	List<EntityRelationshipType> fetchAllRelationshipTypes();

	/**
	 * This method is used to approve Entity
	 * @param entityId
	 * @return
	 */
	Timestamp approveEntity(Integer entityId);

	/**
	 * Save or Update Entity Relationship
	 * @param entityRelationship
	 */
	void saveOrUpdateEntityRelationship(EntityRelationship entityRelationship);

	/**
	 * This method is used to delete person entity relationship
	 * @param personEntityRelId
	 */
	void deletePersonEntityRelationship(Integer personEntityRelId);
	/**
	 * This method is used to get person entity by entity id and person id
	 * @param entityId
	 * @param personId
	 * @return
	 */
	PersonEntity fetchPersonEntityById(Integer entityId, String personId);

	List<CoiTravelDisclosure> loadTravelDisclosureHistory(String personId, Integer entityNumber);

	public List<ValidPersonEntityRelType> getValidPersonEntityRelType();

	/**
	 * This method is used to fetch disclosure
	 * @param dashboardVO
	 * @return
	 */
	List<DisclosureHistoryDto> getDisclosureHistory(CoiDashboardVO dashboardVO);

	/**
	 * This method is used to update risk category of an entity
	 * @param entityDto
	 * @return
	 */
	Timestamp updateEntityRiskCategory(CoiEntityDto entityDto);

	public String getDisclosurePersonIdByDisclosureId(Integer disclosureId);


	/**
	 * This method is used to update person entity
	 * @param personEntityDto
	 */
	Timestamp updatePersonEntity(PersonEntityDto personEntityDto);

	/**
	 * This method is used to update Entity Update Details
	 * @param entityId
	 * @param updateTimestamp
	 */
	void updateEntityUpdateDetails(Integer entityId, Timestamp updateTimestamp);

	/**
	 * This method is used to check a peron entity has a version status of @param versionStatus
	 * @param personEntityNumber
	 * @param versionStatus
	 * @return true/false
	 */
	boolean hasPersonEntityVersionStatusOf(Integer personEntityNumber, String versionStatus);

	/**
	 * This method is used to fetch draft version of person entity by params
	 * @param personEntityNumber
	 * @param versionStatus
	 * @return
	 */
	PersonEntity getPersonEntityByNumberAndStatus(Integer personEntityNumber, String versionStatus);

	public DisclosureActionType fetchDisclosureActionTypeById(String actionLogCreated);

	public void saveOrUpdateDisclosureActionLog(DisclosureActionLog disclosureActionLog);

	public List<CoiTravelDisclosureStatusType> getTravelConflictStatusType();

	public DisclComment getTravelConflictComment(Integer travelDisclosureId);

	public void saveOrUpdateCoiTravelConflictHistory(CoiTravelConflictHistory coiTravelConflictHistory);

	public List<CoiTravelConflictHistory> getCoiTravelConflictHistory(Integer travelDisclosureId);

	public String getCoiTravelConflictStatusByStatusCode(String conflictStatusCode);
	
	public void saveOrUpdateTravelDisclosureActionLog(TravelDisclosureActionLog travelDisclosureActionLog);

	/**
	 * This method is used to sync disclosure risk
	 * @param disclosureId
	 * @param disclosureNumber
	 * @return CoiRiskCategory
	 */
	CoiRiskCategory syncDisclosureRisk(Integer disclosureId, Integer disclosureNumber);

	/**
	 * This method is used to update disclosure risk
	 * @param coiDisclosureDto
	 * @return
	 */
	Timestamp updateDisclosureRiskCategory(CoiDisclosureDto coiDisclosureDto);

	/**
	 * This method is used to fetch all disclosure risk
	 * @return
	 */
	List<CoiRiskCategory> fetchDisclosureRiskCategory();

	/**
	 * This method is used to get disclosure history count
	 *
	 * @param dashboardVO
	 * @return
	 */
	Integer getDisclosureHistoryCount(CoiDashboardVO dashboardVO);
}
