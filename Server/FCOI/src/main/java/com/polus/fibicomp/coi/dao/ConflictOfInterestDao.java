package com.polus.fibicomp.coi.dao;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.pojo.COIDisclosureCategoryType;
import com.polus.fibicomp.coi.pojo.COIDisclosureStatus;
import com.polus.fibicomp.coi.pojo.COIDispositionStatus;
import com.polus.fibicomp.coi.pojo.COIEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityDetails;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityRelType;
import com.polus.fibicomp.coi.pojo.COIReviewStatus;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetails;
import com.polus.fibicomp.coi.pojo.EntityStatus;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetailsStatus;
import com.polus.fibicomp.coi.pojo.CoiFileData;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewActivity;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;

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
	public List<COIFinancialEntity> fetchCOIFinancialEntityByPersonId(String personId);

	/**
	 * This method is used for get entity details
	 * @param searchString
	 * @return
	 */
	public List<COIEntity> searchEnitiy(String searchString);

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
	public List<COIFinancialEntityRelType> fetchCOIFinancialEntityRelType();

	/**
	 * This method is used for get sfi details of a person
	 * @param personId
	 * @return
	 */
	public List<COIFinancialEntity> getSFIOfDisclosure(String personId);

	/**
	 * 
	 * @param 
	 * @return list of Coi Disclosure Detail Statuses
	 */
	public List<CoiDisclosureDetailsStatus> getCoiDisclosureDetailStatuses();

	/**
	 * This method is used for get sfi details based on financialEntityId
	 * @param financialEntityId
	 * @return
	 */
	public COIFinancialEntity getSFIDetails(Integer financialEntityId);
	
	/**
	 * This method is used for get CoiFinancialEntityDetails based on coiFinancialEntityId
	 * @param coiFinancialEntityId
	 * @return list of CoiFinancialEntityDetails
	 */
	public List<COIFinancialEntityDetails> getCoiFinancialEntityDetails(Integer coiFinancialEntityId);

	/**
	 * This method is used for save CoiFinancialEntityDetails
	 * @param coiFinancialEntityDetails
	 * @return saved information of coiFinancialEntity
	 */
	public COIFinancialEntityDetails saveOrUpdateCoiFinancialEntityDetails(COIFinancialEntityDetails coiFinancialEntityDetails);

	/**
	 * This method is used for save SFI details
	 * @param coiFinancialEntity
	 * @return saved information of coiFinancialEntity
	 */
	public COIFinancialEntity saveOrUpdateCoiSFI(COIFinancialEntity coiFinancialEntity);

	/**
	 * This method is used for save coiEntity details
	 * @param coiEntity
	 * @return saved information of coiEntity
	 */
	public COIEntity saveOrUpdateCOIEntity(COIEntity coiEntity);

	/**
	 * This method is used for get number of Disclosure of a person that are in CURRENT_DISCLOSURE type
	 * @return count of disclosures
	 */
	public Integer getNumberOfDisclosure(String disclosureCategoryType);

	/**
	 * This method is used for certify disclosure
	 * @param coiDisclosure
	 */
	public void certifyDisclosure(CoiDisclosure coiDisclosure);

	/**
	 * 
	 * @param disclosureId 
	 * @param 
	 * @return list of Coi Disclosure Details
	 */
	public List<CoiDisclosureDetails> getProjectRelationshipByParam(Integer moduleCode, Integer moduleItemId, String loginPersonId, Integer disclosureId);

	/**
	 * 
	 * @param 
	 * @return save or update of Coi Disclosure Details
	 */
	public CoiDisclosureDetails saveOrUpdateCoiDisclosureDetail(CoiDisclosureDetails entityProjectRelation);

	/**
	 * 
	 * @param 
	 * @return check if SFI Completed For Project
	 */
	public Boolean checkIsSFICompletedForProject(Integer moduleCode, Integer moduleItemId, Integer disclosureId, String personId);

	/**
	 * 
	 * @param 
	 * @return get SFI Based On DisclosureId
	 */
	public List<COIFinancialEntity> getSFIBasedOnDisclosureId(Integer disclosureId);

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
	 * @param coiFinancialEntityDetails
	 * @return list of Disclosure Ids
	 */
	public List<Integer> getDisclosureIdsByCOIFinancialEntityId(Integer coiFinancialEntityId);

	/**
	 * This method is used for get Disclosure Details based on coiFinancialEntityId and sequence statusCodes
	 * @param coiFinancialEntityDetails
	 * @param statusCodes
	 * @return list of Disclosures
	 */
	public List<CoiDisclosure> getActiveAndPendingCoiDisclosureDetailsByDisclosureIdsAndSequenceStatus(List<Integer> disclosureIds, List<String> statusCodes);

	/**
	 * This method is used for get ModuleItemKeys based on coiFinancialEntityId and module code
	 * @param coiFinancialEntityDetails
	 * @param moduleCode
	 * @return list of ModuleItemKeys
	 */
	public List<String> getModuleItemKeysByCOIFinancialEntityIdAndModuleCode(Integer coiFinancialEntityId, Integer moduleCode);

	/**
	 * This method is used for get Proposal Details based on proposalIds
	 * @param proposalIds
	 * @return list of Proposal
	 */
	public List<Proposal> getProposalsBasedOnProposalIds(List<Integer> proposalIds);

	/**
	 * This method is used for get Proposal Details based on proposalIds
	 * @param awardIds
	 * @return list of Award
	 */
	public List<Award> getAwardsBasedOnAwardIds(List<Integer> awardIds);

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
	public COIReviewStatus getReviewStatus(String reviewStatusTypeCode);

	/**
	 * 
	 * @param coiReviewAssigneeHistory
	 * @return
	 */
	public CoiReviewAssigneeHistory saveOrUpdateCoiReviewAssigneeHistory(CoiReviewAssigneeHistory coiReviewAssigneeHistory);

	/**
	 * This method is used for get DisclosureStatus By Code
	 * @param disclosureStatusCode
	 * @return COIDisclosureStatus
	 */
	public COIDisclosureStatus getDisclosureStatusByCode(String disclosureStatusCode);

	/**
	 * This method is used for get Disposition Status By Code
	 * @param dispositionStatusTypeCode
	 * @return COIDispositionStatus
	 */
	public COIDispositionStatus getDispositionStatusByCode(String dispositionStatusTypeCode);

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
	public CoiDisclosureDetails getProjectRelationship(Integer disclosureDetailsId);

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
	public void addReviewerStatus(CoiDisclosureDetails coiDisclosureDetails);

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
	public COIDisclosureCategoryType getDisclosureCategoryTypeByCode(String disclosureCategoryTypeCode);

	/**
	 * @param disclosureNumber
	 * @return
	 */
	public List<CoiDisclosure> getCoiDisclosuresByDisclosureNumber(String disclosureNumber);

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
	 * @param personId login person id
	 * @return list of count objects
	 */
	List<Map<Object, Object>> disclosureStatusCount(Integer moduleCode, Integer moduleItemId, Integer disclosureId, String personId);

	/**
	 * This method is used to get Entity Details by Entity Id
	 * @return COIEntity
	 */
	public COIEntity getCoiEntityDetailsById(Integer coiEntityId);

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

	public List<COIEntity> getAllEntityList(ConflictOfInterestVO vo);

	public void setEntityStatus(ConflictOfInterestVO vo);


	/**
	 *
	 * @param moduleCode
	 * @param personId
	 * @param disclosureId
	 * @param status
	 * @return
	 */
	List<DisclosureDetailDto> getProjectsBasedOnParams(Integer moduleCode, String personId, Integer disclosureId, String status);

	public List<COIEntity> getAllSystemEntityList(ConflictOfInterestVO vo);
}
