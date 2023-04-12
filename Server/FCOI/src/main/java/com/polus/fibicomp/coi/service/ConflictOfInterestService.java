package com.polus.fibicomp.coi.service;


import java.util.List;

import javax.validation.Valid;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityDetails;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclosureOld;
import com.polus.fibicomp.coi.pojo.CoiDisclosureOldDetails;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;

@Transactional
@Service(value = "conflictOfInterestService")
public interface ConflictOfInterestService {

	/**
	 * This method is used to create Disclosure
	 * @param vo
	 * @return created disclosure details, person details and number of sfi
	 */
	public ResponseEntity<Object> createDisclosure(ConflictOfInterestVO vo);
 
	/**
	 * This method is used to get list of disclosure
	 * @param disclosureId
	 * @return 
	 */
	public ResponseEntity<Object> loadDisclosure(Integer disclosureId);

	/**
	 * This method is used to get list of Disclosure Relations.
	 * @param disclosureId 
	 * @return A list of Disclosure Relations.
	 */
	public String getDisclosureRelations(ConflictOfInterestVO vo);

	/**
	 * This method is used for get list of sfi of a person
	 * @param vo 
	 * @return A list of sfi
	 */
	public ResponseEntity<Object> getSFIOfDisclosure(ConflictOfInterestVO vo);

	/**
	 * This method is used for get list of entity table values(enpoint for entity)
	 * @param searchString
	 * @return A list of entity
	 */
	public List<CoiEntity> searchEnitiy(String searchString);

	/**
	 * This method is used for get lookup table of sfi
	 * @return EntityStatus, EntityType, CoiFinancialEntityRelType
	 */
	public ResponseEntity<Object> loadAddSFILookups();

	/**
	 * This method is used to get list of sfi details based on coiFinancialEntityId
	 * @param coiFinancialEntityId
	 * @return A list of sfi details
	 */
	public ResponseEntity<Object> getSFIDetails(Integer coiFinancialEntityId);

	/**
	 * This method is used to create CoiFinancialEntityDetails
	 * @param coiFinancialEntityDetails
	 * @return COIFinancialEntityDetails
	 */
	public COIFinancialEntityDetails saveOrUpdateCoiFinancialEntityDetails(COIFinancialEntityDetails coiFinancialEntityDetails);

	/**
	 * This method is used to create SFI
	 * @param vo
	 * @return vo
	 */
	public ResponseEntity<Object> createSFI(ConflictOfInterestVO vo);

	/**
	 * This method is used to certifyDisclosure
	 * @param CoiDisclosureOld
	 * @return vo
	 */
	public ResponseEntity<Object> certifyDisclosure(CoiDisclosureOld CoiDisclosureOld);

	/**
	 * This method is used to save disclosure Relationship details.
	 * @return vo
	 */
	public ConflictOfInterestVO saveEntityProjectRelation(ConflictOfInterestVO vo);

	/**
	 * This method is used to get disclosure Relationship by proposalId.
	 * @return vo
	 */
	public String getEntityProjectRelations(ConflictOfInterestVO vo);

	/**
	 * This method is used to get sfi relation all conflicts are completed or not.
	 * @return vo
	 */
	public String checkSFICompleted(ConflictOfInterestVO vo);

	/**
	 * This method is used to get admin dashboard detail counts
	 * @return counts
	 */
	public ResponseEntity<Object> loadDisclosureAdminDashboardCounts();

	/**
	 * This method is used to revise Coi disclosure
	 * @return counts
	 */
	public String reviseDisclosure(ConflictOfInterestVO vo);

	/**
	 * This method is used for evaluate DisclosureQuestionnaire
	 * @param vo
	 * @return boolean value
	 */
	public boolean evaluateDisclosureQuestionnaire(ConflictOfInterestVO vo);

	/**
	 * This method is used to get disclosure details for SFI
	 * @param coiFinancialEntityId
	 * @return list of SFI details
	 */
	public ResponseEntity<Object> getDisclosureDetailsForSFI(Integer coiFinancialEntityId);

	/**
	 * This method is used to get Disclosure Relations for SFI
	 * @param coiFinancialEntityId
	 * @return list of Disclosure Relations details
	 */
	public ResponseEntity<Object> getDisclosureRelationsForSFI(Integer coiFinancialEntityId);

	/**
	 * This method is used for save review details
	 * @param vo
	 * @return saved CoiReview object
	 */
	public CoiReview saveOrUpdateCoiReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for get CoiReview based on disclosureId
	 * @param disclosureId
	 * @return List of CoiReview
	 */
	public List<CoiReview> getCoiReview(Integer disclosureId);

	/**
	 * This method is used for Start review
	 * @param vo
	 * @return CoiReview
	 */
	public CoiReview startReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for add comments
	 * @param files
	 * @param formDataJSON
	 * @return saved comments,tag details and attachment details 
	 */
	public String saveOrUpdateCoiReviewComments(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used for get review comment details
	 * @param vo
	 * @return comment details
	 */
	public String loadCoiReviewComments(ConflictOfInterestVO vo);

	/**
	 * This method is used for complete review
	 * @param vo
	 * @return CoiReview
	 */
	public CoiReview completeReview(ConflictOfInterestVO vo);

	/**
	 * This method is used for delete review
	 * @param coiReviewId
	 * @return String
	 */
	public String deleteReview(Integer coiReviewId);

	/**
	 * This method is used for delete comment
	 * @param coiReviewCommentId
	 * @return String
	 */
	public String deleteReviewComment(Integer coiReviewCommentId);

	/**
	 * This method is used for download Attachment
	 * @param attachmentId
	 * @return 
	 */
	public ResponseEntity<byte[]> downloadCoiReviewAttachment(Integer attachmentId);

	/**
	 * This method is used for delete Attachment
	 * @param coiReviewCommentAttId
	 * @return String
	 */
	public String deleteReviewAttachment(Integer coiReviewCommentAttId);

	/**
	 * This method is used for complete Disclosure
	 * @param disclosureId
	 * @return Disclosure details
	 */
	public ResponseEntity<Object> completeDisclosureReview(Integer disclosureId);

	/**
	 * This method is used for update ProjectConflictStatus
	 * @param CoiDisclosureOldDetails
	 * @return CoiDisclosureOldDetails
	 */
	public CoiDisclosureOldDetails updateProjectConflictStatus(CoiDisclosureOldDetails CoiDisclosureOldDetails);

	/**
	 * This method is used for get Project Conflict History
	 * @param coiConflictHistoryId
	 * @return list of CoiConflictHistory
	 */
	public List<CoiConflictHistory> getCoiConflictHistory(Integer coiConflictHistoryId);

	/**
	 * This method is used to get proposals for Disclosure
	 * @param string 
	 * @return list of proposals
	 */
	public String loadProposalsForDisclosure(ConflictOfInterestVO vo);

	/**
	 * @param vo
	 * @return
	 */
	public String loadDisclosureHistory(ConflictOfInterestVO vo);

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
	public ResponseEntity<Object> saveOrUpdateCoiEntity(ConflictOfInterestVO vo);

	/**
	 * This method is used to get entity details based on coiEntityId
	 * @param coiEntityId
	 * @return A list of entity details
	 */
	public ResponseEntity<Object> getEntityDetails(Integer coiEntityId);

	public ResponseEntity<Object> getActiveDisclosure();
	
	/**
	 * This method is used to get COI dasboard data .
	 * @param vo - 
	 * @return A list of dashboard COI data.
	 */
	public String getCOIDashboard(CoiDashboardVO vo);

	/**
	 * This method is used to get COI Admin dasboard data .
	 * @param vo - 
	 * @return A list of dashboard COI data.
	 */
	public String getCOIAdminDashboard(@Valid CoiDashboardVO vo);

	/**
	 * This method is used to get SFI dasboard data .
	 * @param vo - 
	 * @return A list of dashboard SFI data.
	 */
	public String getSFIDashboard(CoiDashboardVO vo);

	public String getCOIDashboardCount(CoiDashboardVO vo);

	public ResponseEntity<Object> getAllEntityList(ConflictOfInterestVO vo);

	public ResponseEntity<Object> setEntityStatus(ConflictOfInterestVO vo);

	public ResponseEntity<Object> getAllSystemEntityList();
	
}
