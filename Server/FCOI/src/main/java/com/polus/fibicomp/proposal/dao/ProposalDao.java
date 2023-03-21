package com.polus.fibicomp.proposal.dao;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import com.polus.fibicomp.proposal.pojo.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerAttachment;
import com.polus.fibicomp.scoring.pojo.WorkflowReviewerScore;

@Transactional
@Service
public interface ProposalDao {

	/**
	 * This method is used to save or update a proposal
	 * @param proposal - Object of a proposal.
	 * @return An object of proposal.
	 */
	public Proposal saveOrUpdateProposal(Proposal proposal);

	/**
	 * This method is used to fetch proposal based on id.
	 * @param proposalId - Id of a proposal.
	 * @return A proposal object.
	 */
	public Proposal fetchProposalById(Integer proposalId);

	/**
	 * This method is used to delete special review of a proposal.
	 * @param specialReview - special review object.
	 * @return an object of deleted special review.
	 */
	public ProposalSpecialReview deleteProposalSpecialReview(ProposalSpecialReview specialReview);

	/**
	 * This method is used to delete proposal based on id.
	 * @param proposalId - Id of the proposal.
	 * @return success message.
	 */
	public String deleteProposal(Integer proposalId);
	
	/**
	 * 
	 * @param string
	 * return
	 */
	public String deleteManitainProjectTeam(Integer proposalProjectTeamId);

	/**
	 * 
	 * @param proposalProjectTeam
	 * @return
	 */
	public String updateManitainProjectTeam(ProposalProjectTeam projectteam);
	
	/**
	 * 
	 * @param projectTeam
	 * @return
	 */
	public String saveManitainProjectTeam(ProposalProjectTeam projectteam);

	/**
	 * This method is used to save proposal person role.
	 * @param personRole.
	 * @return personRole data.
	 */
	public ProposalPersonRoles saveProposalPersonRole(ProposalPersonRoles personRole);

	/**
	 * This method is used to delete proposal person role based on id.
	 * @param personRole.
	 * @return success message.
	 */
	public String deleteProposalPersonRole(ProposalPersonRoles personRole);

	/**
	 * This method is used to find maximum science keyword id.
	 * @return science keyword id.
	 */
	public String getMaxScienceKeyword();

	/**
	 * This method is used to save or update a ScienceKeyword
	 * @param ScienceKeyword - Object of a ScienceKeyword.
	 * @return An object of ScienceKeyword.
	 */
	public ScienceKeyword saveOrUpdateScienceKeyword(ScienceKeyword scienceKeyword);

	/**
	 * This method is used to fetch sorted proposal attachment list. 
	 * @param proposalId     - proposal id.
	 * @param sortBy - sort by field name.
	 * @param reverse - reverse type asc or desc
	 * @return list of proposal attachments.
	 */
	public List<ProposalAttachment> fetchSortedAttachments(Integer proposalId, String sortBy, String reverse);

	/**
	 * This method is used fetch Proposals By GrantCallId and Proposal Status
	 * @param grantCallId :- proposals grantcall Id
	 * @param proposalStatus :- List of proposal Status
	 * @param homeUnitNumber :- proposal home unit number
	 * @return Proposal Types Proposal.
	 */
	public List<Proposal> fetchProposalsByGrantCallIdAndStatus(Integer grantCallId, List<Integer> proposalStatus);

	/**
	 * This method is used to fetch proposals of a grant call.
	 * @param grantCallId     - grant Call Id.
	 * @return list of proposals.
	 */
	public List<Proposal> fetchProposalsOfGrantCall(Integer grantCallId);

	/**
	 * This method is used fetch Proposals By GrantCallId and Proposal Status
	 * @param grantCallId :- proposals grantcall Id
	 * @param proposalStatus :- List of proposal Status
	 * @param homeUnitNumber :- proposal home unit number
	 * @return Proposal Types Proposal.
	 */
	public List<Proposal> fetchProposalsByGrantCallIdAndStatus(Integer grantCallId, List<Integer> proposalStatus, String homeUnitNumber);

	/**
	 * This method is used to fetch attachment based on Id.
	 * @param attachmentId - Id of the attachment.
	 * @return An attachment object.
	 */
	public ProposalPersonAttachment fetchPersonAttachmentById(Integer attachmentId);

	/**
	 * This method is used to Fetch Eligibility criteria.
	 * @param proposal id - proposalId.
	 * @return Eligibility criteria value.
	 */
	public Boolean fetchEligibilityCriteria(Integer proposalId);
	
	/** 
	 * Delete Proposal budget rates
	 * @param proposalRateList
	 */
	public void deleteProposalBudgetRate(List<FibiProposalRate> proposalRateList);

	/**
	 * Fetch proposal person based on proposalId
	 * @param proposalId
	 * @return list of ProposalPerson
	 */
	public List<ProposalPerson> fetchProposalPersonBasedOnProposalId(Integer proposalId);

	/**
	 * Fetch Proposal person attachments
	 * @param proposalPersonId
	 * @return list of ProposalPerson attachment
	 */
	public List<ProposalPersonAttachment> fetchProposalPersonAttachmentBasedOnProposalPersonId(Set<Integer> proposalPersonId);

	/**
	 * Fetch fetchGrantTypeCodeBasedOnProposalId
	 * @param proposalId
	 * @return GrantTypeCode
	 */
	public Integer fetchGrantTypeCodeBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch attachment based on Id.
	 * @param attachmentId - Id of the attachment.
	 * @return An attachment object.
	 */
	public ProposalAttachment fetchProposalAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch the proposal name based on Id.
	 * @param proposalId - Id of the proposal.
	 * @return proposal name.
	 */
	public String getProposalName(Integer proposalId);

	/**
	 * This method is used to fetch fetch ProposalAggregator Based On PersonId And ProposalId.
	 * @param proposalId - Id of the proposal
	 * @param proposalId - Id of the proposal.
	 * @return proposal name.
	 */
	public ProposalPersonRoles fetchProposalAggregatorBasedOnPersonIdAndProposalId(String personId, Integer proposalId);

	/**
	 * This method is used to fetch ProposalPersonRole bsed on proposalPersonRoleId
	 * @param proposalPersonRoleId
	 * @return object of ProposalPersonRole
	 */
	public ProposalPersonRole fetchProposalPersonRoles(Integer proposalPersonRoleId);

	/**
	 * This method is used to delete person attachment.
	 * @param attachmentId - Id of Attachment.
	 */
	public void deleteProposalPersonAttachment(Integer attachmentId);

	/**
	 * This method is used to save or update a ProposalKPI
	 * @param ProposalKPI - Object of a ProposalKPI.
	 * @return An object of ProposalKPI.
	 */
	public ProposalKPI saveOrUpdateProposalKPI(ProposalKPI proposalKpi);

	/**
	 * This method is used to save or update a ProposalKPI
	 * @param ProposalKPI - Object of a ProposalKPI.
	 * @return An object of ProposalKPI.
	 */
	public ProposalKPICriteria saveOrUpdateProposalKPICriterias(ProposalKPICriteria proposalKPICriteria);

	/**
	 * This method is used to fetch all KPIs based on grant call id
	 * @param grantCallId - grantCallId
	 * @return list of KPIs
	 */
	public List<ProposalKPI> fetchAllProposalKPI(Integer proposalId);
 
	public ProposalExtension saveOrUpdateProposalExtension(ProposalExtension proposalExtension);

	public ProposalExtension fetchProposalExtensionById(Integer proposalId);

	/**
	 * This method is used to delete proposalKPI based on proposalKPI.
	 * @param proposalKPI.
	 * @return success message.
	 */
	public String deleteProposalKPI(ProposalKPI proposalKPI);

	/**
	 * This method return scores corresponding to workFlowDetailId
	 * @param workFlowDetailId
	 * @return
	 */
	public Integer getScoreByWorkflowDetailId(Integer workFlowDetailId);

	/**
	 * This method is used to fetch the Person Can Score.
	 * @param proposalId - Id of the proposal.
	 * @param personId - Id of the person.
	 * @param workflowDetailId -Id of workflowDetail
	 * @return canScore.
	 */
	public Boolean fetchPersonCanScore(Integer proposalId, String personId, Integer workflowDetailId);

	/**
	 * This method is used to save and update
	 * WorkflowReviewerScore,WorkflowReviewerComments,WorkflowReviewerAttachments
	 * @param WorkflowReviewerScore - workflowReviewerScore
	 */
	public WorkflowReviewerScore saveOrUpdateWorkflowScoreDetail(WorkflowReviewerScore workflowReviewerScore);

	/**
	 * This method is used to fetch WorkflowReviewerAttachment By workflowReviewerAttmntsId.
	 * @param workflowReviewerAttmntsId - Id of Attachment.
	 */	
	public WorkflowReviewerAttachment fetchWorkflowReviewerAttachmentById(Integer workflowReviewerAttmntsId);

	/**
	 * This method is used to fetch WorkflowReviewerDetails based on workflowDetailId.
	 * @param workflowDetailId - Id of the WorkflowReviewerDetail.
	 * @return WorkflowReviewerScoreDetails.
	 */
	public List<WorkflowReviewerScore> fetchAllWorkflowReviewerDetails(Integer workflowDetailId);

	public boolean checkForProposalEvaluationPanelRank(Integer mapId, Integer proposalId);

	/**
	 * This method is used to fetch the scores.
	 * @param grantCallId- grantcall Id
	 * @return List of Score.
	 */
	public ProposalEvaluationScore fetchEvaluationScoreByGrantCallandProposalId(Integer grantCallId,Integer proposalId);
	
	public BigDecimal fetchMaximumBudget(Integer proposalId);

	/**
	 * This method is used fetch Proposals By GrantCallId and Proposal Status
	 * @param grantCallId :- proposals grantCall Id
	 * @param proposalStatus :- List of proposal Status
	 * @return Proposal Types Proposal.
	 */
	public List<Proposal> fetchProposalsByGrantCallIdAndProposalStatus(Integer grantCallId, List<Integer> proposalStatus);

	/**
	 * This method is used to get submitted proposal List.
	 * 
	 * @param grantcallId
	 * @return it returns proposal list based on grantCallId.
	 */
	public List<Proposal> fetchSubmittedProposalList(Integer grantCallId);
	
	/**
	 * 
	 * @param workflowDetailId
	 * @param scoringCriteriaTypeCode
	 * @return
	 */
	public List<WorkflowReviewerScore> fetchWorkflowReviewer(Integer workflowDetailId, String scoringCriteriaTypeCode);

	/**
	 * This method is used to save or update a proposal comment
	 * @param proposalComment - Object of a ProposalComment.
	 * @return An object of proposalComment.
	 */
	public ProposalComment saveOrUpdateProposalComment(ProposalComment proposalComment);

	/**
	 * This method is used to fetch a proposal comments.
	 * @param proposalId
	 * @param userName
	 * @param isMaintainPrivateComment
	 * @return it returns proposalComment list based on proposal id , user name and maintain private comment right
	 */
	public List<ProposalComment> fetchProposalCommentsByParams(Integer proposalId, String userName, Boolean isViewPrivateComment);

	/**
	 * This method is used to fetch  persons based on grantCallId and isMainPanel.
	 * @param grantCallId - Id of the GrantCall.
	 * @return list of personIds.
	 */
	public List<String> fetchMainPanelPersonIdsByGrantCallId(Integer grantCallId);

	/**
	 * This method is used to check keyword is already exist or not
 	 * @param scienceKeyword - new science keyword
	 * @return true if exist else false.
	 */
	public boolean isKeywordExist(String scienceKeyword);

	/**
	 * This method is used to fetch proposal list based on statusCode
	 * @param proposalStatusCode - statusCode of proposal.
	 * @return List of proposals.
	 */
	public List<Proposal> fetchProposalsByStatusCode(Integer proposalStatusCode);

	/**
	 * This method is used to fetch the proposal person roles based on proposal id and person id
	 * @param personId
	 * @param proposalId
	 * @return list of proposal person roles
	 */
	public List<ProposalPersonRoles> fetchProposalPersonRolesByParams(String personId, Integer proposalId, List<Integer> roleIds);

	/**
	 * This method is used to delete all the proposal milestone
	 * @param proposalMileStones
	 */
	public void deleteProposalMileStones(List<ProposalMileStone> proposalMileStones);

	/**
	 * This method is used to delete the workflow score comment by comment id
	 * @param workflowReviewerCommentsId
	 */
	public void deleteWorkflowScoreComments(Integer workflowReviewerCommentsId);

	/**
	 * This method is used to delete the workflow reviewer attachment by attachment id
	 * @param workflowReviewerAttmntsId
	 */
	public void deleteWorkflowReviewerAttachment(Integer workflowReviewerAttmntsId);

	/**
	 * This method is used to get the grant call id by proposal id
	 * @param proposalId
	 * @return grant call id
	 */
	public Integer getGrantCallIdByProposalId(Integer proposalId);

	/**
	 * This method is used get the person has the given rights or not
	 * @param personId
	 * @param rightName
	 * @param proposalId
	 * @return true or false
	 */
	public Boolean isPersonHasRightInProposal(String personId, List<String> rightName, Integer proposalId);

	/**
	 * This method is used get the proposalOrganizations
	 * @param proposalId
	 * @return List<ProposalOrganization>
	 */
	public List<ProposalOrganization> loadProposalOrganization(Integer proposalId);

	/**
	 * This method is used save or update the proposalOrganizations
	 * @param proposalOrganization
	 * @return updated ProposalOrganization
	 */
	public void saveOrUpdateProposalOrganization(ProposalOrganization proposalOrganization);

	/**
	 * This method is used save or update the ProposalCongDistrict
	 * @param proposalCongDistrict
	 */
	public void saveOrUpdateProposalCongDistrict(ProposalCongDistrict proposalCongDistrict);

	/**
	 * This method is used save or update the ProposalCongDistrict
	 * @param congressionalDistrict
	 */
	public void saveOrUpdateCongDistrict(CongressionalDistrict congressionalDistrict);

	/**
	 * This method is used delete proposal organization and its related tables
	 * @param proposalOrganizationId
	 */
	public void deleteProposalOrganization(Integer proposalOrganizationId);

	/**
	 * This method is used delete proposal congressional district
	 * @param proposalCongDistrictId
	 */
	public void deleteProposalCongDistrict(Integer proposalCongDistrictId);


	public Boolean checkUnitNumberHasRight(String unitNumber, String personId, String rights);

	/**
	 * This method is used to check person has right in proposal person roles
	 *  @param personId
	 *  @param rightName
	 *  @param proposalId
	 */
	public Boolean isPersonHasRightInProposal(String personId, String rightName, Integer proposalId);

	/**
	 * This method is used to save or update proposal keyword
	 * @param proposalKeyword.
	 * @return 
	 */
	public  ProposalKeyword saveOrUpdateProposalKeyword(ProposalKeyword proposalKeyword);

	/**
	 * This method is used to delete proposal keyword
	 * @param proposalId.
	 * @return 
	 */
	public void deleteProposalKeyWords(Integer proposalId);

	/**
	 * This method is used to delete proposal milestone
	 * @param proposalId.
	 * @return 
	 */
	public void deleteProposalMilestone(Integer proposalId);

	/**
	 * This method is used to get science keyword by param
	 * @param description.
	 * @return 
	 */
	public String getScienceKeyword(String description);

	/**
	 * This method is used to get kpi type by param
	 * @param description.
	 * @return 
	 */
	public String getKpiTypeBasedOnParam(String description);

	/**
	 * This method is used to get kpi criteria by param
	 * @param description.
	 * @return 
	 */
	public String getkpiCrieriaBasedOnParam(String description);

	/**
	 * This method is used to get special review type by param
	 * @param reviewType.
	 * @return 
	 */
	public String getSpecialReviewType(String reviewType);

	/**
	 * This method is used to delete proposal kpi
	 * @param proposalId.
	 * @return 
	 */
	public void deleteProposalKPIs(Integer proposalId);

	/**
	 * This method is used to fetch Proposal LeadUnitNumber By ProposalId
	 * @param proposalId
	 * @return
	 */
	public String fetchProposalLeadUnitNumberByProposalId(Integer proposalId);

	/**
	 * This method is used to fetch Proposal person Attachments By ProposalId
	 * @param proposalId
	 * @return list of proposal Person Attachment
	 */
	public List<ProposalPersonAttachment> loadProposalKeyPersonAttachments(Integer proposalId);

	/**
	 * @param proposalHistory
	 */
	public ProposalHistory saveOrUpdateProposalHistory(ProposalHistory proposalHistory);

	/**
	 * @param proposalId
	 * @return
	 */
	public List<ProposalHistory> getProposalHistoryBasedOnProposalId(Integer proposalId);

	/**
	 * @param proposalId
	 * @throws Exception 
	 */
	public void updateDocumentStatusCode(Integer proposalId, String documentStatusCode) throws Exception;

	/**
	 * @param fileId
	 * @return
	 */
	public Boolean getIsFileDataIdFound(String fileId);

	/**
	 * @param proposalId
	 * @return
	 */
	public String getProposalTitleByProposalId(Integer proposalId);

	/**
	 * @param documentStatusCode
	 * @return
	 */
	public ProposalDocumentStatus fetchProposalDocumentStatus(String documentStatusCode);

	/**
	 * This method is used to get the key personnel attachment uploaded distinct person name and id
	 *
	 * @param proposalId
	 * @return
	 */
	List<Object[]> loadProposalKeyPersonnelPersons(Integer proposalId);

	/**
	 * This method is used to delete key personnel attachment
	 * @param attachmentId
	 */
	void deleteKeyPersonnelAttachment(Integer attachmentId);
	/**
	 * @param grantCallId
	 * @param proposalId
	 * @param piRoleCode
	 * @return will check eligibility and return status and message
	 */
	public HashMap<String, Object> checkGrantCallEligibilty(Integer grantCallId, String personId, Integer piRoleCode);

	/**
	 * This method is used to fetch Proposal person Attachments By ProposalId and documentId
	 *
	 * @param proposalId
	 * @param documentId
	 * @return
	 */
	List<ProposalPersonAttachment> loadProposalKeyPersonAttachments(Integer proposalId, Integer documentId);

	/**
	 * @param proposalId
	 * @param personId
	 * @param status
	 * @param nonEmployee 
	 */
	public void updateProposalPersonCertification(Integer proposalId, String personId, Boolean status, Boolean nonEmployee);

	/**
	 * @param proposalId
	 * @return list of proposal persons
	 */
	public List<ProposalPerson> proposalPersonsForCertification(Integer proposalId);

	/**
	 * @param proposalId
	 * @return Boolean
	 */
	public Boolean isAllProposalPersonCertified(Integer proposalId);

	/**
	 * @param personId
	 * @param proposalId 
	 * @return String
	 */
	public String isProposalPersonTrainingCompleted(String personId, Integer proposalId);

	/**
	 * @param proposalCommentAttachment
	 * @return
	 */
	public ProposalCommentAttachment saveOrUpdateProposalCommentAttachment(ProposalCommentAttachment proposalCommentAttachment);

	/**
	 * @param proposalCommentId
	 * @return
	 */
	public List<ProposalCommentAttachment> fetchProposalCommentAttachments(Integer proposalCommentId);

	/**
	 *This method is used to fetch comment Attachment using attachment Id
	 * @param attachmentId
	 * @return proposalCommentAttachment
	 */
	public ProposalCommentAttachment getProposalCommentAttachmentById(Integer attachmentId);

	/**
	 * @param proposalId
	 */
	public void deleteProposalComment(Integer proposalId);

	/**
	 * @param proposalId
	 */
	public void deleteProposalCommentAttachement(List<Integer> proposalCommentIds);
	
	/**
	 * @param proposalId
	 * @return List<Integer>
	 */
	public List<Integer> getAllProposalComment(Integer proposalId);

	/**
	 * This method is used to fetch the last version proposal personal attachment by person id
	 *
	 * @param proposalPersonId Person ID
	 * @return list of ProposalPersonAttachment
	 */
    List<ProposalPersonAttachment> fetchProposalPersonAttachmentWithLastVersion(Set<Integer> proposalPersonId);
}
