package com.polus.fibicomp.evaluation.dao;

import java.sql.ResultSet;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.evaluation.pojo.EvaluationRecommendation;
import com.polus.fibicomp.evaluation.pojo.EvaluationStop;
import com.polus.fibicomp.evaluation.pojo.FinalEvaluationStatus;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanelPersons;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationStatusFlow;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.evaluation.pojo.ReviewAttachment;
import com.polus.fibicomp.evaluation.pojo.ReviewComment;
import com.polus.fibicomp.evaluation.pojo.ReviewStatus;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.roles.pojo.Role;

@Service
public interface EvaluationDao {

	/**
	 * This method is used to fetch ProposalReview by criteria.
	 * 
	 * @param proposalId      - Id of Proposal object.
	 * @param personId        - Id of reviewer person id.
	 * @param preReviewStatus - status code of ReviewStatus object.
	 * @return An list of ProposalPreReview.
	 */
	public List<ProposalReview> fetchProposalReviewsByCriteria(Integer proposalId, String personId, Integer proposalReviewStatus);

	/**
	 * This method is used to get review status by status code.
	 * 
	 * @param statusCode - Id of the PreReviewStatus.
	 * @return An object of review status.
	 */
	public ReviewStatus getReviewStatusByStatusCode(Integer statusCode);

	/**
	 * This method is used to save or update proposal review.
	 * 
	 * @param proposalReview - Object of ProposalReview class.
	 * @return An object of ProposalReview.
	 */
	public ProposalReview saveOrUpdateReview(ProposalReview proposalReview);

	/**
	 * This method is used to load all ProposalReviews by Proposal id.
	 * 
	 * @param proposalId - Id of Proposal object.
	 * @return An list of ProposalReview.
	 */
	public List<ProposalReview> loadAllProposalReviewsByProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalReviewAttachment by Id.
	 * 
	 * @param attachmentId - Id of ProposalReviewAttachment object.
	 * @return An object of ProposalReviewAttachment.
	 */
	public ReviewAttachment fetchAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch inprogress proposal reviews list.
	 * 
	 * @param proposalId       - proposal id.
	 * @param reviewTypeCode   - review type code.
	 * @param reviewStatusCode - review status code.
	 * @return list of proposal reviews.
	 */
	public List<ProposalReview> getInprogressReviews(Integer proposalId, Integer reviewTypeCode, Integer reviewStatusCode);

	/**
	 * This method is used to fetch proposal reviews list.
	 * 
	 * @param proposalId     - proposal id.
	 * @param reviewTypeCode - review type code.
	 * @return list of proposal reviews.
	 */
	public List<ProposalReview> getProposalReviews(Integer proposalId, Integer reviewTypeCode);

	/**
	 * This method is used to save proposal review comment.
	 * 
	 * @param reviewComment - Object of ProposalReview class.
	 * @return An object of ReviewComment.
	 */
	public ReviewComment saveReviewComment(ReviewComment reviewComment);

	/**
	 * This method is used to fetch sorted proposal reviews list. 
	 * @param proposalId     - proposal id.
	 * @param sortBy - sort by field name.
	 * @param reverse - reverse type asc or desc
	 * @return list of proposal reviews.
	 */
	public List<ProposalReview> fetchSortedReviews(Integer proposalId, String sortBy, String reverse);

	/**
	 * This method is used to fetchProposalStatusBasedOnReview.
	 * 
	 * @param newProposalReview - Object of ProposalReview class.
	 * @return An Integer of status Code.
	 */
	public Integer fetchProposalStatusBasedOnReview(ProposalReview newProposalReview);

	/**
	 * This method is used to check if any same type of review completed once.
	 * 
	 * @param newProposalReview - Object of ProposalReview class.
	 * @return An boolean value.
	 */
	public Boolean fetchProposalReviewIsCompletedOnce(ProposalReview newProposalReview);

	/**
	 * This method is used to get ProposalReview By ReviewId.
	 * 
	 * @param reviewId - Object of ProposalReview class.
	 * @return A ProposalReview of object.
	 */
	public ProposalReview getProposalReviewByReviewId(Integer reviewId);

	/**
	 * This method is used to get PersonRole By personId.
	 * 
	 * @param logginPersonId -logged in personId.
	 * @return List of  PersonRoles.
	 */
	public List<PersonRoles> fetchPersonRoleOnEvaluationBasedOnPersonId(String personId, List<Integer> roleIds);

	/**
	 * This method is used to getAllEvaluationsStop
	 * 
	 * @return List of  EvaluationStop.
	 */
	public List<Integer> getDistintRoleIdFromEvaluationsStop();

	/**
	 * This method is used to checkPersonHasRank in current stop
	 * @param proposalId -proposalId .
	 * @return Boolean value.
	 */
	public Boolean checkPersonHasRank(Integer proposalId, String personId);

	/**
	 * This method is used to get ProposalEvaluation Details 
	 * @param proposalReview -object of ProposalReview .
	 * @return ProposalEvaluationStatusFlow object.
	 */
	public ProposalEvaluationStatusFlow getProposalEvaluationDetails(ProposalReview proposalReview);

	/**
	 * This method is used to get get All EvaluationRecomendation Details 
	 * @return getAllEvaluationRecomendation object.
	 */
	public List<EvaluationRecommendation> getAllEvaluationRecomendation();

	/**
	 * This method is used to get get All get Final Evaluation Status Details 
	 * @return getFinalEvaluationStatus object.
	 */
	public List<FinalEvaluationStatus> getFinalEvaluationStatus();

	/**
	 * This method is used to fetchEvaluationStopDetails 
	 * @param rcbfProposal -rcbfProposal type code .
	 * @param statusCode - proposalStatusCode
	 * @return EvaluationStop object.
	 */
	public EvaluationStop fetchEvaluationStopDetails(String rcbfProposal, Integer statusCode);

	/**
	 * This method is used to get EvaluationRecommendation By StatusCode 
	 * @param statusCode - EvaluationRecommendation status code
	 * @return EvaluationRecommendation object.
	 */
	public EvaluationRecommendation getEvaluationRecommendationByStatusCode(Integer statusCode);

	/**
	 * This method is used to check Person Has Recommendation in current stop
	 * @param proposalId -proposalId .
	 * @param personId - logged in personId
	 * @return Boolean value.
	 */
	public Boolean checkPersonHasRecommendation(Integer proposalId, String personId);

	/**
	 * This method is used to check fetch Revision Requested Reviews
	 * @param proposalId -proposalId 
	 * @return Boolean value.
	 */
	public List<ProposalReview> fetchRevisionRequestedReview(Integer proposalId);

	/**
	 * This method is used to getReviewStops.
	 * @param statusCode - proposal statusCode.
	 * @return review stop list.
	 */
//	public List<EvaluationStop> getReviewStopEvaluvationBasedOnProposalStatus(Integer statusCode);

	/**
	 * This method is used to getReviewStops.
	 * @param statusCode - proposal statusCode.
	 * @param activityTypeCode - proposal activityTypeCode.
	 * @return review stop list.
	 */
	public List<EvaluationStop> getReviewStopEvaluvation(Integer statusCode, String activityTypeCode);

	/**
	 * This method is used to fetchEvaluationStopForReviewer.
	 * @param statusCode - proposal statusCode.
	 * @param activityTypeCode - proposal activityTypeCode.
	 * @param roleId - roleId of Proposal Review.
	 * @return review stop list.
	 */
	public EvaluationStop fetchEvaluationStopForReviewer(Integer statusCode, String activityTypeCode, Integer roleId);

	/**
	 * This method is used to checkPersonHasReview.
	 * @param reviewerPersonId - reviewerPersonId.
	 * @param proposalId - proposalId.
	 * @param roleId - roleId of Proposal Review.
	 * @return boolean value.
	 */
	public Boolean checkPersonHasReview(String reviewerPersonId, Integer proposalId, Integer roleId);

	/**
	 * This method is used to save EvaluationPanelsList
	 * @param proposalEvaluationPanel - object of ProposalEvaluationPanel.
	 * @return Object of ProposalEvaluationPanel.
	 */
	public ProposalEvaluationPanel saveProposalEvaluationPanelDetails(ProposalEvaluationPanel evaluationPanel);

	/**
	 * This method is used to fetch ProposalEvaluationPanelsList.
	 * @param proposalId - proposalId of the Proposal.
	 * @return List of ProposalEvaluationPanel.
	 */
	public List<ProposalEvaluationPanel> fetchProposalEvaluationPanelsByProposalId(Integer proposalId);

	/**
	 * This method is used to delete proposal evaluation panel list.
	 * @param proposalEvaluationPanels - List of ProposalEvaluationPanel.
	 * @return
	 */
	public void deleteProposalEvaluationPanels(List<ProposalEvaluationPanel> proposalEvaluationPanels);

	/**
	 * This method is used to build evaluation panel.
	 * @param moduleItemKey - moduleItemKey
	 * @param moduleCode- moduleCode
	 * @param personId- personId
	 * @param updateUser- updateUser
	 * @param actionType- actionType
	 * @param subModuleItemKey - subModuleItemKey
	 * @param subModuleCode - subModuleCode
	 * @return
	 */
	public boolean buildEvaluationPanel(String moduleItemKey, Integer moduleCode, String personId, String updateUser, String actionType, String subModuleItemKey, Integer subModuleCode);

	/**
	 * This method is used to get max approver number sequence.
	 * @param proposalEvaluationPanelId - proposalEvaluationPanelId of ProposalEvaluationPanelPersons.
	 * @return
	 */
	public Integer getMaxApproverNumber(Integer proposalEvaluationPanelId);

	/**
	 * This method is used to fetch max approver number sequence.
	 * @param proposalEvaluationPanelId - proposalEvaluationPanelId of ProposalEvaluationPanelPersons.
	 * @return
	 */
	public ProposalEvaluationPanel fetchProposalEvaluationPanelById(Integer proposalEvaluationPanelId);

	/**
	 * This method is used to add or update evaluation panel person.
	 * @param proposalEvaluationPanelPerson - object of ProposalEvaluationPanelPersons.
	 * @return
	 */
	public ProposalEvaluationPanelPersons saveEvaluationPanelPerson(ProposalEvaluationPanelPersons proposalEvaluationPanelPerson);

	/**
	 * This method is used to fetch ProposalEvaluationPanelPersons by proposalEvaluationPanelPersonId.
	 * @param proposalEvaluationPanelPersonId - object of ProposalEvaluationPanelPersons.
	 * @return
	 */
	public ProposalEvaluationPanelPersons fetchProposalEvaluationPanelPersonById(Integer proposalEvaluationPanelPersonId);

	/**
	 * This method is used to delete a ProposalEvaluationPanelPerson.
	 * @param proposalEvaluationPanelPerson - object of ProposalEvaluationPanelPersons.
	 * @return
	 */
	public void deleteProposalEvaluationPanelPerson(ProposalEvaluationPanelPersons proposalEvaluationPanelPerson);

	/**
	 * @param statusCode
	 * @param activityTypeCode
	 * @return
	 */
	public List<Integer> getEvaluationRolesBasedOnProposalStatusAndActivityTypeCode(Integer statusCode, String activityTypeCode);

	public List<Role> getRolesBasedonRolesIds(Set<Integer> returnRoles);

	/**
	 * This method is used in get the list of person who removed from the panel before restart evaluation panel
	 * @param moduleItemKey
	 * @param moduleCode
	 * @param submoduleItemKey
	 * @param submoduleCode
	 * @return list of person id and map name
	 */
	public ResultSet getRemovedPersonListFromEvaluation(String moduleItemKey, Integer moduleCode, String submoduleItemKey, Integer submoduleCode);

}
