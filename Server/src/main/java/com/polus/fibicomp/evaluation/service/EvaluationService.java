package com.polus.fibicomp.evaluation.service;

import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.evaluation.vo.EvaluationVO;
import com.polus.fibicomp.grantcall.vo.EvaluationMainPanelVO;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.vo.ProposalVO;

@Service
public interface EvaluationService {

	/**
	 * This method is used to add a new proposal review.
	 * 
	 * @param vo - Object of ProposalVO class.
	 * @return vo object to create proposal review.
	 */
	public String createReview(ProposalVO vo);

	/**
	 * This method is used to complete a new proposal review.
	 * 
	 * @param files        - attached files.
	 * @param formDataJSON - form data for the and comment attachments.
	 * @return vo object to complete proposal review.
	 */
	public String approveOrDisapproveReview(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to add review comment by the reviewer for a proposal.
	 * 
	 * @param files        - attached files.
	 * @param formDataJSON - form data for the and comment attachments.
	 * @return A String of details of proposal data with review data.
	 */
	public String addProposalReviewComment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to download proposal review attachment.
	 * 
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadProposalReviewAttachment(Integer attachmentId);

	/**
	 * This method is used to create new review for re submission.
	 * 
	 * @param proposal - Object of the proposal.
	 * @return Proposal object.
	 */
	public Proposal createReviewForResubmit(Integer proposalId);

	/**
	 * This method is used to fetch a sorted proposal review.
	 * @param vo - Object of ProposalVO class.
	 * @return vo object to sorted proposal reviews.
	 */
	public String fetchSortedReviews(ProposalVO vo);

	/**
	 * This method is used to get list of proposals summary and review Summary.
	 * @param proposalId - ProposalId
	 * @param personId - personId
	 * @return A String of details of proposal data with review data.
	 */
	public String getProposalAndReviewSummary(Integer proposalId, String personId);

	/**
	 * This method is used to delete a review comment.
	 * @param vo - Object of ProposalVO class.
	 * @return vo object after deleting reviews.
	 */
	public String deleteReviewComment(ProposalVO vo);

	/**
	 * This method is used to add a reviewer .
	 * @param vo - Object of ProposalVO class.
	 * @return vo object after adding reviewer.
	 */
	public String addReviewer(ProposalVO vo);

	/**
	 * This method is used to create a Grant Admin automatic Review for RCBF proposal .
	 * @param vo - Object of ProposalVO class.
	 * @return vo object after adding reviewer.
	 */
	public void createGrantAdminReview(Proposal proposal);

	/**
	 * This method is used to send proposal review reminder notification for admins.
	 * @param vo - Object of ProposalVO class.
	 * @return vo.
	 */
	public String sendAdminReviewReminderNotification(ProposalVO vo);

	/**
	 * This method is used to send proposal review reminder notification for PI.
	 * @param vo - Object of ProposalVO class.
	 * @return vo.
	 */
	public String sendPIReviewReminderNotification(ProposalVO vo);

	/**
	 * This method is used to fetch EvaluationPanels of a GrantCall 
	 * @param Object of EvaluationVO class.
	 * @return String object of EvaluationVO
	 */
	public String fetchEvaluationPanels(EvaluationVO vo);

	/**
	 * This method is used to save ProposalEvaluationPanelDetails
	 * @param Object of EvaluationVO class.
	 * @return String object of EvaluationVO
	 */
	public String saveProposalEvaluationPanelDetails(EvaluationVO vo);

	/**
	 * This method is used to start evaluation with ProposalEvaluationPanelList
	 * @param Object of EvaluationVO class.
	 * @return String object of ProposalVO
	 */
	public String startEvaluationWithFinalEvaluationPanelList(EvaluationVO vo);

	/**
	 * This method is used to add person to Evaluation Panel.
	 * @param Object of EvaluationVO class.
	 * @return String object of EvaluationVO
	 */
	public String addEvaluationPanelPerson(EvaluationVO vo);

	/**
	 * This method is used to delete proposal Evaluation Panel person.
	 * @param Object of EvaluationVO class.
	 * @return String object of EvaluationVO
	 */
	public String deleteEvaluationPanelPerson(EvaluationVO vo);

	/**
	 * This method is used to save proposal evaluation
	 * @param vo
	 * @return
	 */
	public String saveEvaluation(EvaluationVO vo);

	/**
	 * This method is used to get evaluation persons based on user role and unit number.
	 * @param personRoleId - personRoleId
	 * @param unitNumber - unitNumber
	 * @return person list.
	 */
	public String getEvaluationPersonsBasedOnRole(Integer personRoleId, String unitNumber);

	/**
	 * This method is used to complete a new proposal review.
	 * @param vo        - Object of proposal VO.
	 * @return vo object to complete proposal review.
	 */
	public String approveOrDisapproveReviewForWaf(ProposalVO vo);

	/**
	 * This method is used to complete review review for Waf.
	 * @param proposalVO        - Object of ProposalVO.
	 * @return String value ProposalVO.
	 */
	public String addReviewCommentForWaf(ProposalVO proposalVO);

	/**
	 * This method is used to prepare ProposalReview Details.
	 * @param proposalVO        - Object of ProposalVO.
	 * @return list of proposal reviews.
	 */
	public List<ProposalReview> prepareProposalReviewDetail(ProposalVO vo);

	/**
	 * This method is used to export the grant call evaluation report
	 * @param vo
	 * @param response
	 * @return exported report as byte array
	 */
	public ResponseEntity<byte[]> exportGrantCallEvaluationReport(EvaluationMainPanelVO vo, HttpServletResponse response);

}
