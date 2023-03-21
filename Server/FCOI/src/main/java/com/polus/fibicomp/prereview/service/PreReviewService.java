package com.polus.fibicomp.prereview.service;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.prereview.vo.PreReviewVO;

@Transactional
@Service
public interface PreReviewService {

	/**
	 * This method is used to create a pre review by PI for a proposal.
	 * @param proposalVO - Object of ProposalVO class.
	 * @return A String of details of pre review.
	 */
	public String createPreReview(PreReviewVO vo);

	/**
	 * This method is used to add pre review comment by the reviewer for a proposal.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the and comment attachments.
	 * @param request 
	 * @return A String of details of proposal data with pre review data.
	 */
	public String addPreReviewComment(MultipartFile[] files, String formDataJSON, HttpServletRequest request);

	/**
	 * This method is used to approve or disapprove pre review by the reviewer for a proposal.
	 * @param request 
	 * @param proposalVO - Object of ProposalVO class.
	 * @return A String of details of proposal data with pre review data.
	 */
	public String approveOrDisapprovePreReview(MultipartFile[] files, String formDataJSON, HttpServletRequest request);

	/**
	 * This method is used to download proposal pre review attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadPreReviewAttachment(Integer attachmentId);

	/**
	 * This method is used to fetch a sorted  review.
	 * @param vo - Object of PreReviewVO class.
	 * @return vo object to sorted reviews.
	 */
	public String fetchSortedReviews(PreReviewVO vo);

	public String loadPreReview(PreReviewVO vo);

	/**
	 * This method is used to add pre review comment by the reviewer for a proposal for Waf.
	 * @param vo - object of prereview.
	 * @return A String of details of proposal data with pre review data.
	 */
	public String addPreReviewCommentForWaf(PreReviewVO vo);

	/**
	 * This method is used to approve or disapprove pre review by the reviewer for a proposal for Waf.
	 * @param vo - object of prereview.
	 * @return A String of details of proposal data with pre review data.
	 */
	public String approveOrDisapprovePreReviewForWaf(PreReviewVO vo);

}
