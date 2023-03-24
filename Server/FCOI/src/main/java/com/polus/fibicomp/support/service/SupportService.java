package com.polus.fibicomp.support.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.prereview.vo.PreReviewVO;

@Transactional
@Service
public interface SupportService {

	public String loadSupportQuestion(PreReviewVO vo);

	/**
	 * This method is used to create a Support for a proposal.
	 * @param PreReviewVO - Object of ProposalVO class.
	 * @return A String of details of Support
	 */
	public String createSupportQuestion(PreReviewVO vo);

	/**
	 * This method is used to add Support comment by the reviewer for a proposal.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the and comment attachments.
	 * @return A String of details of proposal data with pre review data.
	 */
	public String addSupportComment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to fetch a sorted Support Questions.
	 * @param vo - Object of PreReviewVO class.
	 * @return vo object to sorted reviews.
	 */
	public String showUnansweredQuestions(PreReviewVO vo);

	/**
	 * This method is used to download proposal Support attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadSupportAttachment(Integer attachmentId);

	public String addPreSupportCommentForWaf(MultipartFile file, String formDataJson);

}
