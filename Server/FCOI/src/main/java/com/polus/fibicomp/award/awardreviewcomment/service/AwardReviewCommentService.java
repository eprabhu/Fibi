package com.polus.fibicomp.award.awardreviewcomment.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.awardreviewcomment.vo.AwardReviewCommentVO;

@Transactional
@Service(value = "awardReviewCommentService")
public interface AwardReviewCommentService {

	/**
	 * This method is used to save or update AwardReviewComment.
	 * @param vo - Object of AwardReviewCommentVO class.
	 * @return A string of details of a AwardReviewComment.
	 */
	public String saveOrUpdateAwardReviewComment(AwardReviewCommentVO vo);

	/**
	 * This method is used to fetch list of Award Review Comments based on awardId.
	 * @param vo - Object of AwardReviewCommentVO class.
	 * @return A string of list of AwardReviewComments.
	 */
	public String fetchAwardReviewCommentsByAwardId(AwardReviewCommentVO vo);

	/**
	 * This method is used to save or update AwardReviewComment.
	 * @param vo - Object of AwardReviewCommentVO class.
	 * @return A string of details of a AwardReviewComment.
	 */
	public String resolveAwardReviewComment(AwardReviewCommentVO vo);

	/**
	 * This method is used to get persons who have commented and can comment against an award.
	 * @param vo - Object of AwardReviewCommentVO class.
	 * @return A string of list of person details(personID & fullName).
	 */
	public String getListOfAwardReviewPersons(AwardReviewCommentVO vo);

	/**
	 * This method is used to delete AwardReviewComment
	 * @param vo - Object of AwardReviewCommentVO class.
	 * @return A string of deleted message.
	 */
	public String deleteAwardReviewComment(AwardReviewCommentVO vo);

}
