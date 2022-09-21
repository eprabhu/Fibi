package com.polus.fibicomp.award.awardreviewcomment.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.AwardReviewComment;

@Transactional
@Service
public interface AwardReviewCommentDao {

	/**
	 * This method is used to save or update an AwardReviewComment.
	 * @param awardReviewComment - Object of an AwardReviewComment.
	 * @return An object of AwardReviewComment.
	 */
	public AwardReviewComment saveOrUpdateAwardReviewComment(AwardReviewComment awardReviewComment);

	/**
	 * This method is used to get AwardReviewComment based on awardReviewCommentId.
	 * @param awardReviewCommentId - id of AwardReviewComment.
	 * @return An object of AwardReviewComment.
	 */
	public AwardReviewComment getAwardReviewCommentById(Integer awardReviewCommentId);

	/**
	 * This method is used to get AwardReviewComments based on awardId.
	 * @param awardId - id of Award.
	 * @param isViewPrivateComment - isViewPrivateComment
	 * @param userName - userName
	 * @return A list of AwardReviewComments.
	 */
	public List<AwardReviewComment> getAwardReviewCommentsByAwardId(Integer awardId, Boolean isViewPrivateComment, String userName, String awardNumber);

	/**
	 * This method is used to get personIds of person who have commented against an award, based on awardId.
	 * @param awardId - id of Award.
	 * @return A list of PersonIds.
	 */
	public List<String> getReviewerPersonIdAndFullNameByAwardId(Integer awardId);

	/**
	 * This method is delete AwardReviewComment by id
	 * @param awardReviewCommentId - id of AwardReviewComment.
	 * @return A String of deleted message.
	 */
	public String deleteAwardReviewComment(Integer awardReviewCommentId);

	/**
	 * This method is to get childCommentIds by awardReviewCommentId
	 * @param parentReviewCommentId - id of AwardReviewComment.
	 * @return A String of deleted message.
	 */
	public List<Integer> getChildAwardReviewCommentIdsByAwardReviewCommentId(Integer parentReviewCommentId);

}
