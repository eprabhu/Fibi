package com.polus.fibicomp.reviewcomments.dao;

import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;

import java.util.List;

public interface ReviewCommentDao {

    /**
     * This method used to save Review Comments related objects
     *
     * @param object
     */
    void saveObject(Object object);

    /**
     * This method fetch all review based on certain criteria
     *
     * @param reviewCommentsDto
     * @return
     */
    List<DisclComment> fetchReviewComments(ReviewCommentsDto reviewCommentsDto);

    /**
     * This method is used to delete object
     *
     * @param commentId
     */
    void deleteReviewComment(Integer commentId);

    /**
     * This method fetches all discl attachments by comment id
     *
     * @param commentId
     * @return
     */
    List<DisclAttachment> loadDisclAttachmentByCommentId(Integer commentId);

    /**
     * This method fetches all child comment ids
     *
     * @param opaReviewCommentId
     * @return
     */
    List<Integer> getAllChildCommentId(Integer opaReviewCommentId);

    /**
     * This method used to get Review comment by id
     *
     * @param commentId
     * @return
     */
    DisclComment fetchReviewCommentByCommentId(Integer commentId);
}
