package com.polus.fibicomp.reviewcomments.service;

import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

public interface ReviewCommentService {

    /**
     * This method is used to save review comment
     *
     * @param files
     * @param disclComment
     * @return
     */
    ResponseEntity<Object> saveOrUpdateReviewComment(MultipartFile[] files, DisclComment disclComment);

    /**
     * @param reviewCommentsDto
     * @return
     */
    ResponseEntity<Object> fetchReviewComments(ReviewCommentsDto reviewCommentsDto);

    /**
     *
     * @param reviewCommentId
     * @return
     */
    ResponseEntity<Object> deleteReviewComment(Integer reviewCommentId);

    /**
     * @param attachmentId
     * @return
     */
    ResponseEntity<Object> deleteReviewAttachment(Integer attachmentId);

    /**
     * @param attachmentId
     * @return
     */
    ResponseEntity<byte[]> downloadReviewAttachment(Integer attachmentId);
}
