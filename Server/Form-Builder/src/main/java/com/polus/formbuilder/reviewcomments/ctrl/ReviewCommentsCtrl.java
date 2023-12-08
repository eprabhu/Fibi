package com.polus.formbuilder.reviewcomments.ctrl;

import com.polus.formbuilder.reviewcomments.modals.ReviewComment;
import com.polus.formbuilder.reviewcomments.serviceClients.ReviewCommentsClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("/reviewComments")
public class ReviewCommentsCtrl {

    protected static Logger logger = LogManager.getLogger(ReviewCommentsCtrl.class.getName());

    @Autowired
    private ReviewCommentsClient reviewCommentsClient;

    @PostMapping
    public ResponseEntity<Object> saveOrUpdateReviewComment(@RequestParam(value = "files", required = false) MultipartFile[] files,
                                                            @RequestParam("formDataJson") String formDataJson) {
        logger.info("Requesting for saveOrUpdateReviewComment");
        return reviewCommentsClient.saveOrUpdateReviewComment(files, formDataJson);
    }

    @PostMapping("/fetch")
    public ResponseEntity<Object> fetchReviewComments(@RequestBody ReviewComment reviewCommentsDto) {
        logger.info("Requesting for fetchReviewComments");
        return reviewCommentsClient.fetchReviewComments(reviewCommentsDto);
    }

    @DeleteMapping(value = "/{reviewCommentId}/{moduleCode}")
    public ResponseEntity<Object> deleteReviewComment(@PathVariable(value = "reviewCommentId") Integer reviewCommentId,
                                                      @PathVariable(value = "moduleCode") Integer moduleCode) {
        logger.info("Requesting for deleteReviewAttachment");
        return reviewCommentsClient.deleteReviewComment(reviewCommentId, moduleCode);
    }

    @DeleteMapping(value = "/attachment/{attachmentId}/{moduleCode}")
    public ResponseEntity<Object> deleteReviewAttachment(@PathVariable(value = "attachmentId") Integer attachmentId,
                                                         @PathVariable(value = "moduleCode") Integer moduleCode) {
        logger.info("Requesting for deleteReviewAttachment");
        return reviewCommentsClient.deleteReviewAttachment(attachmentId, moduleCode);
    }

    @GetMapping(value = "/downloadAttachment/{moduleCode}")
    public ResponseEntity<byte[]> downloadReviewAttachment(@RequestHeader("attachmentId") Integer attachmentId,
                                                           @PathVariable(value = "moduleCode") Integer moduleCode) {
        logger.info("Requesting for downloadReviewAttachment");
        logger.info("downloadReviewAttachmentId : {}", attachmentId);
        return reviewCommentsClient.downloadReviewAttachment(attachmentId, moduleCode);
    }
}
