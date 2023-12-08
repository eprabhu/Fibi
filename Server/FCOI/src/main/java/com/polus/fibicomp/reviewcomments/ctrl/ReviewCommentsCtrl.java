package com.polus.fibicomp.reviewcomments.ctrl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.reviewcomments.config.ServiceRouter;
import com.polus.fibicomp.reviewcomments.dao.ReviewCommentDaoImpl;
import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import com.polus.fibicomp.reviewcomments.service.ReviewCommentService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;


@RestController
@RequestMapping("/coi/reviewComments")
public class ReviewCommentsCtrl {

    protected static Logger logger = LogManager.getLogger(ReviewCommentsCtrl.class.getName());

    @Autowired
    private ServiceRouter serviceRouter;

    @PostMapping
    public ResponseEntity<Object> saveOrUpdateReviewComment(@RequestParam(value = "files", required = false) MultipartFile[] files,
                                                            @RequestParam("formDataJson") String formDataJson) throws JsonProcessingException {
        logger.info("Requesting for saveOrUpdateReviewComment");
        ObjectMapper mapper = new ObjectMapper();
        DisclComment disclComment = mapper.readValue(formDataJson, DisclComment.class);
        return serviceRouter.getServiceBean(disclComment.getModuleCode()).saveOrUpdateReviewComment(files, disclComment);
    }

    @PostMapping("/fetch")
    public ResponseEntity<Object> fetchReviewComments(@RequestBody @Valid ReviewCommentsDto reviewCommentsDto) {
        logger.info("Requesting for fetchReviewComments");
        return serviceRouter.getServiceBean(reviewCommentsDto.getModuleCode()).fetchReviewComments(reviewCommentsDto);
    }

    @DeleteMapping(value = "/{reviewCommentId}/{moduleCode}")
    public ResponseEntity<Object> deleteReviewComment(@PathVariable(value = "reviewCommentId") final Integer reviewCommentId,
                                                      @PathVariable(value = "moduleCode") final Integer moduleCode) {
        logger.info("Requesting for deleteReviewAttachment");
        return serviceRouter.getServiceBean(moduleCode).deleteReviewComment(reviewCommentId);
    }

    @DeleteMapping(value = "/attachment/{attachmentId}/{moduleCode}")
    public ResponseEntity<Object> deleteReviewAttachment(@PathVariable(value = "attachmentId", required = true) final Integer attachmentId,
                                                         @PathVariable(value = "moduleCode") final Integer moduleCode) {
        logger.info("Requesting for deleteReviewAttachment");
        return serviceRouter.getServiceBean(moduleCode).deleteReviewAttachment(attachmentId);
    }

    @GetMapping(value = "/downloadAttachment/{moduleCode}")
    public ResponseEntity<byte[]> downloadReviewAttachment(@RequestHeader("attachmentId") Integer attachmentId,
                                                           @PathVariable(value = "moduleCode") final Integer moduleCode) {
        logger.info("Requesting for downloadReviewAttachment");
        logger.info("downloadReviewAttachmentId : {}", attachmentId);
        return serviceRouter.getServiceBean(moduleCode).downloadReviewAttachment(attachmentId);
    }
}
