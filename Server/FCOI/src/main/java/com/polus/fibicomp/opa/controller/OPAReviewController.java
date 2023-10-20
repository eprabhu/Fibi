package com.polus.fibicomp.opa.controller;

import com.polus.fibicomp.opa.pojo.OPAReview;
import com.polus.fibicomp.opa.service.OPAReviewService;
import lombok.Getter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/opa/review")
public class OPAReviewController {

    protected static Logger logger = LogManager.getLogger(OPAReviewController.class.getName());

    @Autowired
    private OPAReviewService reviewService;

    @PostMapping
    ResponseEntity<Object> saveOrUpdateOPAReview(@RequestBody OPAReview opaReview) {
        logger.info("Request for saveOrUpdateOPAReview");
        return reviewService.saveOrUpdateOPAReview(opaReview);
    }

    @GetMapping("/{opaDisclosureId}")
    ResponseEntity<Object> getOPAReview(@PathVariable("opaDisclosureId") Integer opaDisclosureId) {
        logger.info("Request for getOPAReview");
        return reviewService.getOPAReview(opaDisclosureId);
    }

    @PatchMapping("/start/{opaReviewId}")
    ResponseEntity<Object> startOPAReview(@PathVariable("opaReviewId") Integer opaReviewId) {
        logger.info("Request for startOPAReview");
        return reviewService.startOPAReview(opaReviewId);
    }

    @PatchMapping("/complete/{opaReviewId}")
    ResponseEntity<Object> completeOPAReview(@PathVariable("opaReviewId") Integer opaReviewId) {
        logger.info("Request for completeOPAReview");
        return reviewService.completeOPAReview(opaReviewId);
    }

    @DeleteMapping("/{opaReviewId}")
    ResponseEntity<Object> deleteOPAReview(@PathVariable("opaReviewId") Integer opaReviewId) {
        logger.info("Request for deleteOPAReview");
        return reviewService.deleteOPAReview(opaReviewId);
    }
}
