package com.polus.fibicomp.opa.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.opa.pojo.OPAReview;
import com.polus.fibicomp.opa.service.OPAReviewService;

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

    @PatchMapping("/complete/{opaReviewId}/{opaReviewEndDate}")
    ResponseEntity<Object> completeOPAReview(@PathVariable("opaReviewId") Integer opaReviewId, @PathVariable("opaReviewEndDate") String opaReviewEndDate) {
        logger.info("Request for completeOPAReview");
        return reviewService.completeOPAReview(opaReviewId, opaReviewEndDate);
    }

    @GetMapping("/history/{opaDisclosureId}")
    ResponseEntity<Object> getAllReviewActionLogs(@PathVariable("opaDisclosureId") Integer opaDisclosureId) {
        logger.info("Request for getAllReviewActionLogs");
        return reviewService.getAllReviewActionLogs(opaDisclosureId);
    }

    @DeleteMapping("/{opaReviewId}")
    ResponseEntity<Object> deleteOPAReview(@PathVariable("opaReviewId") Integer opaReviewId) {
        logger.info("Request for deleteOPAReview");
        return reviewService.deleteOPAReview(opaReviewId);
    }
}
