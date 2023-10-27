package com.polus.fibicomp.opa.service;

import com.polus.fibicomp.opa.dto.OPAReviewRequestDto;
import com.polus.fibicomp.opa.pojo.OPAReview;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public interface OPAReviewService {

    /**
     * This method is used to save or update opa review
     * @param opaReview
     * @return
     */
    ResponseEntity<Object> saveOrUpdateOPAReview(OPAReview opaReview);

    /**
     * This method fetches all OPA Review by OPA disclosure id
     * @param opaDisclosureId
     * @return
     */
    ResponseEntity<Object> getOPAReview(Integer opaDisclosureId);

    /**
     * This method Starts a OPA review
     * @param opaReviewId
     * @return
     */
    ResponseEntity<Object> startOPAReview(Integer opaReviewId);

    /**
     * This method completes a OPA Review
     * @param opaReviewId
     * @return
     */
    ResponseEntity<Object> completeOPAReview(Integer opaReviewId);

    /**
     * This method fetches all review action log
     * @param opaDisclosureId
     * @return
     */
    ResponseEntity<Object> getAllReviewActionLogs(Integer opaDisclosureId);
    /**
     *
     * @param opaReviewId
     * @return
     */
    ResponseEntity<Object> deleteOPAReview(Integer opaReviewId);
}
