package com.polus.fibicomp.opa.service;

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
     *
     * @param opaDisclosureId
     * @return
     */
    ResponseEntity<Object> getOPAReview(Integer opaDisclosureId);

    /**
     *
     * @param opaReviewId
     * @return
     */
    ResponseEntity<Object> startOPAReview(Integer opaReviewId);

    /**
     *
     * @param opaReviewId
     * @return
     */
    ResponseEntity<Object> completeOPAReview(Integer opaReviewId);

    /**
     *
     * @param opaReviewId
     * @return
     */
    ResponseEntity<Object> deleteOPAReview(Integer opaReviewId);
}
