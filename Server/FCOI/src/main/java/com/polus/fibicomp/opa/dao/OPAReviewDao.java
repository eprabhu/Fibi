package com.polus.fibicomp.opa.dao;

import com.polus.fibicomp.opa.pojo.OPAReview;

import java.sql.Timestamp;
import java.util.List;

public interface OPAReviewDao {

    void saveOrUpdate(Object entity);

    /**
     * This method updates review of specific fields
     * @param opaReview
     */
    Timestamp updateOPAReview(OPAReview opaReview);

    /**
     * This method used to get the count of review based on review statuses
     * @param opaDisclosureId
     * @param reviewStatusTypeCodes
     * @return
     */
    Long numberOfReviewOfStatuesIn(Integer opaDisclosureId, List<String> reviewStatusTypeCodes);

    /**
     *
     * @param opaDisclosureId
     * @return
     */
    List<OPAReview> fetchAllOPAReviewByDisId(Integer opaDisclosureId);

    /**
     *
     * @param opaReviewId
     * @param reviewStatus
     * @return
     */
    Timestamp updateReviewStatus(Integer opaReviewId, String reviewStatus);

    /**
     *
     * @param opaReviewId
     * @return
     */
    OPAReview getOPAReview(Integer opaReviewId);

    /**
     *
     * @param opaReviewId
     */
    void deleteOPAReview(Integer opaReviewId);
}
