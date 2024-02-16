package com.polus.fibicomp.opa.dao;

import com.polus.fibicomp.opa.pojo.OPAReview;

import java.sql.Timestamp;
import java.util.Date;
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
     * @param opaReviewEndDate 
     * @return
     */
    Timestamp updateReviewStatus(Integer opaReviewId, String reviewStatus, Date opaReviewEndDate);

    /**
     *
     * @param opaReviewId
     * @return
     */
    OPAReview getOPAReview(Integer opaReviewId);

    /**
     * This method checks a review is added or not.
     * @param opaReview
     * @return true : if any review exists without review status completed else : false
     */
    boolean isOPAReviewAdded(OPAReview opaReview);

    /**
     * This method checks a review is exists by or not of statuses
     * @param opaReviewId
     * @param statuses
     * @return true : if review exists else : false
     */
    boolean isOPAReviewExistsOfStatus(Integer opaReviewId, List<String> statuses);


    /**
     *
     * @param opaReviewId
     */
    void deleteOPAReview(Integer opaReviewId);

    /**
	 * This method fetches a person against in a review
	 * @param opaReview
	 * @return
	 */
	boolean isReviewStatusChanged(OPAReview opaReview);

	/**
	 * This method checks if the edit action is allowed
	 * @param opaReview
	 * @return
	 */
	boolean isReviewPresent(OPAReview opaReview);

}
