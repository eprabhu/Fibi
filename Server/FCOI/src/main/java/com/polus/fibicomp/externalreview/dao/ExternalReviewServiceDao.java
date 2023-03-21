package com.polus.fibicomp.externalreview.dao;

import com.polus.fibicomp.externalreview.pojo.*;

import java.sql.Timestamp;
import java.util.List;


public interface ExternalReviewServiceDao {

    /**
     * This method persists the External Review Questionnaire
     *
     * @param extReviewQuestionnaire
     */
    void saveQuestionnaire(ExtReviewQuestionnaire extReviewQuestionnaire);

    /**
     * This method delete the External Review Questionnaire
     *
     * @param extReviewQuestionnaireId
     */
    void deleteQuestionnaire(Integer extReviewQuestionnaireId);

    /**
     * This method fetches all External Review Questionnaires by External Review ID
     *
     * @param extReviewId
     * @return List of ExtReviewQuestionnaire
     */
    List<ExtReviewQuestionnaire> fetchExtQuestionnaires(Integer extReviewId);

    /**
     * This method fetches all External Review Service Type
     *
     * @return List of External Review Service Types
     */
    List<ExtReviewServiceType> fetchExtReviewServiceTypes();

    /**
     * This method fetches all External Review Service Status
     *
     * @return List of External Review Service Status
     */
    List<ExtReviewStatus> fetchExtReviewStatus();

    /**
     * This method is used to save External Review
     *
     * @param externalReview
     */
    void saveExternalReview(ExternalReview externalReview);

    /**
     * This method is used to fetch External Review by module and item key
     *
     * @param externalReview
     * @return List of External Review
     */
    List<ExternalReview> fetchExtReviewByDetails(ExternalReview externalReview);

    /**
     * This method is used to persist External Review Scoring Criteria
     *
     * @param scoringCriteria
     */
    void saveExtReviewScoringCriteria(ExtReviewScoringCriteria scoringCriteria);

    /**
     * This method is used to fetch all External review Scoring Criteria based on Ext Review ID
     *
     * @param extReviewID
     * @return List of ExtReviewScoringCriteria
     */
    List<ExtReviewScoringCriteria> fetchExtReviewScoringCriteria(Integer extReviewID);

    /**
     * This method is used to save External Review Attachment File
     *
     * @param extReviewAttachmentFile ExtReviewAttachmentFile
     */
    void saveExtReviewAttachmentFile(ExtReviewAttachmentFile extReviewAttachmentFile);

    /**
     * This method is used to save External Review Attachment
     *
     * @param extReviewAttachment ExtReviewAttachments
     */
    void saveExtReviewAttachment(ExtReviewAttachments extReviewAttachment);

    /**
     * This method is used to fetch all External Review Attachments by extReviewID
     *
     * @param extReviewID External Review ID
     * @return List of External Review Attachments
     */
    List<ExtReviewAttachments> fetchExtReviewAttachments(Integer extReviewID);

    /**
     * This method is used to fetch all External Review Attachment Types
     *
     * @return List of External Review Attachment Types
     */
    List<ExtReviewAttachmentType> fetchExtReviewAttachmentTypes();

    /**
     * This method is used to fetch External Review Attachments by ID
     *
     * @param extReviewAttachmentId External Review Attachment ID
     * @return External Review Attachment
     */
    ExtReviewAttachments fetchExtReviewAttachmentById(Integer extReviewAttachmentId);

    /**
     * This method is used to delete External Review Attachments by ID
     *
     * @param extReviewAttachmentId External Review Attachment ID
     */
    void deleteExtReviewAttachment(Integer extReviewAttachmentId);

    /**
     * This method is used to delete External Review Attachment File by ID
     *
     * @param fileDataId External Review Attachment File ID
     */
    void deleteExtReviewAttachmentFile(String fileDataId);

    /**
     * This  method is used to get Ext Review Attachment Type By attachmentTypeCode
     *
     * @param attachmentTypeCode attachmentTypeCode
     * @return Ext Review Attachment Type
     */
    ExtReviewAttachmentType fetchExtReviewAttachmentTypeByID(Integer attachmentTypeCode);

    /**
     * This method is used to update the External Review Status
     *
     * @param extReviewID
     * @param sendForReviewStatusCode
     * @param updateTimestamp
     * @param updateUser
     * @param preProposalExtReviewId
     */
    void updateExternalReviewStatus(Integer extReviewID, int sendForReviewStatusCode, Timestamp updateTimestamp, String updateUser, Integer preProposalExtReviewId);

    /**
     * This method is used to fetch ExtReviewStatus by ID
     *
     * @param extReviewStatusCode extReviewStatusCode
     * @return ExtReviewStatus
     */
    ExtReviewStatus fetchExtReviewStatusByID(Integer extReviewStatusCode);

    /**
     * This method is used to fetch ExtReview by ID
     *
     * @param extReviewID ExtReview ID
     * @return ExtReview
     */
    ExternalReview fetchExtReviewByID(Integer extReviewID);

    /**
     * This method is used to update External Review
     *
     * @param externalReview
     */
    void updateExternalReview(ExternalReview externalReview);
    
    /**
     * This method is used to delete ExtReviewScoringCriteria
     *
     * @param extReviewID
     */
    void deleteExtReviewScoringCriteria(Integer extReviewID);

    /**
     * This method is used to get Proposal Ids based on Source Proposal Id
     *
     * @param proposalId
     * @return List of Proposal Ids
     */
	public Integer getPreProposalIdForProposal(Integer proposalId);

	/**
	 * This method is used to get latest Active External Review based on Proposal Id
	 *
	 * @param proposal Id
	 * @return ExternalReview
	 */
	public ExternalReview getLatestActivePreProposalExternalReviewBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to get External Review Reviewers based on ExternalReviewId
	 *
	 * @param extReviewID
	 * @return ExternalReview
	 */
	public List<ExtReviewReviewer> getExtReviewReviewersByExternalReviewId(Integer extReviewID);

	/**
	 * This method is used to save ExtReviewReviewer
	 *
	 * @param reviewer
	 */
	public void saveOrUpdateExtReviewReviewer(ExtReviewReviewer reviewer);

	/**
	 * This method is used to save ExtReviewHistory
	 *
	 * @param extReviewHistory
	 */
	public void saveExtReviewHistory(ExtReviewHistory extReviewHistory);

}
