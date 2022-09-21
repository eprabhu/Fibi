package com.polus.fibicomp.externalreview.service;

import com.polus.fibicomp.externalreview.pojo.ExtReviewAttachments;
import com.polus.fibicomp.externalreview.pojo.ExtReviewQuestionnaire;
import com.polus.fibicomp.externalreview.pojo.ExternalReview;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface ExternalReviewService {

    /**
     * This method is used to create External Review Questionnaire
     *
     * @param reviewQuestionnaire ExtReviewQuestionnaire
     * @return ExtReviewQuestionnaireDto with created ID
     */
    ResponseEntity<Object> saveExtReviewQuestionnaire(ExtReviewQuestionnaire reviewQuestionnaire);

    /**
     * This method is used to delete an External Review Questionnaire
     *
     * @param extReviewQuestionnaireId
     * @return success or failure status
     */
    ResponseEntity<Object> deleteExtReviewQuestionnaire(Integer extReviewQuestionnaireId);

    /**
     * This method is used to get External Review Questionnaires
     *
     * @param extReviewId External Review ID
     * @return ExtReviewQuestionnaireDto List
     */
    ResponseEntity<Object> getExtReviewQuestionnaire(Integer extReviewId);

    /**
     * This method is used to get External Review Service Type
     *
     * @return List of External Review Service Type
     */
    ResponseEntity<Object> getExtReviewServiceType();

    /**
     * This method is used to get External Review status
     *
     * @return List of External Review Status
     */
    ResponseEntity<Object> getExtReviewStatus();

    /**
     * This method is used to create External Review
     *
     * @param externalReview External Review
     * @return External Review
     */
    ResponseEntity<Object> createExternalReview(ExternalReview externalReview);

    /**
     * This method is used to update External Review
     *
     * @param externalReview External Review
     * @return External Review
     */
    ResponseEntity<Object> updateExternalReview(ExternalReview externalReview);

    /**
     * This method is used to fetch External Review by module and item key
     *
     * @param externalReview
     * @param moduleItemCode
     * @return External Review
     */
    ResponseEntity<Object> getExternalReview(ExternalReview externalReview, Integer moduleItemCode);

    /**
     * This method is used to fetch all External review Scoring Criteria based on Ext Review ID
     *
     * @param extReviewID
     * @return List of Ext Review Scoring Criteria
     */
    ResponseEntity<Object> getExtReviewScoringCriteria(Integer extReviewID);

    /**
     * This method is used to get all Grant Call Attachments linked to the Proposal
     *
     * @param proposalID Proposal ID
     * @return list of Grant Call Attachments
     */
    ResponseEntity<Object> getProposalGrantCallAttachments(Integer proposalID);

    /**
     * This method is used to save External Review Attachments
     *
     * @param extReviewAttachments         ExtReviewAttachments List
     * @param grantCallProposalAttachments ExtReviewAttachments List
     * @param files                        MultipartFile
     * @return List of Saved External Review Attachment
     */
    ResponseEntity<Object> saveExtReviewAttachments(List<ExtReviewAttachments> extReviewAttachments,
                                                    List<ExtReviewAttachments> grantCallProposalAttachments, MultipartFile[] files);

    /**
     * This method is used to get all External Review Attachments
     *
     * @param extReviewID External Review ID
     * @return List External Review Attachments
     */
    ResponseEntity<Object> getExtReviewAttachments(Integer extReviewID);

    /**
     * This method is ued to get all External Review Attachment Types
     *
     * @return List External Review Attachment Types
     */
    ResponseEntity<Object> getExtReviewAttachmentTypes();

    /**
     * This method is used to delete External Review Attachment by ID
     *
     * @param extReviewAttachmentId External Review Attachment ID
     * @return success status
     */
    ResponseEntity<Object> deleteExtReviewAttachment(Integer extReviewAttachmentId);

    /**
     * This method is used to update/replace External Review Attachment
     *
     * @param file                MultipartFile
     * @param extReviewAttachment Ext Review Attachment Dto
     * @return Success status
     */
    ResponseEntity<Object> updateExtReviewAttachments(MultipartFile file, ExtReviewAttachments extReviewAttachment);

    /**
     * This method is used to download External Review Attachment
     *
     * @param extReviewAttachmentId External Review Attachment ID
     * @return file
     */
    ResponseEntity<byte[]> downloadExtReviewAttachments(Integer extReviewAttachmentId);

    /**
     *
     * @param extReviewID
     * @return
     */
    ResponseEntity<Object> sendForReview(Integer extReviewID);
}
