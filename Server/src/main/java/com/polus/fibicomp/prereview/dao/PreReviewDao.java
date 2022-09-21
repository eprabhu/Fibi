package com.polus.fibicomp.prereview.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachment;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachmentFile;
import com.polus.fibicomp.prereview.pojo.PreReviewSectionType;
import com.polus.fibicomp.prereview.pojo.PreReviewStatus;
import com.polus.fibicomp.prereview.pojo.PreReviewType;
import com.polus.fibicomp.prereview.pojo.PreReviewer;
import com.polus.fibicomp.prereview.vo.PreReviewVO;


@Transactional
@Service
public interface PreReviewDao {

	/**
	 * This method is used to fetch all pre review types.
	 * @return An list of pre review types.
	 */
	public List<PreReviewType> fetchAllPreReviewTypes();

	/**
	 * This method is used to fetch all pre review status.
	 * @return An list of pre review status.
	 */
	public List<PreReviewStatus> fetchAllPreReviewStatus();

	/**
	 * This method is used to get pre review status by status code.
	 * @param statusCode - Id of the PreReviewStatus.
	 * @return An object of pre review status.
	 */
	public PreReviewStatus getPreReviewStatusByCode(String statusCode);

	/**
	 * This method is used to save or update proposal pre review.
	 * @param preReview - Object of ProposalPreReview class.
	 * @return An object of ProposalPreReview.
	 */
	public PreReview saveOrUpdatePreReview(PreReview preReview);

	/**
	 * This method is used to load all ProposalPreReview by Proposal id.
	 * @param proposalId - Id of Proposal object.
	 * @return An list of ProposalPreReview.
	 */
	//public List<ProposalPreReview> loadAllProposalPreReviewsByProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalPreReview by criteria.
	 * @param proposalId - Id of Proposal object.
	 * @param personId - Id of reviewer person id.
	 * @param preReviewStatus - status code of PreReviewStatus object.
	 * @return An list of ProposalPreReview.
	 */
	public List<PreReview> fetchPreReviewsByCriteria(PreReview preReview);

	/**
	 * This method is used to fetch ProposalPreReviewAttachment by Id.
	 * @param attachmentId - Id of ProposalPreReviewAttachment object.
	 * @return An object of ProposalPreReviewAttachment.
	 */
	public PreReviewAttachment fetchAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch all PreReviewer.
	 * @return A list of PreReviewer.
	 */
	public List<PreReviewer> fetchAllPreReviewer();

	/**
	 * This method is used to fetch sorted proposal reviews list.
	 * @param vo - PreReviewVO.
	 * @return list of proposal reviews.
	 */
	public List<PreReview> fetchSortedReviews(PreReviewVO vo);

	/**
	 * This method is used to fetch all preReview.
	 * @param PreReview - PreReview object.
	 * @return An list of preReviews.
	 */
	public List<PreReview> loadAllPreReviews(PreReview preReview);

	/**
	 * This method is used to fetch all pre review section types.
	 * @return An list of pre review types.
	 */
	public List<PreReviewSectionType> fetchAllPreReviewSectionTypes(String preReviewType);

	/**
	 * This method is used to save FileData object.
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public PreReviewAttachmentFile saveFileData(PreReviewAttachmentFile fileData);

	/**
	 * This method is used to fetch FileData object.
	 * 
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public PreReviewAttachmentFile getFileDataById(String fileDataId);

	/**
	 * This method is used to fetch pre review based on id.
	 * @param preReviewId - preReviewId.
	 * @return pre review object.
	 */
	public PreReview getPreReviewById(Integer preReviewId);

	/**
	 * This method is used to fetch all preReviews of a particular module item code and key where pre review type is completed.
	 * @param moduleItemCode - moduleItemCode.
	 * @param moduleItemKey - moduleItemKey.
	 * @param preReviewTypeCode - preReviewTypeCode.
	 * @param preReviewStatusCode - preReviewStatusCode
	 * @return An list of preReviews.
	 */
	public List<PreReview> loadPreReviewsBasedOnParams(Integer moduleItemCode, String moduleItemKey, String preReviewTypeCode, String preReviewStatusCode);

}
