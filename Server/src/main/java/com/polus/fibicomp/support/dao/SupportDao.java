package com.polus.fibicomp.support.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachmentFile;


@Transactional
@Service
public interface SupportDao {

	/**
	 * This method is used to get PreReviewAttachmentFile.
	 * @return List of  all PreReview.
	 */
	public List<PreReview> showUnansweredQuestions(String personId, Integer limit );

	/**
	 * This method is used to get PreReviewAttachmentFile.
	 * @param fileDataId - string fileDataId.
	 * @return Object of PreReviewAttachmentFile.
	 */
	public PreReviewAttachmentFile getFileDataById(String fileDataId);
}

