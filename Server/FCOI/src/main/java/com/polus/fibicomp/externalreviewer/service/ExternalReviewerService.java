package com.polus.fibicomp.externalreviewer.service;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.externalreviewer.pojo.SpecialismKeyword;
import com.polus.fibicomp.externalreviewer.vo.ExternalReviewerVo;

@Transactional
@Service
public interface ExternalReviewerService {

	/**
	 * This method is used to save and update ExtReviewer details
	 * 
	 * @param vo
	 * @return ExtReviewer details with message
	 */
	public String saveOrUpdateReviewerDetails(ExternalReviewerVo vo);
	
	/**
	 * This method is used to save and update AdditionalDetails details
	 * 
	 * @param vo
	 * @return ExtReviewer details with message
	 */
	public String saveOrUpdateAdditionalDetails(ExternalReviewerVo vo);

	/**
	 * This method is used to save and update userAccess details
	 * 
	 * @param vo
	 * @return ExtReviewer details with message
	 */
	public String saveOrUpdateuserAccess(ExternalReviewerVo vo);
	
	/**
	 * This method is used to save External ReviewerAttachment details
	 * 
	 * @param vo
	 * @return ExtReviewer details with message
	 */
	public String addExtReviewerAttachment(MultipartFile[] files,  String formDataJSON);
	/**
	 * This method is used to get ExtReviewerDetail based on id
	 * 
	 * @param vo
	 * @return ExtReviewer details
	 */
	public String getExtReviewerDetailById(ExternalReviewerVo vo);

	/**
	 * This method is used to get all external Reviewer Details 
	 * 
	 * @param vo
	 * @return ExternalReviewerVo
	 */
	public String getAllExtReviewers(ExternalReviewerVo vo);

	/**
	 * This method is used to get all external Reviewer Details 
	 * 
	 * @param 
	 * @return All lookup data
	 */
	public String getAllExtReviewersLookup();

	/**
	 * This method is used to  downloadExternalReviewerAttachment
	 * 
	 * @param  externalReviewerAttachmentId
	 * @return file
	 */
	public ResponseEntity<byte[]> downloadExternalReviewerAttachment(Integer externalReviewerAttachmentId);

	/**
	 * This method is used to  deleteExtReviewerAttachment
	 * @param  externalReviewerAttachmentId
	 */
	public String deleteExtReviewerAttachment(Integer extReviewerAttachmentId);
    
	/**
	 * This method is used to  updateExtAttachment
	 * @param  ExternalReviewerVo
	 */
	public String updateExtAttachment(ExternalReviewerVo vo);

	/**
	 * 
	 * @param searchString
	 * @return
	 */
	public List<SpecialismKeyword> findSpecialismKeywords(String searchString);

}
