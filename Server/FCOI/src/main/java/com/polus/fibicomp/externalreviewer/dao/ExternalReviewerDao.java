package com.polus.fibicomp.externalreviewer.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAcademicArea;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAcademicRank;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAffilation;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachment;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachmentFile;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerCira;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerOriginality;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerThoroughness;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewer;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerAttachmentType;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerExt;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerRights;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerSpecialization;
import com.polus.fibicomp.externalreviewer.pojo.ReviewerRights;
import com.polus.fibicomp.externalreviewer.pojo.SpecialismKeyword;
import com.polus.fibicomp.externalreviewer.vo.ExternalReviewerVo;

@Transactional
@Service
public interface ExternalReviewerDao {
	
	/**
	 * This method is used to save and update the ExternalReviewer details
	 * @param extReviewer
	 * @return updated extReviewer object
	 */
	public ExternalReviewer saveOrUpdateExtReviewer(ExternalReviewer extReviewer);
	
	/**
	 * This method is used to save and update the ExternalReviewerExt details
	 * @param externalReviewerExt
	 * @return updated externalReviewerExt object
	 */
	public ExternalReviewerExt saveOrUpdateExternalReviewerExt(ExternalReviewerExt externalReviewerExt);
	
	/**
	 * This method is used to save and update the ExternalReviewer Specialization details
	 * @param extReviewerSpecialization
	 * @return updated extReviewerSpecialization object
	 */
	public ExternalReviewerSpecialization saveOrUpdateExtReviewerSpecialization(ExternalReviewerSpecialization extReviewerSpecialization);
	
	/**
	 * This method is used to save and update the ExternalReviewer Rights details
	 * @param externalReviewerRight
	 * @return updated externalReviewerRight object
	 */
	public ExternalReviewerRights saveOrUpdateExternalReviewerRights(ExternalReviewerRights externalReviewerRight);

	/**
	 * This method is used to get details of ExternalReviewer based on id
	 * @param extReviewerId
	 * @return extReviewer
	 */
	public ExternalReviewer getExtReviewerDetailById(Integer extReviewerId);
    
	/**
	 * This method is used to get password
	 * @param extReviewerId
	 * @return password
	 */
	public String getexternalReviewerPassword(Integer externalReviewerId);

	/**
	 * This method is used to check user name is unique
	 * @param userName
	 * @return true/false
	 */
	public boolean checkUniqueUserName(String userName);

	/**
	 * This method is used for get all reviewer details
	 * @param ExternalReviewerVo
	 * @return ExternalReviewerVo
	 */
	public ExternalReviewerVo getAllExtReviewer(ExternalReviewerVo vo);
	
	/**
	 * This method is used for get ExternalReviewerExt details
	 * @param extReviewerId
	 * @return ExternalReviewerExt object
	 */
	public ExternalReviewerExt getExternalReviewerExts(Integer extReviewerId);
	
	/**
	 * This method is used for get External Reviewer Specialization details
	 * @param extReviewerId
	 * @return ExternalReviewerSpecialization object
	 */
	public List<ExternalReviewerSpecialization> getExternalReviewerSpecializations(Integer extReviewerId); 
	
	/**
	 * This method is used for get External Reviewer Right details
	 * @param extReviewerId
	 * @return ExternalReviewerRight object
	 */
	public ExternalReviewerRights getExternalReviewerRights(Integer extReviewerId);
	
	/**
	 * This method is used for save External reviewer Attachment
	 * @param externalReviewerAttachment
	 * @return externalReviewerAttachment object
	 */
	public ExtReviewerAttachment saveOrUpdateExtAttachment(ExtReviewerAttachment externalReviewerAttachment);
	
	/**
	 * This method is used to fetch ExternalReviewerAttachmentType lookup.
	 * @return it returns the ExternalReviewerAttachmentType.
	 */
	public List<ExternalReviewerAttachmentType> fetchExternalReviewerAttachmentTypes();
	
	/**
	 * This method is used for get all ExtReviewerAcademicArea lookup
	 */
	public List<ExtReviewerAcademicArea> fetchExtReviewerAcademicArea();
	
	/**
	 * This method is used for get all ExtReviewerAcademicRank lookup
	 */
	public List<ExtReviewerAcademicRank> fetchExtReviewerAcademicRank();
	
	/**
	 * This method is used for get all ExtReviewerAffilation lookup
	 */
	public List<ExtReviewerAffilation> fetchExtReviewerAffilation();
	
	/**
	 * This method is used for get all fetchExtReviewerCira lookup
	 */
	public List<ExtReviewerCira> fetchExtReviewerCira();
	
	/**
	 * This method is used for get all ExtReviewerOriginality lookup
	 */
	public List<ExtReviewerOriginality> fetchExtReviewerOriginality();
	
	/**
	 * This method is used for get all ExtReviewerThoroughness lookup
	 */
	public List<ExtReviewerThoroughness> fetchExtReviewerThoroughness();
	
	/**
	 * This method is used for get all ReviewerRights lookup
	 */
	public List<ReviewerRights> fetchReviewerRights();

	/**
	 * This method is used for save FileData
	 * @param fileData
	 * @return externalReviewerAttachmentFile object
	 */
	public ExtReviewerAttachmentFile saveFileData(ExtReviewerAttachmentFile fileData);

	/**
	 * This method is used for get External Reviewer Attachment using Id
	 * @param externalReviewerAttachmentId
	 * @return ExtReviewerAttachment object
	 */
	public ExtReviewerAttachment getExtReviewerAttachmentById(Integer externalReviewerAttachmentId);

	/**
	 * This method is used for get FileData using Id
	 * @param fileDataId
	 * @return ExtReviewerAttachmentFile object
	 */
	public ExtReviewerAttachmentFile getFileDataById(String fileDataId);

	/**
	 * This method is used for deleteExtReviewerAttachment
	 * @param ExtReviewerAttachment object
	 */
	public void deleteExtReviewerAttachment(ExtReviewerAttachment extReviewerAttachment);

	/**
	 * This method is used for fetch ExtReviewerAttachment list
	 * @param externalReviewerId
	 * @return ExtReviewerAttachmentFile list
	 */
	public List<ExtReviewerAttachment> fetchExtReviewerAttachment(Integer externalReviewerId);

	/**
	 * This method is used for deleteFileData
	 * @param fileData
	 */
	public void deleteFileData(ExtReviewerAttachmentFile fileData);

	/**
	 * This method is used for update External review attachment
	 * @param externalReviewerAttachmentId
	 */
	public void updateExtAttachment(String description, Integer externalReviewerAttachmentId);

	/**
	 * This method is used for deleting External reviewer specialization
	 * @param extReviewerId
	 */
	public void deleteExtReviewerSpecialization(Integer extReviewerId);

	/**
	 * 
	 * @param searchString
	 * @return
	 */
	public List<SpecialismKeyword> findSpecialismKeywords(String searchString);

}
