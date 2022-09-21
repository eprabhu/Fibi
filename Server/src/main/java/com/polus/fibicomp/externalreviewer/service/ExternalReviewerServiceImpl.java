package com.polus.fibicomp.externalreviewer.service;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externalreviewer.dao.ExternalReviewerDao;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachment;
import com.polus.fibicomp.externalreviewer.pojo.ExtReviewerAttachmentFile;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewer;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerExt;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerRights;
import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewerSpecialization;
import com.polus.fibicomp.externalreviewer.pojo.SpecialismKeyword;
import com.polus.fibicomp.externalreviewer.vo.ExternalReviewerVo;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "externalReviewerService")
public class ExternalReviewerServiceImpl implements ExternalReviewerService {

	protected static Logger logger = LogManager.getLogger(ExternalReviewerServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ExternalReviewerDao externalReviewerDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private PrintService printService;


	@Override
	public String saveOrUpdateReviewerDetails(ExternalReviewerVo vo) {
		try {
			ExternalReviewer extReviewer = vo.getExtReviewer();
			String password = null;
			ExternalReviewerRights externalReviewerRight = new ExternalReviewerRights();
			if ((extReviewer.getExternalReviewerId() == null || (extReviewer.getExternalReviewerId() != null
					&& Boolean.TRUE.equals(extReviewer.getIsUsernameChange())))
					&& Boolean.TRUE.equals(externalReviewerDao.checkUniqueUserName(extReviewer.getPrincipalName()))) {
				vo.setMessage("Username already exists");
				vo.setExtReviewer(null);
				return commonDao.convertObjectToJSON(vo);
			}
			if (extReviewer.getExternalReviewerId() == null) {
				String characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789@*_";
				password = RandomStringUtils.random(15, characters);
				extReviewer.setPassword(commonService.hashBySha(password));
				externalReviewerRight.setReviewerRightId(Constants.REVIEWER);
			} else {
				extReviewer.setPassword(externalReviewerDao.getexternalReviewerPassword(extReviewer.getExternalReviewerId()));
			}
			extReviewer.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			extReviewer.setUpdateUser(AuthenticatedUser.getLoginUserName());
			externalReviewerDao.saveOrUpdateExtReviewer(extReviewer);
			if(externalReviewerRight.getReviewerRightId() != null) {
				 externalReviewerRight.setExternalReviewerId(extReviewer.getExternalReviewerId());
				 vo.setExternalReviewerRight(externalReviewerRight);
				 saveOrUpdateuserAccess(vo);
			}
			vo.setMessage("External reviewer details saved Successfully");
		} catch (Exception e) {
			throw new ApplicationException("error in saveOrUpdateReviewerDetails", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateAdditionalDetails(ExternalReviewerVo vo) {
		ExternalReviewerExt externalReviewerExt = vo.getExternalReviewerExt();
		List<ExternalReviewerSpecialization> externalReviewerSpecializations = vo.getExternalReviewerSpecializations();
		String updateUserName = AuthenticatedUser.getLoginUserName();
		try {
			if (externalReviewerExt != null) {
				externalReviewerExt.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				externalReviewerExt.setUpdateUser(updateUserName);
				externalReviewerDao.saveOrUpdateExternalReviewerExt(externalReviewerExt);
			}
			if (externalReviewerSpecializations != null) {
				externalReviewerSpecializations.forEach(extReviewerSpecialization -> {
					if ("I".equals(extReviewerSpecialization.getActionType())) {
						extReviewerSpecialization.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						extReviewerSpecialization.setUpdateUser(updateUserName);
						externalReviewerDao.saveOrUpdateExtReviewerSpecialization(extReviewerSpecialization);
					} else if ("D".equals(extReviewerSpecialization.getActionType())) {
						externalReviewerDao.deleteExtReviewerSpecialization(extReviewerSpecialization.getExtReviewerSpecializationId());
					}
				});
			}
			vo.setMessage("External additional details saved Successfully");
		} catch (Exception e) {
			throw new ApplicationException("error in saveOrUpdateAdditionalDetails", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateuserAccess(ExternalReviewerVo vo) {
		try {
			ExternalReviewerRights externalReviewerRight = vo.getExternalReviewerRight();
			externalReviewerRight.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			externalReviewerRight.setUpdateUser(AuthenticatedUser.getLoginUserName());
			externalReviewerDao.saveOrUpdateExternalReviewerRights(externalReviewerRight);
			vo.setMessage("External User Access details saved Successfully");
		} catch (Exception e) {
			throw new ApplicationException("error in saveOrUpdateuserAccess", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String addExtReviewerAttachment(MultipartFile[] files, String formDataJSON) {
		ExternalReviewerVo vo = new ExternalReviewerVo();
		 List<ExtReviewerAttachment> attachmentsList = new ArrayList<>();
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ExternalReviewerVo.class);
			List<ExtReviewerAttachment> extAttachments = vo.getExtReviewerAttachments();
			 if (files != null && files.length > 0 && extAttachments != null) {
	                for (int i = 0; i < files.length; i++) {
	                	ExtReviewerAttachment extReviewerAttachment = extAttachments.get(i);
	                	ExtReviewerAttachmentFile fileData = new ExtReviewerAttachmentFile();
	                	extReviewerAttachment.setMimeType(files[i].getContentType());
	                	fileData.setAttachment(files[i].getBytes());
	                	fileData = externalReviewerDao.saveFileData(fileData);
	                	extReviewerAttachment.setFileDataId(fileData.getFileDataId());
						extReviewerAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						extReviewerAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
						externalReviewerDao.saveOrUpdateExtAttachment(extReviewerAttachment);
						attachmentsList.add(extReviewerAttachment);
	                }
	            }
		} catch (Exception e) {
			throw new ApplicationException("error in addExtReviewerAttachment", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(attachmentsList);
	}

	@Override
	public String deleteExtReviewerAttachment(Integer extReviewerAttachmentId) {
		try {
			ExtReviewerAttachment extReviewerAttachment = externalReviewerDao.getExtReviewerAttachmentById(extReviewerAttachmentId);
			if (extReviewerAttachment != null) {
				if (extReviewerAttachment.getFileDataId() != null) {
					externalReviewerDao.deleteFileData(externalReviewerDao.getFileDataById(extReviewerAttachment.getFileDataId()));
				}
				externalReviewerDao.deleteExtReviewerAttachment(extReviewerAttachment);
			}
			return commonDao.convertObjectToJSON("deleted successfully");
		} catch (Exception e) {
			throw new ApplicationException("error in deleteExtReviewerAttachment", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public String getExtReviewerDetailById(ExternalReviewerVo vo) {
		Integer extReviewerId = vo.getExtReviewerId();
		vo.setExtReviewer(externalReviewerDao.getExtReviewerDetailById(extReviewerId));
		vo.setExternalReviewerExt(externalReviewerDao.getExternalReviewerExts(extReviewerId));
		vo.setExternalReviewerRight(externalReviewerDao.getExternalReviewerRights(extReviewerId));
		List<ExternalReviewerSpecialization> externalReviewerSpecializations = externalReviewerDao.getExternalReviewerSpecializations(extReviewerId);
		vo.setExternalReviewerSpecializations(externalReviewerSpecializations);
		vo.setExtReviewerAttachments(externalReviewerDao.fetchExtReviewerAttachment(extReviewerId));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAllExtReviewers(ExternalReviewerVo vo) {
		return commonDao.convertObjectToJSON(externalReviewerDao.getAllExtReviewer(vo));
	}

	@Override
	public String getAllExtReviewersLookup() {
		ExternalReviewerVo vo = new ExternalReviewerVo();
		vo.setExternalReviewerAttachmentType(externalReviewerDao.fetchExternalReviewerAttachmentTypes());
		vo.setExtReviewerAcademicArea(externalReviewerDao.fetchExtReviewerAcademicArea());
		vo.setExtReviewerAcademicRank(externalReviewerDao.fetchExtReviewerAcademicRank());
		vo.setExtReviewerAffilation(externalReviewerDao.fetchExtReviewerAffilation());
		vo.setExtReviewerCira(externalReviewerDao.fetchExtReviewerCira());
		vo.setExtReviewerOriginality(externalReviewerDao.fetchExtReviewerOriginality());
		vo.setReviewerRights(externalReviewerDao.fetchReviewerRights());
		vo.setExtReviewerThoroughness(externalReviewerDao.fetchExtReviewerThoroughness());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<byte[]> downloadExternalReviewerAttachment(Integer externalReviewerAttachmentId) {
		ExtReviewerAttachment attachment = externalReviewerDao.getExtReviewerAttachmentById(externalReviewerAttachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			ExtReviewerAttachmentFile fileData = externalReviewerDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
			return attachmentData;
		} catch (Exception e) {
			throw new ApplicationException("error in downloadExternalReviewerAttachment", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public String updateExtAttachment(ExternalReviewerVo vo) {
		try {
			externalReviewerDao.updateExtAttachment(vo.getExtReviewerAttachment().getDescription(),vo.getExtReviewerAttachment().getExternalReviewerAttachmentId());
			return commonDao.convertObjectToJSON(vo.getExtReviewerAttachment());
		} catch (Exception e) {
			throw new ApplicationException("error in updateExternalReviewerAttachment", e, Constants.JAVA_ERROR);
		}
	}
	
	@Override
	public List<SpecialismKeyword> findSpecialismKeywords(String searchString) {
		return externalReviewerDao.findSpecialismKeywords(searchString);
	}
}
