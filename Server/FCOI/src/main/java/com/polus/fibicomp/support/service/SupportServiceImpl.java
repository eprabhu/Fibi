package com.polus.fibicomp.support.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.prereview.dao.PreReviewDao;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachment;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachmentFile;
import com.polus.fibicomp.prereview.pojo.PreReviewComment;
import com.polus.fibicomp.prereview.vo.PreReviewVO;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.support.dao.SupportDao;

@Transactional
@Service(value = "supportService")
public class SupportServiceImpl implements SupportService {

	protected static Logger logger = LogManager.getLogger(SupportServiceImpl.class.getName());

	@Autowired
	private SupportDao supportDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private PreReviewDao preReviewDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private PrintService printService;

	@Override
	public String loadSupportQuestion(PreReviewVO preReviewVO) {
		preReviewVO.setPreReviews(preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO)));
		preReviewVO.setPreReviewTypes(preReviewDao.fetchAllPreReviewTypes());
		return committeeDao.convertObjectToJSON(preReviewVO);
	}

	@Override
	public String createSupportQuestion(PreReviewVO preReviewVO) {
		PreReview preReview = preReviewVO.getNewPreReview();
		preReview = preReviewDao.saveOrUpdatePreReview(preReview);
		preReviewVO.setPreReviews(preReviewDao.loadAllPreReviews(preReview));
		return committeeDao.convertObjectToJSON(preReviewVO);
	}

	@Override
	public String addSupportComment(MultipartFile[] files, String formDataJSON) {
		PreReviewVO preReviewVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			preReviewVO = mapper.readValue(formDataJSON, PreReviewVO.class);
			//Proposal proposal = proposalVO.getProposal();
			Integer reviewId = preReviewVO.getPreReviewId();
			logger.info("reviewId : " + reviewId);
			PreReviewComment newReviewComment = preReviewVO.getPreNewReviewComment();
			List<PreReview> preReviews = preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO));
			for (PreReview preReview : preReviews) {
				if (preReview.getPreReviewId().equals(reviewId)) {
					List<PreReviewAttachment> reviewAttachments = new ArrayList<>();
					if (files != null && files.length > 0) {
						for (int i = 0; i < files.length; i++) {
							File file = new File(files[i].getOriginalFilename());
							String fileName = file.getName();
							PreReviewAttachment preReviewAttachment = new PreReviewAttachment();
							preReviewAttachment.setPreReviewComment(newReviewComment);
							preReviewAttachment.setFileName(fileName);
							preReviewAttachment.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
							preReviewAttachment.setUpdateUser(preReviewVO.getUserName());
							preReviewAttachment.setMimeType(files[i].getContentType());
							PreReviewAttachmentFile fileData = new PreReviewAttachmentFile();
							fileData.setAttachment(files[i].getBytes());
							fileData = preReviewDao.saveFileData(fileData);
							preReviewAttachment.setFileDataId(fileData.getFileDataId());
							reviewAttachments.add(preReviewAttachment);
						}
						newReviewComment.getPreReviewAttachments().addAll(reviewAttachments);
					}
					newReviewComment.setPreReview(preReview);
					preReview.getPreReviewComments().add(newReviewComment);
					preReview = preReviewDao.saveOrUpdatePreReview(preReview);
				}
			}
			preReviewVO.setPreReviews(preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO)));
		} catch (Exception e) {
			e.printStackTrace();
		}
		String response = committeeDao.convertObjectToJSON(preReviewVO);
		return response;
	}

	@Override
	public String showUnansweredQuestions(PreReviewVO vo) {
		vo.setPreReviews(supportDao.showUnansweredQuestions(AuthenticatedUser.getLoginPersonId(),vo.getLimit()));
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<byte[]> downloadSupportAttachment(Integer attachmentId) {
		PreReviewAttachment attachment = preReviewDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			byte[] data = supportDao.getFileDataById(attachment.getFileDataId()).getAttachment();
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), data);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return attachmentData;
	}

	// Need to modify - Temporary use only
	private PreReview preparePreReviewParams(PreReviewVO preReviewVO) {
		Integer moduleItemCode = preReviewVO.getModuleItemCode();
		Integer moduleSubItemCode = preReviewVO.getModuleSubItemCode();
		String moduleItemKey = preReviewVO.getModuleItemKey();
		String moduleSubItemKey = preReviewVO.getModuleSubItemKey();
		String reviewTypeCode = preReviewVO.getReviewTypeCode();
		logger.info("moduleItemCode : " + moduleItemCode);
		logger.info("moduleSubItemCode : " + moduleSubItemCode);
		logger.info("moduleItemKey : " + moduleItemKey);
		logger.info("moduleSubItemKey : " + moduleSubItemKey);
		logger.info("reviewTypeCode : " + reviewTypeCode);
		PreReview preReviewParam = new PreReview();
		preReviewParam.setModuleItemCode(moduleItemCode);
		preReviewParam.setModuleItemKey(moduleItemKey);
		preReviewParam.setModuleSubItemCode(moduleSubItemCode);
		preReviewParam.setModuleSubItemKey(moduleSubItemKey);
		preReviewParam.setReviewTypeCode(reviewTypeCode);
		return preReviewParam;
	}

	public PreReviewAttachment setPreReviewAttachmentObject(PreReviewComment newReviewComment, MultipartFile multipartFile, String updateUser){
		PreReviewAttachment preReviewAttachment = new PreReviewAttachment();
		try {
			File attachmentFile = new File(multipartFile.getOriginalFilename());
			String fileName = attachmentFile.getName();
			preReviewAttachment.setPreReviewComment(newReviewComment);
			preReviewAttachment.setFileName(fileName);
			preReviewAttachment.setUpdateTimeStamp(committeeDao.getCurrentTimestamp());
			preReviewAttachment.setUpdateUser(updateUser);
			preReviewAttachment.setMimeType(multipartFile.getContentType());
			PreReviewAttachmentFile fileData = new PreReviewAttachmentFile();
			fileData.setAttachment(multipartFile.getBytes());
			fileData = preReviewDao.saveFileData(fileData);
			preReviewAttachment.setFileDataId(fileData.getFileDataId());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return preReviewAttachment;
	}

	@Override
	public String addPreSupportCommentForWaf(MultipartFile file, String formDataJSON) {
		PreReviewVO preReviewVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			MultipartFile multipartFile = null;
			preReviewVO = mapper.readValue(formDataJSON, PreReviewVO.class);
			Integer reviewId = preReviewVO.getPreReviewId();
			logger.info("reviewId : " + reviewId);
			PreReviewComment newReviewComment = preReviewVO.getPreNewReviewComment();
			List<PreReview> preReviews = preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO));
			String name = file.getName();
			Integer remaining = preReviewVO.getRemaining();
			Integer length = preReviewVO.getLength();
			String userId = preReviewVO.getPersonId();
			String contentType = file.getContentType();
			String splicedFile = preReviewVO.getFileContent();
			String timestamp = preReviewVO.getFileTimestamp();
			if (splicedFile != null) {
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || (file == null || file.isEmpty())) {
				for (PreReview preReview : preReviews) {
					if (preReview.getPreReviewId().equals(reviewId)) {
						List<PreReviewAttachment> reviewAttachments = new ArrayList<>();
						if (multipartFile != null && !multipartFile.isEmpty()) {
							reviewAttachments.add(setPreReviewAttachmentObject(newReviewComment, multipartFile, preReviewVO.getUserName()));
						}
						newReviewComment.getPreReviewAttachments().addAll(reviewAttachments);
					}
					newReviewComment.setPreReview(preReview);
					preReview.getPreReviewComments().add(newReviewComment);
					preReview = preReviewDao.saveOrUpdatePreReview(preReview);
				}
			}
			preReviewVO.setPreReviews(preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO)));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(preReviewVO);
	}

}
