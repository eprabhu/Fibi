package com.polus.fibicomp.prereview.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.prereview.dao.PreReviewDao;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachment;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachmentFile;
import com.polus.fibicomp.prereview.pojo.PreReviewComment;
import com.polus.fibicomp.prereview.pojo.PreReviewStatus;
import com.polus.fibicomp.prereview.vo.PreReviewVO;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.security.AuthenticatedUser;

import io.jsonwebtoken.Claims;

@Transactional
@Service(value = "preReviewService")
public class PreReviewServiceImpl implements PreReviewService {

	protected static Logger logger = LogManager.getLogger(PreReviewServiceImpl.class.getName());

	@Autowired
	private PreReviewDao preReviewDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	public ProposalDao proposalDao;

	@Autowired
	private PrintService printService;

	@Autowired
	public InboxService inboxService;

	@Autowired
	public PersonDao personDao;

	@Value("${spring.application.name}")
	private String context;

	@Autowired
	private CommonService commonService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private AwardDao awardDao;

	@Override
	public String createPreReview(PreReviewVO vo) {
		PreReview preReview = vo.getNewPreReview();
		if (vo.getPersonId().equals(preReview.getReviewerPersonId())) {
			vo.setReviewerReview(preReview);
			vo.setIsPreReviewer(true);
		}
		PreReviewStatus reviewStatus = preReviewDao.getPreReviewStatusByCode(Constants.PRE_REVIEW_STATUS_INPROGRESS);
		preReview.setReviewStatusCode(Constants.PRE_REVIEW_STATUS_INPROGRESS);
		preReview.setPreReviewStatus(reviewStatus);
		preReview = preReviewDao.saveOrUpdatePreReview(preReview);
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		commonService.setNotificationRecipients(preReview.getReviewerPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		if (preReview.getModuleItemCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
			Proposal proposal = proposalDao.fetchProposalById(Integer.parseInt(preReview.getModuleItemKey()));
			inboxService.addMessageToInbox(proposal, preReview.getReviewerPersonId(), preReview.getUpdateUser(), Constants.MESSAGE_TYPE_PRE_REVIEW, "P", preReview.getPreReviewId(), "");
			sendPreReviewNotification(vo, Constants.ASSIGN_PROPOSAL_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.PROPOSAL_PRE_REVIEW_SUBMODULE_CODE);
		} else if (preReview.getModuleItemCode().equals(Constants.AWARD_MODULE_CODE)) {
			Award award = awardDao.fetchAwardByAwardId(preReview.getModuleItemKey());
			String userMessage = award.getAwardId().toString() + " - " + award.getTitle() + " - " + "Review";
			inboxService.addAwardMessageToInbox(preReview.getModuleItemKey(), preReview.getReviewerPersonId(), preReview.getUpdateUser(), Constants.MESSAGE_TYPE_AWARD_PRE_REVIEW, "P", preReview.getPreReviewId(), "", Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE, userMessage);
			if (preReview.getPreReviewSectionType().getReviewSectionTypeCode().equals(Constants.IRB_PRE_REVIEW_SECTION_TYPE_CODE)) {
				sendPreReviewNotification(vo, Constants.IRB_ASSIGN_AWARD_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE);
			} else if (preReview.getPreReviewSectionType().getReviewSectionTypeCode().equals(Constants.PI_PRE_REVIEW_SECTION_TYPE_CODE)) {
//				List<PersonRoles> personRoles = personDao.getPersonRoleByUnitNumberAndRoleId(award.getLeadUnitNumber(), Constants.GRANT_MANAGER_ROLE_TYPE_CODE);
//				for (PersonRoles PersonRoles : personRoles) {
//					commonService.setNotificationRecipients(PersonRoles.getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_CC, dynamicEmailRecipients);
//				}
				sendPreReviewNotification(vo, Constants.PI_ASSIGN_AWARD_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE);
			} else {
				sendPreReviewNotification(vo, Constants.ASSIGN_AWARD_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE);
			}
		}
		List<PreReview> reviews = preReviewDao.loadAllPreReviews(preReview);
		vo.setPreReviews(preparePreReviewDetails(vo, reviews));
		vo.setNewPreReview(preReview);
		return commonDao.convertObjectToJSON(vo);
	}

	public String getPrincipalInvestigator(List<ProposalPerson> proposalPersons) {
		String piName = "";
		for (ProposalPerson person : proposalPersons) {
			if (person.getProposalPersonRole().getCode().equals(Constants.PRINCIPAL_INVESTIGATOR)) {
				piName = person.getFullName();
			}
		}
		return piName;
	}

	@Override
	public String addPreReviewComment(MultipartFile[] files, String formDataJSON, HttpServletRequest request) {
		PreReviewVO preReviewVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			preReviewVO = mapper.readValue(formDataJSON, PreReviewVO.class);
			Claims claims = commonService.getLoginPersonDetailFromJWT(request);
			preReviewVO.setPersonId(claims.get(Constants.LOGIN_PERSON_ID).toString());
			preReviewVO.setLoginPersonUnitNumber(claims.get(Constants.LOGIN_PERSON_UNIT).toString());
			Integer reviewId = preReviewVO.getPreReviewId();
			logger.info("reviewId : ",  reviewId);
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
							preReviewAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
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
					preReviewDao.saveOrUpdatePreReview(preReview);
				}
			}
			List<PreReview> reviews = preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO));
			preReviewVO.setPreReviews(preparePreReviewDetails(preReviewVO, reviews));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(preReviewVO);
	}

	@Override
	public String approveOrDisapprovePreReview(MultipartFile[] files, String formDataJSON, HttpServletRequest request) {
		PreReviewVO preReviewVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			preReviewVO = mapper.readValue(formDataJSON, PreReviewVO.class);
			preReviewVO.setPersonId(AuthenticatedUser.getLoginPersonId());
			preReviewVO.setLoginPersonUnitNumber(AuthenticatedUser.getLoginPersonUnit());
			String actionType = preReviewVO.getActionType();
			PreReview preReview = preReviewVO.getReviewerReview();
			PreReviewStatus reviewStatus = null;
			PreReviewComment newReviewComment = preReviewVO.getPreNewReviewComment();
			if (newReviewComment.getReviewComment() != null) {
				List<PreReviewAttachment> reviewAttachments = new ArrayList<>();
				if (files != null && files.length > 0) {
					for (int i = 0; i < files.length; i++) {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						PreReviewAttachment preReviewAttachment = new PreReviewAttachment();
						preReviewAttachment.setPreReviewComment(newReviewComment);
						preReviewAttachment.setFileName(fileName);
						preReviewAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
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
			}
			if (actionType.equals("APPROVE")) {
				reviewStatus = preReviewDao.getPreReviewStatusByCode(Constants.PRE_REVIEW_STATUS_COMPLETE);
				preReview.setReviewStatusCode(Constants.PRE_REVIEW_STATUS_COMPLETE);
				preReview.setPreReviewStatus(reviewStatus);
			} else {
				reviewStatus = preReviewDao.getPreReviewStatusByCode(Constants.PRE_REVIEW_STATUS_REVISION);
				preReview.setReviewStatusCode(Constants.PRE_REVIEW_STATUS_REVISION);
				preReview.setPreReviewStatus(reviewStatus);
			}
			preReview = preReviewDao.saveOrUpdatePreReview(preReview);
			inboxService.removeMessageFromInbox(Integer.parseInt(preReview.getModuleItemKey()), preReview.getPreReviewId(), preReview.getModuleItemCode());
			if (preReviewVO.getPersonId().equals(preReview.getReviewerPersonId())) {
				preReviewVO.setIsPreReviewer(false);
			}
			List<PreReview> preReviews = preReviewDao.loadAllPreReviews(preReview);
			preReviewVO.setPreReviews(preparePreReviewDetails(preReviewVO, preReviews));
			preReviewVO.setNewPreReview(preReview);
			sendApproveOrDisApprovePreReviewNotification(actionType, preReviewVO);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(preReviewVO);
	}

	@Override
	public ResponseEntity<byte[]> downloadPreReviewAttachment(Integer attachmentId) {
		PreReviewAttachment attachment = preReviewDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			PreReviewAttachmentFile fileData = preReviewDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return attachmentData;
	}

	@Override
	public String fetchSortedReviews(PreReviewVO vo) {
		Integer moduleItemCode = vo.getModuleItemCode();
		Integer moduleSubItemCode = vo.getModuleSubItemCode();
		String moduleItemKey = vo.getModuleItemKey();
		String moduleSubItemKey = vo.getModuleSubItemKey();
		String reviewTypeCode = vo.getReviewTypeCode();
		String sortBy = vo.getSortBy();
		String reverse = vo.getReverse();
		logger.info("moduleItemCode : " + moduleItemCode);
		logger.info("moduleSubItemCode : " + moduleSubItemCode);
		logger.info("moduleItemKey : " + moduleItemKey);
		logger.info("moduleSubItemKey : " + moduleSubItemKey);
		logger.info("reviewTypeCode : " + reviewTypeCode);
		logger.info("sortBy : " + sortBy);
		logger.info("reverse : " + reverse);
		List<PreReview> preReviews = preReviewDao.fetchSortedReviews(vo);
		vo.setPreReviews(preparePreReviewDetails(vo, preReviews));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadPreReview(PreReviewVO preReviewVO) {
		List<PreReview> preReviews = preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO));
		preReviewVO.setPreReviews(preparePreReviewDetails(preReviewVO, preReviews));
		preReviewVO.setPreReviewTypes(preReviewDao.fetchAllPreReviewTypes());
		if (preReviewVO.getReviewSectionTypeCode() != null) {
			preReviewVO.setPreReviewSectionTypes(preReviewDao.fetchAllPreReviewSectionTypes(preReviewVO.getReviewSectionTypeCode()));
		}
		return commonDao.convertObjectToJSON(preReviewVO);
	}

	private List<PreReview> preparePreReviewDetails(PreReviewVO preReviewVO, List<PreReview> preReviews) {
		Integer moduleItemCode = null;
		String moduleItemKey = null;
		Boolean isPersonHasPermissions = false;
		if (preReviews != null && !preReviews.isEmpty()) {
			moduleItemCode = preReviews.get(0).getModuleItemCode();
			moduleItemKey = preReviews.get(0).getModuleItemKey();
		}
		if (moduleItemCode != null && moduleItemKey != null) {
			if (moduleItemCode.equals(Constants.AWARD_MODULE_CODE)) {
				isPersonHasPermissions = personDao.isPersonHasPermission(preReviewVO.getPersonId(),
						Constants.VIEW_PRIVATE_COMMENTS_RIGHT,
						awardDao.fetchAwardLeadUnitNumberByAwardId(Integer.parseInt(moduleItemKey)));
			} else if (moduleItemCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
				isPersonHasPermissions = personDao.isPersonHasPermission(preReviewVO.getPersonId(),
						Constants.VIEW_PRIVATE_COMMENTS_RIGHT, proposalDao.fetchProposalLeadUnitNumberByProposalId(Integer.parseInt(moduleItemKey)));
			}
		}
		Boolean checkPersonHasPermission = isPersonHasPermissions;
		List<PreReview> preReviewDetails = new ArrayList<>();
		preReviews.forEach(preReview -> {
			PreReview preReviewDetail = new PreReview();
			ReflectionUtils.shallowCopyFieldState(preReview, preReviewDetail);	
			List<PreReviewComment> preReviewComments = new ArrayList<>();
			preReview.getPreReviewComments().forEach(preReviewComment -> {
				if (Boolean.TRUE.equals(checkPersonHasPermission)
						&& Boolean.TRUE.equals(preReviewComment.getIsPrivateComment())
						|| (Boolean.FALSE.equals(preReviewComment.getIsPrivateComment())
								|| Boolean.TRUE.equals(preReviewComment.getIsPrivateComment())
										&& preReviewVO.getPersonId().equals(preReviewComment.getPersonId()))) {
					preReviewComments.add(preReviewComment);
				}
			});
			preReviewDetail.setPreReviewComments(preReviewComments);
			preReviewDetails.add(preReviewDetail);
		});
		return preReviewDetails;
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

	public PreReviewAttachment setPreReviewAttachmentObject(PreReviewComment newReviewComment,
			MultipartFile multipartFile, String updateUser) {
		PreReviewAttachment preReviewAttachment = new PreReviewAttachment();
		try {
			File attachmentFile = new File(multipartFile.getOriginalFilename());
			String fileName = attachmentFile.getName();
			preReviewAttachment.setPreReviewComment(newReviewComment);
			preReviewAttachment.setFileName(fileName);
			preReviewAttachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
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

	public PreReviewVO sendPreReviewNotification(PreReviewVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients, Integer preReviewSubmoduleCode) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(vo.getNewPreReview().getModuleItemCode());
		emailServiceVO.setSubModuleCode(preReviewSubmoduleCode.toString());
		emailServiceVO.setModuleItemKey(vo.getNewPreReview().getPreReviewId().toString());
		emailServiceVO.setPlaceHolder(getPreReviewPlaceholders(vo));
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && emailServiceVO.getPrompted() != true) {
			vo.setNotificationTypeId(notificationTypeId);
			vo.setBody(emailServiceVO.getBody());
			vo.setSubject(emailServiceVO.getSubject());
		}
		return vo;
	}

	private Map<String, String> getPreReviewPlaceholders(PreReviewVO vo) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		placeHolder.put("{USER_NAME}", "");
		return placeHolder;
	}

	private void sendApproveOrDisApprovePreReviewNotification(String actionType, PreReviewVO preReviewVO) {
		PreReview preReview = preReviewVO.getNewPreReview();
		if (actionType.equals("APPROVE")) {
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			commonService.setNotificationRecipients(preReview.getRequestorPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			if (preReview.getModuleItemCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
				sendPreReviewNotification(preReviewVO, Constants.COMPLETE_PROPOSAL_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.PROPOSAL_PRE_REVIEW_SUBMODULE_CODE);
			} else if (preReview.getModuleItemCode().equals(Constants.AWARD_MODULE_CODE)) {
				if (preReview.getPreReviewSectionType().getReviewSectionTypeCode().equals(Constants.PI_PRE_REVIEW_SECTION_TYPE_CODE)) {
					List<PreReview> preReviews = preReviewDao.loadPreReviewsBasedOnParams(preReview.getModuleItemCode(), preReview.getModuleItemKey(), Constants.PI_PRE_REVIEW_SECTION_TYPE_CODE, Constants.PRE_REVIEW_STATUS_COMPLETE);
					if (preReviews.isEmpty()) {
//						Set<NotificationRecipient> grantManagerRecipients = new HashSet<>();
//						Award award = awardDao.getAwardDetailsById(Integer.parseInt(preReview.getModuleItemKey()));
//						List<PersonRoles> personRoles = personDao.getPersonRoleByUnitNumberAndRoleId(award.getLeadUnitNumber(), Constants.GRANT_MANAGER_ROLE_TYPE_CODE);
//						for (PersonRoles personRole : personRoles) {
//							commonService.setNotificationRecipients(personRole.getPersonId(),
//									Constants.NOTIFICATION_RECIPIENT_TYPE_TO, grantManagerRecipients);
//						}
						sendPreReviewNotification(preReviewVO, Constants.PI_ALL_PRE_REVIEWS_COMPLETE_NOTIFICATION_CODE, new HashSet<>(), Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE);
					}
				}
				sendPreReviewNotification(preReviewVO, Constants.COMPLETE_AWARD_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE);
			}
		} else {
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			commonService.setNotificationRecipients(preReview.getRequestorPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			if (preReview.getModuleItemCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
				sendPreReviewNotification(preReviewVO, Constants.RETURN_PROPOSAL_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.PROPOSAL_PRE_REVIEW_SUBMODULE_CODE);
			} else if (preReview.getModuleItemCode().equals(Constants.AWARD_MODULE_CODE)) {
				sendPreReviewNotification(preReviewVO, Constants.RETURN_AWARD_PRE_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients, Constants.AWARD_PRE_REVIEW_SUBMODULE_CODE);
			}
		}
	}

	@Override
	public String addPreReviewCommentForWaf(PreReviewVO preReviewVO) {
		try {
			MultipartFile multipartFile = null;
			String name = null;
			String contentType = null;
			Integer reviewId = preReviewVO.getPreReviewId();
			logger.info("reviewId : " + reviewId);
			PreReviewComment newReviewComment = preReviewVO.getPreNewReviewComment();
			List<PreReview> preReviews = preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO));
			Integer remaining = preReviewVO.getRemaining();
			Integer length = preReviewVO.getLength();
			String userId = preReviewVO.getPersonId();
			String splicedFile = preReviewVO.getFileContent();
			String timestamp = preReviewVO.getFileTimestamp();
			if (splicedFile != null) {
				name = preReviewVO.getFileName();
				contentType = preReviewVO.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || (splicedFile == null)) {
				for (PreReview preReview : preReviews) {
					if (preReview.getPreReviewId().equals(reviewId)) {
						List<PreReviewAttachment> reviewAttachments = new ArrayList<>();
						if (newReviewComment.getPreReviewCommentId() != null) {
							for (PreReviewComment reviewComment : preReview.getPreReviewComments()) {
								if ((newReviewComment.getPreReviewCommentId() != null) && (newReviewComment.getPreReviewCommentId().equals(reviewComment.getPreReviewCommentId()))) {
									if (multipartFile != null && !multipartFile.isEmpty()) {
										reviewAttachments.add(setPreReviewAttachmentObject(newReviewComment, multipartFile, preReviewVO.getUserName()));
										reviewComment.getPreReviewAttachments().addAll(reviewAttachments);
									}
								}
							}
						} else {
							if (multipartFile != null && !multipartFile.isEmpty()) {
								reviewAttachments.add(setPreReviewAttachmentObject(newReviewComment, multipartFile, preReviewVO.getUserName()));
							}
							newReviewComment.getPreReviewAttachments().addAll(reviewAttachments);
						}
						newReviewComment.setPreReview(preReview);
						if (newReviewComment.getPreReviewCommentId() == null) {
							preReview.getPreReviewComments().add(newReviewComment);
						}
						preReview = preReviewDao.saveOrUpdatePreReview(preReview);
					}
				}
			}
			List<PreReview> reviews = preReviewDao.loadAllPreReviews(preparePreReviewParams(preReviewVO));
			preReviewVO.setPreReviews(preparePreReviewDetails(preReviewVO, reviews));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(preReviewVO);
	}

	@Override
	public String approveOrDisapprovePreReviewForWaf(PreReviewVO preReviewVO) {
		try {
			MultipartFile multipartFile = null;
			String name = null;
			String contentType = null;
			String actionType = preReviewVO.getActionType();
			PreReview preReview = preReviewVO.getReviewerReview();
			PreReviewStatus reviewStatus = null;
			PreReviewComment newReviewComment = preReviewVO.getPreNewReviewComment();
			Integer remaining = preReviewVO.getRemaining();
			Integer length = preReviewVO.getLength();
			String userId = preReviewVO.getPersonId();
			String splicedFile = preReviewVO.getFileContent();
			String timestamp = preReviewVO.getFileTimestamp();
			if (splicedFile != null) {
				name = preReviewVO.getFileName();
				contentType = preReviewVO.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || (splicedFile == null)) {
				if (newReviewComment.getReviewComment() != null) {
					List<PreReviewAttachment> reviewAttachments = new ArrayList<>();
					if (newReviewComment.getPreReviewCommentId() != null) {
						for (PreReviewComment reviewComment : preReview.getPreReviewComments()) {
							if (newReviewComment.getPreReviewCommentId() != null && (newReviewComment.getPreReviewCommentId().equals(reviewComment.getPreReviewCommentId()))) {
								if (multipartFile != null && !multipartFile.isEmpty()) {
									reviewAttachments.add(setPreReviewAttachmentObject(newReviewComment, multipartFile, preReviewVO.getUserName()));
									reviewComment.getPreReviewAttachments().addAll(reviewAttachments);
								}
							}
						}
					} else {
						if (multipartFile != null && !multipartFile.isEmpty()) {
							reviewAttachments.add(setPreReviewAttachmentObject(newReviewComment, multipartFile, preReviewVO.getUserName()));
						}
						newReviewComment.getPreReviewAttachments().addAll(reviewAttachments);
					}
					newReviewComment.setPreReview(preReview);
					if (newReviewComment.getPreReviewCommentId() == null) {
						preReview.getPreReviewComments().add(newReviewComment);
					}
				}
				if (actionType.equals("APPROVE")) {
					reviewStatus = preReviewDao.getPreReviewStatusByCode(Constants.PRE_REVIEW_STATUS_COMPLETE);
					preReview.setReviewStatusCode(Constants.PRE_REVIEW_STATUS_COMPLETE);
					preReview.setPreReviewStatus(reviewStatus);
				} else {
					reviewStatus = preReviewDao.getPreReviewStatusByCode(Constants.PRE_REVIEW_STATUS_REVISION);
					preReview.setReviewStatusCode(Constants.PRE_REVIEW_STATUS_REVISION);
					preReview.setPreReviewStatus(reviewStatus);
				}
				preReview.setCompletionDate(commonDao.getCurrentTimestamp());
				preReview = preReviewDao.saveOrUpdatePreReview(preReview);
				inboxService.removeMessageFromInbox(Integer.parseInt(preReview.getModuleItemKey()), preReview.getPreReviewId(), preReview.getModuleItemCode());
				if (preReviewVO.getPersonId().equals(preReview.getReviewerPersonId())) {
					preReviewVO.setIsPreReviewer(false);
				}
				List<PreReview> preReviews = preReviewDao.loadAllPreReviews(preReview);
				preReviewVO.setPreReviews(preparePreReviewDetails(preReviewVO, preReviews));
				preReviewVO.setNewPreReview(preReview);
				sendApproveOrDisApprovePreReviewNotification(actionType, preReviewVO);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(preReviewVO);
	}

}
