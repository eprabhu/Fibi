package com.polus.fibicomp.evaluation.service;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.evaluation.dao.EvaluationDao;
import com.polus.fibicomp.evaluation.pojo.EvaluationStop;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanelPersons;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationStatusFlow;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.evaluation.pojo.ReviewAttachment;
import com.polus.fibicomp.evaluation.pojo.ReviewComment;
import com.polus.fibicomp.evaluation.vo.EvaluationVO;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dto.ScoredPersonDTO;
import com.polus.fibicomp.grantcall.dto.ScoringReportDto;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.service.GrantCallService;
import com.polus.fibicomp.grantcall.vo.EvaluationMainPanelVO;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.notification.email.dao.EmailMaintenanceDao;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;

@Transactional
@Service(value = "evaluationService")
public class EvaluationServiceImpl implements EvaluationService {

	protected static Logger logger = LogManager.getLogger(EvaluationServiceImpl.class.getName());

	@Autowired
	private EvaluationDao evaluationDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public ProposalDao proposalDao;

	@Autowired
	private EmailMaintenanceDao emailMaintenanceDao;

	@Autowired
	public InboxService inboxService;

	@Value("${spring.application.name}")
	private String context;

	@Autowired
	@Qualifier(value = "proposalService")
	private ProposalService proposalService;

	@Autowired
	private CommonService commonService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private PrintService printService;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	public WorkflowDao workflowDao;

	@Autowired
	private EmailService emailService;

	private ExecutorService executorService = Executors.newCachedThreadPool();

	@Autowired
	private GrantCallService grantCallService;

	@Autowired
	private DashboardService dashboardService;

	private static final int NORMAL_WIDTH = 4000;
	private static final int CRITERIA_WIDTH = 6000;

	@Override
	public String createReview(ProposalVO vo) {
		Proposal proposal = null;
		ProposalReview newProposalReview = vo.getNewProposalReview();
		Map<String, Integer> grantCallMap = new HashMap<>();
		if (vo.getProposalId() == null) {
			List<Integer> proposalIds = vo.getProposalIds();
			for (Integer proposalId : proposalIds) {
				proposal = proposalDao.fetchProposalById(proposalId);
				if (proposal.getGrantCallId() != null) {
					GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
					if (!grantCallMap.containsKey(grantCall.getGrantCallName())) {
						grantCallMap.put(grantCall.getGrantCallName(), 1);
					} else {
						Integer count = grantCallMap.get(grantCall.getGrantCallName());
						grantCallMap.replace(grantCall.getGrantCallName(), count, count + 1);
					}
				}
				proposal.setUpdateUser(vo.getUpdateUser());
				proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				checkReviewIsValid(proposal, newProposalReview);
				//assignReview(proposal, newProposalReview);
			}
			sendAssignReviewNotification(newProposalReview, grantCallMap, proposal);
		} else {
			proposal = proposalDao.fetchProposalById(vo.getProposalId());
			if (proposal.getGrantCallId() != null) {
				GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
				grantCallMap.put(grantCall.getGrantCallName(), 1);
			}
			proposal.setUpdateUser(vo.getUpdateUser());
			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposal = assignReview(proposal, newProposalReview);
			if(proposal.getActivityTypeCode() != null) {
				if (proposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString("RCBF_TYPE_CODE"))) {
					proposal.setIsRcbfProposal(true);
				}
			}
			sendAssignReviewNotification(newProposalReview, grantCallMap, proposal);
			vo.setProposal(proposal);
		}
		proposalService.loadProposalUserFullNames(proposal);
		vo.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposal.getProposalId()));
		vo.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(), proposal.getActivityTypeCode()));
		vo.setHasRank(evaluationDao.checkPersonHasRank(proposal.getProposalId(), vo.getPersonId()));
		vo.setHasRecommendation(evaluationDao.checkPersonHasRecommendation(proposal.getProposalId(), vo.getPersonId()));
		return committeeDao.convertObjectToJSON(vo);
	}

	private void checkReviewIsValid(Proposal proposal, ProposalReview newProposalReview) {
		EvaluationStop evaluationStop = evaluationDao.fetchEvaluationStopForReviewer(proposal.getStatusCode(), proposal.getActivityTypeCode(), newProposalReview.getRoleId());
		if(evaluationStop != null) {
			newProposalReview.setHasEndorsed(evaluationStop.getHasEndorsed());
			newProposalReview.setHasRank(evaluationStop.getHasRank());
			newProposalReview.setHasQuestionnaire(evaluationStop.getHasQuestionnaire());
			newProposalReview.setIsFinal(evaluationStop.getIsFinal());
			newProposalReview.setHasRecommendation(evaluationStop.getHasRecommendation());
			newProposalReview.setEvaluationStopCode(evaluationStop.getEvaluationStopCode());
			newProposalReview.setEvaluationStop(evaluationStop);
			newProposalReview.setRole(evaluationStop.getRole());
			newProposalReview.setRoleId(evaluationStop.getRoleId());
			assignReview(proposal, newProposalReview);
		}
	}

	public Proposal assignReview(Proposal proposal, ProposalReview newProposalReview) {
		Boolean checkIsPersonHasReview = evaluationDao.checkPersonHasReview(newProposalReview.getReviewerPersonId(), proposal.getProposalId(), newProposalReview.getRoleId());
		if (Boolean.FALSE.equals(checkIsPersonHasReview)) {
			createProposalReview(proposal, newProposalReview);
			updateProposalStatus(newProposalReview, proposal);
			proposal = proposalDao.saveOrUpdateProposal(proposal);
		}
		proposal.setIsReviewExist(checkIsPersonHasReview);
		return proposal;
	}

	public void updateProposalStatus(ProposalReview newProposalReview, Proposal proposal) {
		Integer statusCode = evaluationDao.fetchProposalStatusBasedOnReview(newProposalReview);
		proposal.setStatusCode(statusCode);
		proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(statusCode));
	}

	public void createProposalReview(Proposal proposal, ProposalReview proposalReview) {
		ProposalReview review = new ProposalReview();
		review.setProposalId(proposal.getProposalId());
		review.setReviewerEmail(proposalReview.getReviewerEmail());
		review.setReviewerFullName(proposalReview.getReviewerFullName());
		review.setReviewerPersonId(proposalReview.getReviewerPersonId());
		review.setReviewStartDate(proposalReview.getReviewStartDate());
		review.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS));
		review.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		if (Boolean.FALSE.equals(proposalReview.getIsReturned())) {
			proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
			proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS));
		}
		review.setEvaluationStop(proposalReview.getEvaluationStop());
		review.setEvaluationStopCode(proposalReview.getEvaluationStopCode());
		review.setUpdateTimestamp(proposalReview.getUpdateTimestamp());
		review.setUpdateUser(proposalReview.getUpdateUser());
		review.setCompleteReviewerFullName(proposalReview.getCompleteReviewerFullName());
		review.setCompleteReviewerEmail(proposalReview.getCompleteReviewerEmail());
		review.setCompleteReviewerPersonId(proposalReview.getCompleteReviewerPersonId());
		review.setReviewDeadLineDate(proposalReview.getReviewDeadLineDate());
		review.setPiReviewDeadLineDate(proposalReview.getPiReviewDeadLineDate());
		review.setHasEndorsed(proposalReview.getHasEndorsed());
		review.setIsFinal(proposalReview.getIsFinal());
		review.setHasRank(proposalReview.getHasRank());
		review.setHasQuestionnaire(proposalReview.getHasQuestionnaire());
		review.setRoleId(proposalReview.getRoleId());
		review.setRole(proposalReview.getRole());
		review.setHasRecommendation(proposalReview.getHasRecommendation());
		review = proposalModuleDao.saveOrUpdateProposalReview(review);
		if (proposalReview.getReviewerPersonId() == null) {
			addActionListEntryForEvaluation(proposal, review);
		} else {
			inboxService.addMessageToInbox(proposal, proposalReview.getReviewerPersonId(),
					proposalReview.getUpdateUser(), Constants.MESSAGE_TYPE_EVALUATION, Constants.SUBJECT_TYPE_CODE,
					review.getReviewId(), review.getRole().getDescription());
		}
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		commonService.setNotificationRecipients(proposalReview.getReviewerPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		ProposalVO proposalVO = new ProposalVO();
		proposalVO.setReviewDeadLineDate(proposalReview.getReviewDeadLineDate());
		proposalVO.setProposal(proposal);
		proposalService.sendProposalNotification(proposalVO, Constants.NOTIFICATION_CREATE_PROPOSAL_REVIEW, dynamicEmailRecipients);
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
	public String addProposalReviewComment(MultipartFile[] files, String formDataJSON) {
		boolean isBatchOperation = false;
		ProposalVO proposalVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			if (proposalVO.getProposalId() == null) {
				isBatchOperation = true;
				List<Integer> proposalIds = proposalVO.getProposalIds();
				for (Integer proposalId : proposalIds) {
					addReviewComment(proposalVO, files, proposalId, isBatchOperation);
				}
			} else {
				addReviewComment(proposalVO, files, proposalVO.getProposalId(), isBatchOperation);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return committeeDao.convertObjectToJSON(proposalVO);
	}

	private ProposalVO addReviewComment(ProposalVO proposalVO, MultipartFile[] files, Integer proposalId, Boolean isBatchOperation) {
		try {
			Integer reviewId = proposalVO.getReviewId();
			List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId);
			if (Boolean.TRUE.equals(isBatchOperation)) {
				for (ProposalReview proposalReview : proposalReviews) {
					if (proposalReview.getReviewerPersonId() != null && proposalReview.getReviewStatusCode() != null) {
						if (proposalReview.getReviewerPersonId().equals(proposalVO.getCompleteReviewerPersonId()) && proposalReview.getReviewStatusCode() == Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS) {
							reviewId = proposalReview.getReviewId();
						}
					}
				}
			}
			if (reviewId != null) {
				ReviewComment newReviewComment = new ReviewComment();
				if (Boolean.TRUE.equals(isBatchOperation)) {
					newReviewComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					newReviewComment.setUpdateUser(proposalVO.getNewReviewComment().getUpdateUser());
					newReviewComment.setFullName(proposalVO.getNewReviewComment().getFullName());
					newReviewComment.setIsPrivateComment(proposalVO.getNewReviewComment().getIsPrivateComment());
					newReviewComment.setReviewComment(proposalVO.getNewReviewComment().getReviewComment());
					newReviewComment.setPersonId(proposalVO.getNewReviewComment().getPersonId());
				} else {
					newReviewComment = proposalVO.getNewReviewComment();
				}
				newReviewComment.setProposalId(proposalId);
				ProposalReview proposalReview = proposalModuleDao.fetchProposalReview(reviewId);
				List<ReviewAttachment> reviewAttachments = new ArrayList<>();
				if (files != null && files.length > 0) {
					for (int i = 0; i < files.length; i++) {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						ReviewAttachment reviewAttachment = new ReviewAttachment();
						reviewAttachment.setReviewComment(newReviewComment);
						reviewAttachment.setProposalId(proposalId);
						reviewAttachment.setFileName(fileName);
						reviewAttachment.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
						reviewAttachment.setUpdateUser(proposalVO.getUserName());
						reviewAttachment.setMimeType(files[i].getContentType());
						FileData fileData = new FileData();
						fileData.setAttachment(files[i].getBytes());
						fileData = commonDao.saveFileData(fileData);
						reviewAttachment.setFileDataId(fileData.getFileDataId());
						reviewAttachments.add(reviewAttachment);
					}
					newReviewComment.getReviewAttachments().addAll(reviewAttachments);
				}
				newReviewComment.setProposalReview(proposalReview);
				proposalReview.getReviewComments().add(newReviewComment);
				proposalModuleDao.saveOrUpdateProposalReview(proposalReview);
			}
		proposalVO.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return proposalVO;
	}

	@Override
	public String approveOrDisapproveReview(MultipartFile[] files, String formDataJSON) {
		ProposalVO vo = null;
		addProposalReviewComment(files, formDataJSON);
		boolean isBatchOperation = false;
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, ProposalVO.class);
			List<String> ipNumbers = new ArrayList<>();
			vo.setIpNumbers(ipNumbers);
			if(vo.getProposalId() == null) {
				isBatchOperation = true;
				List<Integer> proposalIds = vo.getProposalIds();
				for (Integer proposalId : proposalIds) {
					approveOrDisapprove(vo, proposalId, isBatchOperation);
				}
			} else {
				vo = approveOrDisapprove(vo, vo.getProposalId(), isBatchOperation);
			}
			} catch (Exception e) {
			e.printStackTrace();
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	private ProposalVO approveOrDisapprove(ProposalVO vo, Integer proposalId, Boolean isBatchOperation) {
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId));
		vo.setProposal(proposal);
		String actionType = vo.getActionType();
		Integer reviewId = vo.getReviewId();
		List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId);
		if (Boolean.TRUE.equals(isBatchOperation)) {
			for(ProposalReview proposalReview : proposalReviews) {
				if(proposalReview.getReviewerPersonId() != null && proposalReview.getReviewStatusCode() != null) {
					if(proposalReview.getReviewerPersonId().equals(vo.getCompleteReviewerPersonId()) && proposalReview.getReviewStatusCode() == Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS) {
						reviewId = proposalReview.getReviewId();
					}
				}
			}
		}
		if (reviewId != null) {
				for (ProposalReview proposalReview : proposalReviews) {
				if (proposalReview.getReviewId().equals(reviewId)) {
					if (vo.getCompleteReviewerPersonId() != null) {
						proposalReview.setCompleteReviewerEmail(vo.getCompleteReviewerEmail());
						proposalReview.setCompleteReviewerFullName(vo.getCompleteReviewerFullName());
						proposalReview.setCompleteReviewerPersonId(vo.getCompleteReviewerPersonId());
						if (proposalReview.getReviewerFullName() == null) {
							proposalReview.setReviewerFullName(vo.getCompleteReviewerFullName());
						}
						if(proposalReview.getReviewerPersonId() == null) {
							proposalReview.setReviewerPersonId(vo.getCompleteReviewerPersonId());
						}
						if(proposalReview.getReviewerEmail() == null) {
							proposalReview.setReviewerEmail(vo.getCompleteReviewerEmail());
						}
					}
					if (actionType.equals("APPROVE")) {
						completeReview(proposal, proposalReview, vo.getUserName());
						if (Boolean.TRUE.equals(proposalReview.getHasEndorsed()) && (proposalReview.getHasRecommendation() || proposalReview.getHasRank())) {
							if (proposal.getIsEndorsedOnce() == false) {
								proposal.setIsEndorsedOnce(true);
							}
						}
						List<ProposalReview> proposalReviewList = evaluationDao.getInprogressReviews(proposal.getProposalId(), proposalReview.getRoleId(), Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
						if (proposalReviewList != null && proposalReviewList.isEmpty()) {
							if(vo.getReviewStatusCode() != null) {
								// HOD have both complete review and also able to make it as unsuccessful,both there review status need to be complete,This is to make that possible
								proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION_BY_OTHER);
								proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION_BY_OTHER));
							}
							updateProposalStatus(proposalReview, proposal);
							ProposalEvaluationStatusFlow proposalEvaluationStatusFlow = evaluationDao.getProposalEvaluationDetails(proposalReview);
							if(vo.getReviewStatusCode() != null) {
								proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_COMPLETE);
								proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_COMPLETE));
							}
							if (proposalEvaluationStatusFlow.getNextEvaluationStopCode() != null) {
								createFinalProposalReview(proposalEvaluationStatusFlow, proposalReview);
							}
							if (Boolean.TRUE.equals(proposalReview.getIsFinal()) && vo.getProposalStatusCode() != null) {
								proposal.setStatusCode(vo.getProposalStatusCode());
								proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(vo.getProposalStatusCode()));
							}
						}
						sendCompleteReviewNotification(vo, proposalReview);
					} else {
	//					// review disapprove section
						proposalReview.setPiReviewDeadLineDate(vo.getPiReviewDeadLineDate());
						rejectReview(proposal, proposalReview, vo.getUserName());
						List<ProposalReview> proposalReviewList = evaluationDao.getProposalReviews(proposal.getProposalId(), proposalReview.getRoleId());
						if (proposalReviewList != null && !proposalReviewList.isEmpty()) {
							for (ProposalReview proposalReviewData : proposalReviewList) {
								if (!proposalReview.getReviewId().equals(proposalReviewData.getReviewId())) {
									rejectOtherReview(proposal, proposalReviewData, vo.getUserName());
								}
							}
						}
						updateProposalStatus(proposalReview, proposal);
						vo.setPiReviewDeadLineDate(proposalReview.getPiReviewDeadLineDate());
						sendMailForDynamicUsers(Constants.APPLICATION_REVISION_NOTIFICATION_CODE, vo, null);
					}
				}
			}
		}
		proposal.setUpdateUser(vo.getUserName());
		vo.setUpdateUser(vo.getUserName());
		proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		if(proposal.getActivityTypeCode() != null) {
			if (proposal.getActivityTypeCode().equals(commonDao.getParameterValueAsString("RCBF_TYPE_CODE"))) {
				proposal.setIsRcbfProposal(true);
			}
		}
		if (proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_AWARDED)) {
			vo.setProposalId(proposal.getProposalId());
			proposal = businessRuleService.generateInstitutionalProposal(vo);
			vo.getIpNumbers().add(proposal.getIpNumber());
			// Send notification for PI
			if (proposal.getGrantTypeCode() != null && proposal.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_INTERNAL)) {
				proposalService.sendProposalNotification(vo, Constants.PROJECT_AWARDED_NOTIFICATION_CODE, new HashSet<>());
			}
		}
		if (proposal.getGrantTypeCode() != null && proposal.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_INTERNAL) &&
				proposal.getStatusCode() != null && proposal.getStatusCode().equals(Constants.PROPOSAL_STATUS_CODE_UNSUCCESSFUL)) {
				proposalService.sendProposalNotification(vo, Constants.PROPOSAL_UNSUCCESSFUL_NOTIFICATION_CODE, new HashSet<>());
		}
		vo.setProposal(proposalDao.saveOrUpdateProposal(proposal));
		vo.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposal.getProposalId()));
		vo.setEvaluationReviewStop(evaluationDao.getReviewStopEvaluvation(proposal.getStatusCode(), proposal.getActivityTypeCode()));
		vo.setHasRank(evaluationDao.checkPersonHasRank(proposal.getProposalId(), vo.getCompleteReviewerPersonId()));
		vo.setHasRecommendation(evaluationDao.checkPersonHasRecommendation(proposal.getProposalId(), vo.getCompleteReviewerPersonId()));
		return vo;
	}

	private void sendMailForDynamicUsers(Integer notificationTypeId, ProposalVO vo, Set<String> personIds) {
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			Proposal proposal = vo.getProposal();
			if (proposal.getGrantCallId() != null) {
				GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
				proposal.setGrantCallName(grantCall.getGrantCallName());
			}
			if (notificationTypeId.equals(Constants.APPLICATION_REVISION_NOTIFICATION_CODE)) {
				if (vo.getPiReviewDeadLineDate() != null) {
					vo.setReviewDeadLineDate(new Timestamp(vo.getPiReviewDeadLineDate().getTime()));
				}
				proposalService.sendProposalNotification(vo, Constants.APPLICATION_REVISION_NOTIFICATION_CODE, new HashSet<>());
			} else if (notificationTypeId.equals(Constants.COMPLETED_REVIEW_NOTIFICATION_CODE)) {
				for (String personId : personIds) {
					commonService.setNotificationRecipients(personId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
				}
				proposalService.sendProposalNotification(vo, Constants.COMPLETED_REVIEW_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else if (notificationTypeId.equals(Constants.IRB_ASSESSMENT_COMPLETION_NOTIFICATION_CODE)) {
				for (String personId : personIds) {
					commonService.setNotificationRecipients(personId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
				}
				proposalService.sendProposalNotification(vo, Constants.IRB_ASSESSMENT_COMPLETION_NOTIFICATION_CODE, dynamicEmailrecipients);
			} else if (notificationTypeId.equals(Constants.PROPOSAL_SUBMIT_NOTIFICATION_CODE)) {
				for (String personId : personIds) {
					commonService.setNotificationRecipients(personId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
				}
				proposalService.sendProposalNotification(vo, Constants.PROPOSAL_SUBMIT_NOTIFICATION_CODE, dynamicEmailrecipients);
			}
	}

	private void createFinalProposalReview(ProposalEvaluationStatusFlow proposalEvaluationStatusFlow, ProposalReview proposalReview) {
		ProposalReview review = new ProposalReview();
		review.setEvaluationStop(proposalEvaluationStatusFlow.getEvaluationStop());
		review.setEvaluationStopCode(proposalEvaluationStatusFlow.getNextEvaluationStopCode());
		review.setRole(proposalEvaluationStatusFlow.getEvaluationStop().getRole());
		review.setRoleId(proposalEvaluationStatusFlow.getEvaluationStop().getRoleId());
		review.setIsFinal(proposalEvaluationStatusFlow.getEvaluationStop().getIsFinal());
		review.setHasEndorsed(proposalEvaluationStatusFlow.getEvaluationStop().getHasEndorsed());
		review.setHasQuestionnaire(proposalEvaluationStatusFlow.getEvaluationStop().getHasQuestionnaire());
		review.setHasRank(proposalEvaluationStatusFlow.getEvaluationStop().getHasRank());
		review.setHasRecommendation(proposalEvaluationStatusFlow.getEvaluationStop().getHasRecommendation());
		review.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS));
		review.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		review.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		review.setReviewStartDate(commonDao.getCurrentTimestamp());
		review.setUpdateUser(proposalReview.getUpdateUser());
		review.setProposalId(proposalReview.getProposalId());
		proposalModuleDao.saveOrUpdateProposalReview(review);
		Proposal proposal = proposalDao.fetchProposalById(proposalReview.getProposalId());
		addActionListEntryForEvaluation(proposal, review);
	}

	public void completeReview(Proposal proposal, ProposalReview proposalReview, String updateUser) {
		proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_COMPLETE);
		proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_COMPLETE));
		Timestamp currentTimestamp = committeeDao.getCurrentTimestamp();
		proposalReview.setReviewEndDate(currentTimestamp);
		proposalReview.setUpdateTimestamp(currentTimestamp);
		proposalReview.setUpdateUser(updateUser);
		proposalReview.setHasEndorsed(proposalReview.getHasEndorsed());
		proposalReview.setIsFinal(proposalReview.getIsFinal());
		proposalReview.setHasRank(proposalReview.getHasRank());
		proposalReview = evaluationDao.saveOrUpdateReview(proposalReview);
		inboxService.removeMessageFromInbox(proposal.getProposalId(), proposalReview.getReviewId(), Constants.DEV_PROPOSAL_MODULE_CODE);
	}

	public void rejectReview(Proposal proposal, ProposalReview proposalReview, String updateUser) {
		proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION);
		proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION));
		Timestamp currentTimestamp = committeeDao.getCurrentTimestamp();
		proposalReview.setReviewEndDate(currentTimestamp);
		proposalReview.setUpdateTimestamp(currentTimestamp);
		proposalReview.setUpdateUser(updateUser);
		// Add reject review notification
		proposalReview = evaluationDao.saveOrUpdateReview(proposalReview);
		inboxService.removeMessageFromInbox(proposal.getProposalId(), proposalReview.getReviewId(), Constants.DEV_PROPOSAL_MODULE_CODE);
		inboxService.addMessageToInbox(proposal, proposal.getInvestigator().getPersonId(), proposalReview.getUpdateUser(), Constants.MESSAGE_TYPE_PROPOSAL_REJECT, Constants.SUBJECT_TYPE_CODE, proposalReview.getReviewId(), "");
		Workflow workFlowData = workflowDao.getActiveWorkFlow(proposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		if (workFlowData != null) {
			workFlowData.setWorkflowEndDate(commonDao.getCurrentTimestamp());
			workFlowData.setWorkflowEndPerson(proposalReview.getCompleteReviewerPersonId());
			workflowDao.saveWorkflow(workFlowData);
		}
	}

	public void rejectOtherReview(Proposal proposal, ProposalReview proposalReview, String updateUser) {
		proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION_BY_OTHER);
		proposalReview.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION_BY_OTHER));
		Timestamp currentTimestamp = committeeDao.getCurrentTimestamp();
		proposalReview.setReviewEndDate(currentTimestamp);
		proposalReview.setUpdateTimestamp(currentTimestamp);
		proposalReview.setUpdateUser(updateUser);
		// Add reject review notification
		evaluationDao.saveOrUpdateReview(proposalReview);
		inboxService.removeMessageFromInbox(proposal.getProposalId(), proposalReview.getReviewId(), Constants.DEV_PROPOSAL_MODULE_CODE);
	}

	@Override
	public ResponseEntity<byte[]> downloadProposalReviewAttachment(Integer attachmentId) {
		ReviewAttachment attachment = evaluationDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return attachmentData;
	}

	@Override
	public Proposal createReviewForResubmit(Integer proposalId) {
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		List<ProposalReview> proposalReviews = evaluationDao.fetchRevisionRequestedReview(proposalId);
		ProposalReview proposalReview = new ProposalReview();
		for (ProposalReview proposalReviewData : proposalReviews) {
			if (proposalReview.getReviewId() == null) {
				proposalReview = proposalReviewData;
			}
			if (proposalReview.getReviewId() < proposalReviewData.getReviewId()) {
				proposalReview = proposalReviewData;
			}
		}
		proposalReview.setIsReturned(true);
		createProposalReview(proposal, proposalReview);
		proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		updateProposalStatus(proposalReview, proposal);
		proposalReview.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_REVISION);
		proposal = proposalDao.saveOrUpdateProposal(proposal);
		return proposal;
	}

	@Override
	public String fetchSortedReviews(ProposalVO vo) {
		vo.setProposalReviews(prepareProposalReviewDetail(vo));
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public List<ProposalReview> prepareProposalReviewDetail(ProposalVO vo) {
		String leadUnitNumber = proposalDao.fetchProposalLeadUnitNumberByProposalId(vo.getProposalId());
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(vo.getPersonId(), Constants.VIEW_PRIVATE_COMMENTS_RIGHT, leadUnitNumber);
		List<ProposalReview> proposalReviews = evaluationDao.fetchSortedReviews(vo.getProposalId(), vo.getSortBy(), vo.getReverse());
		List<ProposalReview> proposalReviewDetails = new ArrayList<>();
		proposalReviews.forEach(proposalReview -> {
			ProposalReview proposalReviewDetail = new ProposalReview();
			ReflectionUtils.shallowCopyFieldState(proposalReview, proposalReviewDetail);
			List<ReviewComment> reviewComments = new ArrayList<>();
			proposalReview.getReviewComments().forEach(reviewComment -> {
				if (Boolean.TRUE.equals(isPersonHasPermission)
						&& Boolean.TRUE.equals(reviewComment.getIsPrivateComment())
						|| (Boolean.FALSE.equals(reviewComment.getIsPrivateComment())
								|| Boolean.TRUE.equals(reviewComment.getIsPrivateComment())
										&& vo.getPersonId().equals(reviewComment.getPersonId()))) {
					reviewComments.add(reviewComment);
				}
			});
			proposalReviewDetail.setReviewComments(reviewComments);
			proposalReviewDetails.add(proposalReviewDetail);
		});
		return proposalReviewDetails;
	}

	@Override
	public String getProposalAndReviewSummary(Integer proposalId, String personId) {
		Proposal proposalObject = proposalDao.fetchProposalById(proposalId);
		Proposal proposal = new Proposal();
		ProposalVO vo = new ProposalVO();
		Sponsor sponsor = proposalObject.getSponsor();
		proposal.setProposalId(proposalObject.getProposalId());
		proposal.setTitle(proposalObject.getTitle());
		proposal.setSponsorName(sponsor!= null ? commonService.getSponsorFormatBySponsorDetail(sponsor.getSponsorCode(), sponsor.getSponsorName(), sponsor.getAcronym()) : "");
		proposal.setStartDate(proposalObject.getStartDate());
		proposal.setEndDate(proposalObject.getEndDate());
		proposal.setStatusCode(proposalObject.getStatusCode());
		proposal.setProposalStatus(proposalObject.getProposalStatus());
		proposal.setActivityTypeCode(proposalObject.getActivityTypeCode());
		proposal.setActivityType(proposalObject.getActivityType());
		proposal.setProposalRank(proposalObject.getProposalRank());
		proposal.setIsEndorsedOnce(proposalObject.getIsEndorsedOnce());
		proposal.setGrantTypeCode(proposalObject.getGrantTypeCode());
		proposal.setGrantCallType(proposalObject.getGrantCallType());
		proposal.setRecommendationCode(proposalObject.getRecommendationCode());
		proposal.setEvaluationRecommendation(proposalObject.getEvaluationRecommendation());
		List<ProposalReview> proposalReviewList = new ArrayList<>();
		for (ProposalReview proposalReview : proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId)) {
			ProposalReview propReview = new ProposalReview();
			propReview.setEvaluationStop(proposalReview.getEvaluationStop());
			propReview.setReviewerFullName(proposalReview.getReviewerFullName());
			propReview.setReviewStatus(proposalReview.getReviewStatus());
			propReview.setReviewStartDate(proposalReview.getReviewStartDate());
			propReview.setReviewDeadLineDate(proposalReview.getReviewDeadLineDate());
			propReview.setReviewEndDate(proposalReview.getReviewEndDate());
			proposalReviewList.add(propReview);
		}
		vo.setProposal(proposal);
		vo.setProposalReviews(proposalReviewList);
		vo.setEvaluationRecommendation(evaluationDao.getAllEvaluationRecomendation());
		proposal.setHasRank(evaluationDao.checkPersonHasRank(proposalId, personId));
		proposal.setHasRecommendation(evaluationDao.checkPersonHasRecommendation(proposalId, personId));
		return committeeDao.convertObjectToJSON(vo);
	}

	// Send notification for complete review
	public void sendCompleteReviewNotification(ProposalVO vo, ProposalReview proposalReview) {
		Proposal proposal = vo.getProposal();
		if (proposal.getGrantCallId() != null) {
			Boolean canSendNotification = true;
			List<Proposal> proposals = proposalDao.fetchProposalsOfGrantCall(proposal.getGrantCallId());
			// Identify if all reviews assigned to a role type of a grant call is completed.
			for (Proposal proposalData : proposals) {
				List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalData.getProposalId());
				for (ProposalReview proposalReviewData : proposalReviews) {
					if (proposalReviewData.getRoleId().equals(proposalReview.getRoleId()) && proposalReviewData.getReviewStatusCode().equals(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS)) {
						canSendNotification = false;
					}
				}
			}
			if (Boolean.TRUE.equals(canSendNotification)) {
				if (proposalReview.getRoleId().equals(Constants.PROPOSAL_GRANT_ADMIN_ROLE_TYPE_CODE)) {
					Set<String> emailIds = new HashSet<>();
					List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposal.getProposalId());
					for (ProposalReview proposalReviewData : proposalReviews) {
						if (proposalReviewData.getRoleId().equals(Constants.PROPOSAL_GRANT_MANAGER_ROLE_TYPE_CODE)) {
							emailIds.add(proposalReviewData.getReviewerPersonId());
						}
					}
					vo.setReviewerRole("ORTT Manager(s)");
					vo.setPiReviewDeadLineDate(proposalReview.getPiReviewDeadLineDate());
					sendMailForDynamicUsers(Constants.COMPLETED_REVIEW_NOTIFICATION_CODE, vo, emailIds);
				} else if (proposalReview.getRoleId().equals(Constants.PROPOSAL_DEC_ROLE_TYPE_CODE)) {
					Set<String> emailIds = new HashSet<>();
					List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposal.getProposalId());
					for (ProposalReview proposalReviewData : proposalReviews) {
						if (proposalReviewData.getRoleId().equals(Constants.PROPOSAL_GRANT_ADMIN_ROLE_TYPE_CODE)) {
							emailIds.add(proposalReviewData.getReviewerPersonId());
						}
					}
					vo.setReviewerRole("School/ICL Admins");
					vo.setPiReviewDeadLineDate(proposalReview.getPiReviewDeadLineDate());
					sendMailForDynamicUsers(Constants.COMPLETED_REVIEW_NOTIFICATION_CODE, vo, emailIds);
				} 
			}
		}
	}

	public void sendAssignReviewNotification(ProposalReview newProposalReview, Map<String, Integer> grantCallMap, Proposal proposal) {
		for (Map.Entry<String, Integer> grantCallEntry : grantCallMap.entrySet()) {
			NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(Constants.APPLICATION_REVIEW_NOTIFICATION_CODE);
			commonService.sendMailForAssignReview(notificationType, newProposalReview, grantCallEntry, newProposalReview.getEvaluationStop().getDescription(), proposal);
		}
	}

	@Override
	public String deleteReviewComment(ProposalVO vo) {
		ProposalReview proposalReview = proposalModuleDao.fetchProposalReview(vo.getReviewId());
		List<ReviewComment> commentList = proposalReview.getReviewComments();
		List<ReviewComment> updatedlist = new ArrayList<>(commentList);
		Collections.copy(updatedlist, commentList);
		for (ReviewComment reviewComment : commentList) {
			if (reviewComment.getCommentId().equals(vo.getReviewCommentId())) {
				updatedlist.remove(reviewComment);
			}
		}
		proposalReview.getReviewComments().clear();
		proposalReview.getReviewComments().addAll(updatedlist);
		proposalModuleDao.saveOrUpdateProposalReview(proposalReview);
		vo.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(vo.getProposalId()));
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String addReviewer(ProposalVO vo) {
		ProposalReview review = evaluationDao.getProposalReviewByReviewId(vo.getReviewId());
		Boolean checkPersonHasReview = evaluationDao.checkPersonHasReview(vo.getReviewerPersonId(), vo.getProposalId(), review.getRoleId());
		if (Boolean.FALSE.equals(checkPersonHasReview)) {
			review.setReviewerFullName(vo.getReviewerFullName());
			review.setReviewerEmail(vo.getReviewerEmail());
			review.setReviewerPersonId(vo.getReviewerPersonId());
			review.setReviewDeadLineDate(vo.getReviewDeadLine());
			review = evaluationDao.saveOrUpdateReview(review);
		}
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		inboxDao.markReadMessage(Constants.DEV_PROPOSAL_MODULE_CODE, proposal.getProposalId().toString(),null, Constants.MESSAGE_TYPE_EVALUATION, review.getReviewId().toString(), Constants.PROPOSAL_EVALUATION_SUBMODULE_CODE);
		inboxService.addMessageToInbox(proposal, vo.getReviewerPersonId() , vo.getUpdateUser(), Constants.MESSAGE_TYPE_EVALUATION, Constants.SUBJECT_TYPE_CODE, review.getReviewId(), review.getRole().getDescription());
		vo.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(vo.getProposalId()));
		vo.setHasRank(evaluationDao.checkPersonHasRank(vo.getProposalId(), vo.getLogginPersonId()));
		vo.setHasRecommendation(evaluationDao.checkPersonHasRecommendation(vo.getProposalId(), vo.getLogginPersonId()));
		vo.setFinalEvaluationStatus(evaluationDao.getFinalEvaluationStatus());
		vo.setEvaluationRecommendation(evaluationDao.getAllEvaluationRecomendation());
		vo.getProposal().setIsReviewExist(checkPersonHasReview);
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		commonService.setNotificationRecipients(review.getReviewerPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		ProposalVO proposalVO = new ProposalVO();
		proposalVO.setReviewDeadLineDate(review.getReviewDeadLineDate());
		proposalVO.setProposal(proposal);
		proposalService.sendProposalNotification(proposalVO, Constants.NOTIFICATION_CREATE_PROPOSAL_REVIEW, dynamicEmailRecipients);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void createGrantAdminReview(Proposal proposal) {
		ProposalReview review = new ProposalReview();
		EvaluationStop evaluationStop = evaluationDao.fetchEvaluationStopDetails(commonDao.getParameterValueAsString("RCBF_TYPE_CODE"), commonDao.getParameter("RCBF_STATUS_CODE") );
		review.setEvaluationStop(evaluationStop);
		review.setEvaluationStopCode(evaluationStop.getEvaluationStopCode());
		review.setRole(evaluationStop.getRole());
		review.setRoleId(evaluationStop.getRoleId());
		review.setReviewStatus(evaluationDao.getReviewStatusByStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS));
		review.setReviewStatusCode(Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		review.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		review.setReviewStartDate(commonDao.getCurrentTimestamp());
		review.setUpdateUser(proposal.getUpdateUser());
		review.setProposalId(proposal.getProposalId());
		proposalModuleDao.saveOrUpdateProposalReview(review);
		addActionListEntryForEvaluation(proposal, review);
	}

	@Override
	public String sendAdminReviewReminderNotification(ProposalVO vo) {
		List<ProposalReview> proposalReviews = null;
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		vo.setProposal(proposal);
		if (proposal.getGrantCallId() != null) {
			vo.setNumberOfApplications(1);
		}
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (vo.getUserRole().equals("Grant Admin Review Reminder")) {
			vo.setReviewerRole("School Admins");
			proposalReviews = evaluationDao.getInprogressReviews(vo.getProposalId(),Constants.PROPOSAL_GRANT_ADMIN_ROLE_TYPE_CODE, Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		} else {
			vo.setReviewerRole("DEC Members");
			proposalReviews = evaluationDao.getInprogressReviews(vo.getProposalId(),Constants.PROPOSAL_DEC_ROLE_TYPE_CODE, Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		}
		for (ProposalReview proposalReview : proposalReviews) {
			commonService.setNotificationRecipients(proposalReview.getReviewerPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		}
		proposalService.sendProposalNotification(vo, Constants.APPLICATION_REVIEW_REMINDER_NOTIFICATION_CODE, dynamicEmailRecipients);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String sendPIReviewReminderNotification(ProposalVO vo) {
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(vo.getProposalId()));
		vo.setProposal(proposal);
		if (proposal.getGrantCallId() != null) {
			vo.setNumberOfApplications(1);
		}
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		commonService.setNotificationRecipients(proposal.getInvestigator().getPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
		proposalService.sendProposalNotification(vo, Constants.APPLICATION_REVISION_REMINDER_NOTIFICATION_CODE, dynamicEmailRecipients);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchEvaluationPanels(EvaluationVO vo) {
		if (vo.getProposalId() != null) {
			List<ProposalEvaluationPanel> proposalEvaluationPanels = evaluationDao.fetchProposalEvaluationPanelsByProposalId(vo.getProposalId());
			if (proposalEvaluationPanels != null && !proposalEvaluationPanels.isEmpty()) {
				vo.setProposalEvaluationPanelsList(proposalEvaluationPanels);
				for (ProposalEvaluationPanel proposalEvaluationPanel : proposalEvaluationPanels) {
					proposalEvaluationPanel.setPanelName(proposalEvaluationPanel.getWorkflowMap().getDescription());
				}
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveProposalEvaluationPanelDetails(EvaluationVO vo) {
		List<ProposalEvaluationPanel> evaluationPanelsList = vo.getProposalEvaluationPanelsList();
		for (ProposalEvaluationPanel evaluationPanel : evaluationPanelsList) {
			evaluationPanel.setUpdateUser(vo.getUpdateUser());
			evaluationPanel.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			evaluationDao.saveProposalEvaluationPanelDetails(evaluationPanel);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String startEvaluationWithFinalEvaluationPanelList(EvaluationVO vo) {
		ResultSet resultSet = evaluationDao.getRemovedPersonListFromEvaluation(vo.getProposalId().toString(), Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		executorService.execute(() -> sendMailForRemovedPersons(vo, resultSet));
		boolean success = evaluationDao.buildEvaluationPanel(vo.getProposalId().toString(), Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL, vo.getPersonId(), vo.getUpdateUser(), vo.getActionType(), Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		if (proposal.getProposalStatus().getStatusCode().equals(Constants.ADMIN_CHECK_COMPLETED)) {
			proposal.setStatusCode(Constants.REVIEW_IN_PROGRESS);
			proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.REVIEW_IN_PROGRESS));
			proposal.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			proposalDao.saveOrUpdateProposal(proposal);
		}
		if (success) {
			sendStartEvaluationNotification(vo);
			vo.setMessage("Success");
		} else {
			vo.setMessage("Failed");
		}
		return proposalService.loadProposalById(vo.getProposalId(), vo.getPersonId(), vo.getUpdateUser(), Boolean.FALSE);
	}

	public void sendMailForRemovedPersons(EvaluationVO vo, ResultSet resultSet) {
		try {
			while (resultSet != null && resultSet.next()) {
				Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
				if (resultSet.getString("APPROVER_PERSON_ID") != null) {
				commonService.setNotificationRecipients(resultSet.getString("APPROVER_PERSON_ID"), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				EmailServiceVO emailServiceVO = new EmailServiceVO();
				emailServiceVO.setNotificationTypeId(Constants.EVALUATION_PERSON_REMOVE_NOTIFICATION_CODE);
				emailServiceVO.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
				emailServiceVO.setModuleItemKey(vo.getProposalId().toString());
				getEvaluationPlaceHolders(resultSet);
				emailServiceVO.setPlaceHolder(getEvaluationPlaceHolders(resultSet));
				emailServiceVO.setSubModuleCode((Constants.DEV_PROPOSAL_SUBMODULE_CODE).toString());
				emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				if (!dynamicEmailRecipients.isEmpty()) {
					emailServiceVO.setRecipients(dynamicEmailRecipients);
				}
				emailService.sendEmail(emailServiceVO);
				}
			}
		} catch (SQLException e) {
			logger.error("error occured in sendMailForRemovedPersons : {}", e.getMessage());
		}
		
	}

	private Map<String, String> getEvaluationPlaceHolders(ResultSet resultSet) {
		Map<String, String> placeHolder = new HashMap<>();
		try {
			placeHolder.put("{MAP_NAME}", resultSet.getString("MAP_NAME") == null ? "" : resultSet.getString("MAP_NAME"));
			if (resultSet.getString("APPROVER_PERSON_ID") != null) {
				String personName = personDao.getPersonFullNameByPersonId(resultSet.getString("APPROVER_PERSON_ID"));
				placeHolder.put("{USER_FULL_NAME}",  personName == null ? "" : personName);
			} else {
				placeHolder.put("{USER_FULL_NAME}", "");
			}
			return placeHolder;
		} catch (Exception e) {
			logger.error("error occured in getEvaluationPlaceHolders : {}", e.getMessage());
			return placeHolder;
		}
		
	}

	@Override
	public String addEvaluationPanelPerson(EvaluationVO vo) {
		ProposalEvaluationPanelPersons proposalEvaluationPanelPerson = vo.getProposalEvaluationPanelPerson();
		Integer approverSequenceNumber = evaluationDao.getMaxApproverNumber(vo.getProposalEvaluationPanelId());
		approverSequenceNumber = approverSequenceNumber + 1;
		proposalEvaluationPanelPerson.setApproverNumber(approverSequenceNumber);
		proposalEvaluationPanelPerson.setUpdateUser(vo.getUpdateUser());
		proposalEvaluationPanelPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalEvaluationPanelPerson.setProposalEvaluationPanel(evaluationDao.fetchProposalEvaluationPanelById(vo.getProposalEvaluationPanelId()));
		proposalEvaluationPanelPerson = evaluationDao.saveEvaluationPanelPerson(proposalEvaluationPanelPerson);
		vo.setProposalEvaluationPanelPerson(proposalEvaluationPanelPerson);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteEvaluationPanelPerson(EvaluationVO vo) {
		ProposalEvaluationPanelPersons proposalEvaluationPanelPerson = evaluationDao.fetchProposalEvaluationPanelPersonById(vo.getProposalEvaluationPanelPersonId());
		evaluationDao.deleteProposalEvaluationPanelPerson(proposalEvaluationPanelPerson);
		vo.setMessage("Person deleted successfully");
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveEvaluation(EvaluationVO vo) {
		List<ProposalEvaluationPanel> evaluationPanelList = new ArrayList<>();
		try {
			if (vo.getProposalEvaluationPanelsList() != null && !vo.getProposalEvaluationPanelsList().isEmpty()) {
				List<ProposalEvaluationPanel> evaluationPanelsList = vo.getProposalEvaluationPanelsList();
				for (ProposalEvaluationPanel evaluationPanel : evaluationPanelsList) {
					evaluationPanel.setUpdateUser(vo.getUpdateUser());
					evaluationPanel.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					evaluationPanel = evaluationDao.saveProposalEvaluationPanelDetails(evaluationPanel);
					evaluationPanelList.add(evaluationPanel);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(evaluationPanelList);
	}

	public ReviewAttachment setReviewAttachmentObject(ReviewComment newReviewComment, MultipartFile multipartFile, String updateUser, Integer proposalId) {
		ReviewAttachment reviewAttachment = new ReviewAttachment();
		try {
			File file = new File(multipartFile.getOriginalFilename());
			String fileName = file.getName();
			reviewAttachment.setReviewComment(newReviewComment);
			reviewAttachment.setProposalId(proposalId);
			reviewAttachment.setFileName(fileName);
			reviewAttachment.setUpdateTimestamp(committeeDao.getCurrentTimestamp());
			reviewAttachment.setUpdateUser(updateUser);
			reviewAttachment.setMimeType(multipartFile.getContentType());
			FileData fileData = new FileData();
			fileData.setAttachment(multipartFile.getBytes());
			fileData = commonDao.saveFileData(fileData);
			reviewAttachment.setFileDataId(fileData.getFileDataId());
		} catch (IOException e) {
			e.printStackTrace();
		}
		return reviewAttachment;
	}

	private void sendStartEvaluationNotification(EvaluationVO evaluationVO) {
		List<ProposalEvaluationPanel> proposalEvaluationPanelsList = evaluationVO.getProposalEvaluationPanelsList();
		for (ProposalEvaluationPanel proposalEvaluationPanel : proposalEvaluationPanelsList) {
			if (proposalEvaluationPanel.getIsAdminSelected()) {
				Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
				for (ProposalEvaluationPanelPersons proposalEvaluationPanelPersons : proposalEvaluationPanel.getProposalEvaluationPanelPersons()) {
					commonService.setNotificationRecipients(proposalEvaluationPanelPersons.getApproverPersonId().toString(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				}
				ProposalVO proposalVO = new ProposalVO();
				Proposal proposal = proposalDao.fetchProposalById(proposalEvaluationPanel.getProposalId());
				proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
				if (proposal.getGrantCallId() != null) {
					GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
					proposal.setGrantCallName(grantCall.getGrantCallName());
				}
				proposalVO.setProposal(proposal);
				if (proposalEvaluationPanel.getCanScore()) {
					proposalService.sendProposalNotification(proposalVO, Constants.EVALUATION_PANEL_REVIEW_NOTIFICATION_CODE, dynamicEmailRecipients);
				} else {
					proposalService.sendProposalNotification(proposalVO, Constants.PROPOSAL_APPROVE_NOTIFICATION_CODE, dynamicEmailRecipients);
				}
			}
		}
	}

	@Override
	public String getEvaluationPersonsBasedOnRole(Integer personRoleId, String unitNumber) {
		return committeeDao.convertObjectToJSON(getEvaluationPersons(personRoleId, unitNumber));
	}

	@Override
	public String approveOrDisapproveReviewForWaf(ProposalVO proposalVO) {
		ProposalVO vo = null;
		boolean haveReviewComment = false;
		try {
			if (proposalVO.getNewReviewComment() != null) {
				haveReviewComment = true;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (haveReviewComment) {
			vo = addProposalReviewCommentForWaf(proposalVO);
		}
		boolean isBatchOperation = false;
		try {
			if (vo != null) {
				List<String> ipNumbers = new ArrayList<String>();
				vo.setIpNumbers(ipNumbers);
				String splicedFile = vo.getFileContent();
				if (vo.getIsLastUploadedFile() || (splicedFile == null)) {
					if (vo.getProposalId() == null) {
						isBatchOperation = true;
						List<Integer> proposalIds = vo.getProposalIds();
						for (Integer proposalId : proposalIds) {
							approveOrDisapprove(vo, proposalId, isBatchOperation);
						}
					} else {
						vo = approveOrDisapprove(vo, vo.getProposalId(), isBatchOperation);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	private ProposalVO addProposalReviewCommentForWaf(ProposalVO proposalVO) {
		boolean isBatchOperation = false;
		try {
			MultipartFile multipartFile = null;
			String name = null;
			String contentType = null;
			Integer remaining = proposalVO.getRemaining();
			Integer length = proposalVO.getLength();
			String userId = proposalVO.getPersonId();
			String splicedFile = proposalVO.getFileContent();
			String timestamp = proposalVO.getFileTimestamp();
			if (splicedFile != null) {
				name = proposalVO.getFileName();
				contentType = proposalVO.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || ( splicedFile == null)) {
				if (proposalVO.getProposalId() == null) {
					isBatchOperation = true;
					List<Integer> proposalIds = proposalVO.getProposalIds();
					for (Integer proposalId : proposalIds) {
						addReviewCommentForWaf(proposalVO, multipartFile, proposalId, isBatchOperation);
					}
				} else {
					addReviewCommentForWaf(proposalVO, multipartFile, proposalVO.getProposalId(), isBatchOperation);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return proposalVO;
	}

	@Override
	public String addReviewCommentForWaf(ProposalVO proposalVO) {
		boolean isBatchOperation = false;
		try {
			MultipartFile multipartFile = null;
			String name = null;
			String contentType = null;
			Integer remaining = proposalVO.getRemaining();
			Integer length = proposalVO.getLength();
			String userId = proposalVO.getPersonId();
			String splicedFile = proposalVO.getFileContent();
			String timestamp = proposalVO.getFileTimestamp();
			if (splicedFile != null) {
				name = proposalVO.getFileName();
				contentType = proposalVO.getContentType();
				multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
			}
			if ((multipartFile != null && !multipartFile.isEmpty()) || (splicedFile == null)) {
				if (proposalVO.getProposalId() == null) {
					isBatchOperation = true;
					List<Integer> proposalIds = proposalVO.getProposalIds();
					for (Integer proposalId : proposalIds) {
						addReviewCommentForWaf(proposalVO, multipartFile, proposalId, isBatchOperation);
					}
				} else {
					addReviewCommentForWaf(proposalVO, multipartFile, proposalVO.getProposalId(), isBatchOperation);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return committeeDao.convertObjectToJSON(proposalVO);
	}

	private ProposalVO addReviewCommentForWaf(ProposalVO proposalVO, MultipartFile multipartFile, Integer proposalId, Boolean isBatchOperation) {
		try {
			Integer reviewId = proposalVO.getReviewId();
			List<ProposalReview> proposalReviews = proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId);
			if (isBatchOperation) {
				for (ProposalReview proposalReview : proposalReviews) {
					if (proposalReview.getReviewerPersonId() != null && proposalReview.getReviewStatusCode() != null) {
						if (proposalReview.getReviewerPersonId().equals(proposalVO.getCompleteReviewerPersonId())
								&& proposalReview
										.getReviewStatusCode() == Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS) {
							reviewId = proposalReview.getReviewId();
						}
					}
				}
			}
			if (reviewId != null) {
				ReviewComment newReviewComment = new ReviewComment();
				if (isBatchOperation) {
					newReviewComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					newReviewComment.setUpdateUser(proposalVO.getNewReviewComment().getUpdateUser());
					newReviewComment.setFullName(proposalVO.getNewReviewComment().getFullName());
					newReviewComment.setIsPrivateComment(proposalVO.getNewReviewComment().getIsPrivateComment());
					newReviewComment.setReviewComment(proposalVO.getNewReviewComment().getReviewComment());
					newReviewComment.setPersonId(proposalVO.getNewReviewComment().getPersonId());
					newReviewComment.setProposalId(proposalId);
				} else {
					newReviewComment = proposalVO.getNewReviewComment();
					if (newReviewComment != null) {
						newReviewComment.setProposalId(proposalId);
					}
				}
				ProposalReview proposalReview = proposalModuleDao.fetchProposalReview(reviewId);
				List<ReviewAttachment> reviewAttachments = new ArrayList<>();
				if (newReviewComment.getCommentId() != null) {
					Integer id = newReviewComment.getCommentId();
					for (ReviewComment reviewComment : proposalReview.getReviewComments()) {
						if (reviewComment.getCommentId() != null && reviewComment.getCommentId().equals(id)) {
							if (multipartFile != null && !multipartFile.isEmpty()) {
								reviewAttachments.add(setReviewAttachmentObject(newReviewComment, multipartFile, proposalVO.getUpdateUser(), proposalId));
								reviewComment.getReviewAttachments().addAll(reviewAttachments);
							}
						}
					}
				} else {
					if (multipartFile != null && !multipartFile.isEmpty()) {
						reviewAttachments.add(setReviewAttachmentObject(newReviewComment, multipartFile, proposalVO.getUpdateUser(), proposalId));
					}
					newReviewComment.getReviewAttachments().addAll(reviewAttachments);
				}
				newReviewComment.setProposalReview(proposalReview);
				if (newReviewComment.getCommentId() == null) {
					proposalReview.getReviewComments().add(newReviewComment);
				}
				proposalReview = proposalModuleDao.saveOrUpdateProposalReview(proposalReview);
			}
			proposalVO.setProposalReviews(proposalModuleDao.fetchProposalReviewBasedOnProposalId(proposalId));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return proposalVO;
	}

	private List<Person> getEvaluationPersons(Integer personRoleId, String unitNumber) {
		List<Person> persons = new ArrayList<>();
		List<PersonRoles> personRoles = null;
		if (personRoleId.equals(Constants.PROPOSAL_GRANT_ADMIN_ROLE_TYPE_CODE) || personRoleId.equals(Constants.PROPOSAL_RCBF_ROLE_TYPE_CODE)) {
			personRoles = personDao.getPersonRoleByUnitNumberAndRoleId(unitNumber, personRoleId);
		} else {
			personRoles = personDao.getPersonRolesByRoleId(personRoleId);
		}
		for (PersonRoles personRole : personRoles) {
			persons.add(personRole.getPerson());
		}
		return persons;
	}

	private void addActionListEntryForEvaluation(Proposal proposal, ProposalReview proposalReview) {
		List<Person> persons = getEvaluationPersons(proposalReview.getRoleId(), proposal.getHomeUnitNumber());
		if (!persons.isEmpty()) {
			for (Person person : persons) {
				inboxService.addMessageToInbox(proposal, person.getPersonId(), proposalReview.getUpdateUser(),
						Constants.MESSAGE_TYPE_EVALUATION, Constants.SUBJECT_TYPE_CODE, proposalReview.getReviewId(),
						proposalReview.getRole().getDescription());
			}
		}
	}

	@Override
	public ResponseEntity<byte[]> exportGrantCallEvaluationReport(EvaluationMainPanelVO vo, HttpServletResponse response) {
		vo.setSubmittedProposals(proposalDao.fetchSubmittedProposalList(vo.getGrantCallId()));
		if (vo.getExportIndex() != null && vo.getExportIndex().equals("BY_CRITERIA")) {
			prepareDataForEvaluationByCriteria(vo);
		} else if(vo.getExportIndex() != null && vo.getExportIndex().equals("BY_PERSON")) {
			prepareDataForEvaluationByPerson(vo);
		}
		return prepareWorkbookForEvaluationReportt(vo);
	}

	private void prepareDataForEvaluationByPerson(EvaluationMainPanelVO vo) {
			String loginPersonId = AuthenticatedUser.getLoginPersonId();
			String loginPersonUnitNumber = AuthenticatedUser.getLoginPersonUnit();
			List<String> keys = new ArrayList<>();
			Set<String> scoringMembers = new HashSet<>();
			Set<String> panelMembers = new HashSet<>();
			List<Map<String, Object>> evaluationReports = new ArrayList<>();
			vo.getSubmittedProposals().stream().forEach(proposal-> {
				Map<String, Object> evaluationReport = new HashMap<>();
				setProposalDetails(evaluationReport, proposal);
				Map<String, List<ScoringReportDto>> scoringCriterias = grantCallService.getCriteriaScoreByProposalId(proposal.getProposalId(), loginPersonId, loginPersonUnitNumber, AuthenticatedUser.getLoginUserName(), Boolean.FALSE);
				BigDecimal total = BigDecimal.ZERO;
				for (Entry<String, List<ScoringReportDto>> entry : scoringCriterias.entrySet()) {
					StringBuilder comment = new StringBuilder();
					StringBuilder nonScoringComment = new StringBuilder();
					List<ScoringReportDto> scoringReports = entry.getValue();
					BigDecimal scoreTotal = (scoringReports.parallelStream().filter(scroingReport -> scroingReport.getPerson().getScore()!= null).map(ScoringReportDto::getPerson).map(ScoredPersonDTO :: getScore)).reduce(BigDecimal.ZERO, BigDecimal::add);
					Long scoredPersonCount = (scoringReports.parallelStream().filter(scroingReport -> scroingReport.getPerson().getScore()!= null)).count();
					Set<String> scoringPanel = new HashSet<>();
					scoringReports.stream().forEach(scroingReport -> setCommentAndPanelDetailsForPerson(scroingReport, comment, nonScoringComment, scoringMembers, scoringPanel, panelMembers, entry));
					BigDecimal avg = (scoreTotal != null && scoredPersonCount!= 0 ) ? scoreTotal.divide(new BigDecimal(scoredPersonCount),2) : BigDecimal.ZERO;
					evaluationReport.put(entry.getKey(), avg);
					evaluationReport.put("Scoring Panel", String.join(", ", scoringPanel));
					// empty check for commend and then add to tha map
					if (comment != null) {
						evaluationReport.put(entry.getKey()+ "-comment", comment);
					} 
					if (nonScoringComment != null) {
						evaluationReport.put(entry.getKey()+ "-comment ", nonScoringComment);
					}
					total = total.add(avg);
				}
				evaluationReport.put("Total", total);
				evaluationReports.add(evaluationReport);
			});
			vo.setEvaluationDatas(evaluationReports);
			setKeysForProposalDetails(keys);
			vo.setScoringColorEndAt(scoringMembers.size()*2);
			List<String> scoringMemberList = new ArrayList<>(scoringMembers);
					Collections.sort(scoringMemberList);
			for (String scoringMember : scoringMemberList) {
				keys.add(scoringMember);
				keys.add(scoringMember+ "-comment");
			}
			keys.add("Total");
			List<String> panelMemberList = new ArrayList<>(panelMembers);
			Collections.sort(panelMemberList);
			for (String panelMember : panelMemberList) {
				keys.add(panelMember+ "-comment ");
			}
			vo.setKeys(keys);
	}

	private void setCommentAndPanelDetailsForPerson(ScoringReportDto scroingReport, StringBuilder comment,
			StringBuilder nonScoringComment, Set<String> scoringMembers, Set<String> scoringPanel,
			Set<String> panelMembers, Entry<String, List<ScoringReportDto>> entry) {
		scroingReport.getPerson().getWorkflowReviewerComments().forEach(comments -> {
			if (comments.getComment() != null) {
				if (scroingReport.getPerson().getIsPersonCanScore().equals(Boolean.TRUE)) {
					comment.append(comments.getComment()).append("\n\n");
				} else {
					nonScoringComment.append(comments.getComment()).append("\n\n");
				}
			}
		});
		if(Boolean.TRUE.equals(scroingReport.getPerson().getIsPersonCanScore())) {
			scoringMembers.add(entry.getKey());
			scoringPanel.add(scroingReport.getPerson().getEvaluationMapName());
		} else {
			panelMembers.add(entry.getKey());
		}	
	}

	private void setProposalDetails(Map<String, Object> evaluationReport, Proposal proposal) {
		evaluationReport.put("Proposal Id", proposal.getProposalId());
		evaluationReport.put("Proposal Type", proposal.getProposalTypeDescription());
		evaluationReport.put("Title", proposal.getTitle());
		evaluationReport.put("Category", proposal.getProposalCategory());
		evaluationReport.put("Lead Unit", proposal.getHomeUnitName());
		evaluationReport.put("Status", proposal.getProposalStatus().getDescription());
		evaluationReport.put("Primary Title of Lead PI", proposal.getInvestigator().getPrimaryTitle());
		evaluationReport.put("PI", proposal.getInvestigator().getFullName());
		evaluationReport.put("Duration", proposal.getDuration());
		evaluationReport.put("Total Budget", proposal.getTotalCost());
		evaluationReport.put("Total Score", proposal.getTotal());
		evaluationReport.put("Overall Average Score", proposal.getScore());
		evaluationReport.put("Adjusted Score", proposal.getProposalEvaluationScore().getAdjustedScore());
		evaluationReport.put("Rank", proposal.getProposalEvaluationScore().getProposalRank());
	}

	private void prepareDataForEvaluationByCriteria(EvaluationMainPanelVO vo) {
		String loginPersonId = AuthenticatedUser.getLoginPersonId();
		String loginPersonUnitNumber = AuthenticatedUser.getLoginPersonUnit();
		List<String> keys = new ArrayList<>();
		Set<String> criterias = new HashSet<>();
		List<Map<String, Object>> evaluationReports = new ArrayList<>();
		vo.getSubmittedProposals().stream().forEach(proposal-> {
			Map<String, Object> evaluationReport = new HashMap<>();
			setProposalDetails(evaluationReport, proposal);
			Map<String, List<ScoringReportDto>> scoringCriterias = grantCallService.getCriteriaScoreByProposalId(proposal.getProposalId(), loginPersonId, loginPersonUnitNumber, AuthenticatedUser.getLoginUserName(), Boolean.TRUE);
			BigDecimal total = BigDecimal.ZERO;
			for (Entry<String, List<ScoringReportDto>> entry : scoringCriterias.entrySet()) {
				StringBuilder comment = new StringBuilder();
				List<ScoringReportDto> scoringReports = entry.getValue();
				BigDecimal scoreTotal = (scoringReports.parallelStream().filter(scroingReport -> scroingReport.getPerson().getScore()!= null).map(ScoringReportDto::getPerson).map(ScoredPersonDTO :: getScore)).reduce(BigDecimal.ZERO, BigDecimal::add);
				Long scoredPersonCount = (scoringReports.parallelStream().filter(scroingReport -> scroingReport.getPerson().getScore()!= null)).count();
				Set<String> scoringPanel = new HashSet<>();
				scoringReports.stream().forEach(scroingReport -> setCommentAndPanelDetailsForCriteria(scroingReport, scoringPanel, comment));
				BigDecimal avg = (scoreTotal != null && scoredPersonCount!= 0 ) ? scoreTotal.divide(new BigDecimal(scoredPersonCount),2) : BigDecimal.ZERO;
				evaluationReport.put(entry.getKey(), avg);
				evaluationReport.put(entry.getKey()+ "-comment", comment);
				total = total.add(avg);
				evaluationReport.put("Scoring Panel", String.join(", ", scoringPanel));
				criterias.add(entry.getKey());
			}
			evaluationReport.put("Total", total);
			evaluationReports.add(evaluationReport);
		});
		vo.setEvaluationDatas(evaluationReports);
		setKeysForProposalDetails(keys);
		List<String> criteriaList = new ArrayList<>(criterias);
				Collections.sort(criteriaList);
		for (String criteria : criteriaList) {
			keys.add(criteria);
		}
		keys.add("Total");
		for (String criteria : criteriaList) {
			keys.add(criteria+ "-comment");
		}
		vo.setKeys(keys);
	}

	private void setCommentAndPanelDetailsForCriteria(ScoringReportDto scroingReport, Set<String> scoringPanel,
			StringBuilder comment) {
		scroingReport.getPerson().getWorkflowReviewerComments().forEach(comments -> {
			if (comments.getComment() != null) {
				comment.append(comments.getComment()).append("\n\n");
			}
		});
		if (Boolean.TRUE.equals(scroingReport.getPerson().getIsPersonCanScore())) {
			scoringPanel.add(scroingReport.getPerson().getEvaluationMapName());
		}
	}

	private List<String> setKeysForProposalDetails(List<String> keys) {
		keys.add("Proposal Id");
		keys.add("Proposal Type");
		keys.add("Title");
		keys.add("Category");
		keys.add("Lead Unit");
		keys.add("Status");
		keys.add("Primary Title of Lead PI");
		keys.add("PI");
		keys.add("Duration");
		keys.add("Total Budget");
		keys.add("Total Score");
		keys.add("Overall Average Score");
		keys.add("Adjusted Score");
		keys.add("Rank");
		keys.add("Scoring Panel");
		return keys;
	}

	private ResponseEntity<byte[]> prepareWorkbookForEvaluationReportt(EvaluationMainPanelVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet("Evaluation Report");
		commonService.addDetailsInHeader(workbook,sheet);
		XSSFCellStyle tableBodyStyle =  workbook.createCellStyle();
		int rowNumber = prepareExcelSheetHeader(sheet, vo, workbook, tableBodyStyle, 0);
		addDataToSheet(rowNumber,sheet, vo, tableBodyStyle);
		CommonVO commonVO = new CommonVO();
		commonVO.setExportType("xlsx");
		try {
			return dashboardService.getResponseEntityForDownload(commonVO, workbook);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}

	}

	private void addDataToSheet(int rowNumber, XSSFSheet sheet, EvaluationMainPanelVO vo, XSSFCellStyle tableBodyStyle) {
		for (Map<String, Object> evaluationData : vo.getEvaluationDatas()) {
			int cellNumber = 0;
			XSSFRow row = sheet.createRow(rowNumber);
		for( String key : vo.getKeys()) {
			Object objectData = evaluationData.get(key);
			XSSFCell cell = row.createCell(cellNumber);
			cell.setCellStyle(tableBodyStyle);
			if (objectData instanceof String) {
			cell.setCellValue((String)objectData);
			} else if (objectData instanceof StringBuilder) {
				cell.setCellValue(objectData.toString());
			}
			else if (objectData instanceof Integer) {
				cell.setCellValue((Integer) objectData);
			}
			else if (objectData instanceof BigInteger) {
				String stringValue = ((BigInteger) objectData).toString();
				cell.setCellValue(stringValue);
			} else if (objectData instanceof BigDecimal) {
				cell.setCellValue(((BigDecimal) objectData).doubleValue());
			}
			else if (objectData == null) {
				cell.setCellValue(" ");
			}
			cellNumber++;
		}
		rowNumber++;
		}
	}

	@SuppressWarnings("deprecation")
	private int prepareExcelSheetHeader(XSSFSheet sheet, EvaluationMainPanelVO vo, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle, int rowNumber) {
		int headingCellNumber = 0;
		int rowIndex = rowNumber;
		XSSFRow tableHeadRow = sheet.createRow(rowIndex);
		rowIndex ++;
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		tableHeadStyle.setWrapText(true);
		tableBodyStyle.setWrapText(true);
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		XSSFCellStyle scoringColor = workbook.createCellStyle();		
		scoringColor.cloneStyleFrom(tableHeadStyle);
		scoringColor.setFillForegroundColor(new XSSFColor(new java.awt.Color(255, 255, 153)));
		scoringColor.setFillPattern(FillPatternType.SOLID_FOREGROUND);
		XSSFCellStyle nonScoringColor = workbook.createCellStyle();
		nonScoringColor.cloneStyleFrom(scoringColor);
		nonScoringColor.setFillForegroundColor(new XSSFColor(new java.awt.Color(153, 153, 255)));
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		for (Object heading : vo.getKeys()) {
			XSSFCell cell = tableHeadRow.createCell(headingCellNumber);
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
			sheet.setColumnWidth(headingCellNumber, NORMAL_WIDTH);
			if(headingCellNumber >14) {
				cell.setCellStyle(scoringColor);
				sheet.setColumnWidth(headingCellNumber, CRITERIA_WIDTH);
			}
			if (vo.getScoringColorEndAt() != null && headingCellNumber > 15+vo.getScoringColorEndAt()) {
				cell.setCellStyle(nonScoringColor);
				sheet.setColumnWidth(headingCellNumber, CRITERIA_WIDTH);
			}
			headingCellNumber++;
		}
		return rowIndex;
	}

}
