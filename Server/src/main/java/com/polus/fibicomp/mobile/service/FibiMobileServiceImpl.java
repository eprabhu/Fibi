package com.polus.fibicomp.mobile.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.ip.service.InstitutionalProposalService;
import com.polus.fibicomp.login.service.LoginService;
import com.polus.fibicomp.mobile.dao.FibiMobileDao;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.view.MobileProfile;
import com.polus.fibicomp.view.MobileProposalView;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Transactional
@Configuration
@Service(value = "fibiMobileService")
public class FibiMobileServiceImpl implements FibiMobileService {

	protected static Logger logger = LogManager.getLogger(FibiMobileServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "loginService")
	private LoginService loginService;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private FibiMobileDao fibiMobileDao;

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	private InstitutionalProposalService institutionalProposalService;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private CommonService commonService;

	@Value("${spring.application.name}")
	private String context;

	@Override
	public String fibiMobileLogin(String login_mode, String userName, String password, HttpServletRequest request, HttpServletResponse response) throws Exception {
		MobileProfile mobileProfile = new MobileProfile();
		mobileProfile.setStatus(false);
		try {
			PersonDTO personDTO =  loginService.loginCheck(login_mode, userName, password, request, response);
			if(personDTO.isLogin()) {
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Logged in successfully");
				mobileProfile.setData(personDTO);
			} else {
				mobileProfile.setMessage("Invalid login");
				mobileProfile.setData(null);
			}
		} catch (Exception e) {
			logger.error("Error in method LoginServiceImpl.fibiMobileLogin", e);
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(mobileProfile);
	}

	@Override
	public String getFibiResearchSummary(CommonVO vo) throws Exception {
		MobileProfile mobileProfile = new MobileProfile();
		mobileProfile.setStatus(false);
		mobileProfile.setMessage("Failed");
		List<Object[]> summaryTable = new ArrayList<Object[]>();
		List<Object[]> summaryResponse = new ArrayList<Object[]>();
		try {
			summaryTable = fibiMobileDao.getFibiSummaryTable(vo.getPersonId(), vo.getUnitNumber(), vo.getIsAdmin(), summaryTable);
			Integer documentCount = 0;
			Integer iterator = 0;
			for (Object[] view : summaryTable) {
				documentCount = ((BigDecimal) view[1]).intValueExact();
				if (view[2] == null) {
					view[2] = 0;
				}
				summaryResponse.add(view);
				if (documentCount == 0 && view[2] == null) {
					iterator++;
				}
			}
			if (iterator < 3) {
				mobileProfile.setData(summaryResponse);
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Data found");
				logger.info("summaryResponse : " + summaryResponse);
			}
		} catch (Exception e) {
			logger.error("Error in method DashboardServiceImpl.getFibiResearchSummary", e);
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(mobileProfile);
	}

	@Override
	public String getFibiResearchSummaryData(CommonVO vo) throws Exception {
		logger.info("---------getFibiResearchSummaryData---------");
		String personId = vo.getPersonId();
		String researchSummaryIndex = vo.getResearchSummaryIndex();
		boolean isAdmin = vo.getIsAdmin();
		String unitNumber = vo.getUnitNumber();
		logger.info("personId :" + personId);
		logger.info("researchSummaryIndex :" + researchSummaryIndex);
		logger.info("isAdmin :" + isAdmin);
		logger.info("unitNumber :" + unitNumber);
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		MobileProfile mobileProfile = new MobileProfile();
		mobileProfile.setStatus(false);
		mobileProfile.setMessage("Error fetching research summary data");
		try {
			if (researchSummaryIndex.equals("PROPOSALSINPROGRESS")) {
				dashBoardProfile = fibiMobileDao.getProposalsInProgressForMobile(personId, isAdmin, unitNumber);
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Research summary details fetched successfully");
			}
			if (researchSummaryIndex.equals("PROPOSALSSUBMITTED")) {
				dashBoardProfile = fibiMobileDao.getSubmittedProposalsForMobile(personId, isAdmin, unitNumber);
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Research summary details fetched successfully");
			}
			mobileProfile.setData(dashBoardProfile);
		} catch (Exception e) {
			logger.error("Error in method getFibiResearchSummaryData", e);
		}
		return commonDao.convertObjectToJSON(mobileProfile);
	}

	@Override
	public String getProposalsForCertification(String personId) {
		logger.info("---------getProposalsForCertification---------");
		MobileProfile mobileProfile = new MobileProfile();
		mobileProfile.setStatus(false);
		mobileProfile.setMessage("Error fetching certification data");
		List<MobileProposalView> mobileProposalViews = fibiMobileDao.getProposalsForCertification(personId);
		if (mobileProposalViews != null && !mobileProposalViews.isEmpty()) {
			mobileProfile.setData(mobileProposalViews);
			mobileProfile.setStatus(true);
			mobileProfile.setMessage("Datas retrived sucessfully");
		}
		return commonDao.convertObjectToJSON(mobileProfile);
	}

	@Override
	public String getProposals(CommonVO vo) throws Exception {
		logger.info("---------getProposals---------");
		String actionFlag = vo.getActionFlag();
		String personId = vo.getPersonId();
		boolean isAdmin = vo.getIsAdmin();
		logger.info("actionFlag :" + actionFlag);
		logger.info("personId :" + personId);
		logger.info("isAdmin :" + isAdmin);
		MobileProfile mobileProfile = new MobileProfile();
		mobileProfile.setStatus(false);
		mobileProfile.setMessage("Error fetching proposals");
		try {
			DashBoardProfile dashBoardProfile = new DashBoardProfile();
			if (actionFlag.equals("APPROVAL_PENDING_PROPOSALS")) {
				List<Integer> proposalIds = fibiMobileDao.getApprovalInprogressProposalIdsForMobile(personId,
						Constants.WORKFLOW_STATUS_CODE_WAITING, Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL);
				if (proposalIds != null && !proposalIds.isEmpty()) {
					dashBoardProfile = fibiMobileDao.getDashBoardDataOfProposalsForMobile(proposalIds);
				} else {
					dashBoardProfile.setProposal(new ArrayList<>());
				}
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Approval pending proposals fetched successfully");
				mobileProfile.setData(dashBoardProfile);
			} else if (actionFlag.equals("C")) {
				/*
				 * List<Integer> proposalIds =
				 * dashboardDao.getCertificationInCompleteProposalIds(personId); if (proposalIds
				 * != null && !proposalIds.isEmpty()) { dashBoardProfile =
				 * dashboardDao.getDashBoardDataOfUncertifiedProposals(proposalIds); } else {
				 * dashBoardProfile.setProposal(new ArrayList<>()); }
				 */
				dashBoardProfile = fibiMobileDao.getProposalsInProgressForMobile(personId, isAdmin, null);
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Certification proposals fetched successfully");
				mobileProfile.setData(dashBoardProfile);
			} else if (actionFlag.equals("FYI")) {
				List<Integer> proposalIds = fibiMobileDao.getFYIProposalIds(personId, Constants.MODULE_CODE_DEVELOPMENT_PROPOSAL);
				if (proposalIds != null && !proposalIds.isEmpty()) {
					dashBoardProfile = fibiMobileDao.getDashBoardDataOfProposalsForMobile(proposalIds);
				} else {
					dashBoardProfile.setProposal(new ArrayList<>());
				}
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("FYI proposals fetched successfully");
				mobileProfile.setData(dashBoardProfile);
			} else if (actionFlag.equals("MY_PROPOSALS")) {
				dashBoardProfile = fibiMobileDao.getMobileDashBoardDataForMyProposal(vo);
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("My proposals fetched successfully");
				mobileProfile.setData(dashBoardProfile);
			}
		} catch (Exception e) {
			logger.error("Error in method getProposals", e);
		}
		return commonDao.convertObjectToJSON(mobileProfile);
	}

	@Override
	public String approveOrRejectProposalForMobile(String formDataJSON) {
		ProposalVO proposalVO = null;
		String statusMessage = "";
		try {
			ObjectMapper mapper = new ObjectMapper();
			proposalVO = mapper.readValue(formDataJSON, ProposalVO.class);
			Proposal proposal = proposalDao.fetchProposalById(proposalVO.getProposalId());
			String actionType = proposalVO.getActionType();
			String approverComment = proposalVO.getApproveComment();
			boolean isFinalApprover = true;

			logger.info("actionType : " + actionType);
			logger.info("personId : " + proposalVO.getPersonId());
			logger.info("approverComment : " + approverComment);

			Workflow workflow = new Workflow();
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			for (WorkflowDetail workflowDetail1 : workflowDetails) {
				if (workflowDetail1.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
					isFinalApprover = false;
				}
			}
			Set<NotificationRecipient> dynamicEmailrecipients = null;
			if (isFinalApprover && actionType.equals("A")) {
				String ipNumber = institutionalProposalService.generateInstitutionalProposalNumber();
				logger.info("Initial IP Number : " + ipNumber);
				boolean isIPCreated = institutionalProposalService.createInstitutionalProposal(proposal.getProposalId(), ipNumber, proposal.getUpdateUser());
				logger.info("isIPCreated : " + isIPCreated);
				if (isIPCreated) {
					logger.info("Generated IP Number : " + ipNumber);
					proposal.setIpNumber(ipNumber);
					proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED);
					proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_AWARDED));
					String fyiRecipients = commonDao.getParameterValueAsString(Constants.EMAIL_NOTIFICATION_TEST_ADDRESS);
					if (fyiRecipients != null && !fyiRecipients.isEmpty()) {
						dynamicEmailrecipients = new HashSet<>();
						List<String> recipients = Arrays.asList(fyiRecipients.split(","));
						for (String recipient : recipients) {
							commonService.setNotificationRecipients(recipient, Constants.NOTIFICATION_RECIPIENT_TYPE_TO,
									dynamicEmailrecipients);
						}
					}
				}
				proposal = proposalDao.saveOrUpdateProposal(proposal);
				statusMessage = "Approved proposal successfully";
				proposalService.sendProposalNotification(proposalVO, Constants.PROPOSAL_APPROVE_NOTIFICATION_CODE,
						dynamicEmailrecipients);
			} else if (actionType.equals("R")) {
				proposal.setStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED);
				proposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.PROPOSAL_STATUS_CODE_RETURNED));
				proposal = proposalDao.saveOrUpdateProposal(proposal);
				statusMessage = "Rejected proposal successfully";
				dynamicEmailrecipients = new HashSet<>();
				String piEmailAddress = getPIEmailAddress(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalVO.getProposalId()));
				commonService.setNotificationRecipients(piEmailAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_TO,
						dynamicEmailrecipients);
				proposalService.sendProposalNotification(proposalVO, Constants.NOTIFICATION_PROPOSAL_REJECTED,
						dynamicEmailrecipients);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return statusMessage;
	}

	@Override
	public String loadProposalByIdForMobile(Integer proposalId, String personId) {
		Proposal proposalData = new Proposal();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		if (proposal != null) {
			proposalData.setProposalId(proposal.getProposalId());
			proposalData.setTitle(proposal.getTitle());
			proposalData.setSponsorName(proposal.getSponsorName());
			proposalData.setSponsorCode(proposal.getSponsorCode());
			proposalData.setHomeUnitName(proposal.getHomeUnitName());
			proposalData.setHomeUnitNumber(proposal.getHomeUnitNumber());
			proposalData.setStartDate(proposal.getStartDate());
			proposalData.setEndDate(proposal.getEndDate());
			proposalData.setActivityType(proposal.getActivityType());
			proposalData.setInternalDeadLineDate(proposal.getInternalDeadLineDate());
			if (proposal.getProposalType() != null) {
				proposalData.setApplicationType(proposal.getProposalType().getDescription());
			}
		}
		return commonDao.convertObjectToJSON(proposalData);
	}

	private String getPIEmailAddress(List<ProposalPerson> proposalPersons) {
		String emailAddress = "";
		for (ProposalPerson person : proposalPersons) {
			if (person.getProposalPersonRole().getCode().equals(Constants.PRINCIPAL_INVESTIGATOR)) {
				emailAddress = person.getEmailAddress();
			}
		}
		return emailAddress;
	}

}
