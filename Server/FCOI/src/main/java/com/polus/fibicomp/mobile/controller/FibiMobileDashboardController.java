package com.polus.fibicomp.mobile.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.mobile.service.FibiMobileService;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.view.MobileProfile;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class FibiMobileDashboardController {

	protected static Logger logger = LogManager.getLogger(FibiMobileDashboardController.class.getName());

	@Autowired
	@Qualifier(value = "dashboardService")
	private DashboardService dashboardService;

	@Autowired
	@Qualifier(value = "proposalService")
	private ProposalService proposalService;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	@Qualifier(value = "fibiMobileService")
	private FibiMobileService fibiMobileService;

	@Value("${LOGIN_MODE}")
	private String login_mode;

	@RequestMapping(value = "/fibiLogin", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fibiMobileLogin(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.debug("Received request for Mobile login: ");
		String userName = vo.getUserName();
		String password = vo.getPassword();
		return fibiMobileService.fibiMobileLogin(login_mode, userName, password, request, response);
	}

	@RequestMapping(value = "/getFibiMobileSummary", method = RequestMethod.POST)
	public String getFibiMobileSummary(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getFibiMobileSummary");
		return fibiMobileService.getFibiResearchSummary(vo);
	}

	@RequestMapping(value = "/getFibiResearchSummary", method = RequestMethod.POST)
	public String getFibiResearchSummary(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getFibiResearchSummary");
		return fibiMobileService.getFibiResearchSummaryData(vo);
	}

	@RequestMapping(value = "/getProposalsForCertification", method = RequestMethod.POST)
	public String getProposalsForCertification(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getProposalsForCertification");
		return fibiMobileService.getProposalsForCertification(vo.getPersonId());
	}

	@RequestMapping(value = "/getProposals", method = RequestMethod.POST)
	public String getProposals(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getProposals");
		return fibiMobileService.getProposals(vo);
	}

	@RequestMapping(value = "/approveOrRejectProposalForMobile", method = RequestMethod.POST)
	public String approveOrRejectProposal(@RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for approveOrRejectProposalForMobile");
		return fibiMobileService.approveOrRejectProposalForMobile(formDataJson);
	}

	@RequestMapping(value = "/loadProposalByIdForMobile", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadProposalByIdForMobile(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadProposalByIdForMobile");
		logger.info("proposalId : " + vo.getProposalId());
		logger.info("personId : " + vo.getPersonId());
		return fibiMobileService.loadProposalByIdForMobile(vo.getProposalId(), vo.getPersonId());
	}

	@RequestMapping(value = "/fibiLogout", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fibiMobileLogout(@RequestBody CommonVO vo, ModelMap model, HttpServletRequest request, HttpServletResponse response) throws Exception {
		MobileProfile mobileProfile = new MobileProfile();
		mobileProfile.setStatus(false);
		try {
			String status = logout(model, request, response);
			if("SUCCESS".equalsIgnoreCase(status)) {
				mobileProfile.setStatus(true);
				mobileProfile.setMessage("Logged out successfully");
			} else {
				mobileProfile.setMessage("Logout failed");
			}
		} catch (Exception e) {
			logger.error("Error in method LoginController.fibiMobileLogout", e);
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(mobileProfile);
	}

	public String logout(ModelMap model, HttpServletRequest request, HttpServletResponse response) {
		logger.debug("Log Out");
		HttpSession session = request.getSession(false);
		if (session != null) {
			session.invalidate();
		}
		return "SUCCESS";
	}

}
