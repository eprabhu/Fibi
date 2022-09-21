package com.polus.fibicomp.dashboard.controller;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.dashboard.service.ResearchSummaryService;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class ResearchSummaryController {

	protected static Logger logger = LogManager.getLogger(ResearchSummaryController.class.getName());

	@Autowired
	@Qualifier(value = "researchSummaryService")
	private ResearchSummaryService researchSummaryService;

	@Autowired
	private DashboardService dashboardService;

	private static final String PERSON_ID = "personId : {}";
	private static final String UNIT_NUMBER = "unitNumber : {}";

	@PostMapping(value = "/getExpenditureVolumeChart")
	public String getExpenditureVolumeChart(@RequestBody CommonVO vo) {
		logger.info("Requesting for getExpenditureVolumeChart");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getExpenditureVolumeChart(vo);
	}

	@PostMapping(value = "/getSummaryAwardPieChart")
	public String getSummaryAwardPieChart(@RequestBody CommonVO vo) {
		logger.info("Requesting for getSummaryAwardPieChart");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getSummaryAwardPieChart(vo);
	}

	@PostMapping(value = "/getSummaryProposalPieChart")
	public String getSummaryProposalPieChart(@RequestBody CommonVO vo) {
		logger.info("Requesting for getSummaryProposalPieChart");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getSummaryProposalPieChart(vo);
	}

	@PostMapping(value = "/getSummaryInProgressProposalDonutChart")
	public String getSummaryInProgressProposalDonutChart(@RequestBody CommonVO vo) {
		logger.info("Requesting for getSummaryInProgressProposalDonutChart");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getSummaryInProgressProposalDonutChart(vo);
	}

	@PostMapping(value = "/getSummaryAwardedProposalDonutChart")
	public String getSummaryAwardedProposalDonutChart(@RequestBody CommonVO vo) {
		logger.info("Requesting for getSummaryAwardedProposalDonutChart");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getSummaryAwardedProposalDonutChart(vo);
	}

	@PostMapping(value = "/getPieChartDataByType")
	public String requestPieChartDataBySponsorTypes(@RequestBody CommonVO vo, HttpServletRequest request) {
		logger.info("Requesting for getPieChartDataByType");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info("sponsorCode : {}", vo.getSponsorCode());
		logger.info("pieChartIndex : {}", vo.getPieChartIndex());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getPieChartDataByType(vo);
	}

	@PostMapping(value = "/getDetailedResearchSummary")
	public String requestDetailedResearchSummary(@RequestBody CommonVO vo, HttpServletRequest request) {
		logger.info("Requesting for getDetailedResearchSummary");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info("researchSummaryIndex : {}", vo.getResearchSummaryIndex());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getDetailedSummaryData(vo);
	}

	@PostMapping(value = "/getDonutChartDataBySponsor")
	public String requestDonutChartDataBySponsor(@RequestBody CommonVO vo, HttpServletRequest request) {
		logger.info("Requesting for getDonutChartDataBySponsor");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info("sponsorCode : {}", vo.getSponsorCode());
		logger.info("donutChartIndex : {}", vo.getDonutChartIndex());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getDonutChartDataBySponsor(vo);
	}

	@PostMapping(value = "/exportResearchSummaryDatas")
	public ResponseEntity<byte[]> exportResearchSummaryDatas(HttpServletRequest request, @RequestBody CommonVO vo) throws Exception {
		logger.info("Requesting for exportResearchSummaryDatas");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());;
		logger.info("dashboardIndex : {}", vo.getResearchSummaryIndex());
		logger.info("unitNumber : {}", vo.getUnitNumber());
		return researchSummaryService.getXSSFWorkbookForResearchSummary(vo);
	}

	@PostMapping(value = "/getResearchSummaryTable")
	public String getResearchSummaryTable(@RequestBody CommonVO vo, HttpServletRequest request) {
		logger.info("Requesting for getResearchSummaryTable");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(UNIT_NUMBER, vo.getUnitNumber());
		return researchSummaryService.getResearchSummaryTable(vo);
	}

	@GetMapping(value = "/getAllQuickLinksOrEvents")
	public String getAllQuickLinks() {
		logger.info("Requesting for getAllQuickLinksOrEvents");
		return researchSummaryService.getAllQuickLinksOrEvents();
	}

	@GetMapping(value = "/getWidgetLookups")
	public String getWidgetLookups() {
		logger.info("Requesting for getWidgetLookups");
		return researchSummaryService.getWidgetLookups();
	}

	@PostMapping(value = "/saveUserSelectedWidget")
	public String saveUserSelectedWidget(@RequestBody DashBoardProfile dashBoardProfile) {
		logger.info("Requesting for getWidgetLookups");
		logger.info("personId : {}", AuthenticatedUser.getLoginPersonId());
		return researchSummaryService.saveUserSelectedWidget(dashBoardProfile);
	}

	@DeleteMapping(value = "/deleteUserSelectedWidget")
	public String deleteUserSelectedWidget(@RequestHeader(value = "selectedWidgetId", required = true) Integer selectedWidgetId) {
		logger.info("Requesting for deleteUserSelectedWidget");
		logger.info("personId : {}", AuthenticatedUser.getLoginPersonId());
		logger.info("selectedWidgetId : {}" , selectedWidgetId);
		return researchSummaryService.deleteUserSelectedWidget(selectedWidgetId);
	}

	@PostMapping(value = "/updateWidgetSortOrder")
	public String updateWidgetSortOrder(@RequestBody DashBoardProfile dashBoardProfile) {
		logger.info("Requesting for updateWidgetSortOrder");
		logger.info("personId : {}", AuthenticatedUser.getLoginPersonId());
		return researchSummaryService.updateWidgetSortOrder(dashBoardProfile);
	}

	@PostMapping(value = "/getResearchSummaryDatasByWidget")
	public String getResearchSummaryDatasByWidget(@RequestBody CommonVO commonVO) {
		logger.info("Requesting for getResearchSummaryDatasByWidget");
		commonVO.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("personId : {}", commonVO.getPersonId());
		return researchSummaryService.getResearchSummaryDatasByWidget(commonVO);
	}

	@PostMapping(value = "/getDetailedViewOfWidget")
	public String getDetailedViewOfWidget(@RequestBody CommonVO commonVO) {
		logger.info("Requesting for getDetailedViewOfWidget");
		commonVO.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("personId : {}", commonVO.getPersonId());
		return researchSummaryService.getDetailedViewOfWidget(commonVO);
	}

	@PostMapping(value = "/getAgreementSummary")
	public String getAgreementSummary(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getAgreementSummary");
		return researchSummaryService.getAgreementSummary(vo);
	}
	
	@GetMapping(value = "/getUnitWithRights/{personId}")
	public String getUnitWithRights(@PathVariable(value = "personId", required = true) final String personId) {
		logger.info("Requesting for getUnitWithRights");
		logger.info("PersonId : {}", personId);
		return researchSummaryService.getUnitWithRights(personId);
	}

}
