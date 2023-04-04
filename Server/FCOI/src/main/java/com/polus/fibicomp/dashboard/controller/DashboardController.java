package com.polus.fibicomp.dashboard.controller;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.dashboard.vo.AgreementDashboardVO;
import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;
import com.polus.fibicomp.dashboard.vo.ClaimDashboardVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.dashboard.vo.GrantCallDashboardVO;
import com.polus.fibicomp.dashboard.vo.InstituteProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.NegotiationDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProgressReportDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.ServiceRequestDashboardVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class DashboardController {

	protected static Logger logger = LogManager.getLogger(DashboardController.class.getName());

	@Autowired
	@Qualifier(value = "dashboardService")
	private DashboardService dashboardService;

	@PostMapping(value = "/fibiDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoad(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiDashBoard");
		return dashboardService.getDashBoardData(vo);
	}

	@PostMapping(value = "/exportDashboardDatas")
	public ResponseEntity<byte[]> exportDashboardData(HttpServletRequest request, @RequestBody CommonVO vo) throws Exception {
		logger.info("Requesting for exportDashboardDatas");
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForDashboard(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/fetchRequiredParams")
	public String fetchRequiredParams(HttpServletRequest request) throws Exception {
		logger.info("Requesting for fetchRequiredParams");
		return dashboardService.fetchRequiredParams();
	}

	@PostMapping(value = "/fetchEvaluationStop", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchEvaluationStop(@RequestBody CommonVO vo, HttpServletRequest request) {
		logger.info("Requesting for fetchEvaluationStop");
		return dashboardService.fetchEvaluationStop(vo);
	}

	@PostMapping(value = "/fibiAgreementDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoadAgreement(@RequestBody AgreementDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiAgreementDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getAgreementDashBoardData(vo);
	}

	@PostMapping(value = "/fibiNegotiationDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoadNegotiation(@RequestBody NegotiationDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiNegotiationDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getNegotaiationDashBoardData(vo);
	}

	@PostMapping(value = "/fibiGrantCallDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoadGrantCall(@Valid @RequestBody GrantCallDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiGrantCallDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getGrantCallDashBoardData(vo);
	}

	@PostMapping(value = "/fibiInstituteProposalDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoadInstituteProposal(@RequestBody InstituteProposalDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiInstituteProposalDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getInstituteProposalDashBoardData(vo);
	}

	@PostMapping(value = "/fibiAwardDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoadAward(@Valid @RequestBody AwardDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiAwardDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getAwardDashBoardData(vo);
	}

	@PostMapping(value = "/exportAwardDashboardDatas")
	public ResponseEntity<byte[]> exportAwardDashboardData(HttpServletRequest request, @Valid @RequestBody AwardDashboardVO vo) throws Exception {
		logger.info("Requesting for exportAwardDashboardDatas");	
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForAwardDashboard(vo);
		return dashboardService.getResponseEntityForAwardDownload(vo, workbook);
	}

	@PostMapping(value = "/fibiProposalDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String requestInitialLoadProposal(@Valid @RequestBody ProposalDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for fibiProposalDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getProposalDashBoardData(vo);
	}

	@PostMapping(value = "/exportProposalDashboardDatas")
	public ResponseEntity<byte[]> exportProposalDashboardData(HttpServletRequest request, @Valid @RequestBody ProposalDashboardVO vo) throws Exception {
		logger.info("Requesting for exportProposalDashboardDatas");	
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForProposalDashboard(vo);
		return dashboardService.getResponseEntityForProposalDownload(vo, workbook);
	}

	@PostMapping(value = "/loadServiceRequestDashBoard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadServiceRequestDashBoard(@Valid @RequestBody ServiceRequestDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for loadServiceRequestDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.loadServiceRequestDashBoard(vo);
	}

	@PostMapping(value = "/exportServiceRequestDashBoard")
	public ResponseEntity<byte[]> exportServiceRequestDashBoard(HttpServletRequest request, @Valid @RequestBody ServiceRequestDashboardVO vo) throws Exception {
		logger.info("Requesting for exportServiceRequestDashBoard");	
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookServiceRequestDashBoard(vo);
		return dashboardService.getResponseEntityServiceRequestDashBoard(vo, workbook);
	}

	@PostMapping(value = "/exportInstiuteProposalDashboardDatas")
	public ResponseEntity<byte[]> exportInstiuteProposalDashboardDatas(HttpServletRequest request, @RequestBody InstituteProposalDashboardVO vo) throws Exception {
		logger.info("Requesting for exportInstiuteProposalDashboardDatas");	
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForInstituteProposalDashboard(vo);
		CommonVO commonVo = new CommonVO();
		commonVo.setExportType(vo.getExportType());
		commonVo.setDocumentHeading(vo.getDocumentHeading());
		return dashboardService.getResponseEntityForDownload(commonVo, workbook);
	}

	@PostMapping(value = "/canDeleteGrantCall", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String canDeleteGrantCall(@RequestBody GrantCallDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for canDeleteGrantCall");
		return dashboardService.canDeleteGrantCall(vo);
	}
	
	@PostMapping(value = "/fibiClaimDashBoard")
	public String fibiClaimDashBoard(@RequestBody ClaimDashboardVO vo, HttpServletRequest request) {
		logger.info("Requesting for fibiClaimDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return dashboardService.getClaimDashBoardData(vo);
	}

	@PostMapping(value = "/fibiProgressReportDashBoard")
	public String fibiProgressReportDashBoard(@Valid @RequestBody ProgressReportDashboardVO vo, HttpServletRequest request) {
		logger.info("Requesting for fibiProgressReportDashBoard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return dashboardService.fibiProgressReportDashBoard(vo);
	}
	
	@PostMapping(value = "/exportGrantCallDashboardData")
	public ResponseEntity<byte[]> exportGrantCallDashboardData(@Valid @RequestBody GrantCallDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportGrantCallDashboardData");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForGrantCallDashboard(vo);
		CommonVO commonVo = new CommonVO();
		commonVo.setExportType(vo.getExportType());
		commonVo.setDocumentHeading(vo.getDocumentHeading());
		return dashboardService.getResponseEntityForDownload(commonVo, workbook);
	}

	@PostMapping(value = "/getAgreementBasedOnCategory", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAgreementBasedOnCategory(@Valid @RequestBody AgreementDashboardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getAgreementBasedOnCategory");	
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return dashboardService.getAgreementBasedOnCategory(vo);
	}

	@PostMapping(value = "/exportAgreementDashboardDatas")
	public ResponseEntity<byte[]> exportAgreementDashboardData(HttpServletRequest request, @Valid @RequestBody AgreementDashboardVO vo) throws Exception {
		logger.info("Requesting for exportAgreementDashboardData");
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForAgreementDashboard(vo);
		CommonVO commonVo = new CommonVO();
		commonVo.setExportType(vo.getExportType());
		commonVo.setDocumentHeading(vo.getDocumentHeading());
		return dashboardService.getResponseEntityForDownload(commonVo, workbook);
	}

	@PostMapping(value = "/exportAgreementBasedOnCategory")
	public ResponseEntity<byte[]> exportAgreementBasedOnCategory(HttpServletRequest request, @Valid @RequestBody AgreementDashboardVO vo) throws Exception {
		logger.info("Requesting for exportAgreementBasedOnCategory");
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForAgreementCategoryDashboard(vo);
		CommonVO commonVo = new CommonVO();
		commonVo.setExportType(vo.getExportType());
		commonVo.setDocumentHeading(vo.getExportHeading());
		return dashboardService.getResponseEntityForDownload(commonVo, workbook);
	}

}
