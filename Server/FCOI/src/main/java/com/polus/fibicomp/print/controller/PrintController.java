package com.polus.fibicomp.print.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.polus.fibicomp.common.dto.ResponseData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.print.service.ProgressReportPrintService;
import com.polus.fibicomp.print.vo.PrintVO;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class PrintController {

	protected static Logger logger = LogManager.getLogger(PrintController.class.getName());

	@Autowired
	private PrintService printService;

	@Autowired
	private ProgressReportPrintService progressReportPrintService;

	@PostMapping(value = "/generateProposalReport")
	public void generateProposalReport(HttpServletResponse response, @RequestBody @Valid PrintVO printVO) throws Exception {
		logger.info("Requesting for generateProposalReport");
		printService.generateProposalReport(response, printVO);
	}

	@PostMapping(value = "/generateBudgetReport")
	public ResponseEntity<byte[]> generateBudgetReport(HttpServletResponse response, @RequestBody PrintVO vo) throws Exception {
		logger.info("Requesting for generateBudgetReport");
		logger.info("proposalId : {} ", vo.getProposalId());
		logger.info("budgetId : {} ", vo.getBudgetId());
		logger.info("isBudgetSummaryPrint : {} ", vo.getIsBudgetSummaryPrint());
		logger.info("isSimpleBudgetEnabled : {} ", vo.getIsSimpleBudgetPrint());
		logger.info("isDetailedBudgetEnabled : {} ", vo.getIsDetailedBudgetPrint());
		return printService.generateBudgetReport(response, vo);
	}

	@GetMapping(value = "/generateAwardReport")
	public ResponseEntity<byte[]> printAwardReport(HttpServletResponse response, @RequestHeader(value = "awardId", required = true) Integer awardId, @RequestHeader(value = "personId", required = true) String personId) throws Exception {
		logger.info("Requesting for generateAwardReport");
		logger.info("awardId : {}", awardId);
		logger.info("personId : {}", personId);
		return printService.generateAwardReport(response, awardId, personId);
	}

	@GetMapping(value = "/generateNotifyAwardReport")
	public ResponseEntity<byte[]> generateNotifyAwardReport(HttpServletResponse response, @RequestHeader(value = "awardId", required = true) Integer awardId, @RequestHeader(value = "personId", required = true) String personId) throws Exception {
		logger.info("Requesting for generateNotifyAwardReport");
		logger.info("awardId : {}", awardId);
		logger.info("personId : {}", personId);
		return printService.generateNotifyAwardReport(response, awardId);
	}

	@PostMapping(value = "/generateQuestionnaireReport")
	public ResponseEntity<byte[]> generateQuestionnaireReport(HttpServletRequest request,HttpServletResponse response, @RequestBody CommonVO vo) throws Exception {
		logger.info("Requesting for generateQuestionnaireReport");
		logger.info("moduleCode : {}", vo.getModuleCode());
		logger.info("subModuleCode : {}", vo.getSubModuleCode());
		logger.info("moduleItemKey : {}", vo.getModuleItemKey());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("userName : {}", vo.getUserName());
		logger.info("questionnaireId : {}", vo.getQuestionnaireId());
		logger.info("isSingleQuestionnairePrint : {}", vo.getIsSingleQuestionnairePrint());
		logger.info("exportType : {}", vo.getExportType());
		if (vo.getExportType().equals("xlsx")) {
			return printService.generateQuestionnaireReport(vo);
		}
		return printService.generateQuestionnaireReport(response,vo);
	}

	@PostMapping(value = "/generateAwardBudgetReport")
	public ResponseEntity<byte[]> generateAwardBudgetReport(HttpServletResponse response, @RequestBody PrintVO vo) throws Exception {
		logger.info("Requesting for generateAwardBudgetReport");
		logger.info("awardId : {}", vo.getAwardId());
		logger.info("budgetId : {}", vo.getBudgetId());
		return printService.generateAwardBudgetReport(response, vo);
	}

	@GetMapping(value = "/generateNotifyAwardReports")
	public ResponseEntity<byte[]> generateNotifyAwardReportForNTU(HttpServletResponse response, @RequestHeader(value = "awardId", required = true) Integer awardId, @RequestHeader(value = "awardNumber", required = true) String awardNumber, @RequestHeader(value = "sequenceNumber", required = true) Integer sequenceNumber, @RequestHeader(value = "updateUser", required = true) String updateUser, @RequestHeader(value = "personId", required = true) String personId) throws Exception {
		logger.info("Requesting for generateNotifyAwardReports");
		logger.info("awardId : {}" , awardId);
		logger.info("personId : {}", personId);
		return printService.generateNotifyAwardReports(response, awardId, awardNumber, sequenceNumber, updateUser);
	}

	@GetMapping(value = "/generateBudgetSummaryExcelReport")
	public ResponseEntity<byte[]> generateBudgetSummaryExcelReport(HttpServletResponse response, @RequestHeader(value = "proposalId", required = true) Integer proposalId, @RequestHeader(value = "budgetId", required = true) Integer budgetId) throws Exception {
		logger.info("Requesting for generateBudgetSummaryExcelReport");
		logger.info("proposalId : {} ", proposalId);
		logger.info("budgetId : {} ", budgetId);
		return printService.generateBudgetSummaryExcelReport(response, proposalId, budgetId);
	}

	@GetMapping(value = "/generateProposalDetailedBudgetExcelReport")
	public ResponseEntity<byte[]> exportProposalDetailedBudget(HttpServletResponse response, @RequestHeader(value = "budgetId", required = true) Integer budgetId) throws Exception {
		logger.info("Requesting for exportProposalDetailedBudget");
		logger.info("budgetId : {} ", budgetId);
		return printService.generateProposalDetailedBudgetExcelReport(response, budgetId);
	}

	@GetMapping(value = "/generateProposalSimpleBudgetExcelReport")
	public ResponseEntity<byte[]> generateProposalSimpleBudgetExcelReport(HttpServletResponse response, @RequestHeader(value = "budgetId", required = true) Integer budgetId) throws Exception {
		logger.info("Requesting for generateProposalSimpleBudgetExcelReport");
		logger.info("budgetId : {} ", budgetId);
		return printService.generateProposalSimpleBudgetExcelReport(response, budgetId);
	}

	@GetMapping(value = "/generateAwardDetailedBudgetExcelReport")
	public ResponseEntity<byte[]> generateAwardDetailedBudgetExcelReport(HttpServletResponse response, @RequestHeader(value = "budgetId", required = true) Integer budgetId) throws Exception {
		logger.info("Requesting for generateAwardDetailedBudgetExcelReport");
		logger.info("budgetId : {} ", budgetId);
		return printService.generateAwardDetailedBudgetExcelReport(response, budgetId);
	}

	@GetMapping(value = "/generateAwardBudgetSummaryExcelReport")
	public ResponseEntity<byte[]> generateAwardBudgetSummaryExcelReport(HttpServletResponse response, @RequestHeader(value = "awardId", required = true) Integer awardId, @RequestHeader(value = "budgetId", required = true) Integer budgetId) throws Exception {
		logger.info("Requesting for generateAwardBudgetSummaryExcelReport");
		logger.info("awardId : {} ", awardId);
		logger.info("budgetId : {} ", budgetId);
		return printService.generateAwardBudgetSummaryExcelReport(response, awardId, budgetId);
	}

	@GetMapping(value = "/exportCurrentAndPending")
	public ResponseEntity<byte[]> exportCurrentAndPending(HttpServletResponse response, @RequestHeader(value = "moduleCode", required = true) Integer moduleCode, @RequestHeader(value = "personId", required = false) String personId, @RequestHeader(value = "moduleItemKey", required = true) String moduleItemKey) throws Exception {
		logger.info("Requesting for exportCurrentAndPending");
		logger.info("Module Code : {}" , moduleCode);
		logger.info("personId : {}", personId);
		logger.info("ModuleItemKey : {} ", moduleItemKey);
		return printService.exportCurrentAndPending(response, moduleCode, personId, moduleItemKey);
	}

	@PostMapping(value = "/generateAwardExpenseExcelReport")
	public ResponseEntity<byte[]> generateRevenueExcelReport(HttpServletResponse response, @RequestBody PrintVO vo) throws Exception {
		logger.info("Requesting for generateAwardExpenseExcelReport");
		return printService.generateAwardExpenseExcelReport(vo);
	}

	@PostMapping(value = "/generateAwardExpensePDFReport")
	public ResponseEntity<byte[]> generateRevenuePDFReport(HttpServletResponse response, @RequestBody PrintVO vo) throws Exception {
		logger.info("Requesting for generateAwardExpenseExcelReport");
		return printService.generateAwardExpensePDFReport(response, vo);
	}

	@GetMapping(value = "/generateProgressReport/{progressReportId}")
	public ResponseEntity<byte[]> generateProgressReport(HttpServletResponse response, @PathVariable(value = "progressReportId", required = true) final Integer progressReportId) throws Exception {
		logger.info("Requesting for generateProgressReport");
		logger.info("progressReportId : {} ", progressReportId);
		return progressReportPrintService.generateProgressReport(response, progressReportId);
	}

	@GetMapping(value = "/printEntireProgressReport")
	public void printEntireProgressReport(HttpServletResponse response, @RequestHeader(value = "progressReportId", required = true) final Integer progressReportId, @RequestHeader(value = "awardId", required = true) final Integer awardId, @RequestHeader(value = "awardLeadUnitNumber", required = true) final String awardLeadUnitNumber) {
		logger.info("Requesting for downloadProgressReport");
		logger.info("progressReportId : {} ", progressReportId);
		progressReportPrintService.printEntireProgressReport(progressReportId, awardId, awardLeadUnitNumber, response);
	}

	@PostMapping(value = "/generateAwardKeyPersonTimesheetReport")
	public ResponseEntity<byte[]> generateAwardKeyPersonTimesheetReport(HttpServletResponse response, HttpServletRequest request, @RequestBody CommonVO vo) throws Exception {
		logger.info("Requesting for generateAwardKeyPersonTimesheetReport");
		return printService.generateAwardKeyPersonTimesheetReport(response, vo);
	}

	@GetMapping( "/letterTemplate/{moduleCode}")
	public ResponseEntity<ResponseData> getAllLetterTemplateTypes(@PathVariable("moduleCode") Integer moduleCode) {
		return printService.getAllLetterTemplateTypes(moduleCode);
	}

	@PostMapping(value = "/generateIpReport")
	public void generateIpReport(HttpServletResponse response, @RequestBody @Valid PrintVO printVO) {
		logger.info("Requesting for generateIpReport");
		printService.generateIpReport(response, printVO);
	}
}
