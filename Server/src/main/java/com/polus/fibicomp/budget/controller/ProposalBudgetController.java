package com.polus.fibicomp.budget.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.budget.service.BudgetModularService;
import com.polus.fibicomp.budget.service.BudgetPersonService;
import com.polus.fibicomp.budget.service.BudgetService;
import com.polus.fibicomp.budget.vo.BudgetModularVO;
import com.polus.fibicomp.budget.vo.BudgetVO;

@RestController
public class ProposalBudgetController {

	protected static Logger logger = LogManager.getLogger(ProposalBudgetController.class.getName());

	@Autowired
	@Qualifier(value = "budgetService")
	private BudgetService budgetService;

	@Autowired
	@Qualifier(value = "budgetPersonService")
	private BudgetPersonService budgetPersonService;

	@Autowired
	@Qualifier(value = "budgetModularService")
	private BudgetModularService budgetModularService;

	private static final String PROPOSAL_ID = "proposalId : {}";

	private static final String BUDGET_HEADER_ID = "budgetHeaderId : {}";

	private static final String BUDGET_PERIOD_ID = "budgetPeriodId : {}";

	private static final String CURRENT_PERIOD_ID = "currentPeriodId : {}";

	private static final String BUDGET_ID = "budgetId : {}";

	@PostMapping(value = "/addBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addBudgetPeriod(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addBudgetPeriod");
		return budgetService.addBudgetPeriod(vo);
	}

	@PostMapping(value = "/resetBudgetRates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getBudgetRates(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for resetBudgetRates");
		return budgetService.resetProposalRates(vo);
	}

	@PostMapping(value = "/getSyncBudgetRates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getSyncBudgetRates(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getSyncBudgetRates");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		return budgetService.getSyncBudgetRates(vo);
	}

	@PostMapping(value = "/autoCalculate", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String autoCalculate(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for autoCalculate");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info(BUDGET_HEADER_ID, vo.getBudgetId());
		return budgetService.autoCalculate(vo);
	}

	@PostMapping(value = "/deleteBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteBudgetPeriod(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteBudgetPeriod");
		logger.info(BUDGET_PERIOD_ID, vo.getBudgetPeriodId());
		return budgetService.deleteBudgetPeriod(vo);
	}

	@PostMapping(value = "/deleteBudgetLineItem", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteBudgetLineItem(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteBudgetLineItem");
		logger.info(BUDGET_PERIOD_ID, vo.getBudgetPeriodId());
		logger.info("budgetDetailId : {}", vo.getBudgetDetailId());
		return budgetService.deleteBudgetLineItem(vo);
	}

	@PostMapping(value = "/deletePersonnelLine", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deletePersonnelLine(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteBudgetPerson");
		logger.info(BUDGET_PERIOD_ID, vo.getBudgetPeriodId());
		logger.info("budgetDetailId : {}", vo.getBudgetDetailId());
		logger.info("budgetDetailPersonalId : {}", vo.getBudgetPersonDetailId());
		budgetService.deletePersonnelLine(vo);
		return budgetService.calculateAfterDelete(vo);
	}

	@PostMapping(value = "/copyBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String copyBudgetPeriod(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for copyBudgetPeriod");
		logger.info("copyPeriodId : {}", vo.getCopyPeriodId());
		logger.info(CURRENT_PERIOD_ID, vo.getCurrentPeriodId());
		return budgetService.copyBudgetPeriod(vo);
	}

	@PostMapping(value = "/generateBudgetPeriods", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String generateBudgetPeriods(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for genereteBudgetPeriods");
		logger.info(CURRENT_PERIOD_ID, vo.getCurrentPeriodId());
		return budgetService.generateBudgetPeriods(vo);
	}

	@PostMapping(value = "/getProposalBudgetPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getProposalBudgetPerson(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getProposalBudgetPerson");
		logger.info(BUDGET_ID, vo.getBudgetId());
		logger.info(PROPOSAL_ID, vo.getProposalId());
		return budgetPersonService.getBudgetPersons(vo.getBudgetId(), vo.getProposalId());
	}

	@PostMapping(value = "/saveOrUpdateProposalBudgetPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateProposalBudgetPerson(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateProposalBudgetPerson");
		return budgetPersonService.saveOrUpdateProposalBudgetPerson(vo.getBudgetPerson());
	}

	@PostMapping(value = "/deleteProposalBudgetPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalBudgetPerson(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalBudgetPerson");
		logger.info("budgetPersonId : {}", vo.getBudgetPersonDetailId());
		logger.info(BUDGET_ID, vo.getBudgetId());
		return budgetPersonService.deleteBudgetPerson(vo.getBudgetPersonDetailId(), vo.getBudgetId());
	}

	@PostMapping(value = "/proposalModularBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String proposalModularBudget(@RequestBody BudgetModularVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for proposalModularBudget");
		logger.info(BUDGET_ID, vo.getBudgetId());
		return budgetModularService.proposalModularBudget(vo.getBudgetId());
	}

	@PostMapping(value = "/deleteModularBudgetInDirectLine", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteModularBudgetInDirectLine(@RequestBody BudgetModularVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteModularBudgetInDirectLine");
		logger.info("budgetModularIDCId : {}", vo.getBudgetModularIDCId());
		return budgetModularService.deleteModularBudgetInDirectLine(vo.getBudgetModularIDCId());
	}

	@PostMapping(value = "/saveProposalModularBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveProposalModularBudget(@RequestBody BudgetModularVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveProposalModularBudget");
		return budgetModularService.saveProposalModularBudget(vo);
	}

	@PostMapping(value = "/deleteSimpleBudgetLineItems", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteSimpleBudgetLineItems(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteSimpleBudgetLineItems");
		return budgetService.deleteSimpleBudgetLineItem(vo);
	}

	@PostMapping(value = "/addSimpleBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addSimpleBudgetPeriod(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addSimpleBudgetPeriod");
		return budgetService.addSimpleBudgetPeriod(vo);
	}

	@PostMapping(value = "/fetchBudgetSummaryTable", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchBudgetSummaryTable(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchBudgetSummaryTable");
		logger.info(BUDGET_HEADER_ID, vo.getBudgetId());
		return budgetService.fetchBudgetSummaryTable(vo.getBudgetId());
	}

	@PostMapping(value = "/copyProposalBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String copyProposalBudget(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for copyProposalBudget");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("budgetId: {}", vo.getBudgetId());
		return budgetService.copyProposalBudget(vo);
	}

	@PostMapping(value = "/loadBudgetByProposalId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadBudgetByProposalId(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadBudgetByProposalId");
		logger.info("Proposal Id: {}", vo.getProposalId());
		return budgetService.loadBudgetByProposalId(vo);
	}

	@PostMapping(value = "/loadBudgetByBudgetId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadBudgetByBudgetId(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadBudgetByBudgetId");
		logger.info("Budget Id: {}", vo.getBudgetId());
		return budgetService.loadBudgetByBudgetId(vo);
	}

	@PostMapping(value = "/createProposalBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createProposalBudget(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createProposalBudget");
		return budgetService.createProposalBudget(vo);
	}

	@PostMapping(value = "/saveorUpdateBudgetSummary", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveBudgetBasicDetails(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveorUpdateBudgetSummary");
		return budgetService.saveorUpdateBudgetSummary(vo);
	}

	@PostMapping(value = "/deleteBudgetHeader", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String printEntireProposal(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteBudgetHeader");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info(BUDGET_HEADER_ID, vo.getBudgetId());
		return budgetService.deleteBudgetHeader(vo);
	}

	@PostMapping(value = "/saveOrUpdateProposalBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateProposalBudget(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateProposalBudget");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("updateUserName : {}", vo.getUserName());
		logger.info("periodNumber : {}", vo.getBudgetPeriod());
		return budgetService.saveOrUpdateProposalBudget(vo);
	}

	@PostMapping(value = "/loadSimpleBudgetByBudgetId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadSimpleBudgetByBudgetId(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadSimpleBudgetByBudgetId");
		logger.info("Budget Id: {}", vo.getBudgetId());
		return budgetService.loadSimpleBudgetByBudgetId(vo);
	}

	@PostMapping(value = "/saveOrUpdateSimpleBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateSimpleBudget(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveorUpdateSimpleBudget");
		logger.info("Budget Id: {}", vo.getBudgetId());
		return budgetService.saveOrUpdateSimpleBudget(vo);
	}

	@PostMapping(value = "/generateSimpleBudgetPeriods", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String generateSimpleBudgetPeriods(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for generateSimpleBudgetPeriods");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info(CURRENT_PERIOD_ID, vo.getCurrentPeriodId());
		return budgetService.generateSimpleBudgetPeriods(vo);
	}

	@PostMapping(value = "/checkBudgetPersonAddedInBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public Boolean checkBudgetPersonAddedInBudget(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for checkBudgetPersonAddedInBudget");
		logger.info("budgetPersonId : {}", vo.getBudgetPersonDetailId());
		return budgetPersonService.checkBudgetPersonAddedInBudget(vo.getBudgetPersonDetailId());
	}

	@PostMapping(value = "/createApprovedProposalBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createApprovedProposalBudget(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createApprovedProposalBudget");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("budgetId: {}", vo.getBudgetId());
		return budgetService.createApprovedProposalBudget(vo);
	}
	
	@GetMapping(value = "/fetchCostElementByCategories", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchCostElementByCategories(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchCostElementByCategories");
		return budgetService.fetchCostElementByCategories();
	}

	@PostMapping(value = "/updateProposalBudgetPeriods", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateProposalBudgetPeriods(@RequestBody BudgetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateProposalBudgetPeriods");
		return budgetService.updateProposalBudgetPeriods(vo);
	}

}
