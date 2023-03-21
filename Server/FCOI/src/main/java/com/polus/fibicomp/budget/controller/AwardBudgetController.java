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

import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.budget.service.BudgetPersonService;
import com.polus.fibicomp.budget.vo.AwardBudgetPeriodVO;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;
import com.polus.fibicomp.budget.vo.AwardDetailBudgetVO;

@RestController
public class AwardBudgetController {

	protected static Logger logger = LogManager.getLogger(AwardBudgetController.class.getName());

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Autowired
	@Qualifier(value = "budgetPersonService")
	private BudgetPersonService budgetPersonService;

	@PostMapping(value = "/saveOrUpdateAwardBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardBudget(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAwardBudget");
		return awardBudgetService.saveOrUpdateAwardBudget(vo);
	}

	@PostMapping(value = "/deleteAwardBudgetPersonnelLine", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deletePersonnelLine(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for deleteBudgetLineItem");
		logger.info("budgetPeriodId : {}", vo.getBudgetPeriodId());
		logger.info("budgetDetailId : {}", vo.getBudgetDetailId());
		logger.info("budgetDetailPersonalId : {}", vo.getBudgetPersonDetailId());
		return awardBudgetService.deletePersonnelLine(vo);
	}

	@PostMapping(value = "/loadBudgetByAwardId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadBudgetByAwardId(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for loadBudgetByAwardId");
		logger.info("Award Id : {}", vo.getAwardId());
		return awardBudgetService.loadBudgetByAwardId(vo);
	}

	@PostMapping(value = "/createAwardBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createAwardBudget(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for createAwardBudget");
		return awardBudgetService.createAwardBudget(vo);
	}

	@PostMapping(value = "/getDevProposalBudgetByAwardId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getDevProposalBudgetByAwardId(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for getDevProposalBudgetByAwardId");
		logger.info("Award Id: {}", vo.getAwardId());
		return awardBudgetService.getDevProposalBudgetByAwardId(vo);
	}

	@PostMapping(value = "/importProposalBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String importProposalBudget(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for importProposalBudget");
		logger.info("Award Id : {}", vo.getAwardId());
		logger.info("DevPropBudgetHeaderId : {}", vo.getDevPropBudgetHeaderId());
		logger.info("DevPropBudgetPeriodId : {}", vo.getDevPropBudgetPeriodId());
		logger.info("isAllPeriod : {}", vo.getIsAllPeriod());
		logger.info("awardBudgetId : {}", vo.getAwardBudgetId());
		return awardBudgetService.importProposalBudget(vo);
	}

	@PostMapping(value = "/getAwardBudgetVersionsByAwardId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAwardBudgetVersionsByAwardId(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for getAwardBudgetVersionsByAwardId");
		logger.info("Award Id: {}", vo.getAwardId());
		return awardBudgetService.getAwardBudgetVersionsByAwardId(vo);
	}

	@PostMapping(value = "/addAwardBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardBudgetPeriod(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for addAwardBudgetPeriod");
		logger.info("budgetHeaderId  : {}", vo.getBudgetHeaderId());
		logger.info("userName  : {}", vo.getUserName());
		return awardBudgetService.addAwardBudgetPeriod(vo.getBudgetHeaderId(), vo.getUserName());
	}

	@PostMapping(value = "/getAwardBudgetPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAwardBudgetPerson(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for getProposalBudgetPerson");
		logger.info("budgetId : {}", vo.getBudgetHeaderId());
		logger.info("awardId : {}", vo.getAwardId());
		return budgetPersonService.getAwardBudgetPersons(vo.getBudgetHeaderId(), vo.getAwardId().toString());
	}

	@PostMapping(value = "/saveOrUpdateAwardBudgetPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardBudgetPerson(@RequestBody AwardBudgetVO awardBudgetVO,
			HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAwardBudgetPerson");
		return budgetPersonService.saveOrUpdateAwardBudgetPerson(awardBudgetVO);
	}

	@PostMapping(value = "/deleteAwardBudgetPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardBudgetPerson(@RequestBody AwardBudgetVO awardBudgetPersonVO, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for deleteProposalBudgetPerson");
		logger.info("budgetPersonId : {}", awardBudgetPersonVO.getBudgetPersonId());
		logger.info("budgetId : {}", awardBudgetPersonVO.getBudgetHeaderId());
		logger.info("awardId : {}", awardBudgetPersonVO.getAwardId());
		logger.info("userName : {}", awardBudgetPersonVO.getUserName());
		return budgetPersonService.deleteAwardBudgetPerson(awardBudgetPersonVO.getBudgetPersonId(),
				awardBudgetPersonVO.getBudgetHeaderId(), awardBudgetPersonVO.getAwardId(), awardBudgetPersonVO.getUserName());
	}

	@PostMapping(value = "/copyAwardBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String copyBudgetPeriod(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for copyBudgetPeriod");
		logger.info("copyPeriodId :{}", vo.getCopyPeriodId());
		logger.info("currentPeriodId : {}", vo.getCurrentPeriodId());
		return awardBudgetService.copyAwardBudgetPeriod(vo);
	}

	@PostMapping(value = "/generateAwardBudgetPeriods", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String generateBudgetPeriods(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for genereteBudgetPeriods");
		logger.info("currentPeriodId : {}", vo.getCopyPeriodId());
		return awardBudgetService.generateAwardBudgetPeriods(vo);
	}

	@PostMapping(value = "/copyAwardBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createProposalBudget(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for createProposalBudget");
		return awardBudgetService.copyAwardBudget(vo);
	}

	@PostMapping(value = "/saveOrUpdateAwardBudgetOverView", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardBudgetOverView(@RequestBody AwardBudgetVO vo,
			HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for save or update Award budget");
		return awardBudgetService.saveOrUpdateAwardBudgetOverView(vo);
	}

	@PostMapping(value = "/saveOrUpdateAwardBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardBudgetPeriod(@RequestBody AwardBudgetPeriodVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for save or update award budget period");
		return awardBudgetService.saveOrUpdateAwardBudgetPeriod(vo.getPeriod(), vo.getBudgetId(), vo.getUpdateUser());
	}

	@PostMapping(value = "/saveOrUpdateAwardBudgetLineItem", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardBudgetLineItem(@RequestBody AwardDetailBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for save or update award budget line item");
		return awardBudgetService.saveOrUpdateAwardBudgetLineItem(vo);
	}

	@PostMapping(value = "/deleteAwardBudgetPeriod", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardBudgetPeriod(@RequestBody AwardBudgetPeriodVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for delete award budget period");
		return awardBudgetService.deleteAwardBudgetPeriod(vo.getBudgetPeriodId(), vo.getUpdateUser());
	}

	@PostMapping(value = "/deleteAwardBudgetLineItem", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteBudgetLineItem(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for deleteBudgetLineItem");
		return awardBudgetService.deleteBudgetLineItem(vo);
	}

	@PostMapping(value = "/fetchAwardBudgetSummaryTable", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchBudgetSummaryTable(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for fetchBudgetSummaryTable");
		logger.info("budgetHeaderId : {}", vo.getAwardBudgetId());
		return awardBudgetService.fetchBudgetSummaryTable(vo.getAwardBudgetId());
	}

	@PostMapping(value = "/getSyncAwardBudgetRates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getSyncBudgetRates(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for getSyncBudgetRates");
		logger.info("awardId : {}", vo.getAwardId());
		logger.info("userName : {}", vo.getUserName());
		return awardBudgetService.getSyncAwardBudgetRates(vo);
	}

	@PostMapping(value = "/resetAwardBudgetRates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String resetAwardBudgetRates(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for resetBudgetRates");
		logger.info("awardBudgetId : {}", vo.getAwardBudgetId());
		logger.info("userName : {}", vo.getUserName());
		return awardBudgetService.resetAwardRates(vo.getAwardBudgetId(), vo.getUserName());
	}

	@PostMapping(value = "/applyAwardBudgetRates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String applyAwardRates(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for applyAwardBudgetRates");
		logger.info("budgetHeaderId : {}", vo.getAwardBudgetId());
		logger.info("userName : {}", vo.getUserName());
		return awardBudgetService.applayAwardRates(vo.getAwardBudgetId(), vo.getAwardRates(), vo.getUserName());
	}

	@PostMapping(value = "/checkAwardBudgetPersonAddedInBudget", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public Boolean checkAwardBudgetPersonAddedInBudget(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for checkAwardBudgetPersonAddedInBudget");
		logger.info("budgetPersonId : {}", vo.getBudgetPersonDetailId());
		return awardBudgetService.checkAwardBudgetPersonAddedInBudget(vo.getBudgetPersonDetailId());
	}
	
	@PostMapping(value = "/getBudgetDetailsByAwardId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getBudgetDetailsByAwardId(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for getBudgetDetailsByAwardId");
		logger.info("awardId : {}", vo.getAwardId());
		return awardBudgetService.getBudgetDetailsByAwardId(vo.getAwardId());
	}

	@PostMapping(value = "/deleteAwardBudgetNonPersonnelLine", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteNonPersonnelLine(@RequestBody AwardBudgetVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for deleteAwardBudgetNonPersonnelLine");
		logger.info("awardId : {}", vo.getAwardId());
		logger.info("budgetHeaderId : {}", vo.getBudgetHeaderId());
		logger.info("BudgetNonPersonDtlId : {}", vo.getBudgetNonPersonDtlId());
		return awardBudgetService.deleteNonPersonnelLine(vo);
	}

	@GetMapping(value = "/fetchAllBudgetFundType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAwardPaymentAndInvoices(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAllBudgetFundType");
		return awardBudgetService.fetchAllBudgetFundType();
	}

}
