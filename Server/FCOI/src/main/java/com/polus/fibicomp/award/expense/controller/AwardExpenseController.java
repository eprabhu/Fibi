package com.polus.fibicomp.award.expense.controller;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.award.expense.service.AwardExpenseService;
import com.polus.fibicomp.award.expense.vo.AwardExpenseDetailVO;
import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;
import com.polus.fibicomp.award.expense.vo.AwardHoursLoggedVO;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class AwardExpenseController {

	protected static Logger logger = LogManager.getLogger(AwardExpenseController.class.getName());

	@Autowired
	@Qualifier(value = "awardExpenseService")
	private AwardExpenseService awardExpenseService;

	@Autowired
	@Qualifier(value = "dashboardService")
	private DashboardService dashboardService;

	@RequestMapping(value = "/loadExpenseDetailsByAwardId", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadExpenseDetailsByAwardId(@Valid @RequestBody AwardExpenseDetailVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadExpenseDetailsByAwardId");
		logger.info("AccountNumber : " + vo.getAccountNumber());
		logger.info("AwardNumber : " + vo.getAwardNumber());
		return awardExpenseService.loadExpenseDetailsByAwardId(vo.getAwardNumber(), vo.getAccountNumber(), vo.getType());
	}

	@RequestMapping(value = "/fetchExpenditureTransactions", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchExpenditureTransactions(@Valid @RequestBody AwardExpenseTransactionVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchExpenditureTransactions");
		logger.info("accountNumber : {}", vo.getAccountNumber());
		logger.info("awardNumber : {}", vo.getAwardNumber());
		logger.info("actualOrCommittedFlag : {}", vo.getActualOrCommittedFlag());
		return awardExpenseService.fetchExpenseTransactionsDetails(vo);
	}

	@RequestMapping(value = "/fetchPersonLogDetails", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchPersonLogDetails(@RequestBody AwardHoursLoggedVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchPersonLogDetails");
		logger.info("accountNumber : " + vo.getAccountNumber());
		logger.info("awardNumber : " + vo.getAwardNumber());
		logger.info("internalOrderCode : " + vo.getInternalOrderCode());
		return awardExpenseService.fetchPersonLogDetails(vo);
	}

	@RequestMapping(value = "/updateCommittedAmount", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateCommittedAmount(@RequestBody AwardExpenseDetailVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateCommittedAmount");
		logger.info("accountNumber : " + vo.getAccountNumber());
		logger.info("awardNumber : " + vo.getAwardNumber());
		logger.info("internalOrderCode : " + vo.getInternalOrderCode());
		return awardExpenseService.updateCommittedAmount(vo);
	}

	@RequestMapping(value = "/exportExpenseTrackingDatas", method = RequestMethod.POST)
	public ResponseEntity<byte[]> exportExpenseTrackingData(HttpServletRequest request, @Valid @RequestBody CommonVO vo) throws Exception {
		logger.info("Requesting for exportExpenseTrackingDatas");
		XSSFWorkbook workbook = awardExpenseService.getXSSFWorkbookForExpenseTracking(vo);
		return dashboardService.getResponseEntityForDownloadExpense(vo, workbook);
	}

	@RequestMapping(value = "/fetchExpenseDetailsExtByParams", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchExpenseDetailsExtByParams(@RequestBody AwardExpenseDetailVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchExpenseDetailsExtByParams");
		logger.info("accountNumber : " + vo.getAccountNumber());
		logger.info("awardNumber : " + vo.getAwardNumber());
		logger.info("internalOrderCode : " + vo.getInternalOrderCode());
		return awardExpenseService.fetchAwardExpenseDetailsExtByParams(vo);
	}

	@RequestMapping(value = "/deleteAwardExpenseDetailsExtById", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardExpenseDetailsExtById(@RequestBody AwardExpenseDetailVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAwardExpenseDetailsExtById");
		logger.info("accountNumber : " + vo.getAccountNumber());
		logger.info("awardNumber : " + vo.getAwardNumber());
		logger.info("internalOrderCode : " + vo.getInternalOrderCode());
		logger.info("awardExpenseDetailsId : " + vo.getAwardExpenseDetailsId());
		return awardExpenseService.deleteAwardExpenseDetailsExtById(vo);
	}

	@RequestMapping(value = "/loadAwardExpensePersonDetails", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadAwardExpensePersonDetails(@RequestBody AwardExpenseDetailVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadExpenseDetailsByAwardId");
		logger.info("accountNumber : {}" , vo.getAccountNumber());
		logger.info("awardNumber : {}" , vo.getAwardNumber());
		logger.info("BudgetDetailId : {}" , vo.getBudgetDetailId());
		return awardExpenseService.loadAwardExpensePersonDetails(vo.getBudgetDetailId(), vo.getAwardNumber(), vo.getAccountNumber());
	}

	@RequestMapping(value = "/loadExpenseDetailsBasedOnCostElement", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadExpenseDetailsBasedOnCostElement(@RequestBody AwardExpenseDetailVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadExpenseDetailsByAwardId");
		logger.info("awardNumber : {}" , vo.getAwardNumber());
		return awardExpenseService.loadExpenseDetailsBasedOnCostElement(vo.getAwardNumber(), vo.getAccountNumber(), vo.getType());
	}

	@ResponseStatus(HttpStatus.BAD_REQUEST)
	@ExceptionHandler(MethodArgumentNotValidException.class)
	public Map<String, String> handleValidationExceptions(MethodArgumentNotValidException ex) {
		Map<String, String> errors = new HashMap<>();
		ex.getBindingResult().getAllErrors().forEach((error) -> {
			String fieldName = ((FieldError) error).getField();
			String errorMessage = error.getDefaultMessage();
			errors.put(fieldName, errorMessage);
		});
		return errors;
	}

}
