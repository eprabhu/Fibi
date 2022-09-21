package com.polus.fibicomp.award.revenue.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.award.revenue.service.AwardRevenueService;
import com.polus.fibicomp.award.revenue.vo.AwardRevenueVO;

@RestController
public class AwardRevenueController {

	protected static Logger logger = LogManager.getLogger(AwardRevenueController.class.getName());

	@Autowired
	@Qualifier(value = "awardRevenueService")
	private AwardRevenueService awardRevenueService;

	@PostMapping(value = "/loadRevenueDetailsByParams", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadRevenueDetailsByParams(@RequestBody AwardRevenueVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadRevenueDetailsByAwardId");
		logger.info("AccountNumber : {}", vo.getAccountNumber());
		logger.info("AwardNumber : {}", vo.getAwardNumber());
		return awardRevenueService.loadRevenueDetailsByParams(vo.getAwardNumber(), vo.getAccountNumber());
	}

	@PostMapping(value = "/fetchRevenueTransactions", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchRevenueTransactions(@RequestBody AwardRevenueVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadRevenueDetailsByAwardId");
		logger.info("AccountNumber : {}", vo.getAccountNumber());
		logger.info("AwardNumber : {}", vo.getAwardNumber());
		logger.info("InternalOrderCode : {}", vo.getInternalOrderCode());
		logger.info("FiPostingStartDate : {}", vo.getFiPostingStartDate());
		logger.info("FiPostingEndDate : {}", vo.getFiPostingEndDate());
		return awardRevenueService.fetchRevenueTransactionsByParams(vo.getAwardNumber(), vo.getAccountNumber(), vo.getInternalOrderCodes(), vo.getFiPostingStartDate(), vo.getFiPostingEndDate());
	}

}
