package com.polus.fibicomp.award.datesandamounts.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.award.datesandamounts.dto.AwardAmountFNADistributionDTO;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountFNADistribution;
import com.polus.fibicomp.award.datesandamounts.service.DatesAndAmountService;
import com.polus.fibicomp.award.vo.AwardDatesandAmountVO;
import com.polus.fibicomp.award.vo.AwardVO;

@RestController
public class DatesAndAmountController {

	protected static Logger logger = LogManager.getLogger(DatesAndAmountController.class.getName());

	@Autowired
	private DatesAndAmountService datesAndAmountService;

	@PostMapping(value = "/getAwardDatesAndAmount")
	public String getAwardDatesAndAmountLookupData(@RequestBody AwardDatesandAmountVO awardDatesandAmountVO, HttpServletRequest request) throws Exception {
		logger.info("Request for getAwardDatesAndAmount");
		logger.info("awardId : {}", awardDatesandAmountVO.getAwardId());
		return datesAndAmountService.getAwardDatesAndAmount(awardDatesandAmountVO);
	}

	@PostMapping(value = "/saveTransactionDetails")
	public String saveTransactionDetails(@RequestBody AwardDatesandAmountVO awardDatesandAmountVO, HttpServletRequest request) throws Exception {
		logger.info("Request for saveTransactionDetails");
		return datesAndAmountService.saveTransactionDetails(awardDatesandAmountVO);
	}

	@PostMapping(value = "/getAwardCostShare")
	public String getAwardCostShare(@RequestBody AwardVO awardVO, HttpServletRequest request) throws Exception {
		logger.info("Request for getAwardCostShare");
		logger.info("awardId : {}", awardVO.getAwardId());
		return datesAndAmountService.getAwardCostShare(awardVO);
	}

	@PostMapping(value = "/getAwardFunds")
	public String getAwardFunds(@RequestBody AwardVO awardVO, HttpServletRequest request) throws Exception {
		logger.info("Request for getAwardFunds");
		logger.info("awardId : {}", awardVO.getAwardId());
		return datesAndAmountService.getAwardFunds(awardVO);
	}

	@PostMapping(value = "/saveTotalProjectCostInForeignCurrency")
	public String saveTotalProjectCostInForeignCurrency(@RequestBody AwardDatesandAmountVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveTotalProjectCostInForeignCurrency");
		logger.info("awardId : {}", vo.getAwardId());
		return datesAndAmountService.saveTotalProjectCostInForeignCurrency(vo);
	}

	@PostMapping(value = "/deleteTransactionDetails")
	public String deleteTransactionDetails(@RequestBody AwardDatesandAmountVO awardDatesandAmountVO, HttpServletRequest request) throws Exception {
		logger.info("Request for deleteTransactionDetails");
		return datesAndAmountService.deleteTransactionDetails(awardDatesandAmountVO.getTransactionId());
	}

	@PostMapping(value = "/loadAnticipatedDistribution")
	public String loadAnticipatedDistribution(@RequestBody AwardAmountFNADistributionDTO awardAmountFNADistributionDTO, HttpServletRequest request) {
		logger.info("Request for loadAnticipatedDistribution");
		logger.info("awardNumber {}", awardAmountFNADistributionDTO.getAwardId());
		return datesAndAmountService.loadAnticipatedDistribution(awardAmountFNADistributionDTO);
	}

	@PostMapping(value = "/saveOrUpdateAnticipatedDistribution")
	public String saveOrUpdateAnticipatedDistribution(@RequestBody List<AwardAmountFNADistribution> awardAmountFNADistributionDTO) {
		logger.info("Request for saveOrUpdateAnticipatedDistribution");
		return datesAndAmountService.saveOrUpdateAnticipatedDistribution(awardAmountFNADistributionDTO);
	}

	@DeleteMapping(value = "/deleteAnticipatedDistribution/{fnaDistributionId}")
	public String deleteAnticipatedDistribution(@PathVariable(value = "fnaDistributionId", required = true) final Integer fnaDistributionId) {
		logger.info("Request for deleteAnticipatedDistribution");
		logger.info("fnaDistributionId {}", fnaDistributionId);
		return datesAndAmountService.deleteAnticipatedDistribution(fnaDistributionId);
	}
}
