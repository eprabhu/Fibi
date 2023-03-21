package com.polus.fibicomp.awardbudgetapiintegration.service;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.awardbudgetapiintegration.vo.AwardBudgetPrintVO;
import com.polus.fibicomp.awardbudgetapiintegration.vo.BudgetAPIVO;

@Service
public interface BudgetAPIService {

	/**
	 * This method is used to fetch Budget API data from API.
	 * 
	 * @param
	 * @return A list of Budget data and its details.
	 */
	public void fetchBudgetAPIResponse();
	/**
	 * This method is used to fetch Budget data.
	 * 
	 * @param vo
	 * @return A list of Budget data and its details based on an year and project number.
	 */
	public String fetchBudgetAPIResponseBasedOnParams(BudgetAPIVO budgetAPIVO);

	/**
	 * This method is used to export the budget detail.
	 * @param vo
	 * @param response
	 * @return xlsx and pdf report.
	 */
	public ResponseEntity<byte[]> generateAwardBudgetIntegrationReport(HttpServletResponse response, AwardBudgetPrintVO vo);

}
