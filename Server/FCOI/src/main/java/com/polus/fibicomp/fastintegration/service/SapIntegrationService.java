package com.polus.fibicomp.fastintegration.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.fastintegration.pojo.SapCostCenter;
import com.polus.fibicomp.fastintegration.pojo.SapFundCenter;
import com.polus.fibicomp.fastintegration.pojo.SapGrantCode;
import com.polus.fibicomp.fastintegration.pojo.SapProfitCenter;

@Transactional
@Service
public interface SapIntegrationService {

	/**
	 * This method is used to get profit centers based on search string.
	 * @param searchString - input string.
	 * @return a list of profit centers.
	 */
	public List<SapProfitCenter> findProfitCenter(String searchString);

	/**
	 * This method is used to get fund centers based on search string.
	 * @param searchString - input string.
	 * @return a list of fund centers.
	 */
	public List<SapFundCenter> findFundCenter(String searchString);

	/**
	 * This method is used to get cost centers based on search string.
	 * @param searchString - input string.
	 * @return a list of cost centers.
	 */
	public List<SapCostCenter> findCostCenter(String searchString);

	/**
	 * This method is used to get grant codes based on search string.
	 * @param searchString - input string.
	 * @return a list of grant codes.
	 */
	public List<SapGrantCode> findGrantCode(String searchString);

	/**
	 * This Method is used to get he Profit center Api details from Workday
	 */
	public void getAllProfitCenterAPI();

	/**
	 * This Method is used to get he Fund center Api details from Workday
	 */
	public void getAllFundCenterAPI();

	/**
	 * This Method is used to get he Cost center Api details from Workday
	 */
	public void getAllCostCenterAPI();

	/**
	 * This Method is used to get he grant code Api details from Workday
	 */
	public void getAllGrantCodeFromAPI();

}
