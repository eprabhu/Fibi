package com.polus.fibicomp.fastintegration.dao;

import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.fastintegration.pojo.SapCostCenter;
import com.polus.fibicomp.fastintegration.pojo.SapFundCenter;
import com.polus.fibicomp.fastintegration.pojo.SapGrantCode;
import com.polus.fibicomp.fastintegration.pojo.SapIntegrationLog;
import com.polus.fibicomp.fastintegration.pojo.SapProfitCenter;

@Service
public interface SapIntegrationDao {

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
	 * 
	 * @param oauthApi
	 * @return
	 */
	public String getWorkdayConfigurationData(String oauthApi);

	/**
	 * 
	 * @param profitCenterCode
	 * @param profitCenterArea
	 * @return
	 */
	public SapProfitCenter getSapProfitCenter(String profitCenterCode);

	/**
	 * 
	 * @param sapProfitCenter
	 */
	public void saveOrUpdateSapProfitCenter(SapProfitCenter sapProfitCenter);

	/**
	 * 
	 * @param integrationLog
	 */
	public void saveSapIntergrationLog(SapIntegrationLog integrationLog);

	/**
	 * 
	 * @param sapFundCenterCode
	 * @return
	 */
	public SapFundCenter getSapFundCenter(String sapFundCenterCode);

	/**
	 * 
	 * @param sapFundCenter
	 */
	public void saveOrUpdateSapFundCenter(SapFundCenter sapFundCenter);

	/**
	 * 
	 * @param costCenterCode
	 * @return
	 */
	public SapCostCenter getSapCostCenter(String costCenterCode);

	/**
	 * 
	 * @param sapCostCenter
	 */
	public void saveOrUpdateSapCostCenter(SapCostCenter sapCostCenter);

	/**
	 * To make the feed inactive if it was not updated
	 * @param currentTime
	 * @param sapIntegration 
	 * @return 
	 */
	public Integer callToInactiveFeed(Timestamp currentTime, String sapIntegration);

	/**
	 * 
	 * @param grantNumber
	 * @return
	 */
	public SapGrantCode getGrantCode(String grantNumber);

	/**
	 * 
	 * @param sapGrantCode
	 */
	public void saveOrUpdateGrantCode(SapGrantCode sapGrantCode);

}
