package com.polus.fibicomp.fastintegration.sapfeedmaintenance.service;

import java.io.File;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;

@Transactional
@Service(value = "sapFeedMaintenanceService")
public interface SapFeedMaintenanceService {

	/**
	 * This method is used to get sap award feed batch detail
	 * @param vo
	 * @return list of sap award feed.
	 */
	public String getBatchDetailDashboard(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to get sap award status
	 * @return list of sap award feed status.
	 */
	public String fetchSapAwardFeedStatus();

	/**
	 * This method is used to get sap award feed batch history detail
	 * @param vo
	 * @return list of sap award feed.
	 */
	public String getBatchHistoryDashboard(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to get feeds for interfaces
	 * @param vo
	 * @return list of interfaces.
	 */
	public String getSapAwardFeedDetails(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to update sap award feed status
	 * @param vo
	 */
	public String updateFeedStatus(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to re interface sap award feed 
	 * @param vo
	 */
	public String reInterfaceSapAwardFeed(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to export sap feed generated files 
	 * @param response
	 * @param vo
	 * @return message
	 */
	public String exportSapGeneratedAttachments(SapFeedMaintenanceVO vo, HttpServletResponse response);

	/**
	 * This method is used to send sap feed notification for PI
	 * @param vo
	 */
	public String notifyPI(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to send sap feed re trigger sap award feed
	 * @param vo
	 */
	public String sapFeedReTrigger(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to get files from path based on batch id
	 * @param batchId
	 * @param inbound
	 */
	public File[] getSapAwardFeedFiles(Integer batchId, String inbound);

	/**
	 * This method is used to export sap feed report
	 * @param response
	 * @param vo
	 */
	public ResponseEntity<byte[]> generateSapFeedReport(HttpServletResponse response, SapFeedMaintenanceVO vo);

}
