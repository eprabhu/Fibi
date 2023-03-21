package com.polus.fibicomp.dashboard.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.vo.CommonVO;

@Service
public interface ResearchSummaryService {

	/**
	 * This method is used to retrieve award data in piechart based on sponsor type.
	 * @param personId - Logged User ID
	 * @param vo - Object of CommonVO
	 * @return A list of selected pie chart items based on type.
	 */
	public String getPieChartDataByType(CommonVO vo);

	/**
	 * This method is used to get list of selected type in ResearchSummaryTable.
	 * @param personId - Logged User ID
	 * @param researchSummaryIndex - Selected row in Summary table
	 * @return a String of details of selected item
	 */
	public String getDetailedSummaryData(CommonVO vo);

	/**
	 * This method is used to retrieve proposal data in donutChart based on status(in progress/awarded).
	 * @param vo - Object of CommonVO
	 * @return A list of selected donutChart items based on status.
	 */
	public String getDonutChartDataBySponsor(CommonVO vo);

	/**
	 * This method is used to get XSSFWorkbook based on index clicked in research summary tab.
	 * @param vo - for input of user, dashboardIndex and sponsorCode.
	 * @return XSSFWorkbook that contains excel sheet with data.
	 */
	public ResponseEntity<byte[]> getXSSFWorkbookForResearchSummary(CommonVO vo);

	/**
	 * This method is used to get datas for research summary table
	 * @param vo
	 * @return details for reserach table
	 */
	public String getResearchSummaryTable(CommonVO vo);

	/**
	 * This method is used to get the details to generate the expenditure volume
	 * @param vo
	 * @return list of data to show the expenditure chart
	 */
	public String getExpenditureVolumeChart(CommonVO vo);

	/**
	 * This method is used to get the details to show the award summary pie chart
	 * @param vo
	 * @return list of data to show the award pie chart
	 */
	public String getSummaryAwardPieChart(CommonVO vo);

	/**
	 * This method is used to get the details to show the proposal summary pie chart
	 * @param vo
	 * @return list of data to show the proposal pie chart
	 */
	public String getSummaryProposalPieChart(CommonVO vo);

	/**
	 * This method is used to get the summary details of inprogress proposals
	 * @param vo
	 * @return list of summary details
	 */
	public String getSummaryInProgressProposalDonutChart(CommonVO vo);

	/**
	 * This method is used to get summary details of awarded proposals
	 * @param vo
	 * @return
	 */
	public String getSummaryAwardedProposalDonutChart(CommonVO vo);

	/**
	 * This method is used to get all the links and events if current date is between quick link start date and end date or the start date or end date is null
	 * @return list of quick links
	 */
	public String getAllQuickLinksOrEvents();

	/**
	 * This method is used to get all widget lookups including user selected widgets
	 * @return list of widget lookups
	 */
	public String getWidgetLookups();

	/**
	 * This method is used to save the user selected widget
	 * @param dashBoardProfile
	 * @return saved widget
	 */
	public String saveUserSelectedWidget(DashBoardProfile dashBoardProfile);

	/**
	 * This method is used to delete the selected widget by id
	 * @param selectedWidgetId
	 * @return success or failure message
	 */
	public String deleteUserSelectedWidget(Integer selectedWidgetId);

	/**
	 * This method is used to update the sort order of the user selected widget widgets
	 * @param dashBoardProfile
	 * @return list of user selected widget with updated sort order
	 */
	public String updateWidgetSortOrder(DashBoardProfile dashBoardProfile);

	/**
	 * This method is used to get the widget datas by widget type and person id 
	 * @param commonVO
	 * @return widget data details
	 */
	public String getResearchSummaryDatasByWidget(CommonVO commonVO);

	/**
	 * This method is used to get the detailed view of selected widget
	 * @param commonVO
	 * @return expanded details for the widget
	 */
	public String getDetailedViewOfWidget(CommonVO commonVO);

	/**
	 * This method is used to get Agreement Summary data.
	 * @param vo - Object of CommonVO.
	 * @return Set of values research summary table.
	 */
	public String getAgreementSummary(CommonVO vo);

	/**
	 * 
	 * @param personId
	 * @return
	 */
	public String getUnitWithRights(String personId);

}
