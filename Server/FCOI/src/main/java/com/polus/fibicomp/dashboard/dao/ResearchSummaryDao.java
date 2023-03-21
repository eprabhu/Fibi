package com.polus.fibicomp.dashboard.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.dashboard.pojo.QuickLink;
import com.polus.fibicomp.dashboard.pojo.UserSelectedWidget;
import com.polus.fibicomp.dashboard.pojo.WidgetLookup;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.view.ExpenditureVolume;
import com.polus.fibicomp.view.ResearchSummaryPieChart;
import com.polus.fibicomp.view.ResearchSummaryView;
import com.polus.fibicomp.vo.CommonVO;

@Service
public interface ResearchSummaryDao {

	/**
	 * @param person_id - Logged User ID
	 * @param summaryTable
	 * @return List of Research Summary View
	 */
	public List<ResearchSummaryView> getSummaryTable(String personId, String unitNumber);

	/**
	 * This method is used to retrieve list of in_progress proposals.
	 * @param personId - ID of the user.
	 * @param unitNumber - unit selected by the user.
	 * @return A list of Proposals in progress.
	 * @throws Exception 
	 */
	public DashBoardProfile getProposalsInProgress(String personId, String unitNumber);

	/**
	 * This method is used to retrieve list of submitted proposals.
	 * @param personId - ID of the user.
	 * @param unitNumber - unit selected by the user.
	 * @return A list of Submitted proposals.
	 * @throws Exception 
	 */
	public DashBoardProfile getSubmittedProposals(String personId, String unitNumber);

	/**
	 * This method is used to retrieve inProgress proposal data by sponsor in donutChart.
	 * @param personId - Logged User ID
	 * @param sponsorCode - sponsor_type_code clicked by user in donutChart
	 * @param unitNumber - unit selected by the user. 
	 * @return A list of proposal data based on status = inProgress.
	 * @throws Exception
	 */
	public String getInProgressProposalsBySponsorExpanded(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to retrieve awarded proposal data by sponsor in donutChart.
	 * @param personId - Logged User ID
	 * @param sponsorCode - sponsor_type_code clicked by user in donutChart
	 * @param unitNumber - unit selected by the user.
	 * @return A list of proposal data based on status = Awarded.
	 * @throws Exception
	 */
	public String getAwardedProposalsBySponsorExpanded(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to retrieve award data in piechart based on sponsor type.
	 * @param personId - Logged User ID
	 * @param sponsorCode - sponsor_type_code clicked by user in piechart
	 * @param unitNumber - unit selected by the user.
	 * @return A list of award data based on award type.
	 * @throws Exception
	 */
	public String getAwardBySponsorTypes(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to retrieve proposal data in piechart based on sponsor type.
	 * @param personId - Logged User ID
	 * @param sponsorCode - sponsor_type_code clicked by user in piechart
	 * @param unitNumber - unit selected by the user.
	 * @return A list of award data based on award type.
	 * @throws Exception
	 */
	public String getProposalBySponsorTypes(String personId, String sponsorCode, String unitNumber);
    /**
	 * This method is used to retrieve list of active in progress proposals .
	 * @param personId - ID of the user.
	 * @param unitNumber - unit selected by the user.
	 * @return A list of active in progress proposals
	 * @throws Exception 
	 */
	public DashBoardProfile getApprovalInProgressProposals(String personId, String unitNumber);

	/**
	 * This method is used to get list of inprogress proposals.
	 * @param personId - Logged in person Id.
	 * @param unitNumber - Unit number.
	 * @return A list of inprogress proposals.
	 * @throws Exception
	 */
	public List<Object[]> getInprogressProposalsForDownload(String personId, String unitNumber);

	/**
	 * This method is used to get list of submitted proposals.
	 * @param personId - Logged in person Id.
	 * @param unitNumber - Unit number.
	 * @return A list of submitted proposals.
	 * @throws Exception
	 */
	public List<Object[]> getSubmittedProposalsForDownload(String personId, String unitNumber);

	/**
	 * This method is used to get list of inprogress proposals by sponsor.
	 * @param personId - Logged in person Id.
	 * @param sponsorCode - sponsor_type_code clicked by user in donutChart
	 * @param unitNumber - Unit number.
	 * @return A list of inprogress proposals by sponsor.
	 * @throws Exception
	 */
	public List<Object[]> getInProgressProposalsBySponsorForDownload(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to get list of awarded proposals by sponsor.
	 * @param personId - Logged in person Id.
	 * @param sponsorCode - sponsor_type_code clicked by user in donutChart.
	 * @param unitNumber - Unit number.
	 * @return A list of awarded proposals by sponsor.
	 * @throws Exception
	 */
	public List<Object[]> getAwardedProposalsBySponsorForDownload(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to get list of awards by sponsor types.
	 * @param personId - Logged in person Id.
	 * @param sponsorCode - sponsor_type_code clicked by user in piechart.
	 * @param unitNumber - Unit number.
	 * @return A list of awards by sponsor types.
	 * @throws Exception
	 */
	public List<Object[]> getAwardBySponsorTypesForDownload(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to get list of proposals by sponsor types.
	 * @param personId - Logged in person Id.
	 * @param sponsorCode - sponsor_type_code clicked by user in piechart.
	 * @param unitNumber - Unit number.
	 * @return A list of proposals by sponsor types.
	 * @throws Exception
	 */
	public List<Object[]> getProposalBySponsorTypesForDownload(String personId, String sponsorCode, String unitNumber);

	/**
	 * This method is used to retrieve list of active in progress proposals .
	 * @param personId - ID of the user.
	 * @param unitNumber - unit selected by the user.
	 * @return A list of active in progress proposals
	 * @throws Exception 
	 */
	public List<Object[]> getApprovalInProgressProposalsForDownlaod(String personId, String unitNumber);

	/**
	 * This method is used to get the details to generate the expenditure volume
	 * @param personId
	 * @param unitNumber
	 * @return list of data to show the expenditure chart
	 */
	public List<ExpenditureVolume> getExpenditureVolumeChart(String personId, String unitNumber);

	/**
	 * This method is used to get the details to show the award summary pie chart
	 * @param personId
	 * @param unitNumber
	 * @return list of data to show the award pie chart
	 */
	public List<ResearchSummaryPieChart> getSummaryAwardPieChart(String personId, String unitNumber);

	/**
	 * This method is used to get the details to show the proposal summary pie chart
	 * @param personId
	 * @param unitNumber
	 * @return list of data to show the proposal pie chart
	 */
	public List<ResearchSummaryPieChart> getSummaryProposalPieChart(String personId, String unitNumber);

	/**
	 * This method is used to get the summary details of inprogress proposals
	 * @param personId
	 * @param unitNumber
	 * @return list of summary details
	 */
	public List<ResearchSummaryPieChart> getSummaryInProgressProposalDonutChart(String personId, String unitNumber);

	/**
	 * This method is used to get summary details of awarded proposals
	 * @param personId
	 * @param unitNumber
	 * @return
	 */
	public List<ResearchSummaryPieChart> getSummaryAwardedProposalDonutChart(String personId, String unitNumber);

	/**
	 * This method is used to get all the links and events if current date is between quick link start date and end date or the start date or end date is null
	 * @return list of quick links
	 */
	public List<QuickLink> getAllQuickLinksOrEvents();

	/**
	 * This method is used to get all the widget lookups
	 * @return list of widget lookups
	 */
	public List<WidgetLookup> getAllWidgetLookups();

	/**
	 * This method is used to get the user selected widget based on the person id
	 * @param loginPersonId
	 * @return selected widgets
	 */
	public List<UserSelectedWidget> getUserSelectedWidgets(String loginPersonId);

	/**
	 * This method is used to save the user selected widget
	 * @param userSelectedWidget
	 */
	public void saveUserSelectedWidget(UserSelectedWidget userSelectedWidget);

	/**
	 * This method is used to delete the user selected widget by Id
	 * @param selectedWidgetId
	 */
	public void deleteUserSelectedWidget(Integer selectedWidgetId);

	/**
	 * This method is used to get the summary data of widget
	 * @param commonVO
	 * @return widget summary datas
	 */
	public List<Object[]> getResearchSummaryDatasByWidget(CommonVO commonVO);

	/**
	 * This method is used to get the detailed view of widget
	 * @param commonVO
	 * @return detailed data for widget
	 */
	public List<Object[]> getDetailedViewOfWidget(CommonVO commonVO);

	/**
	 * This method is used to check weather the widget is already selected by user
	 * @param widgetId
	 * @param personId
	 * @return true if no duplicate widget
	 */
	public UserSelectedWidget checkIfWidgetAlreadyExist(Integer widgetId, String personId);

	/**
	 * This method is used to get Agreement Summary data.
	 * @param personId - personId.
	 * @param isAdmin - boolean value to check whether is admin or not.
	 * @return Set of values research summary table.
	 */
	public String getDashBoardResearchSummary(String personId, Boolean isAdmin);

	/**
	 * 
	 * @param personId
	 * @return
	 */
	public List<Unit> getUnitWithRights(String personId);

}
