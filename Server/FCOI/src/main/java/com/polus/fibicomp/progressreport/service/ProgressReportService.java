package com.polus.fibicomp.progressreport.service;

import java.util.Date;
import java.util.Set;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.vo.ProgressReportVO;

@Service(value = "progressReportService")
public interface ProgressReportService {

	/**
	 * @param progressReportVO
	 * @return progressReportVO updated progress report
	 */
	public String saveOrUpdateProgressReport(ProgressReportVO progressReportVO);

	/**
	 * @param progressReportId
	 * @param import 
	 * @return progressReportVO progress report
	 */
	public String loadAwardProgressReport(Integer progressReportId, Boolean progressReportImported);

	/**
	 * @param progressReportId
	 * @return progressReportVO progress report milestone
	 */
	public String loadProgressReportMilestone(Integer progressReportId);

	/**
	 * @param progressReportVO
	 * @return progressReportVO updated progress report milestone 
	 */
	public String saveOrUpdateProgressReportMilestone(ProgressReportVO progressReportVO);

	/**
	 * @param progressReportId
	 * @return progressReportVO submitted progress report
	 */
	public String submitProgressReport(Integer progressReportId);

	/**
	 * @param progressReportVO
	 */
	public void canProgressReportTakeRoutingAction(ProgressReportVO progressReportVO);

	/**
	 * @param progressReportVO
	 * @param notificationTypeId
	 * @param dynamicEmailRecipients
	 */
	public void sendProgressReportNotification(ProgressReportVO progressReportVO, Integer notificationTypeId,
			Set<NotificationRecipient> dynamicEmailRecipients);

	/**
	 * @param files
	 * @param formDataJson
	 * @return updated attachment
	 */
	public String saveOrUpdateProgressReportAttachment(MultipartFile[] files, String formDataJson);

	/**
	 * @param progressReportVO
	 * @return list of attachments
	 */
	public String loadProgressReportAttachments(ProgressReportVO progressReportVO);

	/**
	 * @param attachmentId
	 * @return blob data
	 */
	public ResponseEntity<byte[]> downloadProgressReportAttachment(Integer attachmentId);

	/**
	 * @param progressReportVO
	 * @return updated progress report status
	 */
	public String performFundingAgencyAction(ProgressReportVO progressReportVO);

	/**
	 * @param progressReportVO
	 * @return updated progress report achievements
	 */
	public String updateProgressReportAchievements(ProgressReportVO progressReportVO);

	/**
	 * @param progressReportId
	 * @return list of progress report kpi summary
	 */
	public String loadProgressReportKPISummary(Integer progressReportId);

	/**
	 * @param kpiSummaryId
	 * @param sectionCode
	 * @return list kpi summary details for the corresponding section
	 */
	public String loadProgressReportKPISummaryDetails(Integer kpiSummaryId, String sectionCode);

	/**
	 * @param progressReportVO
	 * @return updated summary detail
	 */
	public String saveOrUpdateKPISummaryDetails(ProgressReportVO progressReportVO);

	/**
	 * @param kpiSummaryId 
	 * @param sectionCode
	 * @param id which will be the primary key for that particular row which needs to be deleted
	 * @return
	 */
	public String deleteKPISummaryDetail(Integer kpiSummaryId, String sectionCode, Integer id);

	/**
	 * @return map of lookups
	 */
	public String loadProgressReportKPILookups() throws Exception;

	/**
	 * @param progressReportId
	 * @return AwardProgressReport with all related details
	 */
	public AwardProgressReport loadAwardProgressReportDetails(Integer progressReportId);

	/**
	 * @param awardNumber
	 * @return all progress report for given awardId
	 */
	public String loadProgressReportForAward(String awardNumber);

	/**
	 * @param import progress report details
	 * @return import overview, milestone, KPI details
	 */
	public ProgressReportVO importProgressReportTemplate(MultipartFile[] files, String formDataJson);

	/**
	 * @param awardNumber
	 * @param awardId 
	 * @param reportClassCode 
	 * @param dueDate 
	 * @return award progress report start date
	 */
	public String getAwardProgressReportStartDate(String awardNumber, Integer awardId, String reportClassCode, Date dueDate);

	/**
	 * @param progressReportVO
	 * @return updated ProgressReport Dates
	 */
	public String updateProgressReportDates(ProgressReportVO progressReportVO);
	
	/**
	 * Delete Progress Report and child table values
	 * @param progressReportId
	 * @return
	 */
	public String deleteProgressReportDetails(Integer progressReportId);

	String copyKpiIntoProgressReport(String result);
}
