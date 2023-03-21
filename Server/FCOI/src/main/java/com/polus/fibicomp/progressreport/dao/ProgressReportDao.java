package com.polus.fibicomp.progressreport.dao;

import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Map;
import com.polus.fibicomp.award.pojo.ReportClass;
import org.springframework.stereotype.Service;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAchievement;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAttachment;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportKPISummary;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportMilestone;
import com.polus.fibicomp.progressreport.pojo.KPIManpowerDevelopmentCurrentStatus;
import com.polus.fibicomp.progressreport.pojo.KPIPublicationStatus;
import com.polus.fibicomp.progressreport.pojo.KPITechnologyDisclosureStatus;
import com.polus.fibicomp.progressreport.pojo.ProgressReportAchievementType;
import com.polus.fibicomp.progressreport.pojo.ProgressReportAttachmentType;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICashFunding;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICollaborationProjects;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICompetitiveGrants;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIConferencePresentation;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIGrantSpecific;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIHealthSpecificOutcomes;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIImpactPublications;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIInkindContributions;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPILicenses;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIManpowerDevelopment;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIMapping;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPatents;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPostDocsEmployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPISuccessfulStartups;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologiesDeployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologyDisclosure;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIUndergraduateStudent;
import com.polus.fibicomp.progressreport.pojo.ProgressReportStatus;

@Service(value = "progressReportDao")
public interface ProgressReportDao {

	public void saveOrUpdateProgressReport(AwardProgressReport awardProgressReport);

	/**
	 * @param progressReportId
	 * @return progress report
	 */
	public AwardProgressReport loadAwardProgressReport(Integer progressReportId);

	/**
	 * @param mileStoneNumbers
	 * @param awardId
	 * @return set of milestone numbers not in Progress report
	 */
	public List<String> getMileStoneNumbersNotInPR(List<String> mileStoneNumbers, Integer awardId);

	/**
	 * @param progressReportId
	 * @return progress report milestone
	 */
	public List<AwardProgressReportMilestone> loadProgressReportMilestone(Integer progressReportId);

	/**
	 * @param progressReportId
	 * @return list of award milestone
	 */
	public List<AwardMileStone> getAwardMilestoneByParam(Integer progressReportId);

	/**
	 * @param awardProgressReportMilestone
	 */
	public void saveOrUpdateProgressReportMilestone(AwardProgressReportMilestone awardProgressReportMilestone);

	/**
	 * @param progressReportId
	 * @return progress report status code
	 */
	public String getProgressReportStatusCode(Integer progressReportId);

	/**
	 * @param progressReportId
	 * @param progressReportStatusCodeApprovalInProgress
	 */
	public void updateProgressReportStatus(Integer progressReportId, String progressReportStatusCodeApprovalInProgress,Timestamp funderApprovalDate);

	/**
	 * @param claimStatusCodeApproved
	 * @return ProgressReportStatus
	 */
	public ProgressReportStatus getProgressReportStatus(String claimStatusCodeApproved);

	/**
	 * @return String next progress report number
	 */
	public String progressReportNextValue();

	/**
	 * @param attachment
	 */
	public void saveOrUpdateProgressReportAttachment(AwardProgressReportAttachment attachment);

	/**
	 * @return next document id
	 */
	public Integer generateDocumentId();

	/**
	 * @param documentId
	 * @param progressReportId
	 * @param versionNumber
	 */
	public void archiveOldAttachmentVersion(Integer documentId, Integer progressReportId, int versionNumber);

	/**
	 * @param awardProgressReportAttachment
	 */
	public void deleteProgressReportAttachment(AwardProgressReportAttachment awardProgressReportAttachment);

	/**
	 * @param progressReportId
	 * @return list of active attachments
	 */
	public List<AwardProgressReportAttachment> loadProgressReportAttachments(Integer progressReportId, Boolean isPersonHasPermission);

	/**
	 * @param progressReportId
	 * @param documentId
	 * @param isPersonHasPermission 
	 * @return list of version attachment of a documentId
	 */
	public List<AwardProgressReportAttachment> loadProgressReportAttachmentVersions(Integer progressReportId, Integer documentId, Boolean isPersonHasPermission);

	/**
	 * @return list of attachment types
	 */
	public List<ProgressReportAttachmentType> loadProgressReportAttachmentTypes();

	/**
	 * @param attachmentId
	 * @return progress report attachment
	 */
	public AwardProgressReportAttachment getProgressReportAttachment(Integer attachmentId);

	/**
	 * @return List<ProgressReportAchievementType>
	 */
	public List<ProgressReportAchievementType> loadProgressReportAchievementType();

	/**
	 * @param awardProgressReportAchievement
	 */
	public void saveOrUpdateProgressReportAchievements(AwardProgressReportAchievement awardProgressReportAchievement);

	/**
	 * @return List<ProgressReportKPIMapping>
	 */
	public List<ProgressReportKPIMapping> getKPImapping();

	/**
	 * @param progressReportId
	 * @return List<AwardProgressReportKPISummary>
	 */
	public List<AwardProgressReportKPISummary> loadProgressReportKPISummary(Integer progressReportId);

	/**
	 * @param kpiCriteriaCode
	 * @param awardId
	 * @return List<AwardKPICriteria>
	 */
	public List<AwardKPICriteria> getAwardKPINotInPR(List<String> kpiCriteriaCode, Integer awardId);

	/**
	 * @param tableName
	 * @param kpiSummaryId
	 * @return list of summary details
	 */
	public List<?> getSummaryDetail(String tableName, Integer kpiSummaryId);

	/**
	 * @param summaryDetail
	 */
	public void saveOrUpdateKPISummaryDetail(Object summaryDetail);

	/**
	 * @param entity
	 */
	public void deleteKPISummaryDetail(Object entity);

	/**
	 * @return list of KPIPublicationStatus
	 */
	public List<KPIPublicationStatus> getKPIPublicationStatus();

	/**
	 * @return list of KPITechnologyDisclosureStatus
	 */
	public List<KPITechnologyDisclosureStatus> getKPITechnologyDisclosureStatus();

	/**
	 * @param awardId
	 * @return List<AwardKPICriteria>
	 */
	public List<AwardKPICriteria> getfetchAllAwardKPICriterias(Integer awardId);

	/**
	 * @param progressReportId
	 * @return List<Object[]> which contains achieved value history
	 */
	public List<Object[]> getKPICriteriaAchievedValueHistory(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param awardNumber
	 * @return List<String> of progressReportNumber
	 */
	public List<String> getAllProgressReportNumberOfAward(String awardNumber);

	/**
	 * @param awardNumber
	 * @return List<AwardProgressReport> for given awardNumber
	 */
	public List<AwardProgressReport> loadProgressReportForAward(String awardNumber);

	/**
	 * @return List<KPIManpowerDevelopmentCurrentStatus>
	 */
	public List<KPIManpowerDevelopmentCurrentStatus> getKPIManpowerDevelopmentCurrentStatus();

	/**
	 * @return Date - reporting period start date
	 * @param awardProgressReport
	 */
	public Timestamp getReportPeriodStartDate(AwardProgressReport awardProgressReport);

	/**
	 * @return List<AwardProgressReport> - list of award progress report
	 * @param trackingId
	 */
	public List<AwardProgressReport> getAllProgressReport(List<Integer> trackingId);

	/**
	 * This method is used to get ProgressReportKPIImpactPublications details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIImpactPublications> getProgressReportKPIImpactPublicationsBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPICollaborationProjects details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPICollaborationProjects> getProgressReportKPICollaborationProjectsBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPITechnologyDisclosure details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPITechnologyDisclosure> getProgressReportKPITechnologyDisclosureBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIManpowerDevelopment details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIManpowerDevelopment> getProgressReportKPIManpowerDevelopmentBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIUndergraduateStudent details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIUndergraduateStudent> getProgressReportKPIUndergraduateStudentBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIConferencePresentation details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIConferencePresentation> getProgressReportKPIConferencePresentationBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPICompetitiveGrants details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPICompetitiveGrants> getProgressReportKPICompetitiveGrantsBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIPatents details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIPatents> getProgressReportKPIPatentsBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPILicenses details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPILicenses> getProgressReportKPILicensesBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPISuccessfulStartups details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPISuccessfulStartups> getProgressReportKPISuccessfulStartupsBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIHealthSpecificOutcomes details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIHealthSpecificOutcomes> getProgressReportKPIHealthSpecificOutcomesBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIPostDocsEmployed details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIPostDocsEmployed> getProgressReportKPIPostDocsEmployedBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIGrantSpecific details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIGrantSpecific> getProgressReportKPIGrantSpecificBasedOnCriteriaBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPICashFunding details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPICashFunding> getProgressReportKPICashFundingBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPIInkindContributions details
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPIInkindContributions> getProgressReportKPIInkindContributionsBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * This method is used to get ProgressReportKPITechnologiesDeployed details.
	 * @param kpiSummaryId
	 * @return
	 */
	public List<ProgressReportKPITechnologiesDeployed> getProgressReportKPITechnologiesDeployedBasedOnCriteria(Integer kpiSummaryId);

	/**
	 * @param awardNumber
	 * @param dueDate 
	 * @param reportClassCode 
	 * @param progressReportId
	 * @return List<Object>
	 */
	public List<Map<Object, Object>> getAllProgressReportNumberAndDueDateOfAward(Integer progressReportId);

	/**
	 * @param progressReportId
	 * @param dueDate
	 * @param awardNumber				 					
	 * @param reportClassCode 
	 */
	public void updateReportTracking(Integer progressReportId, Date dueDate, String awardNumber, String reportClassCode);

	/**
	 * @param achivementDescription
	 * @param progressReportId
	 * @param awardId
	 * @return AwardProgressReportAchievement
	 */
	public AwardProgressReportAchievement checkProgressReportAchievementExist(String achivementDescription, Integer progressReportId, Integer awardId);

	/**
	 * @param progressReportId
	 * @param awardId
	 * @param startDate
	 * @param endDate
	 * @param milestone
	 */
	public AwardProgressReportMilestone checkProgressReportMilestoneExist(Integer progressReportId, Integer awardId, Timestamp startDate, Timestamp endDate, String milestone);

	/**
	 * @param status
	 */
	public String getProgressMilestoneStatus(String status);

	/**
	 * @param description
	 */
	public String getKpiCriteriaMappingDetail(String description);

	/**
	 * @param awardId
	 * @param progressReportId
	 * @param description
	 */
	public AwardProgressReportKPISummary getKPISummaryDetailByParam(Integer awardId, Integer progressReportId, String description);

	/**
	 * @param status
	 */
	public String getManpowerCurrentStatus(String status);

	/**
	 * @param sponsorName
	 */
	public String getsponsorCodeBySponsorName(String sponsorName);

	/**
	 * @param awardId
	 * @param progressReportId
	 */
	public List<String> getAwardKpiCriterias(Integer awardId, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiImpactPublicationId
	 */
	public void deleteKPIImpactPublications(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiImpactPublicationId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiCollaborationProjectId
	 */
	public void deleteKpiCollaborationProjects(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiCollaborationProjectId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiTechnologyDisclosureId
	 */
	public void deleteKpiTechnologyDisclosure(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiTechnologyDisclosureId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiUnderGraduateStudId
	 */
	public void deleteKPIUndergraduateStudent(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiUnderGraduateStudId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiConferencePresentationId
	 */
	public void deleteKpiConferencePresentation(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiConferencePresentationId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiCompetitiveGrantsId
	 */
	public void deleteKpiCompetitiveGrants(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiCompetitiveGrantsId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiPatentId
	 */
	public void deleteProgressReportKPIPatents(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiPatentId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiLicenseId
	 */
	public void deleteProgressReportKpiLicenses(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiLicenseId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiSuccessfulStartupId
	 */
	public void deleteProgressReportKPISuccessfulStartups(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiSuccessfulStartupId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiHealthSpecificOutcomeId
	 */
	public void deleteProgressReportKPIHealthSpecificOutcomes(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiHealthSpecificOutcomeId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiPostDocsEmployedId
	 */
	public void deleteProgressReportKPIPostDocsEmployed(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiPostDocsEmployedId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiCashFundingId
	 */
	public void deleteProgressReportKPICashFunding(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiCashFundingId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiInkindContributionId
	 */
	public void deleteProgressReportKPIInkindContributions(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiInkindContributionId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 * @param kpiTechnologiesDeployedId
	 */
	public void deleteProgressReportKPITechnologiesDeployed(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiTechnologiesDeployedId);

	/**
	 * @param awardNumber
	 * @return list of orcid work details
	 */
	public List<Object[]> getOrcidDataForProgressReportKPI(String awardNumber);

	/**
	 * @param awardNumber
	 * @param reportClassCode 
	 * @return progress report start date
	 */
	public Date getReportPeriodStartDateByAwardNumber(String awardNumber);

	/**
	 * @param awardNumber
	 * @return boolean true if unproved progress report exist for an awardNumber
	 */
	public Boolean checkInprogressProgressReportExist(String awardNumber);

	/**
	 * @param awardNumber
	 * @param reportClassCode 
	 * @return latest approved progress report of an award
	 */
	public AwardProgressReport getLatestApprovedProgessReport(String awardNumber, String reportClassCode);

	/**
	 * @param progressReportId
	 */
	public void updateProgressReportUpdatedTimeAndUser(Integer progressReportId);

	/**
	 * @param progressReportId
	 * @param sectionType
	 * @return AwardProgressReportAchievement type names
	 */
	public List<String> fetchAwardProgressReportAchievementTypesById(Integer progressReportId, String sectionType);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	public void deleteKpiProgressReportKPIManpowerDevelopment(Integer progressReportId, String kpiCriteriaTypeCode, Integer kpiManpowerDevelopmentId);

	/**
	 * @param awardId
	 * @return leadUnitNumber
	 */
	public String getAwardLeadUnit(Integer awardId);

	/**
	 * @param progressReportId
	 * @return report class code
	 */
	public String getReportClassCodeByProgressReportId(Integer progressReportId);

	/**
	 * @param field -field name
	 * @param tableName - tableName
	 * @return data length
	 */
	public Integer getDataTypeLengthForDescription(String field, String tableName);

	/**
	 * @param awardNumber
	 * @return date
	 */
	public Timestamp getAwardStartDateByAwardNumber(String awardNumber);

	/**
	 * @param kpiSummaryId
	 * @param kpiCriteriaCode 
	 * @return List<Integer>
	 */
	public List<Integer> getPutCodeForOrcidInImpactPublications(Integer kpiSummaryId, String kpiCriteriaCode);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	public void updateKPIAchievedCountImpactPublication(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountCollaborationProjects(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountTechnologyDisclosure(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountManpowerDevelopment(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountUndergraduateStudent(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountConferencePresentation(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountCompetitiveGrants(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountPatents(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountLicenses(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountSuccessfulStartups(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountHealthSpecificOutcomes(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountPostDocsEmployed(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountPostGrantSpecific(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountCashFunding(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountInkindContributions(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param kpiCriteriaTypeCode
	 * @param progressReportId
	 */
	void updateKPIAchievedCountTechnologiesDeployed(String kpiCriteriaTypeCode, Integer progressReportId);

	/**
	 * @param tableName
	 * @param id
	 */
	Object getProgressReportKPIDetailById(String tableName, Integer id);

	/**
	 * @param progressReportId
	 * @param reportStartDate
	 * @param reportEndDate
	 * @param title
	 */
	void updateProgressReportDates(Integer progressReportId, Timestamp reportStartDate, Timestamp reportEndDate, String title);

	/**
	 * @param progressReportId
	 * @return List<AwardProgressReportKPISummary>
	 */
	List<AwardProgressReportKPISummary> getProgressReportKPISummaryById(Integer progressReportId);

	/**
	 * To delete parent table
	 * @param progressReportId
	 */
	public void deleteAwardProgressReport(Integer progressReportId);

	/**
	 * To deleteKPI summary table values
	 * @param progressReportId
	 */
	public void deleteAwardProgressReportKPISummary(Integer progressReportId);

	/**
	 * To deletAward Progress Report Milestone table values
	 * @param progressReportId
	 */
	public void deleteAwardProgressReportMilestone(Integer progressReportId);

	/**
	 * 
	 * @param progressReportId
	 */
	public void deleteAwardProgressReportAchievement(Integer progressReportId);

	/**
	 * 
	 * @param progressReportId
	 */
	public void deleteAwardProgressReportAttachment(Integer progressReportId);

	/**
	 * 
	 * @param progressReportId
	 */
	public void removeAwardReportTrackingProgressReportId(Integer progressReportId);

	/**
	 * 
	 * @param progressReportId
	 * @return
	 */
	public List<AwardProgressReportKPISummary> getProgressReportKPISummaryDetailsById(Integer progressReportId);

	/**
	 * @param awardId
	 * @param dueDate
	 * @param reportClassCode 
	 * @return return default reporting period for that particular due date
	 */
	public  Object[] getProgressReportReportingPeriod(Integer awardId, Date dueDate, String reportClassCode);

	/**
	 * Get Report Class by Report Class Code
	 *
	 * @param reportClassCode
	 * @return
	 */
	ReportClass  getReportClass(String reportClassCode);

	/**
	 * This method is used delete progress report kpi grant specific
	 * @param progressReportId
	 * @param kpiCriteriaTypeCode
	 * @param kpiGrantSpecificId 
	 * @return 
	 */
	public void deleteProgressReportKPIGrantSpecific(Integer progressReportId, String kpiCriteriaTypeCode,
			Integer kpiGrantSpecificId);

	/**
	 * This method is used for updateProgressReportSubmitUser
	 * @param progressReportId
	 */
	public void updateProgressReportSubmitUser(Integer progressReportId);

}
