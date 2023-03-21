package com.polus.fibicomp.progressreport.service;

import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.persistence.Column;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.claims.claimsIntegration.ics.dao.IcsDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.progressreport.dao.ProgressReportDao;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAchievement;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAttachment;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportKPISummary;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportMilestone;
import com.polus.fibicomp.progressreport.pojo.ProgressReportAchievementType;
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
import com.polus.fibicomp.progressreport.vo.ProgressReportVO;
import com.polus.fibicomp.questionnaire.dao.QuestionnaireDAO;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Service(value = "progressReportService")
@Transactional
public class ProgressReportServiceImpl implements ProgressReportService {

	protected static Logger logger = LogManager.getLogger(ProgressReportServiceImpl.class.getName());

	@Autowired
	private ProgressReportDao progressReportDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	public BusinessRuleDao businessRuleDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	public InboxService inboxService;

	@Autowired
	public InboxDao inboxDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private CommonService commonService;

	@Autowired
	public AuthorizationService authorizationService;

	@Autowired
	private PrintService printService;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private IcsDao icsDao;
	
	@Autowired
	private QuestionnaireDAO questionnaireDAO;

	private static final String KPI_DATE_FORMAT = "dd-MMM-yy";

	@Override
	public String saveOrUpdateProgressReport(ProgressReportVO progressReportVO) {
		AwardProgressReport awardProgressReport = progressReportVO.getAwardProgressReport();
		if(progressReportDao.checkInprogressProgressReportExist(awardProgressReport.getAwardNumber())) {
			progressReportVO.setStatus(false);
			progressReportVO.setMessage("A report is already in progress for this award. Hence unable to create");
			return commonDao.convertObjectToJSON(progressReportVO);
		}
		Integer awardId = awardProgressReport.getAwardId();
		AwardProgressReport latestProgressReport = progressReportDao.getLatestApprovedProgessReport(awardProgressReport.getAwardNumber(), awardProgressReport.getReportClassCode());
		progressReportVO.setStatus(true);
		if (latestProgressReport == null)
			latestProgressReport = new AwardProgressReport();
		awardProgressReport.setProgressReportNumber(createProgressReportNumber());
		progressReportVO.setLastProgressReport(latestProgressReport);
		List<AwardKPICriteria> awardKPICriteria = progressReportDao.getfetchAllAwardKPICriterias(awardProgressReport.getAwardId());
		createProgressReport(awardProgressReport, latestProgressReport, awardKPICriteria);
		awardProgressReport.setAwardId(awardId);
		Map<String, AwardKPICriteria> criteriaInAward = awardKPICriteria.stream().collect(Collectors.toMap(AwardKPICriteria :: getKpiCriteriaTypeCode, criteria -> criteria));
		latestProgressReport.getAwardProgressReportKPISummarys().stream().filter(distinctByKey(AwardProgressReportKPISummary::getKpiCriteriaTypeCode)).forEach(awardKPI-> {
			if(criteriaInAward.containsKey(awardKPI.getKpiCriteriaTypeCode())) {
				AwardProgressReportKPISummary newKPISummary = new AwardProgressReportKPISummary();
				BeanUtils.copyProperties(awardKPI, newKPISummary);
				newKPISummary.setKpiSummaryId(null);
				newKPISummary.setAwardProgressReport(awardProgressReport);
				newKPISummary.setOriginatingProgressReportId(awardProgressReport.getProgressReportId());
				newKPISummary.setTarget(criteriaInAward.get(awardKPI.getKpiCriteriaTypeCode()).getTarget());
				newKPISummary.setAchieved(BigDecimal.ZERO);
				awardProgressReport.getAwardProgressReportKPISummarys().add(newKPISummary);
			}
		});
		progressReportDao.saveOrUpdateProgressReport(awardProgressReport);
		if (awardProgressReport.getAwardReportTrackingId() != null)
			progressReportDao.updateReportTracking(awardProgressReport.getProgressReportId(), awardProgressReport.getDueDate(), awardProgressReport.getAwardNumber(), awardProgressReport.getReportClassCode());		
		awardProgressReport.setAwardId(awardId);
		progressReportVO.setAwardProgressReport(awardProgressReport);
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	@Override
	public String copyKpiIntoProgressReport(String result) {
		ObjectMapper mapper = new ObjectMapper();
		ProgressReportVO progressReportVO = new ProgressReportVO();
		try {
			progressReportVO = mapper.readValue(result, ProgressReportVO.class);
		} catch (Exception e) {
			logger.error("Exception in copyKpiIntoProgressReport object mapper {}", e.getMessage());
		} 
		Integer lastProgressReportId = progressReportVO.getLastProgressReport().getProgressReportId();
		Integer currentProgressReportId = progressReportVO.getAwardProgressReport().getProgressReportId();
		AwardProgressReport progressReport = progressReportVO.getAwardProgressReport();
		if (progressReportVO.getAwardProgressReport().getReportClassCode().equals(Constants.FINAL_REPORT_CLASS_CODE) || 
				(progressReportVO.getLastProgressReport().getProgressReportId() != null
				&& progressReportVO.getAwardProgressReport().getReportClassCode().equals(Constants.PROGRESS_REPORT_CLASS_CODE))) {
			Map<String, Integer> kpiMap  = progressReportVO.getLastProgressReport().getAwardProgressReportKPISummarys().stream()
					.filter(kpi -> kpi.getOriginatingProgressReportId().equals(lastProgressReportId))
					.filter(distinctByKey(AwardProgressReportKPISummary::getKpiCriteriaTypeCode))
					.collect(Collectors.toMap(AwardProgressReportKPISummary :: getKpiCriteriaTypeCode, AwardProgressReportKPISummary :: getKpiSummaryId));
			copyProgressReportKPIIntoNextReport(progressReportVO.getAwardProgressReport().getAwardProgressReportKPISummarys().stream()
					.filter(kpi -> kpi.getOriginatingProgressReportId().equals(currentProgressReportId))
					.filter(distinctByKey(AwardProgressReportKPISummary::getKpiCriteriaTypeCode)).collect(Collectors.toList()), kpiMap);
		}
		if (progressReportVO.getAwardProgressReport().getAwardProgressReportKPISummarys() != null && 
				!progressReportVO.getAwardProgressReport().getAwardProgressReportKPISummarys().isEmpty()) {
			List<Object[]> data = progressReportDao.getOrcidDataForProgressReportKPI(progressReportVO.getAwardProgressReport().getAwardNumber());
			progressReportVO.getAwardProgressReport().getAwardProgressReportKPISummarys().stream()
					.filter(kpi -> kpi.getOriginatingProgressReportId().equals(currentProgressReportId))
					.filter(distinctByKey(AwardProgressReportKPISummary::getKpiCriteriaTypeCode))
					.filter(summary -> summary.getKpiCategoryTypeCode() != null && summary.getKpiCategoryTypeCode().equals("1") && !data.isEmpty())
					.forEach(kpi -> insertOrcidData(progressReport, kpi, data));
		}
		return commonDao.convertObjectToJSON(progressReportVO);
	}
	
	private String createProgressReportNumber() {
		SimpleDateFormat formatter = new SimpleDateFormat("MMyyyy");
		String strDate = formatter.format(commonDao.getCurrentDate());
		int year = commonDao.getCurrentDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate().getYear();
		int month = commonDao.getCurrentDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate().getMonth().getValue();
		if (month < 4) {
			strDate = strDate.replace(Integer.toString(year), Integer.toString(year - 1));
		}
		return strDate + progressReportDao.progressReportNextValue();
	}

	private void insertOrcidData(AwardProgressReport awardProgressReport, AwardProgressReportKPISummary kpi, List<Object[]> data) {
		List<Integer> putCodes = progressReportDao.getPutCodeForOrcidInImpactPublications(kpi.getKpiSummaryId(), kpi.getKpiCriteriaTypeCode());
		if(!data.isEmpty()) {
			data.forEach(summary -> {
				if(summary[0] != null && (putCodes == null || putCodes.isEmpty()) || (!putCodes.isEmpty() && !putCodes.contains(Integer.parseInt(summary[6].toString())))) {
					ProgressReportKPIImpactPublications impactPublications = new ProgressReportKPIImpactPublications();
					impactPublications.setKpiCriteriaCode(kpi.getKpiCriteriaTypeCode());
					impactPublications.setTitleOfArticle(summary[0] != null ? summary[0].toString() : null);
					impactPublications.setJournalName(summary[1] != null ? summary[1].toString() : null);
					impactPublications.setYear(summary[2] != null ? summary[2].toString() : null);
					if(summary[5] != null && summary[5].toString().equals("2"))
						impactPublications.setPublisher(summary[3] != null ? summary[3].toString() : null);
					impactPublications.setAuthorName(summary[4] != null ? summary[4].toString() : null);
					impactPublications.setKpiSummaryId(kpi.getKpiSummaryId());
					impactPublications.setPublicationStatusCode("1");
					impactPublications.setProgressReportId(awardProgressReport.getProgressReportId());
					impactPublications.setPutCode(Integer.parseInt(summary[6].toString()));
					progressReportDao.saveOrUpdateKPISummaryDetail(impactPublications);
				}						
			});
			progressReportDao.updateKPIAchievedCountImpactPublication(kpi.getKpiCriteriaTypeCode(), awardProgressReport.getProgressReportId());
		}
	}

	private void copyProgressReportKPIIntoNextReport(List<AwardProgressReportKPISummary> awardProgressReportKPI, Map<String, Integer> kpiMap) {
		awardProgressReportKPI.forEach(kpiSummary -> {
			switch (kpiSummary.getSectionCode()) {
			case "1":
				List<ProgressReportKPIImpactPublications> progressReportKPIImpactPublications = progressReportDao.getProgressReportKPIImpactPublicationsBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIImpactPublications.forEach(impactPublications -> {
					commonDao.detachEntityFromSession(impactPublications);
					impactPublications.setKpiImpactPublicationId(null);
					impactPublications.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					impactPublications.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(impactPublications);
				});
				progressReportDao.updateKPIAchievedCountImpactPublication(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "2":
				List<ProgressReportKPICollaborationProjects> progressReportKPICollaborationProjects = progressReportDao.getProgressReportKPICollaborationProjectsBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPICollaborationProjects.forEach(collaborationProjects -> {
					commonDao.detachEntityFromSession(collaborationProjects);
					collaborationProjects.setKpiCollaborationProjectId(null);
					collaborationProjects.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					collaborationProjects.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(collaborationProjects);
				});
				progressReportDao.updateKPIAchievedCountCollaborationProjects(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "3":
				List<ProgressReportKPITechnologyDisclosure> progressReportKPITechnologyDisclosure = progressReportDao.getProgressReportKPITechnologyDisclosureBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPITechnologyDisclosure.forEach(technologyDisclosure -> {
					commonDao.detachEntityFromSession(technologyDisclosure);
					technologyDisclosure.setKpiTechnologyDisclosureId(null);
					technologyDisclosure.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					technologyDisclosure.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(technologyDisclosure);
				});
				progressReportDao.updateKPIAchievedCountTechnologyDisclosure(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "4":
				List<ProgressReportKPIManpowerDevelopment> progressReportKPIManpowerDevelopments = progressReportDao.getProgressReportKPIManpowerDevelopmentBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIManpowerDevelopments.forEach(manpowerDevelopments -> {
					commonDao.detachEntityFromSession(manpowerDevelopments);
					manpowerDevelopments.setKpiManpowerDevelopmentId(null);
					manpowerDevelopments.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					manpowerDevelopments.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(manpowerDevelopments);
				});
				progressReportDao.updateKPIAchievedCountManpowerDevelopment(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "5":
				List<ProgressReportKPIUndergraduateStudent> progressReportKPIUndergraduateStudents = progressReportDao.getProgressReportKPIUndergraduateStudentBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIUndergraduateStudents.forEach(undergraduateStudents -> {
					commonDao.detachEntityFromSession(undergraduateStudents);
					undergraduateStudents.setKpiUnderGraduateStudId(null);
					undergraduateStudents.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					undergraduateStudents.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(undergraduateStudents);
				});
				progressReportDao.updateKPIAchievedCountUndergraduateStudent(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "6":
				List<ProgressReportKPIConferencePresentation> progressReportKPIConferencePresentations = progressReportDao.getProgressReportKPIConferencePresentationBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIConferencePresentations.forEach(conferencePresentations -> {
					commonDao.detachEntityFromSession(conferencePresentations);
					conferencePresentations.setKpiConferencePresentationId(null);
					conferencePresentations.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					conferencePresentations.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(conferencePresentations);
				});
				progressReportDao.updateKPIAchievedCountConferencePresentation(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "7":
				List<ProgressReportKPICompetitiveGrants> progressReportKPICompetitiveGrants = progressReportDao.getProgressReportKPICompetitiveGrantsBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPICompetitiveGrants.forEach(competitiveGrants -> {
					commonDao.detachEntityFromSession(competitiveGrants);
					competitiveGrants.setKpiCompetitiveGrantsId(null);
					competitiveGrants.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					competitiveGrants.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(competitiveGrants);
				});
				progressReportDao.updateKPIAchievedCountCompetitiveGrants(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "8":
				List<ProgressReportKPIPatents> progressReportKPIPatents = progressReportDao.getProgressReportKPIPatentsBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIPatents.forEach(KPIPatents -> {
					commonDao.detachEntityFromSession(KPIPatents);
					KPIPatents.setKpiPatentId(null);
					KPIPatents.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					KPIPatents.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(KPIPatents);
				});
				progressReportDao.updateKPIAchievedCountPatents(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "9":
				List<ProgressReportKPILicenses> progressReportKPILicenses = progressReportDao.getProgressReportKPILicensesBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPILicenses.forEach(KPILicenses -> {
					commonDao.detachEntityFromSession(KPILicenses);
					KPILicenses.setKpiLicenseId(null);
					KPILicenses.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					KPILicenses.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(KPILicenses);
				});
				progressReportDao.updateKPIAchievedCountLicenses(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "10":
				List<ProgressReportKPISuccessfulStartups> progressReportKPISuccessfulStartups = progressReportDao.getProgressReportKPISuccessfulStartupsBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPISuccessfulStartups.forEach(successfulStartups -> {
					commonDao.detachEntityFromSession(successfulStartups);
					successfulStartups.setKpiSuccessfulStartupId(null);
					successfulStartups.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					successfulStartups.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(successfulStartups);
				});
				progressReportDao.updateKPIAchievedCountSuccessfulStartups(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "11":
				List<ProgressReportKPIHealthSpecificOutcomes> progressReportKPIHealthSpecificOutcomes = progressReportDao.getProgressReportKPIHealthSpecificOutcomesBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIHealthSpecificOutcomes.forEach(specificOutcomes -> {
					commonDao.detachEntityFromSession(specificOutcomes);
					specificOutcomes.setKpiHealthSpecificOutcomeId(null);
					specificOutcomes.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					specificOutcomes.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(specificOutcomes);
				});
				progressReportDao.updateKPIAchievedCountHealthSpecificOutcomes(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "12":
				List<ProgressReportKPIPostDocsEmployed> progressReportKPIPostDocsEmployed = progressReportDao.getProgressReportKPIPostDocsEmployedBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIPostDocsEmployed.forEach(postDocsEmployed -> {
					commonDao.detachEntityFromSession(postDocsEmployed);
					postDocsEmployed.setKpiPostDocsEmployedId(null);
					postDocsEmployed.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					postDocsEmployed.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(postDocsEmployed);
				});
				progressReportDao.updateKPIAchievedCountPostDocsEmployed(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "13":
				List<ProgressReportKPIGrantSpecific> progressReportKPIGrantSpecific = progressReportDao.getProgressReportKPIGrantSpecificBasedOnCriteriaBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIGrantSpecific.forEach(grantSpecific -> {
					commonDao.detachEntityFromSession(grantSpecific);
					grantSpecific.setKpiGrantSpecificId(null);
					grantSpecific.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					grantSpecific.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(grantSpecific);
				});
				progressReportDao.updateKPIAchievedCountPostGrantSpecific(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "14":
				List<ProgressReportKPICashFunding> progressReportKPICashFunding = progressReportDao.getProgressReportKPICashFundingBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPICashFunding.forEach(cashFunding -> {
					commonDao.detachEntityFromSession(cashFunding);
					cashFunding.setKpiCashFundingId(null);
					cashFunding.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					cashFunding.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(cashFunding);
				});
				progressReportDao.updateKPIAchievedCountCashFunding(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "15":
				List<ProgressReportKPIInkindContributions> progressReportKPIInkindContributions = progressReportDao.getProgressReportKPIInkindContributionsBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPIInkindContributions.forEach(inkindContributions -> {
					commonDao.detachEntityFromSession(inkindContributions);
					inkindContributions.setKpiInkindContributionId(null);
					inkindContributions.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					inkindContributions.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(inkindContributions);
				});
				progressReportDao.updateKPIAchievedCountInkindContributions(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			case "16":
				List<ProgressReportKPITechnologiesDeployed> progressReportKPITechnologiesDeployed = progressReportDao.getProgressReportKPITechnologiesDeployedBasedOnCriteria(kpiMap.get(kpiSummary.getKpiCriteriaTypeCode()));
				progressReportKPITechnologiesDeployed.forEach(technologiesDeployed -> {
					commonDao.detachEntityFromSession(technologiesDeployed);
					technologiesDeployed.setKpiTechnologiesDeployedId(null);
					technologiesDeployed.setKpiSummaryId(kpiSummary.getKpiSummaryId());
					technologiesDeployed.setProgressReportId(kpiSummary.getOriginatingProgressReportId());
					progressReportDao.saveOrUpdateKPISummaryDetail(technologiesDeployed);
				});
				progressReportDao.updateKPIAchievedCountTechnologiesDeployed(kpiSummary.getKpiCriteriaTypeCode(), kpiSummary.getOriginatingProgressReportId());
				break;
			default:
				break;
			}
		});			
	}

	private void setProgressReportAchievementsDetails(AwardProgressReport awardProgressReport, AwardProgressReport latestProgressReport) {
		List<AwardProgressReportAchievement> awardProgressReportAchievement = new ArrayList<>();
		if(latestProgressReport.getAwardProgressReportAchievements() != null && !latestProgressReport.getAwardProgressReportAchievements().isEmpty()) {
			latestProgressReport.getAwardProgressReportAchievements().forEach(achievementType -> {
				AwardProgressReportAchievement achievement = new AwardProgressReportAchievement();
				BeanUtils.copyProperties(achievementType, achievement);
				achievement.setProgressReportAchievementId(null);
				achievement.setAwardProgressReport(awardProgressReport);
				awardProgressReportAchievement.add(achievement);
			});
		} else {
			List<ProgressReportAchievementType> progressReportAchievementTypes = progressReportDao.loadProgressReportAchievementType();
			progressReportAchievementTypes.forEach(achievementType -> {
				AwardProgressReportAchievement achievement = new AwardProgressReportAchievement();
				achievement.setProgressReportAchievementType(achievementType);
				achievement.setAchievementTypeCode(achievementType.getAchievementTypeCode());
				achievement.setAwardProgressReport(awardProgressReport);
				awardProgressReportAchievement.add(achievement);
			});
		}
		awardProgressReport.setAwardProgressReportAchievements(awardProgressReportAchievement);
	}

	private void createProgressReport(AwardProgressReport awardProgressReport, AwardProgressReport latestProgressReport, List<AwardKPICriteria> awardKPICriteria) {
		awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_PENDING);
		setProgressReportMilestoneOnCreate(awardProgressReport, latestProgressReport);
		setProgressReportAchievementsDetails(awardProgressReport, latestProgressReport);
		progressReportDao.saveOrUpdateProgressReport(awardProgressReport);
		setProgressReportKPIOnCreate(awardProgressReport, latestProgressReport, awardKPICriteria);
	}

	private void setProgressReportMilestoneOnCreate(AwardProgressReport awardProgressReport, AwardProgressReport latestProgressReport) {
		Set<String> mileStoneNumbers = awardDao.fetchAwardMileStonesBasedOnAwardId(awardProgressReport.getAwardId())
				.stream().map(AwardMileStone::getMilestoneNumber).collect(Collectors.toSet());
		Set<String>	latestMileStoneNumbers = latestProgressReport.getAwardProgressReportMilestones().stream().map(AwardProgressReportMilestone::getMilestoneNumber).collect(Collectors.toSet());
		List<AwardProgressReportMilestone> awardProgressReportMilestones = new ArrayList<>();
		mileStoneNumbers.forEach(mileStone -> {
			if(!latestMileStoneNumbers.contains(mileStone)) {
				AwardProgressReportMilestone awardProgressReportMilestone = new AwardProgressReportMilestone();
				awardProgressReportMilestone.setAwardId(awardProgressReport.getAwardId());
				awardProgressReportMilestone.setAwardNumber(awardProgressReport.getAwardNumber());
				awardProgressReportMilestone.setSequenceNumber(awardProgressReport.getSequenceNumber());
				awardProgressReportMilestone.setMilestoneNumber(mileStone);
				awardProgressReportMilestone.setAwardProgressReport(awardProgressReport);
				awardProgressReportMilestones.add(awardProgressReportMilestone);
			}
		});
		Set<String> addedMileStoneNumbers = awardProgressReportMilestones.stream().map(AwardProgressReportMilestone::getMilestoneNumber).collect(Collectors.toSet());
		latestProgressReport.getAwardProgressReportMilestones().forEach(milestone -> {
			if(!addedMileStoneNumbers.contains(milestone.getMilestoneNumber())) {
				AwardProgressReportMilestone newMilestone = new AwardProgressReportMilestone();
				BeanUtils.copyProperties(milestone, newMilestone);
				newMilestone.setProgressReportMilestoneId(null);
				newMilestone.setAwardProgressReport(awardProgressReport);
				awardProgressReportMilestones.add(newMilestone);
			}
		});
		awardProgressReport.setAwardProgressReportMilestones(awardProgressReportMilestones);
	}

	private void setProgressReportKPIOnCreate(AwardProgressReport awardProgressReport, AwardProgressReport latestProgressReport, List<AwardKPICriteria> awardKPICriterias) {
		Map<String, ProgressReportKPIMapping> kpiMapping = progressReportDao.getKPImapping().stream()
				.collect(Collectors.toMap(ProgressReportKPIMapping :: getKpiCriteriaTypeCode, mapping -> mapping));
		Map<String, AwardKPICriteria> criteriaInAward = awardKPICriterias.stream().collect(Collectors.toMap(AwardKPICriteria :: getKpiCriteriaTypeCode, criteria -> criteria));
		Set<String> latestKPICriteria = latestProgressReport.getAwardProgressReportKPISummarys().stream().map(AwardProgressReportKPISummary::getKpiCriteriaTypeCode).collect(Collectors.toSet());
		List<AwardProgressReportKPISummary> awardProgressReportKPI = new ArrayList<>();
		Set<Integer> prevProgressReportIds = latestProgressReport.getAwardProgressReportKPISummarys().stream().map(AwardProgressReportKPISummary::getOriginatingProgressReportId).collect(Collectors.toSet());
		prevProgressReportIds.add(awardProgressReport.getProgressReportId());
		prevProgressReportIds.forEach(prevProgressReportId -> {
			awardKPICriterias.forEach(awardKPICriteria -> {
				if (kpiMapping.containsKey(awardKPICriteria.getKpiCriteriaTypeCode()) && !latestKPICriteria.contains(awardKPICriteria.getKpiCriteriaTypeCode())) {
					AwardProgressReportKPISummary kpiSummary = new AwardProgressReportKPISummary();
					kpiSummary.setAwardId(awardProgressReport.getAwardId());
					kpiSummary.setAwardNumber(awardProgressReport.getAwardNumber());
					kpiSummary.setSequenceNumber(awardProgressReport.getSequenceNumber());
					kpiSummary.setOriginatingProgressReportId(prevProgressReportId);
					kpiSummary.setKpiCriteriaTypeCode(awardKPICriteria.getKpiCriteriaTypeCode());
					kpiSummary.setKpiCategoryTypeCode(awardKPICriteria.getAwardKPI().getKpiTypeCode());
					kpiSummary.setSectionCode(kpiMapping.get(awardKPICriteria.getKpiCriteriaTypeCode()).getSectionCode());
					kpiSummary.setTarget(awardKPICriteria.getTarget() != null ? awardKPICriteria.getTarget() : null);
					kpiSummary.setAwardProgressReport(awardProgressReport);
					awardProgressReportKPI.add(kpiSummary);
				}
			});
		});
		Set<String> newKPICriteria = awardProgressReportKPI.stream().map(AwardProgressReportKPISummary::getKpiCriteriaTypeCode).collect(Collectors.toSet());
		latestProgressReport.getAwardProgressReportKPISummarys().forEach(awardKPI-> {
			if(!newKPICriteria.contains(awardKPI.getKpiCriteriaTypeCode()) && criteriaInAward.containsKey(awardKPI.getKpiCriteriaTypeCode())) {
				AwardProgressReportKPISummary newKPISummary = new AwardProgressReportKPISummary();
				BeanUtils.copyProperties(awardKPI, newKPISummary);
				newKPISummary.setKpiSummaryId(null);
				newKPISummary.setAwardProgressReport(awardProgressReport);
				newKPISummary.setTarget(criteriaInAward.get(awardKPI.getKpiCriteriaTypeCode()).getTarget());
				awardProgressReportKPI.add(newKPISummary);
			}
		});
		awardProgressReport.setAwardProgressReportKPISummarys(awardProgressReportKPI);
	}

	private static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor) {
		Set<Object> seen = ConcurrentHashMap.newKeySet();
		return t -> seen.add(keyExtractor.apply(t));
	}

	@Override
	public String loadAwardProgressReport(Integer progressReportId, Boolean progressReportImported) {
		ProgressReportVO vo = new ProgressReportVO();
		AwardProgressReport awardProgressReport = loadAwardProgressReportDetails(progressReportId);
		setProgressReportKPI(awardProgressReport);
		setProgressReportMilestoneDetails(awardProgressReport);
		vo.setProgressReportId(progressReportId);
		vo.setWorkflowFeedbackTypes(workflowDao.fetchWorkflowFeedbackTypeBasedOnParam(Constants.PROGRESS_REPORT_MODULE_CODE, Constants.PROGRESS_REPORT_SUBMODULE_CODE));
		vo.setAwardProgressReport(awardProgressReport);
		loadProgressReportWorkflowDetails(vo);
		AwardProgressReport finalAwardProgressReport = new AwardProgressReport();
		BeanUtils.copyProperties(awardProgressReport, finalAwardProgressReport);
		if(finalAwardProgressReport.getAwardProgressReportKPISummarys() != null)
			finalAwardProgressReport.setAwardProgressReportKPISummarys(finalAwardProgressReport.getAwardProgressReportKPISummarys().stream()
					.filter(summary -> summary.getOriginatingProgressReportId().equals(progressReportId))
					.sorted(Comparator.comparing(AwardProgressReportKPISummary::getKpiCategoryTypeCode)
					.thenComparing(summary -> summary.getKpiCriteriaType().getDescription())).collect(Collectors.toList()));
		Award award = finalAwardProgressReport.getAward();
		if (award.getSponsor() != null) {
			award.setSponsorName(commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
		}
		finalAwardProgressReport.setAward(award);
		vo.setAwardProgressReport(finalAwardProgressReport);
		vo.setProgressReportImported(progressReportImported);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public AwardProgressReport loadAwardProgressReportDetails(Integer progressReportId) {
		AwardProgressReport awardProgressReport = progressReportDao.loadAwardProgressReport(progressReportId);
		awardProgressReport.setCreatedPersonName(personDao.getUserFullNameByUserName(awardProgressReport.getCreateUser()));
		awardProgressReport.setUpdatedPersonName(personDao.getUserFullNameByUserName(awardProgressReport.getUpdateUser()));
		awardProgressReport.setLastReportEndDate(progressReportDao.getReportPeriodStartDate(awardProgressReport));
		awardProgressReport.setCreatedPersonId(personDao.getPersonIdByUserName(awardProgressReport.getCreateUser()));
		return awardProgressReport;
	}

	private void setProgressReportKPI(AwardProgressReport awardProgressReport) {
		List<AwardProgressReportKPISummary> awardProgressReportKPISummary = awardProgressReport.getAwardProgressReportKPISummarys();
		if (!awardProgressReportKPISummary.isEmpty() && awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_PENDING)) {
			List<String> kpiCriteriaCode = awardProgressReportKPISummary.stream().map(AwardProgressReportKPISummary::getKpiCriteriaTypeCode).collect(Collectors.toList());	
			Map<String, AwardProgressReportKPISummary> mapPrKPICriteria = awardProgressReportKPISummary.stream()
					.filter(summary -> summary.getOriginatingProgressReportId().equals(awardProgressReport.getProgressReportId()))
					.collect(Collectors.toMap(AwardProgressReportKPISummary :: getKpiCriteriaTypeCode, criteria -> criteria));
			List<AwardKPICriteria> kpiCriteriaNotInPR = progressReportDao.getfetchAllAwardKPICriterias(awardProgressReport.getAwardId());		
			Map<String, ProgressReportKPIMapping> kpiMapping = progressReportDao.getKPImapping().stream().collect(Collectors.toMap(ProgressReportKPIMapping :: getKpiCriteriaTypeCode, criteria -> criteria));
			Set<String>	newKpiCriteriaTypeCodes = new HashSet<>();
			Set<Integer> prevProgressReportIds = awardProgressReportKPISummary.stream().map(AwardProgressReportKPISummary::getOriginatingProgressReportId).collect(Collectors.toSet());
			prevProgressReportIds.forEach(previousProgressReportId -> {
				kpiCriteriaNotInPR.forEach(awardKPI -> {
					if(!kpiCriteriaCode.contains(awardKPI.getKpiCriteriaTypeCode())) {
						AwardProgressReportKPISummary kpiSummary = new AwardProgressReportKPISummary();
						kpiSummary.setAwardId(awardProgressReport.getAwardId());
						kpiSummary.setAwardNumber(awardProgressReport.getAwardNumber());
						kpiSummary.setSequenceNumber(awardProgressReport.getSequenceNumber());
						kpiSummary.setKpiType(awardKPI.getAwardKPI().getKpiType());
						kpiSummary.setKpiCriteriaType(awardKPI.getKpiCriteriaType());
						kpiSummary.setKpiCriteriaTypeCode(awardKPI.getKpiCriteriaTypeCode());
						kpiSummary.setKpiCategoryTypeCode(awardKPI.getAwardKPI().getKpiTypeCode());
						kpiSummary.setSectionCode(kpiMapping.get(awardKPI.getKpiCriteriaTypeCode()).getSectionCode());
						kpiSummary.setTarget(awardKPI.getTarget() != null ? awardKPI.getTarget() : null);
						kpiSummary.setAwardProgressReport(awardProgressReport);
						kpiSummary.setOriginatingProgressReportId(previousProgressReportId);
						awardProgressReport.getAwardProgressReportKPISummarys().add(kpiSummary);
						newKpiCriteriaTypeCodes.add(awardKPI.getKpiCriteriaTypeCode());
					} else {
						AwardProgressReportKPISummary updatedSummary = mapPrKPICriteria.get(awardKPI.getKpiCriteriaTypeCode());
						awardProgressReport.getAwardProgressReportKPISummarys().remove(updatedSummary);
						updatedSummary.setTarget(awardKPI.getTarget());
						awardProgressReport.getAwardProgressReportKPISummarys().add(updatedSummary);
					}
				});
			});
			progressReportDao.saveOrUpdateProgressReport(awardProgressReport);			
			List<Object[]> data = progressReportDao.getOrcidDataForProgressReportKPI(awardProgressReport.getAwardNumber());		
			awardProgressReport.getAwardProgressReportKPISummarys().stream()
			.filter(summary -> summary.getKpiCategoryTypeCode() != null && summary.getKpiCategoryTypeCode().equals("1") && !data.isEmpty() && newKpiCriteriaTypeCodes.contains(summary.getKpiCriteriaTypeCode()))
			.forEach(kpi -> insertOrcidData(awardProgressReport, kpi, data));
			
		} else if (awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_PENDING)) {
			Map<String, List<ProgressReportKPIMapping>> kpiMapping = progressReportDao.getKPImapping().stream().collect(Collectors.groupingBy(ProgressReportKPIMapping::getKpiCriteriaTypeCode));
			List<AwardKPICriteria> awardKPICriterias = progressReportDao.getfetchAllAwardKPICriterias(awardProgressReport.getAwardId());
			Set<Integer> prevProgressReportIds = new HashSet<>();
			prevProgressReportIds = progressReportDao.loadProgressReportForAward(awardProgressReport.getAwardNumber()).stream().map(AwardProgressReport::getProgressReportId).collect(Collectors.toSet());;
			prevProgressReportIds.add(awardProgressReport.getProgressReportId());
			prevProgressReportIds.forEach(progressReportId -> {
				awardKPICriterias.forEach(awardKPICriteria -> {
					if (kpiMapping.containsKey(awardKPICriteria.getKpiCriteriaTypeCode())) {
						AwardProgressReportKPISummary kpiSummary = new AwardProgressReportKPISummary();
						kpiSummary.setAwardId(awardProgressReport.getAwardId());
						kpiSummary.setAwardNumber(awardProgressReport.getAwardNumber());
						kpiSummary.setSequenceNumber(awardProgressReport.getSequenceNumber());
						kpiSummary.setKpiType(awardKPICriteria.getAwardKPI().getKpiType());
						kpiSummary.setKpiCriteriaType(awardKPICriteria.getKpiCriteriaType());
						kpiSummary.setKpiCriteriaTypeCode(awardKPICriteria.getKpiCriteriaTypeCode());
						kpiSummary.setKpiCategoryTypeCode(awardKPICriteria.getAwardKPI().getKpiTypeCode());
						kpiSummary.setSectionCode(kpiMapping.get(awardKPICriteria.getKpiCriteriaTypeCode()).get(0).getSectionCode());
						kpiSummary.setOriginatingProgressReportId(progressReportId);
						kpiSummary.setTarget(awardKPICriteria.getTarget() != null ? awardKPICriteria.getTarget() : null);
						kpiSummary.setAwardProgressReport(awardProgressReport);
						awardProgressReport.getAwardProgressReportKPISummarys().add(kpiSummary);
					}
				});
			});
			List<Object[]> data = progressReportDao.getOrcidDataForProgressReportKPI(awardProgressReport.getAwardNumber());		
			awardProgressReport.getAwardProgressReportKPISummarys().stream()
			.filter(summary -> summary.getKpiCategoryTypeCode() != null && summary.getKpiCategoryTypeCode().equals("1") && !data.isEmpty())
			.forEach(kpi -> insertOrcidData(awardProgressReport, kpi, data));
			if (!awardKPICriterias.isEmpty())
				progressReportDao.saveOrUpdateProgressReport(awardProgressReport);
		}
	}

	private void setProgressReportMilestoneDetails(AwardProgressReport awardProgressReport) {
		List<AwardProgressReportMilestone> awardMileStones = new ArrayList<>(awardProgressReport.getAwardProgressReportMilestones());
		if (!awardMileStones.isEmpty() && awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_PENDING)) {
			List<String> mileStoneNumbers = awardMileStones.stream().map(AwardProgressReportMilestone::getMilestoneNumber).collect(Collectors.toList());
			Set<String> mileStoneNumbersFromAward = awardDao.fetchAwardMileStonesBasedOnAwardId(awardProgressReport.getAwardId()).stream()
					.map(AwardMileStone::getMilestoneNumber).collect(Collectors.toSet());
			if (!mileStoneNumbers.isEmpty()) {
				mileStoneNumbersFromAward.stream().filter(mileStone-> !mileStoneNumbers.contains(mileStone)).forEach(mileStone -> {
						AwardProgressReportMilestone awardProgressReportMilestone = new AwardProgressReportMilestone();
						awardProgressReportMilestone.setAwardId(awardProgressReport.getAwardId());
						awardProgressReportMilestone.setAwardNumber(awardProgressReport.getAwardNumber());
						awardProgressReportMilestone.setSequenceNumber(awardProgressReport.getSequenceNumber());
						awardProgressReportMilestone.setMilestoneNumber(mileStone);
						awardProgressReportMilestone.setAwardProgressReport(awardProgressReport);
						awardProgressReport.getAwardProgressReportMilestones().add(awardProgressReportMilestone);
					});
					awardMileStones.stream().filter(item -> !mileStoneNumbersFromAward.contains(item.getMilestoneNumber()) 
							&& item.getActualStartMonth() == null && item.getActualEndMonth() == null 
							&& item.getMilestoneStatusCode() == null).forEach(item -> {
						awardProgressReport.getAwardProgressReportMilestones().remove(item);
					});
				progressReportDao.saveOrUpdateProgressReport(awardProgressReport);
			}
		} else if (awardProgressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_PENDING)) {
			Set<String> mileStoneNumbersFromAward = awardDao.fetchAwardMileStonesBasedOnAwardId(awardProgressReport.getAwardId()).stream()
					.map(AwardMileStone::getMilestoneNumber).collect(Collectors.toSet());
			mileStoneNumbersFromAward.forEach(mileStone -> {
				AwardProgressReportMilestone awardProgressReportMilestone = new AwardProgressReportMilestone();
				awardProgressReportMilestone.setAwardId(awardProgressReport.getAwardId());
				awardProgressReportMilestone.setAwardNumber(awardProgressReport.getAwardNumber());
				awardProgressReportMilestone.setSequenceNumber(awardProgressReport.getSequenceNumber());
				awardProgressReportMilestone.setMilestoneNumber(mileStone);
				awardProgressReportMilestone.setAwardProgressReport(awardProgressReport);
				awardProgressReport.getAwardProgressReportMilestones().add(awardProgressReportMilestone);
			});
			if (!mileStoneNumbersFromAward.isEmpty())
				progressReportDao.saveOrUpdateProgressReport(awardProgressReport);
		}
	}

	@Override
	public String loadProgressReportMilestone(Integer progressReportId) {
		ProgressReportVO vo = new ProgressReportVO();
		List<AwardProgressReportMilestone> awardProgressMileStones = progressReportDao.loadProgressReportMilestone(progressReportId);
		List<String> mileStoneNumbers = awardProgressMileStones.stream().map(AwardProgressReportMilestone::getMilestoneNumber).collect(Collectors.toList());
		if (!mileStoneNumbers.isEmpty()) {
			List<AwardMileStone> awardMilestones = progressReportDao.getAwardMilestoneByParam(progressReportId);
			Map<String, List<AwardMileStone>> collect = awardMilestones.stream().collect(Collectors.groupingBy(AwardMileStone::getMilestoneNumber));
			awardProgressMileStones.stream().filter(item -> collect.containsKey(item.getMilestoneNumber())).forEach(item -> item.setAwardMileStone(collect.get(item.getMilestoneNumber()).get(0)));
			Comparator<AwardProgressReportMilestone> comparator = Comparator.comparing(milestone -> milestone.getAwardMileStone().getStartDate());
		    Comparator<AwardProgressReportMilestone> reverseComparator = comparator.reversed().thenComparing(milestone -> milestone.getAwardMileStone().getMilestone());
			awardProgressMileStones = awardProgressMileStones.stream().filter(milestone -> milestone.getAwardMileStone() != null && milestone.getAwardMileStone().getStartDate() != null)
					.sorted(reverseComparator).collect(Collectors.toList());
		}
		vo.setProgressReportMilestoneStatuses(awardDao.getAwardMilestoneStatus());
		vo.setAwardProgressReportMilestones(awardProgressMileStones);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateProgressReportMilestone(ProgressReportVO progressReportVO) {
		progressReportDao.saveOrUpdateProgressReportMilestone(progressReportVO.getAwardProgressReportMilestone());
		progressReportDao.updateProgressReportUpdatedTimeAndUser(progressReportVO.getAwardProgressReportMilestone().getAwardProgressReport().getProgressReportId());
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	@Override
	public String submitProgressReport(Integer progressReportId) {
		ProgressReportVO progressReportVO = new ProgressReportVO();
		progressReportDao.updateProgressReportStatus(progressReportId,Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS, null);
		progressReportDao.updateProgressReportSubmitUser(progressReportId);
		AwardProgressReport awardProgressReport = loadAwardProgressReportDetails(progressReportId);
		progressReportVO.setProgressReportId(progressReportId);
		buildProgressReportWorkflow(progressReportVO);
		fetchPreviousWorkFlowsList(progressReportVO);
		if (progressReportVO.getWorkflow() == null || progressReportVO.getWorkflow().getWorkflowDetails().isEmpty()) {
			awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED);
			awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED));
			progressReportDao.updateProgressReportStatus(progressReportId,Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED, null);
			String createUserId = personDao.getPersonIdByUserName(awardProgressReport.getCreateUser());
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			commonService.setNotificationRecipients(createUserId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO,	dynamicEmailRecipients);
			sendProgressReportNotification(progressReportVO, Constants.PROGRESS_REPORT_COMPLETE_NOTIFICATION_CODE, dynamicEmailRecipients);
		} 
		inboxDao.markReadMessage(Constants.PROGRESS_REPORT_MODULE_CODE,progressReportVO.getProgressReportId().toString(), null,
				Constants.MESSAGE_TYPE_PROGRESS_REPORT_REJECT, Constants.SUBMODULE_ITEM_KEY,Constants.PROGRESS_REPORT_SUBMODULE_CODE);	
		awardProgressReport.setPrincipalInvestigator(awardProgressReport.getAward().getPrincipalInvestigator());
		awardProgressReport.setSubmitUserFullName(AuthenticatedUser.getLoginUserFullName());
		awardProgressReport.setCreatedPersonName(personDao.getUserFullNameByUserName(awardProgressReport.getCreateUser()));
		AwardProgressReport finalAwardProgressReport = new AwardProgressReport();
		BeanUtils.copyProperties(awardProgressReport, finalAwardProgressReport);
		if(finalAwardProgressReport.getAwardProgressReportKPISummarys() != null)
			finalAwardProgressReport.setAwardProgressReportKPISummarys(finalAwardProgressReport.getAwardProgressReportKPISummarys().stream()
					.filter(summary -> summary.getOriginatingProgressReportId().equals(progressReportId))
					.sorted(Comparator.comparing(AwardProgressReportKPISummary::getKpiCategoryTypeCode)
					.thenComparing(summary -> summary.getKpiCriteriaType().getDescription())).collect(Collectors.toList()));
		progressReportVO.setAwardProgressReport(finalAwardProgressReport);
		businessRuleService.evaluateAndSentNotification(Constants.PROGRESS_REPORT_MODULE_CODE,Constants.PROGRESS_REPORT_SUBMODULE_CODE, progressReportVO.getProgressReportId().toString(),
				Constants.SUBMODULE_ITEM_KEY, AuthenticatedUser.getLoginPersonId(),AuthenticatedUser.getLoginUserName(),getProgressReportPlaceholder(progressReportVO));
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	private ProgressReportVO buildProgressReportWorkflow(ProgressReportVO progressReportVO) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Constants.PROGRESS_REPORT_MODULE_CODE);
		evaluateValidationRuleVO.setSubModuleCode(Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		evaluateValidationRuleVO.setModuleItemKey(progressReportVO.getProgressReportId().toString());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		evaluateValidationRuleVO.setLogginPersonId(AuthenticatedUser.getLoginPersonId());
		evaluateValidationRuleVO.setUpdateUser(AuthenticatedUser.getLoginUserName());
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			progressReportVO.setWorkflow(workflowDao.fetchActiveWorkflowByParams(progressReportVO.getProgressReportId().toString(), Constants.PROGRESS_REPORT_MODULE_CODE,
					Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(),evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(),
				evaluateValidationRuleVO.getSubModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode());
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(),evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(),
				evaluateValidationRuleVO.getSubModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode());
		progressReportVO.setCanApproveRouting(canApproveRouting.toString());
		progressReportVO.setIsFinalApprover(isFinalApprover);
		if (progressReportVO.getWorkflow() != null) {
			List<WorkflowDetail> workflowDetails = progressReportVO.getWorkflow().getWorkflowDetails();
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workflowDetails) {
					if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
						commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(),Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
						progressReportVO.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
						progressReportVO.setMapId(workflowDetail.getMapId());
					}
				}
			}
			sendProgressReportNotification(progressReportVO, Constants.PROGRESS_REPORT_SUBMITTED_NOTIFICATION_CODE,dynamicEmailRecipients);
		}
		return progressReportVO;
	}

	@Override
	public void sendProgressReportNotification(ProgressReportVO progressReportVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.PROGRESS_REPORT_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.PROGRESS_REPORT_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(progressReportVO.getProgressReportId().toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO.setPlaceHolder(getProgressReportPlaceholder(progressReportVO));
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getProgressReportPlaceholder(ProgressReportVO progressReportVO) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{WORKFLOW_COMMENT}", progressReportVO.getApproveComment() != null ? progressReportVO.getApproveComment() : "No Comments");
		String stopName = commonService.getPlaceHolderDataForRouting(progressReportVO.getApproverStopNumber(),progressReportVO.getMapId(),progressReportVO.getWorkFlowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	private void fetchPreviousWorkFlowsList(ProgressReportVO progressReportVO) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(progressReportVO.getProgressReportId().toString(),
				Constants.PROGRESS_REPORT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
				Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			progressReportVO.setWorkflowList(workFlows);
		}
	}

	public void loadProgressReportWorkflowDetails(ProgressReportVO progressReportVO) {
		AwardProgressReport progressReport = progressReportVO.getAwardProgressReport();
		if (progressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_HOLD_FOR_FA) || 
				progressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVED)
				|| progressReport.getProgressReportStatusCode().equals(Constants.PROGRESS_REPORT_STATUS_CODE_REVISION)
				|| progressReport.getProgressReportStatusCode()
						.equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS)
				|| (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE)
						.equals(Constants.EVALUATION_MAP_ROUTING)
						&& (!progressReport.getProgressReportStatusCode()
								.equals(Constants.PROGRESS_REPORT_STATUS_CODE_PENDING)))) {
			canProgressReportTakeRoutingAction(progressReportVO);
			Workflow workflow = workflowDao.fetchActiveWorkflowByParams(
					progressReportVO.getProgressReportId().toString(), Constants.PROGRESS_REPORT_MODULE_CODE,
					Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE);
			if (workflow != null) {
				workflowService.prepareWorkflowDetails(workflow);
				progressReportVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(
						progressReportVO.getProgressReportId().toString(), Constants.PROGRESS_REPORT_MODULE_CODE,
						Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					progressReportVO.setWorkflowList(workFlows);
				}
			}
		}
		Integer canApproveRouting = businessRuleDao.canApproveRouting(progressReportVO.getProgressReportId().toString(),
				AuthenticatedUser.getLoginPersonId(), Constants.PROGRESS_REPORT_MODULE_CODE,
				Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		progressReportVO.setCanApproveRouting(canApproveRouting.toString());
		progressReportVO.setIsFinalApprover(
				businessRuleDao.workflowfinalApproval(progressReportVO.getProgressReportId().toString(),
						AuthenticatedUser.getLoginPersonId(), Constants.PROGRESS_REPORT_MODULE_CODE,
						Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_SUBMODULE_CODE));
		progressReportVO.setIsSubmit("1");
		progressReportVO.setAwardProgressReport(progressReport);
		progressReportVO.setAvailableRights(authorizationService.allDepartmentPermission(
				Constants.PROGRESS_REPORT_MODULE_CODE, AuthenticatedUser.getLoginPersonId(),
				progressReport.getAward().getLeadUnitNumber(), progressReportVO.getProgressReportId()));
	}

	@Override
	public void canProgressReportTakeRoutingAction(ProgressReportVO progressReportVO) {
		String progressReportStatusCode = progressReportDao
				.getProgressReportStatusCode(progressReportVO.getProgressReportId());
		Workflow workflow = progressReportVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(progressReportVO.getProgressReportId().toString(),
					Constants.PROGRESS_REPORT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
					Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		}
		if (workflow != null) {
			progressReportVO.getAwardProgressReport().setPrincipalInvestigator(
					progressReportVO.getAwardProgressReport().getAward().getPrincipalInvestigator());
			progressReportVO.getAwardProgressReport()
					.setSubmitUserFullName(personDao.getPersonFullNameByPersonId(workflow.getWorkflowStartPerson()));
			progressReportVO.getAwardProgressReport().setCreatedPersonName(
					personDao.getUserFullNameByUserName(progressReportVO.getAwardProgressReport().getCreateUser()));
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(),
					maxApprovalStopNumber);
			for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
				if (finalWorkflowDetail.getApproverPersonId().equals(AuthenticatedUser.getLoginPersonId())
						|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
					progressReportVO.setFinalApprover(true);
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				if (progressReportStatusCode.equals(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (workflowDetail.getApproverPersonId().equals(AuthenticatedUser.getLoginPersonId())) {
							if (workflowDetail.getApprovalStatusCode()
									.equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
								progressReportVO.setIsApproved(true);
							} else {
								progressReportVO.setIsApproved(false);
							}
							progressReportVO.setIsApprover(true);
						}
					}
				}
			}
		}
	}

	@Override
	public String saveOrUpdateProgressReportAttachment(MultipartFile[] files, String formDataJson) {
		ProgressReportVO progressReportVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			progressReportVO = mapper.readValue(formDataJson, ProgressReportVO.class);
			switch (progressReportVO.getActionType()) {
			case Constants.acTypeInsert:
				addNewProgressReportAttachment(files, progressReportVO.getAwardProgressReportAttachments());
				break;
			case Constants.acTypeUpdate:
				progressReportDao.saveOrUpdateProgressReportAttachment(progressReportVO.getAwardProgressReportAttachments().get(0));
				break;
			case Constants.acTypeDelete:
				commonDao.deleteFileData(commonDao.getFileDataById(progressReportVO.getAwardProgressReportAttachments().get(0).getFileDataId()));
				progressReportDao.deleteProgressReportAttachment(progressReportVO.getAwardProgressReportAttachments().get(0));
				break;
			case Constants.acTypeReplace:
				replaceProgressReportAttachment(files, progressReportVO.getAwardProgressReportAttachments().get(0));
				break;
			default:
				break;
			}
		} catch (Exception e) {
			logger.error("error in saveOrUpdateProgressReportAttachment : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	private void replaceProgressReportAttachment(MultipartFile[] files, AwardProgressReportAttachment progressReportAttachment) throws IOException {
		progressReportAttachment.setProgressReportAttachmentId(null);
		progressReportAttachment.setVersionNumber(progressReportAttachment.getVersionNumber() + 1);
		progressReportAttachment.setDocumentStatusCode("1");
		FileData fileData = new FileData();
		fileData.setAttachment(files[0].getBytes());
		fileData = commonDao.saveFileData(fileData);
		progressReportAttachment.setFileDataId(fileData.getFileDataId());
		progressReportDao.saveOrUpdateProgressReportAttachment(progressReportAttachment);
		progressReportDao.archiveOldAttachmentVersion(progressReportAttachment.getDocumentId(),
				progressReportAttachment.getProgressReportId(), progressReportAttachment.getVersionNumber() - 1);
	}

	private void addNewProgressReportAttachment(MultipartFile[] files, List<AwardProgressReportAttachment> attachments)
			throws IOException {
		Integer documentId = progressReportDao.generateDocumentId();
		int i = 0;
		while (files.length > i) {
			for (AwardProgressReportAttachment attachment : attachments) {
				documentId = documentId + 1;
				attachment.setFileName(files[i].getOriginalFilename());
				attachment.setMimeType(files[i].getContentType());
				attachment.setVersionNumber(1);
				attachment.setDocumentStatusCode("1");
				attachment.setDocumentId(documentId);
				FileData fileData = new FileData();
				fileData.setAttachment(files[i].getBytes());
				fileData = commonDao.saveFileData(fileData);
				attachment.setFileDataId(fileData.getFileDataId());
				progressReportDao.saveOrUpdateProgressReportAttachment(attachment);
				i++;
			}
		}
	}

	@Override
	public String loadProgressReportAttachments(ProgressReportVO progressReportVO) {
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.VIEW_CONFIDENTIAL_PROGRESS_REPORT_ATTACHMENTS_RIGHT);
		Boolean isPersonHasPermission = commonDao.checkPersonHasRightInModule(Constants.AWARD_MODULE_CODE, progressReportVO.getAwardId(), rightNames, AuthenticatedUser.getLoginPersonId());
		if(Boolean.FALSE.equals(isPersonHasPermission)) {
			isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_CONFIDENTIAL_PROGRESS_REPORT_ATTACHMENTS_RIGHT, progressReportVO.getAwardLeadUnitNumber());
		}
		List<AwardProgressReportAttachment> attachments;
		if (progressReportVO.getDocumentId() != null) {
			attachments = progressReportDao.loadProgressReportAttachmentVersions(progressReportVO.getProgressReportId(), progressReportVO.getDocumentId(), isPersonHasPermission);
		} else {
			attachments = progressReportDao.loadProgressReportAttachments(progressReportVO.getProgressReportId(), isPersonHasPermission);
			progressReportVO.setProgressReportAttachmentTypes(progressReportDao.loadProgressReportAttachmentTypes());
		}
		getFullNameOfUpdateUser(attachments);
		progressReportVO.setAwardProgressReportAttachments(attachments);
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	private void getFullNameOfUpdateUser(List<AwardProgressReportAttachment> attachments) {
		Set<String> userName = attachments.stream().map(AwardProgressReportAttachment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			attachments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	@Override
	public ResponseEntity<byte[]> downloadProgressReportAttachment(Integer attachmentId) {
		AwardProgressReportAttachment attachment = progressReportDao.getProgressReportAttachment(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("Exception in downloadProgressReportAttachment {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String performFundingAgencyAction(ProgressReportVO progressReportVO) {
		try {
		AwardProgressReport awardProgressReport = new AwardProgressReport();
		if (progressReportVO.getActionType().equals(Constants.HOLD_FOR_FA_REVIEW)) {
			awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_HOLD_FOR_FA);
			awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_HOLD_FOR_FA));
			progressReportDao.updateProgressReportStatus(progressReportVO.getProgressReportId(), Constants.PROGRESS_REPORT_STATUS_CODE_HOLD_FOR_FA, null);
		} else if (progressReportVO.getActionType().equals(Constants.FA_REVIEW_COMPLETED)) {
			awardProgressReport.setProgressReportStatusCode(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS);
			awardProgressReport.setProgressReportStatus(progressReportDao.getProgressReportStatus(Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS));
			progressReportDao.updateProgressReportStatus(progressReportVO.getProgressReportId(), Constants.PROGRESS_REPORT_STATUS_CODE_APPROVAL_IN_PROGRESS, progressReportVO.getFunderApprovalDate());
		}
		awardProgressReport.setFunderApprovalDate(progressReportVO.getFunderApprovalDate());
		progressReportVO.setAwardProgressReport(awardProgressReport);
		} catch (Exception e) {
			logger.error("Exception in performFundingAgencyAction {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	@Override
	public String updateProgressReportAchievements(ProgressReportVO progressReportVO) {
		progressReportVO.getAwardProgressReportAchievements().forEach(achievements -> progressReportDao.saveOrUpdateProgressReportAchievements(achievements));
		progressReportDao.updateProgressReportUpdatedTimeAndUser(progressReportVO.getProgressReportId());
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	@Override
	public String loadProgressReportKPISummary(Integer progressReportId) {
		ProgressReportVO progressReportVO = new ProgressReportVO();
		List<AwardProgressReportKPISummary> awardProgressReportKPISummary = progressReportDao.loadProgressReportKPISummary(progressReportId);
		awardProgressReportKPISummary = awardProgressReportKPISummary.stream().filter(distinctByKey(AwardProgressReportKPISummary::getKpiCriteriaTypeCode)).sorted(Comparator.comparing(AwardProgressReportKPISummary::getKpiCategoryTypeCode)
				.thenComparing(summary -> summary.getKpiCriteriaType().getDescription())).collect(Collectors.toList());
		awardProgressReportKPISummary.forEach(kpiSummary -> {
			commonDao.detachEntityFromSession(kpiSummary.getKpiType());
			getKPICriteriaAchievedValueHistory(kpiSummary, progressReportId);
			kpiSummary.getKpiType().setKpiCriteriaType(null);
		});
		if (!awardProgressReportKPISummary.isEmpty()) {
			progressReportVO.setSummaryHistoryData(progressReportDao.getAllProgressReportNumberAndDueDateOfAward(awardProgressReportKPISummary.get(0).getAwardProgressReport().getProgressReportId()));
		}
		progressReportVO.setAwardProgressReportKPISummary(awardProgressReportKPISummary);
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	private void getKPICriteriaAchievedValueHistory(AwardProgressReportKPISummary summary, Integer progressReportId) {
		double count = 0;
		List<Object[]> achievedValueHistory = progressReportDao.getKPICriteriaAchievedValueHistory(summary.getKpiCriteriaTypeCode(), progressReportId);
		count = achievedValueHistory.stream().filter(history -> history[1] != null).mapToDouble(history -> Double.parseDouble(history[1].toString())).sum();
		Map<Object, Object> achievedValues = achievedValueHistory.stream().collect(Collectors.toMap(history -> history[0], history -> history[1] == null ? 0 : history[1]));
		summary.setTotalAchieved(count);
		summary.setAchievedValues(achievedValues);
	}

	@Override
	public String loadProgressReportKPISummaryDetails(Integer kpiSummaryId, String sectionCode) {
		Map<String, List<ProgressReportKPIMapping>> kpiMapping = progressReportDao.getKPImapping().stream().collect(Collectors.groupingBy(ProgressReportKPIMapping::getSectionCode));
		Map<String, Object> summaryDetail = new HashMap<>();
		if (kpiMapping.containsKey(sectionCode)) {
			summaryDetail.put("summaryDetail", progressReportDao.getSummaryDetail("com.polus.fibicomp.progressreport.pojo." + kpiMapping.get(sectionCode).get(0).getTemplateTable(), kpiSummaryId));
		}
		return commonDao.convertObjectToJSON(summaryDetail);
	}

	@Override
	public String saveOrUpdateKPISummaryDetails(ProgressReportVO progressReportVO) {
		Integer progressReportId = null;
		switch (progressReportVO.getSectionCode()) {
		case "1":
			ProgressReportKPIImpactPublications progressReportKPIImpactPublications = progressReportVO.getProgressReportKPIImpactPublications();
			progressReportId = progressReportKPIImpactPublications.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIImpactPublications);
			progressReportDao.updateKPIAchievedCountImpactPublication(progressReportKPIImpactPublications.getKpiCriteriaCode(), progressReportId);
			break;
		case "2":
			ProgressReportKPICollaborationProjects progressReportKPICollaborationProjects = progressReportVO.getProgressReportKPICollaborationProjects();
			progressReportId = progressReportKPICollaborationProjects.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPICollaborationProjects);
			progressReportDao.updateKPIAchievedCountCollaborationProjects(progressReportKPICollaborationProjects.getKpiCriteriaCode(), progressReportId);
			break;
		case "3":
			ProgressReportKPITechnologyDisclosure progressReportKPITechnologyDisclosure = progressReportVO.getProgressReportKPITechnologyDisclosure();
			progressReportId = progressReportKPITechnologyDisclosure.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPITechnologyDisclosure);
			progressReportDao.updateKPIAchievedCountTechnologyDisclosure(progressReportKPITechnologyDisclosure.getKpiCriteriaCode(), progressReportId);
			break;
		case "4":
			ProgressReportKPIManpowerDevelopment progressReportKPIManpowerDevelopment = progressReportVO.getProgressReportKPIManpowerDevelopment();
			progressReportId = progressReportKPIManpowerDevelopment.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIManpowerDevelopment);
			progressReportDao.updateKPIAchievedCountManpowerDevelopment(progressReportKPIManpowerDevelopment.getKpiCriteriaCode(), progressReportId);
			break;
		case "5":
			ProgressReportKPIUndergraduateStudent progressReportKPIUndergraduateStudent = progressReportVO.getProgressReportKPIUndergraduateStudent();
			progressReportId = progressReportKPIUndergraduateStudent.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIUndergraduateStudent);
			progressReportDao.updateKPIAchievedCountUndergraduateStudent(progressReportKPIUndergraduateStudent.getKpiCriteriaCode(), progressReportId);
			break;
		case "6":
			ProgressReportKPIConferencePresentation progressReportKPIConferencePresentation = progressReportVO.getProgressReportKPIConferencePresentation();
			progressReportId = progressReportKPIConferencePresentation.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIConferencePresentation);
			progressReportDao.updateKPIAchievedCountConferencePresentation(progressReportKPIConferencePresentation.getKpiCriteriaCode(), progressReportId);
			break;
		case "7":
			ProgressReportKPICompetitiveGrants progressReportKPICompetitiveGrants = progressReportVO.getProgressReportKPICompetitiveGrants();
			progressReportId = progressReportKPICompetitiveGrants.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPICompetitiveGrants);
			progressReportDao.updateKPIAchievedCountCompetitiveGrants(progressReportKPICompetitiveGrants.getKpiCriteriaCode(), progressReportId);
			break;
		case "8":
			ProgressReportKPIPatents progressReportKPIPatents = progressReportVO.getProgressReportKPIPatents();
			progressReportId = progressReportKPIPatents.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIPatents);
			progressReportDao.updateKPIAchievedCountPatents(progressReportKPIPatents.getKpiCriteriaCode(), progressReportId);
			break;
		case "9":
			ProgressReportKPILicenses progressReportKPILicenses = progressReportVO.getProgressReportKPILicenses();
			progressReportId = progressReportKPILicenses.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPILicenses);
			progressReportDao.updateKPIAchievedCountLicenses(progressReportKPILicenses.getKpiCriteriaCode(), progressReportId);
			break;
		case "10":
			ProgressReportKPISuccessfulStartups progressReportKPISuccessfulStartups = progressReportVO.getProgressReportKPISuccessfulStartups();
			progressReportId = progressReportKPISuccessfulStartups.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPISuccessfulStartups);
			progressReportDao.updateKPIAchievedCountSuccessfulStartups(progressReportKPISuccessfulStartups.getKpiCriteriaCode(), progressReportId);
			break;
		case "11":
			ProgressReportKPIHealthSpecificOutcomes progressReportKPIHealthSpecificOutcomes = progressReportVO.getProgressReportKPIHealthSpecificOutcomes();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIHealthSpecificOutcomes);
			progressReportId = progressReportKPIHealthSpecificOutcomes.getProgressReportId();
			progressReportDao.updateKPIAchievedCountHealthSpecificOutcomes(progressReportKPIHealthSpecificOutcomes.getKpiCriteriaCode(), progressReportId);
			break;
		case "12":
			ProgressReportKPIPostDocsEmployed progressReportKPIPostDocsEmployed = progressReportVO.getProgressReportKPIPostDocsEmployed();
			progressReportId = progressReportKPIPostDocsEmployed.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIPostDocsEmployed);
			progressReportDao.updateKPIAchievedCountPostDocsEmployed(progressReportKPIPostDocsEmployed.getKpiCriteriaCode(), progressReportId);
			break;
		case "13":
			ProgressReportKPIGrantSpecific progressReportKPIGrantSpecific = progressReportVO.getProgressReportKPIGrantSpecific();
			progressReportId = progressReportKPIGrantSpecific.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIGrantSpecific);
			progressReportDao.updateKPIAchievedCountPostGrantSpecific(progressReportKPIGrantSpecific.getKpiCriteriaCode(), progressReportId);
			break;
		case "14":
			ProgressReportKPICashFunding progressReportKPICashFunding = progressReportVO.getProgressReportKPICashFunding();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPICashFunding);
			progressReportId = progressReportKPICashFunding.getProgressReportId();
			progressReportDao.updateKPIAchievedCountCashFunding(progressReportKPICashFunding.getKpiCriteriaCode(), progressReportId);
			break;
		case "15":
			ProgressReportKPIInkindContributions progressReportKPIInkindContributions = progressReportVO.getProgressReportKPIInkindContributions();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIInkindContributions);
			progressReportId = progressReportKPIInkindContributions.getProgressReportId();
			progressReportDao.updateKPIAchievedCountInkindContributions(progressReportKPIInkindContributions.getKpiCriteriaCode(), progressReportId);
			break;
		case "16":
			ProgressReportKPITechnologiesDeployed progressReportKPITechnologiesDeployed = progressReportVO.getProgressReportKPITechnologiesDeployed();
			progressReportId = progressReportKPITechnologiesDeployed.getProgressReportId();
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPITechnologiesDeployed);
			progressReportDao.updateKPIAchievedCountTechnologiesDeployed(progressReportKPITechnologiesDeployed.getKpiCriteriaCode(), progressReportId);
			break;
		default:
			break;
		}
		progressReportDao.updateProgressReportUpdatedTimeAndUser(progressReportId);
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	@Override
	public String deleteKPISummaryDetail(Integer kpiSummaryId, String sectionCode, Integer id) {
		String kpiCriteriaCode;
		Integer progressreportId;
		switch (sectionCode) {
		case "1":
			ProgressReportKPIImpactPublications impactPublications = (ProgressReportKPIImpactPublications) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIIMPACTPUBLICATIONS, id);
			kpiCriteriaCode = impactPublications.getKpiCriteriaCode();
			progressreportId = impactPublications.getProgressReportId();
			progressReportDao.deleteKPIImpactPublications(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountImpactPublication(kpiCriteriaCode, progressreportId);
			break;
		case "2":
			ProgressReportKPICollaborationProjects collaborationProject = (ProgressReportKPICollaborationProjects) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPICOLLABORATIONPROJECTS, id);
			kpiCriteriaCode = collaborationProject.getKpiCriteriaCode();
			progressreportId = collaborationProject.getProgressReportId();
			progressReportDao.deleteKpiCollaborationProjects(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountCollaborationProjects(kpiCriteriaCode, progressreportId);
			break;
		case "3":
			ProgressReportKPITechnologyDisclosure technologyDisclosure = (ProgressReportKPITechnologyDisclosure) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPITECHNOLOGYDISCLOSURE, id);
			kpiCriteriaCode = technologyDisclosure.getKpiCriteriaCode();
			progressreportId = technologyDisclosure.getProgressReportId();
			progressReportDao.deleteKpiTechnologyDisclosure(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountTechnologyDisclosure(kpiCriteriaCode, progressreportId);
			break;
		case "4":
			ProgressReportKPIManpowerDevelopment manpowerDevelopment = (ProgressReportKPIManpowerDevelopment) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIMANPOWERDEVELOPMENT, id);
			kpiCriteriaCode = manpowerDevelopment.getKpiCriteriaCode();
			progressreportId = manpowerDevelopment.getProgressReportId();
			progressReportDao.deleteKpiProgressReportKPIManpowerDevelopment(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountManpowerDevelopment(kpiCriteriaCode, progressreportId);
			break;
		case "5":
			ProgressReportKPIUndergraduateStudent underGraduateStudent = (ProgressReportKPIUndergraduateStudent) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIUNDERGRADUATESTUDENT, id);
			kpiCriteriaCode = underGraduateStudent.getKpiCriteriaCode();
			progressreportId = underGraduateStudent.getProgressReportId();
			progressReportDao.deleteKPIUndergraduateStudent(progressreportId, kpiCriteriaCode ,id);
			progressReportDao.updateKPIAchievedCountUndergraduateStudent(kpiCriteriaCode, progressreportId);
			break;
		case "6":
			ProgressReportKPIConferencePresentation conferencePresentation = (ProgressReportKPIConferencePresentation) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPICONFERENCEPRESENTATION, id);
			kpiCriteriaCode = conferencePresentation.getKpiCriteriaCode();
			progressreportId = conferencePresentation.getProgressReportId();
			progressReportDao.deleteKpiConferencePresentation(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountConferencePresentation(kpiCriteriaCode, progressreportId);
			break;
		case "7":
			ProgressReportKPICompetitiveGrants competitiveGrants = (ProgressReportKPICompetitiveGrants) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPICOMPETITIVEGRANTS, id);
			kpiCriteriaCode = competitiveGrants.getKpiCriteriaCode();
			progressreportId = competitiveGrants.getProgressReportId();
			progressReportDao.deleteKpiCompetitiveGrants(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountCompetitiveGrants(kpiCriteriaCode, progressreportId);
			break;
		case "8":
			ProgressReportKPIPatents patents = (ProgressReportKPIPatents) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIPATENTS, id);
			kpiCriteriaCode = patents.getKpiCriteriaCode();
			progressreportId = patents.getProgressReportId();
			progressReportDao.deleteProgressReportKPIPatents(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountPatents(kpiCriteriaCode, progressreportId);
			break;
		case "9":
			ProgressReportKPILicenses licenses = (ProgressReportKPILicenses) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPILICENSES, id);
			kpiCriteriaCode = licenses.getKpiCriteriaCode();
			progressreportId = licenses.getProgressReportId();
			progressReportDao.deleteProgressReportKpiLicenses(progressreportId,kpiCriteriaCode,id);
			progressReportDao.updateKPIAchievedCountLicenses(kpiCriteriaCode, progressreportId);
			break;
		case "10":
			ProgressReportKPISuccessfulStartups successfulStartups = (ProgressReportKPISuccessfulStartups) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPISUCCESSFULSTARTUPS, id);
			kpiCriteriaCode = successfulStartups.getKpiCriteriaCode();
			progressreportId = successfulStartups.getProgressReportId();
			progressReportDao.deleteProgressReportKPISuccessfulStartups(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountSuccessfulStartups(kpiCriteriaCode, progressreportId);
			break;
		case "11":
			ProgressReportKPIHealthSpecificOutcomes specificOutcomes = (ProgressReportKPIHealthSpecificOutcomes) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIHEALTHSPECIFICOUTCOMES, id);
			kpiCriteriaCode = specificOutcomes.getKpiCriteriaCode();
			progressreportId = specificOutcomes.getProgressReportId();
			progressReportDao.deleteProgressReportKPIHealthSpecificOutcomes(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountHealthSpecificOutcomes(kpiCriteriaCode, progressreportId);
			break;
		case "12":
			ProgressReportKPIPostDocsEmployed docsEmployed = (ProgressReportKPIPostDocsEmployed) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIPOSTDOCSEMPLOYED, id);
			kpiCriteriaCode = docsEmployed.getKpiCriteriaCode();
			progressreportId = docsEmployed.getProgressReportId();
			progressReportDao.deleteProgressReportKPIPostDocsEmployed(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountPostDocsEmployed(kpiCriteriaCode, progressreportId);
			break;
		case "13":
			ProgressReportKPIGrantSpecific grantsSpecific = (ProgressReportKPIGrantSpecific) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIGRANTSPECIFIC, id);
			kpiCriteriaCode = grantsSpecific.getKpiCriteriaCode();
			progressreportId = grantsSpecific.getProgressReportId();
			progressReportDao.deleteProgressReportKPIGrantSpecific(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountPostGrantSpecific(kpiCriteriaCode, progressreportId);
			break;
		case "14":
			ProgressReportKPICashFunding cashFunding = (ProgressReportKPICashFunding) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPICASHFUNDING, id);
			kpiCriteriaCode = cashFunding.getKpiCriteriaCode();
			progressreportId = cashFunding.getProgressReportId();
			progressReportDao.deleteProgressReportKPICashFunding(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountCashFunding(kpiCriteriaCode, progressreportId);
			break;
		case "15":
			ProgressReportKPIInkindContributions inKindContributions = (ProgressReportKPIInkindContributions) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPIINKINDCONTRIBUTIONS, id);
			kpiCriteriaCode = inKindContributions.getKpiCriteriaCode();
			progressreportId = inKindContributions.getProgressReportId();
			progressReportDao.deleteProgressReportKPIInkindContributions(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountInkindContributions(kpiCriteriaCode, progressreportId);
			break;
		case "16":
			ProgressReportKPITechnologiesDeployed technologiesDeployed = (ProgressReportKPITechnologiesDeployed) progressReportDao.getProgressReportKPIDetailById(Constants.PROGRESSREPORTKPITECHNOLOGIESDEPLOYED, id);
			kpiCriteriaCode = technologiesDeployed.getKpiCriteriaCode();
			progressreportId = technologiesDeployed.getProgressReportId();
			progressReportDao.deleteProgressReportKPITechnologiesDeployed(progressreportId, kpiCriteriaCode, id);
			progressReportDao.updateKPIAchievedCountTechnologiesDeployed(kpiCriteriaCode, progressreportId);
			break;
		default:
			break;
		}
		Map<String, Object> response = new HashMap<>();
		response.put("status", "success");
		return commonDao.convertObjectToJSON(response);
	}

	@Override
	public String loadProgressReportKPILookups() {
		Map<String, Object> kpiLookups = new HashMap<>();
		kpiLookups.put("kpiPublicationStatus", progressReportDao.getKPIPublicationStatus());
		kpiLookups.put("kpiTechnologyDisclosureStatus", progressReportDao.getKPITechnologyDisclosureStatus());
		kpiLookups.put("kpiManpowerDevelopmentCurrentStatus", progressReportDao.getKPIManpowerDevelopmentCurrentStatus());
		return commonDao.convertObjectToJSON(kpiLookups);
	}

	@Override
	public String loadProgressReportForAward(String awardNumber) {
		Map<String, List<AwardProgressReport>> vo = new HashMap<>();
		List<AwardProgressReport> awardProgressReports = progressReportDao.loadProgressReportForAward(awardNumber);
		awardProgressReports.forEach(awardProgressReport -> {
			awardProgressReport.setReportStartDate(progressReportDao.getReportPeriodStartDate(awardProgressReport));
			if(awardProgressReport.getReportStartDate() == null)
				awardProgressReport.setReportStartDate(progressReportDao.getAwardStartDateByAwardNumber(awardNumber));
		});
		getFullNameOfUser(awardProgressReports);
		vo.put("progressReports", awardProgressReports);
		return commonDao.convertObjectToJSON(vo);
	}

	private void getFullNameOfUser(List<AwardProgressReport> awardProgressReports) {
		Set<String> userName = awardProgressReports.stream().map(AwardProgressReport::getCreateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			awardProgressReports.stream().filter(item -> item.getCreateUser() != null).filter(item -> collect.containsKey(item.getCreateUser().toUpperCase()))
			.forEach(item -> item.setCreatedPersonName(collect.get(item.getCreateUser().toUpperCase())));
		}
	}

	@Override
	public ProgressReportVO importProgressReportTemplate(MultipartFile[] files, String formDataJson) {
		ProgressReportVO progressReportVO = null;
		XSSFWorkbook workbook = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			progressReportVO = mapper.readValue(formDataJson, ProgressReportVO.class);
			if (progressReportVO.getAwardId() != null && progressReportVO.getProgressReportId() != null && !files[0].isEmpty() && files[0] != null) {
				Integer progressReportId = progressReportVO.getProgressReportId();
				Integer awardId = progressReportVO.getAwardId();
				List<String> kpiCriterias = progressReportDao.getAwardKpiCriterias(awardId, progressReportId);
			    workbook = (XSSFWorkbook) WorkbookFactory.create(files[0].getInputStream());
				XSSFSheet sheet = workbook.getSheetAt(0);
				Boolean isRowInserted = Boolean.FALSE;
				List<String> achievementTypes = progressReportDao.fetchAwardProgressReportAchievementTypesById(progressReportId, Constants.PROGRESS_REPORT_SECTION_TYPE_SUMMARY);
				for (Row row : sheet) {
					if (row != null && Boolean.FALSE.equals(isRowInserted)) {
						for (Cell cell : row) {
							Cell nextCellValue = null;
							if (cell != null && !cell.getCellType().equals(CellType.NUMERIC) && cell.getStringCellValue().contains("SECTION 1: SUMMARY OF PROGRESS")) {
								isRowInserted = Boolean.TRUE;
								Integer nextSection = 1;
								Integer i = 1;
								String tableName = "AWARD_PROGRESS_REPORT_ACHIEVEMENT";
								Field field = AwardProgressReportAchievement.class.getDeclaredField("description");
								Integer dataLength = getDataTypeLengthForSummary(field, tableName);
								while (nextSection > 0) {
									Row nextRow = sheet.getRow(row.getRowNum() + i + 1);
									if (nextRow != null) {
										for (Cell nextCell : nextRow) {
											for (String achievementType : achievementTypes) {
												if (nextCell != null && nextCell.getStringCellValue().trim().contains(achievementType)) {
													int val = nextCell.getColumnIndex() + 8;
													Cell cellValue = nextRow.getCell(val);
													prepareOverviewData(dataLength, achievementType, cellValue.getStringCellValue(), progressReportVO);
												}
											}
											if (nextCell != null && nextCell.getStringCellValue().contains("SECTION 2: RESEARCH MILESTONE")) {
												nextSection--;
												Integer cellVlue = nextCell.getColumnIndex();
												nextCellValue = nextRow.getCell(cellVlue);
											}
										}
										i++;
									}
								}
							}
							if (nextCellValue != null && nextCellValue.getStringCellValue().contains("SECTION 2: RESEARCH MILESTONE")) {
								Integer nextSection = 1;
								Integer milestoneSection = 1;
								String tableName = "AWARD_PROGRESS_REPORT_MILESTONE";
								Field field = AwardProgressReportMilestone.class.getDeclaredField("remark");
								Integer dataLength = getDataTypeLengthForSummary(field, tableName);
								Integer i = nextCellValue.getRow().getRowNum();
								while (milestoneSection > 0) {
									Row milestoneRow = sheet.getRow(i);
									if (milestoneRow != null && milestoneRow.getCell(5) != null && !milestoneRow.getCell(5).getCellType().equals(CellType.NUMERIC)
											&& milestoneRow.getCell(5).getStringCellValue().contains("No")) {
								Integer milestoneRowNum = i+1;
								List<AwardProgressReportMilestone> awardMileStones = new ArrayList<>();
								while (nextSection > 0) {
									AwardProgressReportMilestone awardMileStone = new AwardProgressReportMilestone();
									Row nextRow = sheet.getRow(milestoneRowNum);
									Cell milestone = nextRow.getCell(12);
									if (milestone != null && !milestone.getStringCellValue().isEmpty()) {
										awardMileStone.setMilestone(milestone.getStringCellValue().trim());
									}
									Cell committedStartMonth = nextRow.getCell(15);
									if (committedStartMonth != null && committedStartMonth.getCellType().equals(CellType.NUMERIC) && committedStartMonth.getDateCellValue() != null) {
										awardMileStone.setCommittedStartDate(dateFormater(committedStartMonth.getDateCellValue().toString()));
									} else if(committedStartMonth != null && committedStartMonth.getCellType().equals(CellType.STRING) && committedStartMonth.getStringCellValue() != null && !committedStartMonth.getStringCellValue().equals("")) {
										Date date = prepareDateValue(committedStartMonth.getStringCellValue(), Constants.DEFAULT_DATE_FORMAT);
										awardMileStone.setCommittedStartDate(new Timestamp((date.getTime())));
									}
									Cell committedEndMonth = nextRow.getCell(19);
									if (committedEndMonth != null && committedEndMonth.getCellType().equals(CellType.NUMERIC) && committedEndMonth.getDateCellValue() != null) {
										awardMileStone.setCommittedEndDate(dateFormater(committedEndMonth.getDateCellValue().toString()));
									} else if(committedEndMonth != null && committedEndMonth.getCellType().equals(CellType.STRING) && committedEndMonth.getStringCellValue() != null && !committedEndMonth.getStringCellValue().equals("")){
										Date date = prepareDateValue(committedEndMonth.getStringCellValue(), Constants.DEFAULT_DATE_FORMAT);
										awardMileStone.setCommittedEndDate(new Timestamp((date.getTime())));
									}
									Cell reportStartDate = nextRow.getCell(32);
									if (reportStartDate != null && reportStartDate.getCellType().equals(CellType.NUMERIC) && reportStartDate.getDateCellValue() != null) {
										awardMileStone.setActualStartMonth(new Timestamp((reportStartDate.getDateCellValue().getTime())));
									} else if(reportStartDate != null  && reportStartDate.getCellType().equals(CellType.STRING) && reportStartDate.getStringCellValue() != null && !reportStartDate.getStringCellValue().equals("")) {
										Date date = prepareDateValue(reportStartDate.getStringCellValue(), Constants.DEFAULT_DATE_FORMAT);
										awardMileStone.setActualStartMonth(new Timestamp((date.getTime())));
									}
									Cell reportEndDate = nextRow.getCell(38);
									if (reportEndDate != null && reportStartDate.getCellType().equals(CellType.NUMERIC) && reportEndDate.getDateCellValue() != null) {
										awardMileStone.setActualEndMonth(new Timestamp((reportEndDate.getDateCellValue().getTime())));
									} else if(reportEndDate != null && reportStartDate.getCellType().equals(CellType.STRING) && reportEndDate.getStringCellValue() != null && !reportEndDate.getStringCellValue().equals("")) {
										Date date = prepareDateValue(reportEndDate.getStringCellValue(), Constants.DEFAULT_DATE_FORMAT);
										awardMileStone.setActualEndMonth(new Timestamp((date.getTime())));
									}
									Cell status = nextRow.getCell(41);
									if (status != null && !status.getStringCellValue().isEmpty()) {
										awardMileStone.setStatus(status.getStringCellValue());
									}
									Cell remark = nextRow.getCell(51);
									if(remark != null && !remark.getStringCellValue().isEmpty()) {
										String description = remark.getStringCellValue().trim();
										if (remark.getStringCellValue().trim().length() > dataLength) {
											description = remark.getStringCellValue().trim().substring(0, dataLength);
										}
										awardMileStone.setRemark(description);
									}
									if (awardMileStone.getCommittedStartDate() != null && awardMileStone.getCommittedEndDate() != null) {
										awardMileStones.add(awardMileStone);
									}
									if (nextRow.getCell(6) != null && nextRow.getCell(6).getStringCellValue().equals("SECTION 3: TECHNICAL MILESTONE")) {
										nextSection--;
										nextCellValue = nextRow.getCell(6);
									}
									milestoneRowNum++;
								}
								 saveAwardMilestone(awardMileStones, progressReportId, awardId, progressReportVO);
								}
								if (nextCellValue != null && nextCellValue.getStringCellValue().contains("SECTION 3: TECHNICAL MILESTONE")) {
									milestoneSection--;	
								}
								i++;
								}
							}
							if (nextCellValue != null && nextCellValue.getStringCellValue().contains("SECTION 3: TECHNICAL MILESTONE")) {
								isRowInserted = Boolean.TRUE;
								Integer nextSection = 1;
								Integer rowNumber = nextCellValue.getRow().getRowNum();
								while (nextSection > 0) {
									Row nextRow = sheet.getRow(rowNumber);
									if (nextRow != null) {
										for (Cell nextCell : nextRow) {
											if (nextCell != null && !nextCell.getCellType().equals(CellType.NUMERIC) && nextCell.getStringCellValue().contains("SECTION 5: FUTURE PLANS")) {
												Integer cellValue = nextCell.getColumnIndex();
												nextCellValue = nextRow.getCell(cellValue);
												nextCellValue = prepareFuturePlanDetails(sheet, progressReportVO, nextCellValue, progressReportId);
												if (nextCellValue != null && !nextCellValue.getCellType().equals(CellType.NUMERIC) && nextCellValue.getStringCellValue().contains("SECTION 6: PERFORMANCE INDICATORS")) {
													nextSection --;
												}
											}
										}
										rowNumber++;
									}
								}
							}
							if (nextCellValue != null) {
								Boolean kpiRowInserted = Boolean.FALSE;
								Integer nextIteration = 1;
								Integer rowNumber = nextCellValue.getRow().getRowNum();
								while (nextIteration > 0) {
									Row kpiRowValue = sheet.getRow(rowNumber);
									if (kpiRowValue != null && Boolean.FALSE.equals(kpiRowInserted)) {
										for (Cell kpi : kpiRowValue) {
											if (kpi != null && !(kpi.getCellType().equals(CellType.NUMERIC)) && kpi.getStringCellValue().contains("KPI No")) {
												Integer rowNum = kpi.getRow().getRowNum();
												Integer nextSection = 1;
												if (kpiCriterias != null) {
													rowNum = setKpiTemplateDetails(kpiCriterias, nextSection, rowNum, sheet, awardId, progressReportId, progressReportVO);
													kpiRowInserted = Boolean.TRUE;
												}
											}
										}
									}
									if (Boolean.TRUE.equals(kpiRowInserted)) {
										nextIteration--;
									} else {
										rowNumber ++;
									}
								}
							}
						}
					}
				}
				workbook.close();
			}
			logger.info("completed importProgressReportTemplate");
		} catch(ApplicationException e) {
			throw e;
		} catch(Exception e) {
			throw new ApplicationException("error importProgressReportTemplate", e, Constants.JAVA_ERROR);
		}
		finally {
			try {
				workbook.close();
			} catch (IOException e) {
				logger.error("exception in finally importProgressReportTemplate: {} ", e.getMessage());
			}
		}
		return progressReportVO;
	}

	private Cell prepareFuturePlanDetails(XSSFSheet sheet, ProgressReportVO progressReportVO, Cell nextCellValue, Integer progressReportId) {
		try {
		List<String> achievementTypes = progressReportDao.fetchAwardProgressReportAchievementTypesById(progressReportId, Constants.PROGRESS_REPORT_SECTION_TYPE_FUTURE);
		Integer nextSection = 1;
		Integer i = nextCellValue.getRow().getRowNum();
		String tableName = "AWARD_PROGRESS_REPORT_ACHIEVEMENT";
		Field field = AwardProgressReportAchievement.class.getDeclaredField("description");
		Integer dataLength = getDataTypeLengthForSummary(field, tableName);
		while (nextSection > 0) {
			Row nextRow = sheet.getRow(i);
			if (nextRow != null) {
				for (Cell nextCell : nextRow) {
					for (String achievementType : achievementTypes) {
						if (nextCell != null && !nextCell.getCellType().equals(CellType.NUMERIC) && nextCell.getStringCellValue().trim().contains(achievementType)) {
							int val = nextCell.getColumnIndex() + 7;
							Cell cellValue = nextRow.getCell(val);
							prepareOverviewData(dataLength, achievementType, cellValue.getStringCellValue(), progressReportVO);
						}
					}
					if (nextCell != null && !nextCell.getCellType().equals(CellType.NUMERIC) && nextCell.getStringCellValue().contains("SECTION 6: PERFORMANCE INDICATORS")) {
						nextSection--;
						Integer cellValue = nextCell.getColumnIndex();
						nextCellValue = nextRow.getCell(cellValue);
					}
				}
				i++;
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error prepareFuturePlanDetails", e, Constants.JAVA_ERROR);
		}
		return nextCellValue;
	}

	private Integer setKpiTemplateDetails(List<String> kpiCriterias, Integer nextSection, Integer rowNum, XSSFSheet sheet, Integer awardId, Integer progressReportId, ProgressReportVO progressReportVO) {
		Integer kpiCellCount = 1;
		while (nextSection > 0) {
			Row kpiRow = sheet.getRow(rowNum);
			Integer rowCount = rowNum;
			for (Cell kpiCell : kpiRow) {
				Integer count = 0;
				if (kpiCell != null && !kpiCell.getCellType().equals(CellType.NUMERIC) && kpiCell.getStringCellValue() != null && Boolean.TRUE.equals(checkAwardCriteriaExist(kpiCriterias, kpiCell.getStringCellValue().trim()))) {
					rowNum = prepareKpiTemplateDetails(awardId, progressReportId, sheet, rowNum, count, kpiCellCount, kpiCell, progressReportVO);
				}
				if (kpiCell != null && !kpiCell.getCellType().equals(CellType.NUMERIC) && kpiCell.getStringCellValue().contains("DECLARATION")) {
					nextSection --;
					kpiCellCount--;
				}
			}
			if (rowCount.equals(rowNum)) {
				rowNum++;
			}
		}
		return rowNum;
	}

	private Boolean checkAwardCriteriaExist(List<String> kpiCriterias, String cellValue) {
		Boolean valueExist = Boolean.FALSE;
		if(kpiCriterias != null && !kpiCriterias.isEmpty() && !cellValue.isEmpty() && cellValue.contains("KPI No")) {
			cellValue = cellValue.trim().split("\\s*\\:\\s*")[1];
			if (kpiCriterias.contains(cellValue)) {
				kpiCriterias.remove(cellValue);
				valueExist = Boolean.TRUE;
			}
		}
		return valueExist;
	}

	private Integer prepareKpiTemplateDetails(Integer awardId, Integer progressReportId, XSSFSheet sheet, Integer rowNum ,Integer count, Integer kpiCellCount, Cell kpiCell, ProgressReportVO progressReportVO) {
		String description = kpiCell.getStringCellValue().trim().split("\\s*\\:\\s*")[1];
		String sectionCode = progressReportDao.getKpiCriteriaMappingDetail(description);
		AwardProgressReportKPISummary awardProgressReportKPISummary = progressReportDao.getKPISummaryDetailByParam(awardId, progressReportId, description);
		if (awardProgressReportKPISummary != null) {
		Integer deletionCount = 0;
		while (kpiCellCount > 0) {
			Row kpiData = sheet.getRow(rowNum +1);
			if ((kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) || (kpiData.getCell(5) != null && kpiData.getCell(5).getStringCellValue().contains("DECLARATION"))) {
				kpiCellCount--;
				rowNum  = kpiData.getRowNum();
			} else {
			count ++;
			if (count != 1 && sectionCode != null) {
				progressReportVO.setProgressReportImported(true);
				switch (sectionCode) {
				case "1":
					if (deletionCount == 0) {
						 progressReportDao.deleteKPIImpactPublications(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiImpactPublications(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountImpactPublication(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "2":
					if (deletionCount == 0) {
						progressReportDao.deleteKpiCollaborationProjects(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiCollaborationProjects(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountCollaborationProjects(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "3":
					if (deletionCount == 0) {
						progressReportDao.deleteKpiTechnologyDisclosure(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiTechnologyDisclosure(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountTechnologyDisclosure(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "4":
					if (deletionCount == 0) {
						progressReportDao.deleteKpiProgressReportKPIManpowerDevelopment(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiIManpowerDevelopment(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountManpowerDevelopment(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "5":
					if (deletionCount == 0) {
						progressReportDao.deleteKPIUndergraduateStudent(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiUndergraduateStudent(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountUndergraduateStudent(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "6":
					if (deletionCount == 0) {
						progressReportDao.deleteKpiConferencePresentation(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiConferencePresentation(awardProgressReportKPISummary, description, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountConferencePresentation(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "7":
					if (deletionCount == 0) {
						progressReportDao.deleteKpiCompetitiveGrants(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiCompetitiveGrants(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountCompetitiveGrants(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "8":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPIPatents(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiPatents(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountPatents(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "9":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKpiLicenses(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiLicenses(awardProgressReportKPISummary, description, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountLicenses(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "10":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPISuccessfulStartups(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiSuccessfulStartups(awardProgressReportKPISummary, description, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountSuccessfulStartups(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "11":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPIHealthSpecificOutcomes(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiHealthSpecificOutcomes(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountHealthSpecificOutcomes(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "12":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPIPostDocsEmployed(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiPostDocsEmployed(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountPostDocsEmployed(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "13":
					@SuppressWarnings("unused") ProgressReportKPIGrantSpecific grantsSpecific = null;
					break;
				case "14":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPICashFunding(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiCashFunding(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountCashFunding(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "15":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPIInkindContributions(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiInkindContributions(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountInkindContributions(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				case "16":
					if (deletionCount == 0) {
						progressReportDao.deleteProgressReportKPITechnologiesDeployed(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId(), awardProgressReportKPISummary.getKpiCriteriaTypeCode(), null);
					}
					deletionCount++;
					kpiCellCount = prepareKpiTechnologiesDeployed(awardProgressReportKPISummary, kpiData, kpiCellCount);
					progressReportDao.updateKPIAchievedCountTechnologiesDeployed(awardProgressReportKPISummary.getKpiCriteriaTypeCode(), progressReportId);
					break;
				default:
					break;
				}
		    }
			rowNum++;
			}
		   }
		}
		return rowNum;
	}

	private Integer prepareKpiIManpowerDevelopment(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData,  Integer kpiCellCount) {
		ProgressReportKPIManpowerDevelopment manpowerDevelopment = new ProgressReportKPIManpowerDevelopment();
		String tableName = "PROGRESS_REPORT_KPI_MANPOWER_DEVELOPMENT";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell nameOfStudent = kpiData.getCell(8);
			if (nameOfStudent != null && nameOfStudent.getStringCellValue() != null) {
				Field field = ProgressReportKPIManpowerDevelopment.class.getDeclaredField("nameOfStudent");
				manpowerDevelopment.setNameOfStudent(getDataTypeLengthForDescription(nameOfStudent.getStringCellValue().trim(), field, tableName));
			}
			Cell citizenship = kpiData.getCell(12);
			if (citizenship != null && citizenship.getStringCellValue() != null) {
				Field field = ProgressReportKPIManpowerDevelopment.class.getDeclaredField("citizenship");
				manpowerDevelopment.setCitizenship(getDataTypeLengthForDescription(citizenship.getStringCellValue().trim(), field, tableName));
			}
			Cell currentStatus = kpiData.getCell(13);
			if (currentStatus != null && currentStatus.getStringCellValue() != null) {
				 String currentStatusCode = progressReportDao.getManpowerCurrentStatus(currentStatus.getStringCellValue());
				manpowerDevelopment.setCurrentStatusCode(currentStatusCode);
			}
			Cell dateEnrolled = kpiData.getCell(16);
			if (dateEnrolled != null && dateEnrolled.getCellType().equals(CellType.NUMERIC) && dateEnrolled.getDateCellValue() != null) {
				manpowerDevelopment.setDateEnrolled(dateEnrolled.getDateCellValue());
			} else if (dateEnrolled != null && dateEnrolled.getCellType().equals(CellType.STRING) && dateEnrolled.getStringCellValue() != null && !dateEnrolled.getStringCellValue().equals("")) {
				manpowerDevelopment.setDateEnrolled(prepareDateValue(dateEnrolled.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell dateGraduated = kpiData.getCell(18);
			if (dateGraduated != null && dateGraduated.getCellType().equals(CellType.NUMERIC) && dateGraduated.getDateCellValue() != null) {
				manpowerDevelopment.setDateGraduated(dateGraduated.getDateCellValue());
			} else if (dateGraduated != null  && dateGraduated.getCellType().equals(CellType.STRING) && dateGraduated.getStringCellValue() != null && !dateGraduated.getStringCellValue().equals("")) {
				manpowerDevelopment.setDateGraduated(prepareDateValue(dateGraduated.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell dateJoining = kpiData.getCell(21);
			if (dateJoining != null && dateJoining.getCellType().equals(CellType.NUMERIC) && dateJoining.getDateCellValue() != null) {
				manpowerDevelopment.setDateOfJoining(dateJoining.getDateCellValue());
			} else if (dateJoining != null  && dateJoining.getCellType().equals(CellType.STRING) && dateJoining.getStringCellValue() != null && !dateJoining.getStringCellValue().equals("")) {
				manpowerDevelopment.setDateOfJoining(prepareDateValue(dateJoining.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell dateLeaving = kpiData.getCell(25);
			if (dateLeaving != null && dateLeaving.getCellType().equals(CellType.NUMERIC) && dateLeaving.getDateCellValue() != null) {
				manpowerDevelopment.setDateOfLeaving(dateLeaving.getDateCellValue());
			} else if (dateLeaving != null  && dateLeaving.getCellType().equals(CellType.STRING) && dateLeaving.getStringCellValue() != null && !dateLeaving.getStringCellValue().equals("") ) {
				manpowerDevelopment.setDateOfLeaving(prepareDateValue(dateLeaving.getStringCellValue(), KPI_DATE_FORMAT));
			}
			manpowerDevelopment.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			manpowerDevelopment.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			manpowerDevelopment.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(manpowerDevelopment);
			}
		}
		}
		catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiIManpowerDevelopment", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiPostDocsEmployed(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPIPostDocsEmployed docsEmployed = new ProgressReportKPIPostDocsEmployed();
		String tableName = "PROGRESS_REPORT_KPI_POST_DOCS_EMPLOYED";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell employeeName = kpiData.getCell(8);
			if (employeeName != null && employeeName.getStringCellValue() != null) {
				Field field = ProgressReportKPIPostDocsEmployed.class.getDeclaredField("employeeName");
				docsEmployed.setEmployeeName(getDataTypeLengthForDescription(employeeName.getStringCellValue().trim(), field, tableName));
			}
			Cell nationality = kpiData.getCell(12);
			if (nationality != null && nationality.getStringCellValue() != null) {
				docsEmployed.setNationality(nationality.getStringCellValue());
			}
			Cell address = kpiData.getCell(13);
			if (address != null && address.getStringCellValue() != null) {
				Field field = ProgressReportKPIPostDocsEmployed.class.getDeclaredField("permanentResidence");
				docsEmployed.setPermanentResidence(getDataTypeLengthForDescription(address.getStringCellValue().trim(), field, tableName));
			}
			Cell employmentStartDate = kpiData.getCell(16);
			if (employmentStartDate != null && employmentStartDate.getCellType().equals(CellType.NUMERIC) && employmentStartDate.getDateCellValue() != null) {
				docsEmployed.setEmploymentStartDate(employmentStartDate.getDateCellValue());
			} else if (employmentStartDate != null && employmentStartDate.getCellType().equals(CellType.STRING) && employmentStartDate.getStringCellValue() != null && !employmentStartDate.getStringCellValue().equals("")) {
				docsEmployed.setEmploymentStartDate(prepareDateValue(employmentStartDate.getStringCellValue(), KPI_DATE_FORMAT));
			}
			docsEmployed.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			docsEmployed.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			docsEmployed.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(docsEmployed);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiPostDocsEmployed", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiCompetitiveGrants(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPICompetitiveGrants competitiveGrants = new ProgressReportKPICompetitiveGrants();
		String tableName = "PROGRESS_REPORT_KPI_COMPETITIVE_GRANTS";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell nameOfGrantRecieved = kpiData.getCell(8);
			if (nameOfGrantRecieved != null && nameOfGrantRecieved.getStringCellValue() != null) {
				Field field = ProgressReportKPICompetitiveGrants.class.getDeclaredField("nameOfGrantReceived");
				competitiveGrants.setNameOfGrantReceived(getDataTypeLengthForDescription(nameOfGrantRecieved.getStringCellValue().trim(), field, tableName));
			}
			Cell projectReferenceNumber = kpiData.getCell(12);
			if (projectReferenceNumber != null && projectReferenceNumber.getStringCellValue() != null) {
				Field field = ProgressReportKPICompetitiveGrants.class.getDeclaredField("projectReferenceNo");
				competitiveGrants.setProjectReferenceNo(getDataTypeLengthForDescription(projectReferenceNumber.getStringCellValue().trim(), field, tableName));
			}
			Cell fundingAgency = kpiData.getCell(13);
			if (fundingAgency != null && fundingAgency.getStringCellValue() != null) {
				String sponsorName = progressReportDao.getsponsorCodeBySponsorName(fundingAgency.getStringCellValue());
				if(sponsorName != null) {
				competitiveGrants.setSponsorCode(sponsorName);
				}
			}
			Cell recipientOfGrant = kpiData.getCell(16);
			if (recipientOfGrant != null && recipientOfGrant.getStringCellValue() != null) {
				Field field = ProgressReportKPICompetitiveGrants.class.getDeclaredField("recipientOfGrant");
				competitiveGrants.setRecipientOfGrant(getDataTypeLengthForDescription(recipientOfGrant.getStringCellValue().trim(), field, tableName));
			}
			Cell hostInstitution = kpiData.getCell(18);
			if (hostInstitution != null && hostInstitution.getStringCellValue() != null) {
				Field field = ProgressReportKPICompetitiveGrants.class.getDeclaredField("hostInsitution");
				competitiveGrants.setHostInsitution(getDataTypeLengthForDescription(hostInstitution.getStringCellValue().trim(), field, tableName));
			}
			Cell directCost = kpiData.getCell(21);
			if (directCost != null) {
				competitiveGrants.setDirectCost(BigDecimal.valueOf(directCost.getNumericCellValue()));
			}
			Cell indirectCost = kpiData.getCell(25);
			if (indirectCost != null) {
				competitiveGrants.setIndirectCost(BigDecimal.valueOf(indirectCost.getNumericCellValue()));
			}
			Cell projectStartDate = kpiData.getCell(30);
			if (projectStartDate != null && projectStartDate.getCellType().equals(CellType.NUMERIC) && projectStartDate.getDateCellValue() != null) {
				competitiveGrants.setProjectStartDate(projectStartDate.getDateCellValue());
			} else if (projectStartDate != null && projectStartDate.getCellType().equals(CellType.STRING) && projectStartDate.getStringCellValue() != null && !projectStartDate.getStringCellValue().equals("")) {
				competitiveGrants.setProjectStartDate(prepareDateValue(projectStartDate.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell projectEndDate = kpiData.getCell(35);
			if (projectEndDate != null && projectEndDate.getCellType().equals(CellType.NUMERIC) && projectEndDate.getDateCellValue() != null) {
				competitiveGrants.setProjectEndDate(projectEndDate.getDateCellValue());
			} else if (projectEndDate != null && projectEndDate.getCellType().equals(CellType.STRING) && projectEndDate.getStringCellValue() != null && !projectEndDate.getStringCellValue().equals("")) {
				competitiveGrants.setProjectEndDate(prepareDateValue(projectEndDate.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell projectTitle = kpiData.getCell(39);
			if (projectTitle != null && projectTitle.getStringCellValue() != null) {
				Field field = ProgressReportKPICompetitiveGrants.class.getDeclaredField("projectTitle");
				competitiveGrants.setProjectTitle(getDataTypeLengthForDescription(projectTitle.getStringCellValue().trim(), field, tableName));
			}
			competitiveGrants.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			competitiveGrants.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			competitiveGrants.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(competitiveGrants);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiCompetitiveGrants", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiConferencePresentation(AwardProgressReportKPISummary awardProgressReportKPISummary, String description, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPIConferencePresentation conferencePresentation = new ProgressReportKPIConferencePresentation();
		String tableName = "PROGRESS_REPORT_KPI_CONFERENCE_PRESENTATION";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell name = kpiData.getCell(12);
			if (name != null && name.getStringCellValue() != null && description.contains("Number of presentations at major conferences")) {
				Field field = ProgressReportKPIConferencePresentation.class.getDeclaredField("nameOfPresenter");
				conferencePresentation.setNameOfPresenter(getDataTypeLengthForDescription(name.getStringCellValue().trim(), field, tableName));
			} else if(name != null && name.getStringCellValue() != null) {
				Field field = ProgressReportKPIConferencePresentation.class.getDeclaredField("organiser");
				conferencePresentation.setOrganiser(getDataTypeLengthForDescription(name.getStringCellValue().trim(), field, tableName));
			}
			Cell conferenceTitle = kpiData.getCell(13);
			if (conferenceTitle != null && conferenceTitle.getStringCellValue() != null) {
				Field field = ProgressReportKPIConferencePresentation.class.getDeclaredField("conferenceTitle");
				conferencePresentation.setConferenceTitle(getDataTypeLengthForDescription(conferenceTitle.getStringCellValue().trim(), field, tableName));
			}
			Cell commentOrLocation = kpiData.getCell(16);
			if(commentOrLocation != null && commentOrLocation.getStringCellValue() != null && description.contains("Number of presentations at major conferences")) {
				Field field = ProgressReportKPIConferencePresentation.class.getDeclaredField("conferenceLocation");
				conferencePresentation.setConferenceLocation(getDataTypeLengthForDescription(commentOrLocation.getStringCellValue().trim(), field, tableName));
			} else if(commentOrLocation != null  && commentOrLocation.getStringCellValue() != null && !commentOrLocation.getStringCellValue().equals("")) {
				Field field = ProgressReportKPIConferencePresentation.class.getDeclaredField("comments");
				conferencePresentation.setComments(getDataTypeLengthForDescription(commentOrLocation.getStringCellValue().trim(), field, tableName));
			}
			Cell date = kpiData.getCell(18);
			if (date != null && date.getCellType().equals(CellType.NUMERIC) && date.getDateCellValue() != null) {
				conferencePresentation.setDate(date.getDateCellValue());
			} else if (date != null  && date.getCellType().equals(CellType.STRING) && date.getStringCellValue() != null && !date.getStringCellValue().equals("")) {
				conferencePresentation.setDate(prepareDateValue(date.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell title = kpiData.getCell(21);
			if (title != null && title.getStringCellValue() != null) {
				Field field = ProgressReportKPIConferencePresentation.class.getDeclaredField("title");
				conferencePresentation.setTitle(getDataTypeLengthForDescription(title.getStringCellValue().trim(), field, tableName));
			}
			conferencePresentation.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			conferencePresentation.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			conferencePresentation.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(conferencePresentation);
			}
		}
		}catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiConferencePresentation", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiHealthSpecificOutcomes(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPIHealthSpecificOutcomes specificOutcomes = new ProgressReportKPIHealthSpecificOutcomes();
		try {
			String tableName = "PROGRESS_REPORT_KPI_HEALTH_SPECIFIC_OUTCOMES";
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell title = kpiData.getCell(8);
			if (title != null && title.getStringCellValue() != null) {
				Field field = ProgressReportKPIHealthSpecificOutcomes.class.getDeclaredField("title");
				specificOutcomes.setTitle(getDataTypeLengthForDescription(title.getStringCellValue().trim(), field, tableName));
			}
			Cell date = kpiData.getCell(18);
			if (date != null && date.getCellType().equals(CellType.NUMERIC) && date.getDateCellValue() != null) {
				specificOutcomes.setDateEstablished(date.getDateCellValue());
			} else if(date != null  && date.getCellType().equals(CellType.STRING) && date.getStringCellValue() != null && !date.getStringCellValue().equals("")) {
				specificOutcomes.setDateEstablished(prepareDateValue(date.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell numberOfLifeYears = kpiData.getCell(19);
			if (numberOfLifeYears != null && numberOfLifeYears.getStringCellValue() != null) {
				specificOutcomes.setNumberOfLifeYears(String.valueOf(numberOfLifeYears.getNumericCellValue()));
			}
			specificOutcomes.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			specificOutcomes.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			specificOutcomes.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(specificOutcomes);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiHealthSpecificOutcomes", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiUndergraduateStudent(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPIUndergraduateStudent underGraduateStudent = new ProgressReportKPIUndergraduateStudent();
		String tableName = "PROGRESS_REPORT_KPI_UNDERGRADUATE_STUDENT";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell nameOfStudent = kpiData.getCell(8);
			if (nameOfStudent != null && nameOfStudent.getStringCellValue() != null) {
				Field field = ProgressReportKPIUndergraduateStudent.class.getDeclaredField("nameOfStudent");
				underGraduateStudent.setNameOfStudent(getDataTypeLengthForDescription(nameOfStudent.getStringCellValue().trim(), field, tableName));
			}
			Cell citizenship = kpiData.getCell(12);
			if (citizenship != null && citizenship.getStringCellValue() != null) {
				Field field = ProgressReportKPIUndergraduateStudent.class.getDeclaredField("citizenship");
				underGraduateStudent.setCitizenship(getDataTypeLengthForDescription(citizenship.getStringCellValue().trim(), field, tableName));
			}
			Cell currentStatus = kpiData.getCell(13);
			if (currentStatus != null && currentStatus.getStringCellValue() != null) {
			    String currentStatusCode = progressReportDao.getManpowerCurrentStatus(currentStatus.getStringCellValue());
				underGraduateStudent.setCurrentStatusCode(currentStatusCode);
			}
			Cell dateJoined = kpiData.getCell(21);
			if (dateJoined != null && dateJoined.getCellType().equals(CellType.NUMERIC) && dateJoined.getDateCellValue() != null) {
				underGraduateStudent.setDateOfJoining(dateJoined.getDateCellValue());
			} else if (dateJoined != null  && dateJoined.getCellType().equals(CellType.STRING) && dateJoined.getStringCellValue() != null && !dateJoined.getStringCellValue().equals("")) {
				underGraduateStudent.setDateOfJoining(prepareDateValue(dateJoined.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell dateLeaving = kpiData.getCell(25);
			if (dateLeaving != null && dateLeaving.getCellType().equals(CellType.NUMERIC) && dateLeaving.getDateCellValue() != null) {
				underGraduateStudent.setDateOfLeaving(dateLeaving.getDateCellValue());
			} else if (dateLeaving != null  && dateLeaving.getCellType().equals(CellType.STRING) && dateLeaving.getStringCellValue() != null && !dateLeaving.getStringCellValue().equals("")) {
				underGraduateStudent.setDateOfLeaving(prepareDateValue(dateLeaving.getStringCellValue(), KPI_DATE_FORMAT));
			}
			underGraduateStudent.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			underGraduateStudent.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			underGraduateStudent.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(underGraduateStudent);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiUndergraduateStudent", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiSuccessfulStartups(AwardProgressReportKPISummary awardProgressReportKPISummary, String description, Row kpiData, Integer kpiCellCount) {
		try {
		Cell dateEstablished = null;
		Cell valuationCriteria = null;
		Cell annualRevenueCrieria = null;
		ProgressReportKPISuccessfulStartups successfullStartups = new ProgressReportKPISuccessfulStartups();
		String tableName = "PROGRESS_REPORT_KPI_SUCCESSFUL_STARTUPS";
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell nameOfCompany = kpiData.getCell(8);
			if (nameOfCompany != null && nameOfCompany.getStringCellValue() != null) {
				Field field = ProgressReportKPISuccessfulStartups.class.getDeclaredField("nameOfCompany");
				successfullStartups.setNameOfCompany(getDataTypeLengthForDescription(nameOfCompany.getStringCellValue().trim(), field, tableName));
			}
			Cell companyUen = kpiData.getCell(12);
			if (companyUen != null && companyUen.getStringCellValue() != null) {
				successfullStartups.setCompanyUen(companyUen.getStringCellValue());
			}
			Cell dateOfEstablished = kpiData.getCell(13);
			if (dateOfEstablished != null && dateOfEstablished.getCellType().equals(CellType.NUMERIC) && dateOfEstablished.getDateCellValue() != null) {
				successfullStartups.setDateOfEstablishment(dateOfEstablished.getDateCellValue());
			} else if (dateOfEstablished != null  && dateOfEstablished.getCellType().equals(CellType.STRING) && dateOfEstablished.getStringCellValue() != null && !dateOfEstablished.getStringCellValue().equals("")) {
				successfullStartups.setDateOfEstablishment(prepareDateValue(dateOfEstablished.getStringCellValue(), KPI_DATE_FORMAT));
			}
			dateEstablished = kpiData.getCell(16);
			valuationCriteria = kpiData.getCell(18);
			annualRevenueCrieria = kpiData.getCell(21);
			if (description.contains("Total Number of Successful Start-Ups")) {
				if (dateEstablished != null && !(dateEstablished.getCellType().equals(CellType.NUMERIC)) && dateEstablished.getStringCellValue() != null) {
					Field field = ProgressReportKPISuccessfulStartups.class.getDeclaredField("externalFundingCriteria");
					successfullStartups.setExternalFundingCriteria(getDataTypeLengthForDescription(dateEstablished.getStringCellValue().trim(), field, tableName));
				}
				if(valuationCriteria != null && valuationCriteria.getStringCellValue() != null) {
					Field field = ProgressReportKPISuccessfulStartups.class.getDeclaredField("valuationCriteria");
					successfullStartups.setValuationCriteria(getDataTypeLengthForDescription(valuationCriteria.getStringCellValue().trim(), field, tableName));
				}
				if(annualRevenueCrieria != null && annualRevenueCrieria.getStringCellValue() != null) {
					Field field = ProgressReportKPISuccessfulStartups.class.getDeclaredField("annualRevenueCriteria");
					successfullStartups.setAnnualRevenueCriteria(getDataTypeLengthForDescription(annualRevenueCrieria.getStringCellValue().trim(), field, tableName));
				}
			} else {
				if (dateEstablished != null && dateEstablished.getDateCellValue() != null) {
					successfullStartups.setDateEstablished(dateEstablished.getDateCellValue());
				}
				if (valuationCriteria != null && valuationCriteria.getStringCellValue() != null) {
					Field field = ProgressReportKPISuccessfulStartups.class.getDeclaredField("externalFundingCriteria");
					successfullStartups.setExternalFundingCriteria(getDataTypeLengthForDescription(valuationCriteria.getStringCellValue().trim(), field, tableName));
				}
				if(annualRevenueCrieria != null && annualRevenueCrieria.getStringCellValue() != null) {
					Field field = ProgressReportKPISuccessfulStartups.class.getDeclaredField("valuationCriteria");
					successfullStartups.setValuationCriteria(getDataTypeLengthForDescription(annualRevenueCrieria.getStringCellValue().trim(), field, tableName));
				}
			}
			successfullStartups.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			successfullStartups.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			successfullStartups.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(successfullStartups);
			}
		}
		}catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiSuccessfulStartups", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiLicenses(AwardProgressReportKPISummary awardProgressReportKPISummary, String description, Row kpiData,	Integer kpiCellCount) {
		try {
			ProgressReportKPILicenses licenses = new ProgressReportKPILicenses();
			String tableName = "PROGRESS_REPORT_KPI_LICENSES";
			Cell detailsOfLicense = null;			
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell nameOfLicence = kpiData.getCell(8);
			if (nameOfLicence != null && nameOfLicence.getStringCellValue() != null) {
				Field field = ProgressReportKPILicenses.class.getDeclaredField("nameOfLicense");
				licenses.setNameOfLicense(getDataTypeLengthForDescription(nameOfLicence.getStringCellValue().trim(), field, tableName));
			}
			Cell companyUen = kpiData.getCell(12);
			if (companyUen != null && companyUen.getStringCellValue() != null) {
				licenses.setCompanyUen(companyUen.getStringCellValue());
			}
			Cell startDate = kpiData.getCell(13);
			if (startDate != null && startDate.getCellType().equals(CellType.NUMERIC) && startDate.getDateCellValue() != null) {
				licenses.setStartDate(startDate.getDateCellValue());
			} else if (startDate != null  && startDate.getCellType().equals(CellType.STRING) && startDate.getStringCellValue() != null && !startDate.getStringCellValue().equals("")) {
				licenses.setStartDate(prepareDateValue(startDate.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell licensingPeriod = kpiData.getCell(16);
			if (licensingPeriod != null && licensingPeriod.getStringCellValue() != null) {
				licenses.setLicensingPeriod(licensingPeriod.getStringCellValue());
			}
			detailsOfLicense = kpiData.getCell(18);
			if (detailsOfLicense != null && !detailsOfLicense.getCellType().equals(CellType.NUMERIC) && detailsOfLicense.getStringCellValue() != null && !(description.equals("TOTAL Number of Licenses [Industry (MNC + LLE + SME) + Public Agency]"))) {
				Field field = ProgressReportKPILicenses.class.getDeclaredField("detailsOfLicense");
				licenses.setDetailsOfLicense(getDataTypeLengthForDescription(detailsOfLicense.getStringCellValue().trim(), field, tableName));
			} else {
				detailsOfLicense = kpiData.getCell(21);
				if (detailsOfLicense != null && detailsOfLicense.getStringCellValue() != null) {
					licenses.setDetailsOfLicense(detailsOfLicense.getStringCellValue());
				}
			}
			licenses.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			licenses.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			licenses.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(licenses);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiLicenses", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiPatents(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPIPatents patents = new ProgressReportKPIPatents();
		String tableName = "PROGRESS_REPORT_KPI_PATENTS";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell title = kpiData.getCell(8);
			if (title != null && title.getStringCellValue() != null) {
				Field field = ProgressReportKPIPatents.class.getDeclaredField("title");
				patents.setTitle(getDataTypeLengthForDescription(title.getStringCellValue().trim(), field, tableName));
			}
			Cell description = kpiData.getCell(12);
			if (description != null && description.getStringCellValue() != null) {
				Field field = ProgressReportKPIPatents.class.getDeclaredField("description");
				patents.setDescription(getDataTypeLengthForDescription(description.getStringCellValue().trim(), field, tableName));
			}
			Cell ownership = kpiData.getCell(13);
			if (ownership != null && ownership.getStringCellValue() != null) {
				Field field = ProgressReportKPIPatents.class.getDeclaredField("ownership");
				patents.setOwnership(getDataTypeLengthForDescription(ownership.getStringCellValue().trim(), field, tableName));
			}
			Cell patentNumber = kpiData.getCell(16);
			if (patentNumber != null && patentNumber.getStringCellValue() != null) {
				Field field = ProgressReportKPIPatents.class.getDeclaredField("patentNumber");
				patents.setPatentNumber(getDataTypeLengthForDescription(patentNumber.getStringCellValue().trim(), field, tableName));
			}
			Cell dateField = kpiData.getCell(21);
			if (dateField != null && dateField.getCellType().equals(CellType.NUMERIC) && dateField.getDateCellValue() != null) {
				patents.setDateFiled(dateField.getDateCellValue());
			} else if (dateField != null && dateField.getCellType().equals(CellType.STRING) && dateField.getStringCellValue() != null) {
				patents.setDateFiled(prepareDateValue(dateField.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell dateGranted = kpiData.getCell(25);
			if (dateGranted != null && dateGranted.getCellType().equals(CellType.NUMERIC) && dateGranted.getDateCellValue() != null) {
				patents.setDateGranted(dateGranted.getDateCellValue());
			} else if (dateGranted != null && dateGranted.getCellType().equals(CellType.STRING) && dateGranted.getStringCellValue() != null) {
				patents.setDateGranted(prepareDateValue(dateGranted.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell comments = kpiData.getCell(31);
			if (comments != null && comments.getStringCellValue() != null) {
				Field field = ProgressReportKPIPatents.class.getDeclaredField("comments");
				patents.setComments(getDataTypeLengthForDescription(comments.getStringCellValue().trim(), field, tableName));
			}
			patents.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			patents.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			patents.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(patents);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiPatents", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiTechnologiesDeployed(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData,	Integer kpiCellCount) {
		ProgressReportKPITechnologiesDeployed progressReportKPITechnologiesDeployed = new ProgressReportKPITechnologiesDeployed();
		try {
			String tableName = "PROGRESS_REPORT_KPI_TECHNOLOGIES_DEPLOYED";
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell companyName = kpiData.getCell(8);
			if (companyName != null && companyName.getStringCellValue() != null) {
				Field field = ProgressReportKPITechnologiesDeployed.class.getDeclaredField("nameOfCompany");
				progressReportKPITechnologiesDeployed.setNameOfCompany(getDataTypeLengthForDescription(companyName.getStringCellValue().trim(), field, tableName));
			}
			Cell countryCode = kpiData.getCell(12);
			if (countryCode != null && countryCode.getStringCellValue() != null) {
				Country country = icsDao.fetchCountryCodeByCountryName(countryCode.getStringCellValue());
				if (country != null) {
					progressReportKPITechnologiesDeployed.setCountryCode(country.getCountryCode());
				}
			}
			Cell companyUen = kpiData.getCell(13);
			if (companyUen != null && companyUen.getStringCellValue() != null) {
				progressReportKPITechnologiesDeployed.setCompanyUen(companyUen.getStringCellValue());
			}
			Cell dateOfDeploying = kpiData.getCell(16);
			if (dateOfDeploying != null && dateOfDeploying.getCellType().equals(CellType.NUMERIC) && dateOfDeploying.getDateCellValue() != null) {
				progressReportKPITechnologiesDeployed.setDateOfDeploying(dateOfDeploying.getDateCellValue());
			} else if (dateOfDeploying != null  && dateOfDeploying.getCellType().equals(CellType.STRING) && dateOfDeploying.getStringCellValue() != null && !dateOfDeploying.getStringCellValue().equals("")) {
				progressReportKPITechnologiesDeployed.setDateOfDeploying(prepareDateValue(dateOfDeploying.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell detaisOfTechnologies = kpiData.getCell(18);
			if (detaisOfTechnologies != null && detaisOfTechnologies.getStringCellValue() != null) {
				Field field = ProgressReportKPITechnologiesDeployed.class.getDeclaredField("detailsOfTechnology");
				progressReportKPITechnologiesDeployed.setDetailsOfTechnology(getDataTypeLengthForDescription(detaisOfTechnologies.getStringCellValue().trim(), field, tableName));
			}
			progressReportKPITechnologiesDeployed.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			progressReportKPITechnologiesDeployed.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			progressReportKPITechnologiesDeployed.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPITechnologiesDeployed);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiTechnologiesDeployed", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiInkindContributions(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPIInkindContributions inKindContributions = new ProgressReportKPIInkindContributions();
		try {
			String tableName = "PROGRESS_REPORT_KPI_INKIND_CONTRIBUTIONS";
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell companyName = kpiData.getCell(8);
			if (companyName != null && companyName.getStringCellValue() != null) {
				Field field = ProgressReportKPIInkindContributions.class.getDeclaredField("nameOfCompany");
				inKindContributions.setNameOfCompany(getDataTypeLengthForDescription(companyName.getStringCellValue().trim(), field, tableName));
			}
			Cell countryCode = kpiData.getCell(12);
			if (countryCode != null && countryCode.getStringCellValue() != null) {
				Country country = icsDao.fetchCountryCodeByCountryName(countryCode.getStringCellValue());
				if (country != null) {
					inKindContributions.setCountryCode(country.getCountryCode());
				}
			}
			Cell companyUen = kpiData.getCell(13);
			if (companyUen != null && companyUen.getStringCellValue() != null) {
				inKindContributions.setCompanyUen(companyUen.getStringCellValue());
			}
			Cell dateOfContribution = kpiData.getCell(16);
			if (dateOfContribution != null && dateOfContribution.getCellType().equals(CellType.NUMERIC) && dateOfContribution.getDateCellValue() != null) {
				inKindContributions.setDateOfContribution(dateOfContribution.getDateCellValue());
			} else if (dateOfContribution != null  && dateOfContribution.getCellType().equals(CellType.STRING) && dateOfContribution.getStringCellValue() != null && !dateOfContribution.getStringCellValue().equals("")) {
				inKindContributions.setDateOfContribution(prepareDateValue(dateOfContribution.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell amount = kpiData.getCell(18);
			if (amount != null && amount.getCellType().equals(CellType.STRING) && !amount.getStringCellValue().isEmpty() && amount.getStringCellValue() != null) {
				inKindContributions.setAmount(new BigDecimal(amount.getStringCellValue()));
			} else if (amount != null && amount.getCellType().equals(CellType.NUMERIC)) {
				inKindContributions.setAmount(BigDecimal.valueOf(amount.getNumericCellValue()));
			}
			inKindContributions.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			inKindContributions.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			inKindContributions.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(inKindContributions);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiInkindContributions", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiCashFunding(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		try {
		ProgressReportKPICashFunding progressReportKPICashFunding = new ProgressReportKPICashFunding();
		String tableName = "PROGRESS_REPORT_KPI_CASH_FUNDING";
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
				Cell companyName = kpiData.getCell(8);
				if (companyName != null && companyName.getStringCellValue() != null) {
					Field field = ProgressReportKPICashFunding.class.getDeclaredField("nameOfCompany");
					progressReportKPICashFunding.setNameOfCompany(getDataTypeLengthForDescription(companyName.getStringCellValue().trim(), field, tableName));
				}
				Cell countryCode = kpiData.getCell(12);
				if (countryCode != null && countryCode.getStringCellValue() != null) {
					Country country = icsDao.fetchCountryCodeByCountryName(countryCode.getStringCellValue());
					if (country != null) {
						progressReportKPICashFunding.setCountryCode(country.getCountryCode());
					}
				}
				Cell companyUen = kpiData.getCell(13);
				if (companyUen != null && companyUen.getStringCellValue() != null) {
					progressReportKPICashFunding.setCompanyUen(companyUen.getStringCellValue());
				}
				Cell dateOfContribution = kpiData.getCell(16);
				if (dateOfContribution != null && dateOfContribution.getCellType().equals(CellType.NUMERIC) && dateOfContribution.getDateCellValue() != null) {
					progressReportKPICashFunding.setDateOfContribution(dateOfContribution.getDateCellValue());
				} else if(dateOfContribution != null  && dateOfContribution.getCellType().equals(CellType.STRING) && dateOfContribution.getStringCellValue() != null && !dateOfContribution.getStringCellValue().equals("")) {
					progressReportKPICashFunding.setDateOfContribution(prepareDateValue(dateOfContribution.getStringCellValue(), KPI_DATE_FORMAT));
				}
				Cell amount = kpiData.getCell(18);
				if (amount != null && amount.getCellType().equals(CellType.NUMERIC)) {
					progressReportKPICashFunding.setAmount(BigDecimal.valueOf(amount.getNumericCellValue()));
				} else if(amount != null && amount.getCellType().equals(CellType.STRING) && amount.getStringCellValue() != null) {
					progressReportKPICashFunding.setAmount(new BigDecimal(amount.getStringCellValue()));
				}
				progressReportKPICashFunding.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
				progressReportKPICashFunding.setProgressReportId(
						awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
				progressReportKPICashFunding.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
				progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPICashFunding);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiCashFunding", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiTechnologyDisclosure(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData,	Integer kpiCellCount) {
		ProgressReportKPITechnologyDisclosure progressReportKPITechnologyDisclosure = new ProgressReportKPITechnologyDisclosure();
		String tableName = "PROGRESS_REPORT_KPI_TECHNOLOGY_DISCLOSURE";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell tiltle = kpiData.getCell(8);
			if (tiltle != null && tiltle.getStringCellValue() != null) {
				Field field = ProgressReportKPITechnologyDisclosure.class.getDeclaredField("titleOfPatent");
				progressReportKPITechnologyDisclosure.setTitleOfPatent(getDataTypeLengthForDescription(tiltle.getStringCellValue().trim(), field, tableName));
			}
			Cell comment = kpiData.getCell(12);
			if (comment != null && comment.getStringCellValue() != null) {
				Field field = ProgressReportKPITechnologyDisclosure.class.getDeclaredField("comments");
				progressReportKPITechnologyDisclosure.setComments(getDataTypeLengthForDescription(comment.getStringCellValue().trim(), field, tableName));
			}
			Cell author = kpiData.getCell(13);
			if (author != null && author.getStringCellValue() != null) {
				Field field = ProgressReportKPITechnologyDisclosure.class.getDeclaredField("authorName");
				progressReportKPITechnologyDisclosure.setAuthorName(getDataTypeLengthForDescription(author.getStringCellValue().trim(), field, tableName));
			}
			Cell date = kpiData.getCell(16);
			if (date != null && date.getCellType().equals(CellType.NUMERIC) && date.getDateCellValue() != null) {
				progressReportKPITechnologyDisclosure.setDateOffilling(date.getDateCellValue());
			} else if (date != null  && date.getCellType().equals(CellType.STRING) && date.getStringCellValue() != null && !date.getStringCellValue().equals("")) {
				progressReportKPITechnologyDisclosure.setDateOffilling(prepareDateValue(date.getStringCellValue(), KPI_DATE_FORMAT));
			}
			progressReportKPITechnologyDisclosure.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			progressReportKPITechnologyDisclosure.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			progressReportKPITechnologyDisclosure.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPITechnologyDisclosure);
			}
			}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiTechnologyDisclosure", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Integer prepareKpiCollaborationProjects(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer kpiCellCount) {
		ProgressReportKPICollaborationProjects progressReportKPICollaborationProjects = new ProgressReportKPICollaborationProjects();
		String tableName = "PROGRESS_REPORT_KPI_COLLABORATION_PROJECTS";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			kpiCellCount--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell tiltle = kpiData.getCell(8);
			if (tiltle != null && tiltle.getStringCellValue() != null) {
				Field field = ProgressReportKPICollaborationProjects.class.getDeclaredField("projectTitle");
				progressReportKPICollaborationProjects.setProjectTitle(getDataTypeLengthForDescription(tiltle.getStringCellValue().trim(), field, tableName));
			}
			Cell projectDescription = kpiData.getCell(12);
			if (projectDescription != null && projectDescription.getStringCellValue() != null) {
				Field field = ProgressReportKPICollaborationProjects.class.getDeclaredField("projectDescription");
				progressReportKPICollaborationProjects.setProjectDescription(getDataTypeLengthForDescription(projectDescription.getStringCellValue().trim(), field, tableName));
			}
			Cell projectStartDate = kpiData.getCell(13);
			if (projectStartDate != null && projectStartDate.getCellType().equals(CellType.NUMERIC) && projectStartDate.getDateCellValue() != null) {
				progressReportKPICollaborationProjects.setProjectStartDate(projectStartDate.getDateCellValue());
			} else if(projectStartDate != null && projectStartDate.getCellType().equals(CellType.STRING) && !projectStartDate.getStringCellValue().isEmpty() && projectStartDate.getStringCellValue() != null) {
				progressReportKPICollaborationProjects.setProjectStartDate(prepareDateValue(projectStartDate.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell projectEndDate = kpiData.getCell(16);
			if(projectEndDate != null && projectEndDate.getCellType().equals(CellType.NUMERIC) && projectEndDate.getDateCellValue() != null) {
				progressReportKPICollaborationProjects.setProjectEndDate(projectEndDate.getDateCellValue());
			} else if (projectEndDate != null && projectEndDate.getCellType().equals(CellType.STRING) && !projectEndDate.getStringCellValue().isEmpty() && projectEndDate.getStringCellValue() != null) {
				progressReportKPICollaborationProjects.setProjectEndDate(prepareDateValue(projectEndDate.getStringCellValue(), KPI_DATE_FORMAT));
			}
			Cell collaboratingOrganization = kpiData.getCell(18);
			if (collaboratingOrganization != null && collaboratingOrganization.getStringCellValue() != null) {
				Field field = ProgressReportKPICollaborationProjects.class.getDeclaredField("collaboratingOrganization");
				progressReportKPICollaborationProjects.setCollaboratingOrganization(getDataTypeLengthForDescription(collaboratingOrganization.getStringCellValue().trim(), field, tableName));
			}
			Cell countryCode = kpiData.getCell(21);
			if (countryCode != null && countryCode.getStringCellValue() != null) {
				Country country = icsDao.fetchCountryCodeByCountryName(countryCode.getStringCellValue());
				if (country != null) {
					progressReportKPICollaborationProjects.setCountryCode(country.getCountryCode());
				}
			}
			Cell companyUen = kpiData.getCell(25);
			if (companyUen != null && companyUen.getStringCellValue() != null) {
				progressReportKPICollaborationProjects.setCompanyUen(companyUen.getStringCellValue());
			}
			progressReportKPICollaborationProjects.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			progressReportKPICollaborationProjects.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			progressReportKPICollaborationProjects.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPICollaborationProjects);
		   }
		}
		} catch(Exception e) {
			throw new ApplicationException("error occured in prepareKpiCollaborationProjects", e, Constants.JAVA_ERROR);
		}
		return kpiCellCount;
	}

	private Date prepareDateValue( String dateVal, String format) {
		Date date =  null;
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat(format);
			date = dateFormat.parse(dateVal);
			return date;
		} catch (Exception e) {
			logger.error("error in prepareDateValue {}", e.getMessage());
			return null;
		}
	}

	private Timestamp dateFormater(String dateVal) {
		Date date = null;
		Timestamp value = null;
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzzz yyyy");
			date = dateFormat.parse(dateVal);
			value = Timestamp.valueOf(new SimpleDateFormat("yyyy-MM-dd 00:00:00.0").format(date));
		} catch (Exception e) {
			logger.error("error in dateFormater {}", e.getMessage());
			return value;
		}
		return value;
	}

	private Integer prepareKpiImpactPublications(AwardProgressReportKPISummary awardProgressReportKPISummary, Row kpiData, Integer nextSection) {
		ProgressReportKPIImpactPublications progressReportKPIImpactPublications = new ProgressReportKPIImpactPublications();
		String tableName = "PROGRESS_REPORT_KPI_IMPACT_PUBLICATIONS";
		try {
		if (kpiData.getCell(8) != null && kpiData.getCell(8).getStringCellValue().contains("KPI No")) {
			nextSection--;
		} else {
			if (Boolean.FALSE.equals(isRowEmpty(kpiData))) {
			Cell tiltle = kpiData.getCell(8);
			if (tiltle != null && tiltle.getStringCellValue() != null && !tiltle.getStringCellValue().isEmpty()) {
				Field field = ProgressReportKPIImpactPublications.class.getDeclaredField("titleOfArticle");
				progressReportKPIImpactPublications.setTitleOfArticle(getDataTypeLengthForDescription(tiltle.getStringCellValue().trim(), field, tableName));
			}
			Cell journalName = kpiData.getCell(12);
			if (journalName != null && journalName.getStringCellValue() != null && !journalName.getStringCellValue().isEmpty()) {
				Field field = ProgressReportKPIImpactPublications.class.getDeclaredField("journalName");
				progressReportKPIImpactPublications.setJournalName(getDataTypeLengthForDescription(journalName.getStringCellValue().trim(), field, tableName));
			}

			Cell publicationDate =  kpiData.getCell(13);
			if (publicationDate != null && publicationDate.getCellType().equals(CellType.NUMERIC) && publicationDate.getDateCellValue() != null) {
				progressReportKPIImpactPublications.setPublicationDate(publicationDate.getDateCellValue());
			} else if(publicationDate != null && publicationDate.getCellType().equals(CellType.STRING) && !publicationDate.getStringCellValue().isEmpty() && publicationDate.getStringCellValue() != null) {
				progressReportKPIImpactPublications.setPublicationDate(prepareDateValue(publicationDate.getStringCellValue(), KPI_DATE_FORMAT));
			}

			Cell authour = kpiData.getCell(18);
			if (authour != null && authour.getStringCellValue() != null && !authour.getStringCellValue().isEmpty()) {
				Field field = ProgressReportKPIImpactPublications.class.getDeclaredField("authorName");
				progressReportKPIImpactPublications.setAuthorName(getDataTypeLengthForDescription(authour.getStringCellValue().trim(), field, tableName));
			}
			Cell publisher = kpiData.getCell(21);
			if (publisher != null && publisher.getStringCellValue() != null && !publisher.getStringCellValue().isEmpty()) {
				Field field = ProgressReportKPIImpactPublications.class.getDeclaredField("publisher");
				progressReportKPIImpactPublications.setPublisher(getDataTypeLengthForDescription(publisher.getStringCellValue().trim(), field, tableName));
			}
			progressReportKPIImpactPublications.setKpiCriteriaCode(awardProgressReportKPISummary.getKpiCriteriaTypeCode());
			progressReportKPIImpactPublications.setProgressReportId(awardProgressReportKPISummary.getAwardProgressReport().getProgressReportId());
			progressReportKPIImpactPublications.setKpiSummaryId(awardProgressReportKPISummary.getKpiSummaryId());
			progressReportDao.saveOrUpdateKPISummaryDetail(progressReportKPIImpactPublications);
			}
		}
		} catch(Exception e) {
			throw new ApplicationException("error prepareKpiImpactPublications", e, Constants.JAVA_ERROR);
		}
		return nextSection;
	}

	public static Boolean isRowEmpty(Row row) {
		try {
			int firstCol = row.getFirstCellNum();
			for (int cnt = 0; cnt < 4; cnt++) {
				Cell cell = row.getCell(firstCol + cnt);
				if (cell != null && cell.getCellType() != CellType.BLANK) {
					return Boolean.FALSE;
				}
			}
			return Boolean.TRUE;
		} catch (Exception e) {
			logger.error("error in isRowEmpty {}", e.getMessage());
			return Boolean.TRUE;
		}
	}

	private void saveAwardMilestone(List<AwardProgressReportMilestone> awardMileStones, Integer progressReportId,
			Integer awardId, ProgressReportVO progressReportVO) {
			if (awardMileStones != null && !awardMileStones.isEmpty()) {
				awardMileStones.forEach(awardMileStone -> {
					AwardProgressReportMilestone awardProgressReportMilestone = progressReportDao
							.checkProgressReportMilestoneExist(progressReportId, awardId, awardMileStone.getCommittedStartDate(), awardMileStone.getCommittedEndDate(),
									awardMileStone.getMilestone());
					if (awardProgressReportMilestone != null) {
						awardProgressReportMilestone.setMilestoneStatusCode(
								progressReportDao.getProgressMilestoneStatus(awardMileStone.getStatus()));
						awardProgressReportMilestone.setActualStartMonth(awardMileStone.getActualStartMonth());
						awardProgressReportMilestone.setActualEndMonth(awardMileStone.getActualEndMonth());
						awardProgressReportMilestone.setRemark(awardMileStone.getRemark());
						progressReportDao.saveOrUpdateProgressReportMilestone(awardProgressReportMilestone);
						progressReportVO.setProgressReportImported(true);
					}
				});
			}
	}

	private void prepareOverviewData(Integer dataLength, String achivementDescription, String cellValue, ProgressReportVO progressReportVO) {
		try {
			AwardProgressReportAchievement awardProgressReportAchievement = progressReportDao
					.checkProgressReportAchievementExist(achivementDescription, progressReportVO.getProgressReportId(),
							progressReportVO.getAwardId());
			if (awardProgressReportAchievement != null && cellValue != null) {
				String description = cellValue;
				if (cellValue.length() > dataLength) {
					description = cellValue.substring(0, dataLength);
				}
			    awardProgressReportAchievement.setDescription(description);
				progressReportDao.saveOrUpdateProgressReportAchievements(awardProgressReportAchievement);
				progressReportVO.setProgressReportImported(true);
			}
		} catch (Exception e) {
			logger.info("error in prepareOverviewData {}", e.getMessage());
		}
	}

	private String getDataTypeLengthForDescription(String cellValue, Field field, String tableName) {
		Column columnAnnotation = field.getAnnotation(Column.class);
		String description = cellValue;
		Integer dataLength = progressReportDao.getDataTypeLengthForDescription(columnAnnotation.name(), tableName);
		if (cellValue.length() > dataLength) {
			description = cellValue.substring(0, dataLength);
		}
		return description;
	}

	private Integer getDataTypeLengthForSummary(Field field, String tableName) {
		Column columnAnnotation = field.getAnnotation(Column.class);
		return progressReportDao.getDataTypeLengthForDescription(columnAnnotation.name(), tableName);
	}

	@Override
	public String getAwardProgressReportStartDate(String awardNumber, Integer awardId, String reportClassCode, Date dueDate) {
		Boolean flag;
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.CREATE_PROGRESS_REPORT);
		flag = commonDao.checkPersonHasRightInModule(Constants.AWARD_MODULE_CODE, awardId, rightNames, AuthenticatedUser.getLoginPersonId());
		if(Boolean.FALSE.equals(flag)) {
			String leadUnitNumber =  progressReportDao.getAwardLeadUnit(awardId);
			flag = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.CREATE_PROGRESS_REPORT, leadUnitNumber);
		}
		Boolean inProgressPRExist = Boolean.FALSE;
		Object reportStartDate = null;
		Object reportEndDate = null;
		Date awardStartDate = null;
		if(Boolean.TRUE.equals(flag)) {
			inProgressPRExist = progressReportDao.checkInprogressProgressReportExist(awardNumber);			
			if(inProgressPRExist.equals(Boolean.FALSE)) {
				if (dueDate != null) {
					Object[] reportingDates = progressReportDao.getProgressReportReportingPeriod(awardId, dueDate, reportClassCode);
					if (reportingDates != null) {
						reportStartDate = reportingDates[0];
						reportEndDate = reportingDates[1];
					} else {
						reportStartDate = progressReportDao.getReportPeriodStartDateByAwardNumber(awardNumber);
					}
				} else {
					reportStartDate = progressReportDao.getReportPeriodStartDateByAwardNumber(awardNumber);
				}
				awardStartDate = progressReportDao.getAwardStartDateByAwardNumber(awardNumber);
			}				
		}		
		Map<String, Object> result = new HashMap<>();
		result.put("reportStartDate", reportStartDate);
		result.put("reportEndDate", reportEndDate);
		result.put("awardStartDate", awardStartDate);
		result.put("inProgressPRExist", inProgressPRExist);
		result.put("canCreatePR", flag);
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public String updateProgressReportDates(ProgressReportVO progressReportVO) {
		progressReportDao.updateProgressReportDates(progressReportVO.getProgressReportId(), progressReportVO.getReportStartDate(), progressReportVO.getReportEndDate(), progressReportVO.getTitle());
		progressReportDao.getProgressReportKPISummaryById(progressReportVO.getProgressReportId()).forEach(summary -> {
			switch (summary.getSectionCode()) {
				case "1":
					progressReportDao.updateKPIAchievedCountImpactPublication(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "2":
					progressReportDao.updateKPIAchievedCountCollaborationProjects(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "3":
					progressReportDao.updateKPIAchievedCountTechnologyDisclosure(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "4":
					progressReportDao.updateKPIAchievedCountManpowerDevelopment(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "5":
					progressReportDao.updateKPIAchievedCountUndergraduateStudent(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "6":
					progressReportDao.updateKPIAchievedCountConferencePresentation(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "7":
					progressReportDao.updateKPIAchievedCountCompetitiveGrants(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "8":
					progressReportDao.updateKPIAchievedCountPatents(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "9":
					progressReportDao.updateKPIAchievedCountLicenses(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "10":
					progressReportDao.updateKPIAchievedCountSuccessfulStartups(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "11":
					progressReportDao.updateKPIAchievedCountHealthSpecificOutcomes(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "12":
					progressReportDao.updateKPIAchievedCountPostDocsEmployed(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "13":
					progressReportDao.updateKPIAchievedCountPostGrantSpecific(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "14":
					progressReportDao.updateKPIAchievedCountCashFunding(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "15":
					progressReportDao.updateKPIAchievedCountInkindContributions(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				case "16":
					progressReportDao.updateKPIAchievedCountTechnologiesDeployed(summary.getKpiCriteriaTypeCode(), progressReportVO.getProgressReportId());
					break;
				default:
					break;
			}
		});
		return commonDao.convertObjectToJSON(progressReportVO);
	}

	@Override
	public String deleteProgressReportDetails(Integer progressReportId) {
		Map<String, Object> response = new HashMap<>();
		try {
			progressReportDao.getProgressReportKPISummaryDetailsById(progressReportId).forEach(summary -> {
				switch (summary.getSectionCode()) {
				case "1":
					progressReportDao.deleteKPIImpactPublications(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "2":
					progressReportDao.deleteKpiCollaborationProjects(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "3":
					progressReportDao.deleteKpiTechnologyDisclosure(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "4":
					progressReportDao.deleteKpiProgressReportKPIManpowerDevelopment(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "5":
					progressReportDao.deleteKPIUndergraduateStudent(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "6":
					progressReportDao.deleteKpiConferencePresentation(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "7":
					progressReportDao.deleteKpiCompetitiveGrants(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "8":
					progressReportDao.deleteProgressReportKPIPatents(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "9":
					progressReportDao.deleteProgressReportKpiLicenses(progressReportId, summary.getKpiCriteriaTypeCode(),null);
					break;
				case "10":
					progressReportDao.deleteProgressReportKPISuccessfulStartups(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "11":
					progressReportDao.deleteProgressReportKPIHealthSpecificOutcomes(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "12":
					progressReportDao.deleteProgressReportKPIPostDocsEmployed(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "13":
					progressReportDao.deleteKpiCompetitiveGrants(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "14":
					progressReportDao.deleteProgressReportKPICashFunding(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "15":
					progressReportDao.deleteProgressReportKPIInkindContributions(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				case "16":
					progressReportDao.deleteProgressReportKPITechnologiesDeployed(progressReportId, summary.getKpiCriteriaTypeCode(), null);
					break;
				default:
					break;
				}
			});
			progressReportDao.deleteAwardProgressReportKPISummary(progressReportId);
			progressReportDao.deleteAwardProgressReportMilestone(progressReportId);
			progressReportDao.deleteAwardProgressReportAchievement(progressReportId);
			progressReportDao.deleteAwardProgressReportAttachment(progressReportId);
			questionnaireDAO.deleteQuestAnswerAttachment(progressReportId.toString(), Constants.PROGRESS_REPORT_MODULE_CODE, Constants.PROGRESS_REPORT_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
			questionnaireDAO.deleteQuestAnswer(progressReportId.toString(), Constants.PROGRESS_REPORT_MODULE_CODE, Constants.PROGRESS_REPORT_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
			questionnaireDAO.deleteQuestAnswerHeader(progressReportId.toString(), Constants.PROGRESS_REPORT_MODULE_CODE, Constants.PROGRESS_REPORT_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
			progressReportDao.removeAwardReportTrackingProgressReportId(progressReportId);
			progressReportDao.deleteAwardProgressReport(progressReportId);
			response.put("status", "success");
			logger.info("deleteProgressReport Completed");
		} catch (Exception e) {
			logger.error("Exception in deleteProgressReportDetails", e);
			response.put("status", "error");
		}
		return commonDao.convertObjectToJSON(response);
	}

}
