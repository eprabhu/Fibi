package com.polus.fibicomp.progressreport.controller;

import java.util.Date;

import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.progressreport.service.ProgressReportService;
import com.polus.fibicomp.progressreport.vo.ProgressReportVO;

@RestController
public class ProgressReportController {

	protected static Logger logger = LogManager.getLogger(ProgressReportController.class.getName());

	@Autowired
	private ProgressReportService progressReportService;

	@Autowired
	private UserDocumentAuthorization documentAuthorization;

	@PostMapping(value = "/saveOrUpdateProgressReport")
	public String saveOrUpdateProgressReport(@RequestBody ProgressReportVO progressReportVO){
		logger.info("Request for saveOrUpdateProgressReport");
		return progressReportService.copyKpiIntoProgressReport(progressReportService.saveOrUpdateProgressReport(progressReportVO));
	}

	@GetMapping(value = "/loadAwardProgressReport/{progressReportId}")
	public ResponseEntity<String> loadAwardProgressReport(@PathVariable(value = "progressReportId", required = true) final Integer progressReportId) {
		logger.info("Request for loadAwardProgressReport");
		logger.info("progressReportId {}", progressReportId);
		if (!documentAuthorization.isAuthorized(Constants.PROGRESS_REPORT_MODULE_CODE, progressReportId.toString(), AuthenticatedUser.getLoginPersonId())) {
			return new ResponseEntity<>("Not Authorized to view this Progress report", HttpStatus.FORBIDDEN);
		}
		return new ResponseEntity<>(progressReportService.loadAwardProgressReport(progressReportId, false), HttpStatus.OK);
	}

	@GetMapping(value = "/loadProgressReportMilestone/{progressReportId}")
	public String loadProgressReportMilestone(@PathVariable(value = "progressReportId", required = true) final Integer progressReportId) {
		logger.info("Request for loadProgressReportMilestone");
		logger.info("progressReportId {}", progressReportId);
		return progressReportService.loadProgressReportMilestone(progressReportId);
	}

	@PostMapping(value = "/saveOrUpdateProgressReportMilestone")
	public String saveOrUpdateProgressReportMilestone(@RequestBody ProgressReportVO progressReportVO){
		logger.info("Request for saveOrUpdateProgressReportMilestone");
		return progressReportService.saveOrUpdateProgressReportMilestone(progressReportVO);
	}

	@PostMapping(value = "/submitProgressReport/{progressReportId}")
	public String submitProgressReport(@PathVariable(value = "progressReportId", required = true) final Integer progressReportId) {
		logger.info("Request for submitProgressReport");
		logger.info("progressReportId {}", progressReportId);
		return progressReportService.submitProgressReport(progressReportId);
	}
	
	@PostMapping(value = "/saveOrUpdateProgressReportAttachment", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateProgressReportAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Request for adding saveOrUpdateProgressReportAttachment");
		return progressReportService.saveOrUpdateProgressReportAttachment(files, formDataJson);
	}
	
	@PostMapping(value = "/loadProgressReportAttachments")
	public String loadClaimAttachments(@RequestBody ProgressReportVO progressReportVO) {
		logger.info("Request for loadProgressReportAttachments");
		logger.info("progressReportId id {}",progressReportVO.getProgressReportId());
		return progressReportService.loadProgressReportAttachments(progressReportVO);
	}
	
	@GetMapping(value = "/downloadProgressReportAttachment")
	public ResponseEntity<byte[]> downloadProgressReportAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadProgressReportAttachment");
		logger.info("progress report attachmentId {}",attachmentId);
		return progressReportService.downloadProgressReportAttachment(Integer.parseInt(attachmentId));
	}
	
	@PatchMapping(value = "/performPRFundingAgencyAction")
	public String performFundingAgencyAction(@RequestBody ProgressReportVO progressReportVO) {
		logger.info("Requesting for performFundingAgencyAction");
		logger.info("progressReportId {}", progressReportVO.getProgressReportId());
		return progressReportService.performFundingAgencyAction(progressReportVO);
	}
	
	@PatchMapping(value = "/updateProgressReportAchievements")
	public String updateProgressReportAchievements(@RequestBody ProgressReportVO progressReportVO) {
		logger.info("Requesting for updateProgressReportAchievements");
		return progressReportService.updateProgressReportAchievements(progressReportVO);
	}
	
	@GetMapping(value = "/loadProgressReportKPISummary/{progressReportId}")
	public String loadProgressReportKPISummary(@PathVariable(value = "progressReportId", required = true) final Integer progressReportId) {
		logger.info("Request for loadProgressReportKPISummary");
		logger.info("progressReportId {}", progressReportId);
		return progressReportService.loadProgressReportKPISummary(progressReportId);
	}
	
	@GetMapping(value = "/loadProgressReportKPISummaryDetails/{kpiSummaryId}/{sectionCode}")
	public String loadProgressReportKPISummaryDetails(@PathVariable(value = "kpiSummaryId", required = true) final Integer kpiSummaryId,
			@PathVariable(value = "sectionCode", required = true) final String sectionCode) {
		logger.info("Request for loadProgressReportKPISummaryDetails");
		logger.info("kpiSummaryId {}", kpiSummaryId);
		return progressReportService.loadProgressReportKPISummaryDetails(kpiSummaryId, sectionCode);
	}
	
	@PostMapping(value = "/saveOrUpdateKPISummaryDetails")
	public String saveOrUpdateKPISummaryDetails(@RequestBody ProgressReportVO progressReportVO) {
		logger.info("Request for saveOrUpdateKPISummaryDetails");
		return progressReportService.saveOrUpdateKPISummaryDetails(progressReportVO);
	}
	
	@DeleteMapping(value = "/deleteKPISummaryDetail/{kpiSummaryId}/{sectionCode}/{id}")
	public String deleteKPISummaryDetail(@PathVariable(value = "kpiSummaryId", required = true) final Integer kpiSummaryId,
			@PathVariable(value = "sectionCode", required = true) final String sectionCode,
			@PathVariable(value = "id", required = true) final Integer id) {
		logger.info("Request for deleteKPISummaryDetail");
		return progressReportService.deleteKPISummaryDetail(kpiSummaryId, sectionCode, id);
	}
	
	@GetMapping(value = "/loadProgressReportKPILookups")
	public String loadProgressReportKPILookups() throws Exception {
		logger.info("Request for loadProgressReportKPILookups");
		return progressReportService.loadProgressReportKPILookups();
	}
	
	@GetMapping(value = "/loadProgressReportForAward/{awardNumber}")
	public String loadProgressReportForAward(@PathVariable(value = "awardNumber", required = true) final String awardNumber) {
		logger.info("Request for getProgressReportForAward");
		logger.info("awardNumber {}", awardNumber);
		return progressReportService.loadProgressReportForAward(awardNumber);
	}

	@PostMapping(value = "/importProgressReportTemplate")
	public ResponseEntity<String> importProgressReportTemplate(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for importProgressReportTemplate");
		ProgressReportVO progressReportVO = progressReportService.importProgressReportTemplate(files, formDataJson);
		return new ResponseEntity<>(progressReportService.loadAwardProgressReport(progressReportVO.getProgressReportId(), progressReportVO.getProgressReportImported()), HttpStatus.OK);
	}

	@GetMapping(value = { "/getAwardProgressReportStartDate/{awardNumber}/{awardId}/{reportClassCode}", "/getAwardProgressReportStartDate/{awardNumber}/{awardId}/{reportClassCode}/{dueDate}" })
	public String getAwardProgressReportStartDate(@PathVariable(value = "awardNumber") final String awardNumber,
												  @PathVariable(value = "awardId", required = true) final Integer awardId,
												  @PathVariable(value = "reportClassCode", required = false) final String reportClassCode,
												  @PathVariable(value = "dueDate", required = false) final Long dueDate) {
		logger.info("Request for getAwardProgressReportStartDate");
		logger.info("awardNumber {}", awardNumber);
		return progressReportService.getAwardProgressReportStartDate(awardNumber, awardId, reportClassCode, dueDate == null ? null : new Date(dueDate));
	}

	@PatchMapping(value = "/updateProgressReportDates")
	public String updateProgressReportDates(@RequestBody ProgressReportVO progressReportVO) {
		logger.info("Request for updateProgressReportDates");
		return progressReportService.updateProgressReportDates(progressReportVO);
	}
	
	@DeleteMapping(value = "/deleteProgressReport/{progressReportId}")
	public String deleteProgressReport(@PathVariable(value = "progressReportId", required = true) final Integer progressReportId) {
		logger.info("Request for deleteProgressReport");
		return progressReportService.deleteProgressReportDetails(progressReportId);
	}
}
