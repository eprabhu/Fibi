package com.polus.fibicomp.coi.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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

import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
@RequestMapping("/coi")
public class ConflictOfInterestController {

	protected static Logger logger = LogManager.getLogger(ConflictOfInterestController.class.getName());

	@Autowired
	@Qualifier(value = "conflictOfInterestService")
	private ConflictOfInterestService conflictOfInterestService;

	@PostMapping("/createDisclosure")
	public ResponseEntity<Object> createDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for createDisclosure");
		return conflictOfInterestService.createDisclosure(vo);
	}

	@GetMapping("/loadDisclosure/{disclosureId}")
	public ResponseEntity<Object> loadDisclosure(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Request for loadDisclosure");
		return conflictOfInterestService.loadDisclosure(disclosureId);
	}

	@PostMapping("/getDisclosureRelations")
	public String getDisclosureRelations(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getDisclosureRelations");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getDisclosureRelations(vo);
	}

	@PostMapping("/getSFIOfDisclosure")
	public ResponseEntity<Object> getSFIOfDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getSFIOfDisclosure");
		return conflictOfInterestService.getSFIOfDisclosure(vo);
	}

	@PostMapping(value = "/searchEntity")
	public List<CoiEntity> searchEnitiy(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for searchEntity");
		return conflictOfInterestService.searchEnitiy(vo.getSearchString());
	}

	@GetMapping("/loadSFILookups")
	public ResponseEntity<Object> loadSFILookups() {
		logger.info("Requesting for loadSFILookups");
		return conflictOfInterestService.loadAddSFILookups();
	}

	@GetMapping("/getSFIDetails/{coiFinancialEntityId}")
	public ResponseEntity<Object> getSFIDetails(@PathVariable("coiFinancialEntityId") Integer coiFinancialEntityId) {
		logger.info("Requesting for getSFIDetails");
		return conflictOfInterestService.getSFIDetails(coiFinancialEntityId);
	}

	@PostMapping(value = "/createSFI")
	public ResponseEntity<Object> createSFI(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for createSFI");
		return conflictOfInterestService.createSFI(vo);
	}

	@PostMapping("/saveOrUpdateCoiFinancialEntityDetails")
	public PersonEntityRelationship saveCoiFinancialEntityDetails(@RequestBody PersonEntityRelationship vo) {
		logger.info("Request for saveOrUpdateCoiFinancialEntityDetails");
		return conflictOfInterestService.saveOrUpdatePersonEntityRelationship(vo);
	}

	@PatchMapping("/certifyDisclosure")
	public ResponseEntity<Object> certifyDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for certifyDisclosure");
		return conflictOfInterestService.certifyDisclosure(vo.getCoiDisclosure());
	}

	@PostMapping("/getEntityProjectRelations")
	public String getEntityProjectRelations(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getEntityProjectRelations");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getEntityProjectRelations(vo);
	}

	@PostMapping("/saveEntityProjectRelation")
	public String saveEntityProjectRelation(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for saveEntityProjectRelation");
		vo = conflictOfInterestService.saveEntityProjectRelation(vo);
		return conflictOfInterestService.checkSFICompleted(vo);
	}

	@GetMapping(value = "/loadDisclosureAdminDashboardCounts")
	public ResponseEntity<Object> loadDisclosureAdminDashboardCounts() {
		logger.info("Request for loadDisclosureAdminDashboardCounts");
		logger.info("Login Person Id : {}", AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.loadDisclosureAdminDashboardCounts();
	}

	@PostMapping("/reviseDisclosure")
	public String reviseDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for reviseDisclosure");
		return conflictOfInterestService.reviseDisclosure(vo);
	}

	@PostMapping("/evaluateDisclosureQuestionnaire")
	public Boolean evaluateDisclosureQuestionnaire(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for evaluateDisclosureQuestionnaire");
		return conflictOfInterestService.evaluateDisclosureQuestionnaire(vo);
	}

	@GetMapping("/getDisclosureDetailsForSFI/{coiFinancialEntityId}")
	public ResponseEntity<Object> getDisclosureDetailsForSFI(
			@PathVariable("coiFinancialEntityId") Integer coiFinancialEntityId) {
		logger.info("Requesting for getDisclosureDetailsForSFI");
		logger.info("Coi Financial Entity Id : {}", coiFinancialEntityId);
		return conflictOfInterestService.getDisclosureDetailsForSFI(coiFinancialEntityId);
	}

	@PostMapping("/getDisclosureRelationForSFI")
	public ResponseEntity<Object> getDisclosureRelationForSFI(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getDisclosureRelationForSFI");
		logger.info("Coi Financial Entity Id : {}", vo.getCoiFinancialEntityId());
		return conflictOfInterestService.getDisclosureRelationsForSFI(vo.getCoiFinancialEntityId());
	}

	@PostMapping("/saveOrUpdateCoiReview")
	public CoiReview saveOrUpdateCoiReview(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for saveOrUpdateCoiReview");
		return conflictOfInterestService.saveOrUpdateCoiReview(vo);
	}

	@GetMapping("/getCoiReview/{disclosureId}")
	public List<CoiReview> getCoiReview(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Requesting for getCoiReview");
		return conflictOfInterestService.getCoiReview(disclosureId);
	}

	@PostMapping("/startCOIReview")
	public CoiReview startReview(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for startReview");
		return conflictOfInterestService.startReview(vo);
	}

	@PostMapping(value = "/addCOIReviewComment")
	public String addExtReviewerAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files,
			@RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addCOIReviewComment");
		return conflictOfInterestService.saveOrUpdateCoiReviewComments(files, formDataJson);
	}

	@PostMapping("/loadCoiReviewComments")
	public String loadCoiReviewComments(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for loadCoiReviewComments");
		return conflictOfInterestService.loadCoiReviewComments(vo);
	}
	
	@PostMapping("/completeCOIReview")
	public CoiReview completeReview(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for CompleteReview");
		return conflictOfInterestService.completeReview(vo);
	}

	@DeleteMapping(value = "/deleteReview/{coiReviewId}")
	public String deleteReview(@PathVariable(value = "coiReviewId", required = true) final Integer coiReviewId) {
		logger.info("Requesting for deleteReview");
		return conflictOfInterestService.deleteReview(coiReviewId);
	}

	@DeleteMapping(value = "/deleteCOIReviewComment/{coiReviewCommentId}")
	public String deleteReviewComment(@PathVariable(value = "coiReviewCommentId", required = true) final Integer coiReviewCommentId) {
		logger.info("Requesting for deleteReviewComment");
		return conflictOfInterestService.deleteReviewComment(coiReviewCommentId);
	}
	
	@DeleteMapping(value = "/deleteReviewAttachment/{coiReviewCommentAttId}")
	public String deleteReviewAttachment(@PathVariable(value = "coiReviewCommentAttId", required = true) final Integer coiReviewCommentAttId) {
		logger.info("Requesting for deleteReviewAttachment");
		return conflictOfInterestService.deleteReviewAttachment(coiReviewCommentAttId);
	}

	@GetMapping(value = "/downloadCoiReviewAttachment")
	public ResponseEntity<byte[]> downloadCoiReviewAttachment(HttpServletResponse response, @RequestHeader("attachmentId") Integer attachmentId) {
		logger.info("Requesting for downloadCoiReviewAttachment");
		logger.info("downloadCoiReviewAttachmentId : {}", attachmentId);
		return conflictOfInterestService.downloadCoiReviewAttachment(attachmentId);
	}

	@PostMapping("/completeDisclosureReview/{disclosureId}")
	public ResponseEntity<Object> completeDisclosureReview(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Request for completeDisclosureReview");
		return conflictOfInterestService.completeDisclosureReview(disclosureId);
	}

	@PostMapping("/updateProjectConflictStatus")
	public CoiDisclEntProjDetails updateProjectConflictStatus(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for updateProjectConflictStatus");
		return conflictOfInterestService.updateProjectConflictStatus(vo.getCoiDisclEntProjDetail());
	}
	
	@GetMapping("/loadProjectConflictHistory/{disclosureDetailsId}")
	public List<CoiConflictHistory> getCoiConflictHistory(@PathVariable("disclosureDetailsId") Integer disclosureDetailsId) {
		logger.info("Request for getCoiConflictHistory");
		return conflictOfInterestService.getCoiConflictHistory(disclosureDetailsId);
	}

	@PostMapping(value = "/loadProposalsForDisclosure")
	public String loadProposalsForDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for loadProposalsForDisclosure");
		logger.info("searchString : {}", vo.getSearchString());
		return conflictOfInterestService.loadProposalsForDisclosure(vo);
	}

	@PostMapping(value = "/loadDisclosureHistory")
	public String loadDisclosureHistory(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for loadDisclosureHistory");
		logger.info("DisclosureNumber : {}", vo.getDisclosureNumber());
		return conflictOfInterestService.loadDisclosureHistory(vo);
	}

	@PostMapping("/singleEntityProjectRelation")
	public String saveSingleEntityProjectRelation(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for saveEntityProjectRelation");
		vo = conflictOfInterestService.saveSingleEntityProjectRelation(vo);
		return conflictOfInterestService.checkSFICompleted(vo);
	}
	
	@PostMapping(value = "/saveOrUpdateCoiEntity")
	public ResponseEntity<Object> saveOrUpdateCoiEntity(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for createEntity");
		return conflictOfInterestService.saveOrUpdateCoiEntity(vo);
	}
	
	@GetMapping("/getEntityDetails/{coiEntityId}")
	public ResponseEntity<Object> getEntityDetails(@PathVariable("coiEntityId") Integer coiEntityId) {
		logger.info("Requesting for getEntityDetails");
		return conflictOfInterestService.getEntityDetails(coiEntityId);
	}
	
	@GetMapping("/getActiveDisclosures")
	public ResponseEntity<Object> getActiveDisclosure() {
		logger.info("Requesting for getActiveDisclosure");
		return conflictOfInterestService.getActiveDisclosure();
	}
	
	@PostMapping(value = "/getCOIDashboard")
	public String getCOIDashboard(@Valid @RequestBody CoiDashboardVO vo, HttpServletRequest request) {
		logger.info("Requesting for getCOIDashboard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getCOIDashboard(vo);
	}

	@PostMapping(value = "/getCOIAdminDashboard")
	public String getCOIAdminDashboard(@Valid @RequestBody CoiDashboardVO vo, HttpServletRequest request) {
		logger.info("Requesting for getCOIAdminDashboard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getCOIAdminDashboard(vo);
	}

	@PostMapping(value = "/getSFIDashboard")
	public String getSFIDashboard(@Valid @RequestBody CoiDashboardVO vo, HttpServletRequest request) {
		logger.info("Requesting for getSFIDashboard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getSFIDashboard(vo);
	}
	
	@PostMapping(value = "/getTabCount")
	public String getCOIDashboardCount(@Valid @RequestBody CoiDashboardVO vo, HttpServletRequest request) {
		logger.info("Requesting for getCOIDashboard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getCOIDashboardCount(vo);
	}
	
	@PostMapping(value = "/getAllEntityList")
	public ResponseEntity<Object> getAllEntityList(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getAllEntityList");
		return conflictOfInterestService.getAllEntityList(vo);
	}
	
	@PostMapping(value = "/setEntityStatus")
	public ResponseEntity<Object> setEntityStatus(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for setEntityStatus");
		return conflictOfInterestService.setEntityStatus(vo);
	}
	
	@PostMapping("/getAllSystemEntityList")
	public ResponseEntity<Object> getAllSystemEntityList(@RequestBody CoiDashboardVO vo) {
		logger.info("Requesting for getAllSystemEntityList");
		return conflictOfInterestService.getAllSystemEntityList(vo);
	}

	@GetMapping("/getCoiProjectTypes")
	public ResponseEntity<Object> getCoiProjectTypes() {
		logger.info("Requesting for getCoiProjectTypes");
		return conflictOfInterestService.getCoiProjectTypes();
	}

	@PostMapping(value = "/getPersonEntityDashboard")
	public ResponseEntity<Object> getPersonEntityDashboard(@RequestBody CoiDashboardVO vo) {
		logger.info("Requesting for getPersonEntityDetails");
		return conflictOfInterestService.getPersonEntityDashboard(vo);
	}


	@PostMapping(value = "/getCOIReviewerDashboard")
	public ResponseEntity<Object> getCOIReviewerDashboard(@Valid @RequestBody CoiDashboardVO vo) {
		logger.info("Requesting for getCOIReviewerDashboard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.getCOIReviewerDashboard(vo);
	}

	@GetMapping(value = "/loadDisclosureReviewerQuickCardCounts")
	public ResponseEntity<Object> loadDisclosureReviewerQuickCardCounts() {
		logger.info("Request for loadDisclosureReviewerQuickCardCounts");
		logger.info("Login Person Id : {}", AuthenticatedUser.getLoginPersonId());
		return conflictOfInterestService.loadDisclosureReviewerQuickCardCounts();
	}
	
	@GetMapping("/getCoiEntityDetails/{personEntityId}")
	public ResponseEntity<Object> getCoiEntityDetails(@PathVariable("personEntityId") Integer personEntityId) {
		logger.info("Requesting for getCoiEntityDetails");
		return conflictOfInterestService.getCoiEntityDetails(personEntityId);
	}
	
	@GetMapping("/getPersonEntityDetails/{personEntityId}")
	public ResponseEntity<Object> getPersonEntityDetails(@PathVariable("personEntityId") Integer personEntityId) {
		logger.info("Requesting for getPersonEntityDetails");
		return conflictOfInterestService.getPersonEntityDetails(personEntityId);
	}
	
	@GetMapping("/getRelationshipLookup/{tabName}")
	public ResponseEntity<Object> getRelatioshipDetails(@PathVariable("tabName") String tabName) {
		logger.info("Requesting for getRelatioshipDetails");
		return conflictOfInterestService.getRelatioshipDetails(tabName);
	}
	
	@PostMapping("/getPersonEntityRelationship")
	public ResponseEntity<Object> getPersonEntityRelationship(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getPersonEntityRelationship");
		return conflictOfInterestService.getPersonEntityRelationship(vo);
	}
	
	@PostMapping(value = "/createCoiTravelDisclosure")
	public ResponseEntity<Object> createCoiTravelDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for createCoiTravelDisclosure");
		return conflictOfInterestService.createCoiTravelDisclosure(vo);
	}

	@GetMapping(value = "/getAllCoiTravelDisclosureList")
	public ResponseEntity<Object> getAllCoiTravelDisclosureList() {
		logger.info("Request for getAllCoiTravelDisclosures");
		return conflictOfInterestService.getAllCoiTravelDisclosureList();
	}

	@GetMapping(value = "/getCoiTravelDisclosureDetailsById/{travelDisclosureId}")
	public CoiTravelDisclosure getCoiTravelDisclosureDetailsById(@PathVariable("travelDisclosureId") Integer travelDisclosureId) {
		logger.info("Request for getCoiTravelDisclosureById");
		return conflictOfInterestService.getCoiTravelDisclosureDetailsById(travelDisclosureId);
	}

	@GetMapping("/loadTravellerTypesLookup")
	public ResponseEntity<Object> loadTravellerTypesLookup() {
		logger.info("Requesting for loadTravellerTypesLookup");
		return conflictOfInterestService.loadTravellerTypesLookup();
	}

	@GetMapping("/loadTravelStatusTypesLookup")
	public ResponseEntity<Object> loadTravelStatusTypesLookup() {
		logger.info("Requesting for loadTravelStatusTypesLookup");
		return conflictOfInterestService.loadTravelStatusTypesLookup();
	}
	
	@GetMapping("hello")
	public ResponseEntity<String> hello() {
		return new ResponseEntity<>("Hello from COI", HttpStatus.OK);
	} 
}
