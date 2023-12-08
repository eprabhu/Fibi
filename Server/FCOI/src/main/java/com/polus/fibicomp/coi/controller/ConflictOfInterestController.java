package com.polus.fibicomp.coi.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.coi.dto.CoiAssignTravelDisclosureAdminDto;
import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiEntityDto;
import com.polus.fibicomp.coi.dto.CoiTravelDisclosureDto;
import com.polus.fibicomp.coi.dto.CoiTravelHistoryDto;
import com.polus.fibicomp.coi.dto.CompleteReivewRequestDto;
import com.polus.fibicomp.coi.dto.NotesDto;
import com.polus.fibicomp.coi.dto.NotificationBannerDto;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.dto.SearchDto;
import com.polus.fibicomp.coi.dto.TravelDisclosureActionLogDto;
import com.polus.fibicomp.coi.pojo.Attachments;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiTravelConflictHistory;
import com.polus.fibicomp.coi.pojo.EntityRelationship;
import com.polus.fibicomp.coi.pojo.Notes;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;
import com.polus.fibicomp.coi.service.GeneralService;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
@RequestMapping("/coi")
public class ConflictOfInterestController {

	protected static Logger logger = LogManager.getLogger(ConflictOfInterestController.class.getName());

	@Autowired
	@Qualifier(value = "conflictOfInterestService")
	private ConflictOfInterestService conflictOfInterestService;

	@Autowired
	private GeneralService generalService;

	@Autowired
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private ActionLogService actionLogService;

	@GetMapping("hello")
	public ResponseEntity<String> hello() {
		return new ResponseEntity<>("Hello from COI", HttpStatus.OK);
	} 	
	
	@PostMapping("/createDisclosure")
	public ResponseEntity<Object> createDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for createDisclosure");
		return conflictOfInterestService.createDisclosure(vo);
	}

	@GetMapping("/loadDisclosure/{disclosureId}")
	public ResponseEntity<Object> loadDisclosure(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Request for loadDisclosure");
		if (!documentAuthorization.isAuthorized(Constants.COI_MODULE_CODE, disclosureId.toString(), AuthenticatedUser.getLoginPersonId())) {
			return new ResponseEntity<>("Not Authorized to view this Disclosure",HttpStatus.FORBIDDEN);
		}
		return conflictOfInterestService.loadDisclosure(disclosureId);
	}

	@PostMapping("/getDisclosureRelations")
	public String getDisclosureRelations(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getDisclosureRelations");
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
		return conflictOfInterestService.searchEntity(vo);
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
	public ResponseEntity<Object> createSFI(@RequestBody PersonEntity personEntity) {
		logger.info("Requesting for createSFI");
		return conflictOfInterestService.createSFI(personEntity);
	}

	@PostMapping("/saveOrUpdateCoiFinancialEntityDetails")
	public ResponseEntity<Object> saveCoiFinancialEntityDetails(@RequestBody PersonEntityRelationship vo) {
		logger.info("Request for saveOrUpdateCoiFinancialEntityDetails");
		return conflictOfInterestService.saveOrUpdatePersonEntityRelationship(vo);
	}

	@PatchMapping("/certifyDisclosure")
	public ResponseEntity<Object> certifyDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for certifyDisclosure");
		return conflictOfInterestService.certifyDisclosure(vo.getCoiDisclosure());
	}

	@PostMapping("/disclosure/project/relations")
	public ResponseEntity<Object> getDisclosureProjectRelations(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for /disclosure/project/relations");
		return conflictOfInterestService.getDisclosureProjectRelations(vo);
	}

	@PostMapping("/saveEntityProjectRelation")
	public String saveEntityProjectRelation(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for saveEntityProjectRelation");
		vo = conflictOfInterestService.saveEntityProjectRelation(vo);
		return conflictOfInterestService.checkSFICompleted(vo);
	}

	@PostMapping("/reviseDisclosure")
	public ResponseEntity<Object> reviseDisclosure(@RequestBody ConflictOfInterestVO vo) {
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

	@PostMapping("/saveOrUpdateCoiReview")
	public ResponseEntity<Object> saveOrUpdateCoiReview(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for saveOrUpdateCoiReview");
		return conflictOfInterestService.saveOrUpdateCoiReview(vo);
	}

	@GetMapping("/getCoiReview/{disclosureId}")
	public List<CoiReview> getCoiReview(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Requesting for getCoiReview");
		return conflictOfInterestService.getCoiReview(disclosureId);
	}

	@PostMapping("/startCOIReview")
	public ResponseEntity<Object> startReview(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for startReview");
		return conflictOfInterestService.startReview(vo);
	}

	@PostMapping("/completeCOIReview")
	public ResponseEntity<Object> completeReview(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for CompleteReview");
		return conflictOfInterestService.completeReview(vo);
	}

	@DeleteMapping(value = "/deleteReview/{coiReviewId}")
	public ResponseEntity<Object> deleteReview(@PathVariable(value = "coiReviewId", required = true) final Integer coiReviewId) {
		logger.info("Requesting for deleteReview");
		return conflictOfInterestService.deleteReview(coiReviewId);
	}

	@DeleteMapping(value = "/deleteCOIReviewCommentTags/{coiReviewCommentTagId}")
	public String deleteReviewCommentTag(@PathVariable(value = "coiReviewCommentTagId", required = true) final Integer coiReviewCommentTagId) {
		logger.info("Requesting for deleteReviewCommentTag");
		return conflictOfInterestService.deleteReviewCommentTag(coiReviewCommentTagId);
	}

	@PostMapping("/completeDisclosureReview/{disclosureId}/{disclosureNumber}")
	public ResponseEntity<Object> completeDisclosureReview(@PathVariable("disclosureId") Integer disclosureId,
														   @PathVariable("disclosureNumber") Integer disclosureNumber) {
		logger.info("Request for completeDisclosureReview");
		return conflictOfInterestService.completeDisclosureReview(disclosureId, disclosureNumber);
	}

	@PatchMapping("/completeDisclosureReviews")
	public ResponseEntity<Object> completeDisclosureReviews(@RequestBody CompleteReivewRequestDto requestDto) {
		logger.info("Request for completeDisclosureReviews");
		return conflictOfInterestService.completeDisclosureReviews(requestDto.getDisclosureIdNumberMap());
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
	public String loadProposalsForDisclosure(@RequestBody SearchDto searchDto) {
		logger.info("Request for loadProposalsForDisclosure");
		logger.info("searchString : {}", searchDto.getSearchString());
		return conflictOfInterestService.loadProposalsForDisclosure(searchDto.getSearchString());
	}

	@PostMapping(value = "/loadAwardsForDisclosure")
	public String loadAwardsForDisclosure(@RequestBody SearchDto searchDto) {
		logger.info("Request for loadAwardsForDisclosure");
		logger.info("searchString : {}", searchDto.getSearchString());
		return conflictOfInterestService.loadAwardsForDisclosure(searchDto.getSearchString());
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
	
	@GetMapping("/getRelationshipLookup")
	public ResponseEntity<Object> getValidPersonRelationshipLookUp() {
		logger.info("Requesting for getValidPersonRelationshipLookUp");
		return conflictOfInterestService.getValidPersonRelationshipLookUp();
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

	@GetMapping("/fetchAllCoiRights")
	public ResponseEntity<Object> fetchAllCoiRights(){
		return generalService.fetchAllCoiOpaRights();
	}

	@GetMapping("/entity/isLinked/{entityId}/personEntity")
	public ResponseEntity<Object> checkEntityAdded(@PathVariable("entityId") Integer entityId) {
		return conflictOfInterestService.checkEntityAdded(entityId);
	}

	@GetMapping("/validate/{moduleCode}/disclosure/{moduleItemId}")
	public ResponseEntity<Object> validateDisclosure(@PathVariable("moduleCode") Integer moduleCode,
												   @PathVariable("moduleItemId") String moduleItemId) {
		return conflictOfInterestService.validateDisclosure(moduleCode, moduleItemId);
	}

	@PatchMapping("/disclosure/assignAdmin")
	public ResponseEntity<Object> assignDisclosureAdmin(@RequestBody CoiDisclosureDto dto) {
		return conflictOfInterestService.assignDisclosureAdmin(dto);
	}
	
	@GetMapping(value = "/loadTravelDisclosure/{travelDisclosureId}")
	public ResponseEntity<Object> loadTravelDisclosure(@PathVariable("travelDisclosureId") Integer travelDisclosureId) {
		logger.info("Request for loadTravelDisclosureById");
		return conflictOfInterestService.loadTravelDisclosure(travelDisclosureId);
	}

	@PatchMapping("/travelDisclosure/assignAdmin")
	public ResponseEntity<Object> assignTravelDisclosureAdmin(@RequestBody CoiAssignTravelDisclosureAdminDto dto) {
		return conflictOfInterestService.assignTravelDisclosureAdmin(dto);
	}

	@PostMapping("/certifyTravelDisclosure")
	public ResponseEntity<Object> certifyTravelDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for certifyTravelDisclosure");
		return conflictOfInterestService.certifyTravelDisclosure(vo);
	}

	@PostMapping("/submitTravelDisclosure")
	public ResponseEntity<Object> submitTravelDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for submitting TravelDisclosure");
		return conflictOfInterestService.submitTravelDisclosure(vo);
	}
	
	@PostMapping(value = "/withdrawTravelDisclosure")
	public ResponseEntity<Object> withdrawTravelDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for withdrawing TravelDisclosure");
		return conflictOfInterestService.withdrawTravelDisclosure(vo.getTravelDisclosureId(), vo.getDescription());
	}
	
	@PostMapping(value = "/approveTravelDisclosure")
	public ResponseEntity<Object> approveTravelDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for approving TravelDisclosure");
		String description = vo.getDescription() != null ? vo.getDescription() : "";
		return conflictOfInterestService.approveTravelDisclosure(vo.getTravelDisclosureId(), description);
	}
	
	@PostMapping(value = "/returnTravelDisclosure")
	public ResponseEntity<Object> returnTravelDisclosure(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for returning TravelDisclosure");
		return conflictOfInterestService.returnTravelDisclosure(vo.getTravelDisclosureId(), vo.getDescription());
	}

	@GetMapping("/adminGroup/adminPersons/{moduleCode}")
	public ResponseEntity<Object> fetchAdminGroupsAndPersons(@PathVariable("moduleCode") Integer moduleCode) {
		return generalService.fetchAdminGroupsAndPersons(moduleCode);
	}

	@GetMapping("/validateConflicts/{disclosureId}")
	public ResponseEntity<Object> validateConflicts(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Requesting for validateConflicts");
		return conflictOfInterestService.validateConflicts(disclosureId);
	}

	@GetMapping("/evaluateValidation/{disclosureId}")
	public ResponseEntity<Object> evaluateValidation(@PathVariable("disclosureId") Integer disclosureId) {
		logger.info("Requesting for evaluateValidation");
		return conflictOfInterestService.evaluateValidation(disclosureId);
	}

	@GetMapping("/getProjConflictStatusType")
	public ResponseEntity<Object> getProjConflictStatusType() {
		logger.info("Requesting for getProjConflictStatusType");
		return conflictOfInterestService.getProjConflictStatusType();
	}

	@DeleteMapping("/personEntity/{personEntityId}")
	public ResponseEntity<Object>deletePersonEntity(@PathVariable("personEntityId") Integer personEntityId) {
		return conflictOfInterestService.deletePersonEntity(personEntityId);
	}

	@PostMapping(value = "/updateProjectRelationship")
	public ResponseEntity<Object> updateProjectRelationship(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for updateProjectRelationship");
		return conflictOfInterestService.updateProjectRelationship(vo);
	}


	@PutMapping("/entity/activateInactivate")
	public ResponseEntity<Object> activateOrInactivateEntity(@RequestBody CoiEntityDto coiEntityDto) {
		return conflictOfInterestService.activateOrInactivateEntity(coiEntityDto);
	}

	@PutMapping("/personEntity/activateInactivate")
	public ResponseEntity<Object> activateOrInactivatePersonEntity(@RequestBody PersonEntityDto personEntityDto) {
		return conflictOfInterestService.activateOrInactivatePersonEntity(personEntityDto);
	}

	@GetMapping("/entity/relationshipTypes")
	public ResponseEntity<Object> fetchAllRelationshipTypes() {
		return conflictOfInterestService.fetchAllRelationshipTypes();
	}

	@PutMapping("/entity/approval")
	public ResponseEntity<Object> approveEntity(@RequestBody EntityRelationship entityRelationship) {
		return conflictOfInterestService.approveEntity(entityRelationship);
	}

	@PostMapping(value = "/loadTravelDisclosureHistory")
	public List<CoiTravelHistoryDto> loadTravelDisclosureHistory(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Request for loadTravelDisclosureHistory");
		return conflictOfInterestService.loadTravelDisclosureHistory(vo.getPersonId(), vo.getEntityNumber());
	}

	@PutMapping("/personEntity")
	public ResponseEntity<Object> updatePersonEntity(@RequestBody PersonEntityDto personEntityDto) {
		return conflictOfInterestService.updatePersonEntity(personEntityDto);
	}

	@DeleteMapping("/personEntity/relationship/{personEntityRelId}/{personEntityId}")
	public ResponseEntity<Object> deletePersonEntityRelationship(@PathVariable(name = "personEntityRelId") Integer personEntityRelId,
																 @PathVariable(name = "personEntityId") Integer personEntityId) {
		return conflictOfInterestService.deletePersonEntityRelationship(personEntityRelId, personEntityId);
	}

	@PostMapping("/personEntity/modify")
	public ResponseEntity<Object> modifyPersonEntity( @RequestBody PersonEntityDto personEntityDto) {
		return conflictOfInterestService.modifyPersonEntity(personEntityDto.getPersonEntityId());
	}

	@PutMapping("/personEntity/finalize")
	public ResponseEntity<Object> finalizePersonEntity(@RequestBody PersonEntityDto personEntityDto) {
		return conflictOfInterestService.finalizePersonEntity(personEntityDto);
	}

	@PostMapping(value = "/withdrawDisclosure")
    public ResponseEntity<Object> withdrawDisclosure(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Request for withdrawing Disclosure");
        return conflictOfInterestService.withdrawDisclosure(vo.getDisclosureId(), vo.getDescription());
    }

    @PostMapping(value = "/returnDisclosure")
    public ResponseEntity<Object> returnDisclosure(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Request for returning Disclosure");
        return conflictOfInterestService.returnDisclosure(vo.getDisclosureId(), vo.getDescription());
    }

    @GetMapping("/disclosureHistory/{disclosureId}")
	public ResponseEntity<Object> getDisclosureHistoryById(@PathVariable("disclosureId") Integer disclosureId) {
		return actionLogService.getDisclosureHistoryById(disclosureId);
	}

    @GetMapping("/getTravelConflictStatusType")
	public ResponseEntity<Object> getTravelConflictStatusType() {
		logger.info("Requesting for getTravelConflictStatusType");
		return conflictOfInterestService.getTravelConflictStatusType();
	}

    @PostMapping(value = "/manageTravelConflict")
    public ResponseEntity<Object> manageTravelConflict(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Request for managing travel conflict");
        return conflictOfInterestService.manageTravelConflict(vo);
    }

    @GetMapping("/loadTravelConflictHistory/{travelDisclosureId}")
	public List<CoiTravelConflictHistory> loadTravelConflictHistory(@PathVariable("travelDisclosureId") Integer travelDisclosureId) {
		logger.info("Request for loadTravelConflictHistory");
		return conflictOfInterestService.getCoiTravelConflictHistory(travelDisclosureId);
	}

    @GetMapping("/travelDisclosureHistory/{travelDisclosureId}")
	public ResponseEntity<Object> getTravelDisclosureHistoryById(@PathVariable("travelDisclosureId") Integer travelDisclosureId) {
		return actionLogService.getTravelDisclosureHistoryById(travelDisclosureId);
	}

    @PostMapping("/getCoiSectionsTypeCode")
	public ResponseEntity<Object> getCoiSectionsTypeCode(@RequestBody ConflictOfInterestVO vo) {
		logger.info("Requesting for getCoiSectionsTypeCode");
		return conflictOfInterestService.getCoiSectionsTypeCode(vo);
	}

    @PutMapping("/travelDisclosure/modifyRisk")
    public ResponseEntity<Object> modifyRisk(@RequestBody CoiTravelDisclosureDto travelDisclosureDto) {
        return conflictOfInterestService.modifyTravelDisclosureRisk(travelDisclosureDto);
    }

    @PostMapping("/travelDisclosure/history")
    public ResponseEntity<Object> fetchtravelDisclosureHistory(@RequestBody TravelDisclosureActionLogDto actionLogDto) {
        return conflictOfInterestService.fetchTravelDisclosureHistory(actionLogDto);
    }

    @GetMapping("/reviewHistory/{disclosureId}")
   	public ResponseEntity<Object> getReviewHistoryById(@PathVariable("disclosureId") Integer disclosureId) {
   		return actionLogService.getReviewHistoryById(disclosureId);
   	}

	@GetMapping("/personEntity/{personEntityNumber}/latestVersion")
	public ResponseEntity<Object> getSFILatestVersion(@PathVariable("personEntityNumber") Integer personEntityNumber) {
		return conflictOfInterestService.getSFILatestVersion(personEntityNumber);
	}

	@GetMapping("/loadDisclAttachTypes")
	public ResponseEntity<Object> loadDisclAttachTypes() {
		logger.info("Requesting for loadDisclAttachTypes");
		return conflictOfInterestService.loadDisclAttachTypes();
	}

	@PostMapping("/fetchAllActiolListEntriesForBanners")
	public List<Inbox> fetchAllActiolListEntriesForBanners(@RequestBody NotificationBannerDto notifyBannerDto) {
		logger.info("Request for fetchAllActiolListEntriesForBanners");
		return conflictOfInterestService.fetchAllActiolListEntriesForBanners(notifyBannerDto);
	}

	@PostMapping("/saveOrUpdatePersonNote")
	public ResponseEntity<Object> saveOrUpdatePersonNote(@RequestBody NotesDto dto) {
		logger.info("Request for saveOrUpdatePersonNote");
		return conflictOfInterestService.saveOrUpdatePersonNote(dto);
	}

    @GetMapping("/fetchAllNotesForPerson/{personId}")
   	public List<Notes> fetchAllNotesForPerson(@PathVariable("personId") String personId) {
    	logger.info("Request for fetchAllNotesForPerson");
   		return conflictOfInterestService.fetchAllNotesForPerson(personId);
   	}

    @GetMapping("/getNoteDetailsForNoteId/{noteId}")
   	public Notes getNoteDetailsForNoteId(@PathVariable("noteId") Integer noteId) {
    	logger.info("Request for getNoteDetailsForNoteId");
   		return conflictOfInterestService.getNoteDetailsForNoteId(noteId);
   	}

	@DeleteMapping(value = "/deleteNote/{noteId}")
	public ResponseEntity<Object> deleteNote(@PathVariable(value = "noteId", required = true) final Integer noteId) {
		logger.info("Requesting for deleteNote");
		return conflictOfInterestService.deleteNote(noteId);
	}

	@PostMapping(value = "/saveOrUpdateAttachments", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<Object> saveOrUpdateAttachments(@RequestParam(value = "files", required = false) MultipartFile[] files,
			@RequestParam("formDataJson") String formDataJson) {
		logger.info("Request for saveOrUpdateAttachments");
		return conflictOfInterestService.saveOrUpdateAttachments(files, formDataJson);
	}

    @GetMapping("/loadAllAttachmentsForPerson/{personId}")
   	public List<Attachments> loadAllAttachmentsForPerson(@PathVariable("personId") String personId) {
    	logger.info("Request for loadAllAttachmentsForPerson");
   		return conflictOfInterestService.loadAllAttachmentsForPerson(personId);
   	}

	@PostMapping("/personEntity/history")
	public ResponseEntity<Object> fetAllPersonEntityActionLog(@RequestBody PersonEntityDto personEntityDto) {
		logger.info("Requesting for fetAllPersonEntityActionLog");
		return actionLogService.getAllPersonEntityActionLog(personEntityDto);
	}

	@GetMapping("/getSFIRelationshipDetails")
   	public ResponseEntity<Object> getSFIRelationshipDetails() {
    	logger.info("Request for getSFIRelationshipDetails");
   		return conflictOfInterestService.getSFIRelationshipDetails();
   	}

}
