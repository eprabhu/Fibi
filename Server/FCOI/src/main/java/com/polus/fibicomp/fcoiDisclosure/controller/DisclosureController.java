package com.polus.fibicomp.fcoiDisclosure.controller;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.coi.controller.ConflictOfInterestController;
import com.polus.fibicomp.coi.dto.DisclosureProjectDto;
import com.polus.fibicomp.coi.dto.PersonEntityRelationshipDto;
import com.polus.fibicomp.coi.vo.CoiDashboardVO;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fcoiDisclosure.dto.ProjectEntityRequestDto;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiDisclProjectEntityRel;
import com.polus.fibicomp.fcoiDisclosure.service.FcoiDisclosureService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.coi.dto.CoiDisclosureDto;
import com.polus.fibicomp.coi.dto.DisclosureActionLogDto;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;

import java.util.List;

@RestController
@RequestMapping("/coi/fcoiDisclosure")
public class DisclosureController {

    protected static Logger logger = LogManager.getLogger(DisclosureController.class.getName());

    @Autowired
    private ConflictOfInterestService conflictOfInterestService;

    @Autowired
    private FcoiDisclosureService disclosureService;

    @Autowired
    private UserDocumentAuthorization documentAuthorization;

    //  /createDisclosure
    @PostMapping
    public ResponseEntity<Object> createDisclosure(@RequestBody CoiDisclosureDto vo) throws JsonProcessingException {
        logger.info("Request for createDisclosure");
        return disclosureService.createDisclosure(vo);
    }

    //loadDisclosure
    @GetMapping("/fetch/{disclosureId}")
    public ResponseEntity<Object> loadDisclosure(@PathVariable("disclosureId") Integer disclosureId) {
        logger.info("Request for loadDisclosure");
        if (!documentAuthorization.isAuthorized(Constants.COI_MODULE_CODE, disclosureId.toString(), AuthenticatedUser.getLoginPersonId())) {
            return new ResponseEntity<>("Not Authorized to view this Disclosure", HttpStatus.FORBIDDEN);
        }
        return disclosureService.loadDisclosure(disclosureId);
    }

    @PatchMapping("/certifyDisclosure")
    public ResponseEntity<Object> certifyDisclosure(@RequestBody CoiDisclosureDto coiDisclosureDto) {
        logger.info("Requesting for certifyDisclosure");
        return disclosureService.certifyDisclosure(coiDisclosureDto);
    }

    @PostMapping("/project/relations")
    public ResponseEntity<Object> getDisclosureProjectRelations(@RequestBody ProjectEntityRequestDto vo) throws JsonProcessingException {
        logger.info("Requesting for /project/relations");
        List<DisclosureProjectDto> disclProjects = disclosureService.getDisclProjectEntityRelations(vo);
        if (disclProjects.isEmpty()) {
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        }
        return new ResponseEntity<>(getObjectMapper().writeValueAsString(disclProjects), HttpStatus.OK);
    }

    @PostMapping("/entity/relations")
    public ResponseEntity<Object> getDisclosureEntityRelations(@RequestBody ProjectEntityRequestDto vo) throws JsonProcessingException {
        logger.info("Requesting for /entity/relations");
        List<PersonEntityRelationshipDto> disclPersonEntities = disclosureService.getDisclosureEntityRelations(vo);
        if (disclPersonEntities.isEmpty()) {
            return new ResponseEntity<>(HttpStatus.NO_CONTENT);
        }
        return new ResponseEntity<>(getObjectMapper().writeValueAsString(disclPersonEntities), HttpStatus.OK);
    }

    public ObjectMapper getObjectMapper() {
        ObjectMapper objectMapper = new ObjectMapper();
//        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        return objectMapper;
    }

    @PostMapping("/relation/conflict")
    public ProjectEntityRequestDto applyAllDisclosureConflict(@RequestBody ProjectEntityRequestDto vo) {
        logger.info("Requesting for saveEntityProjectRelation");
        return disclosureService.saveDisclosureConflict(vo);
    }

    @PostMapping("/reviseDisclosure")
    public ResponseEntity<Object> reviseDisclosure(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Requesting for reviseDisclosure");
        return disclosureService.reviseDisclosure(vo);
    }


    @PostMapping("/historyDashboard")
    public ResponseEntity<Object> getDisclosureHistory(@RequestBody CoiDashboardVO dashboardVO) {
        return conflictOfInterestService.getDisclosureHistory(dashboardVO);
    }

    @PutMapping("/modifyRisk")
    public ResponseEntity<Object> modifyRisk(@RequestBody CoiDisclosureDto disclosureDto) {
        return disclosureService.modifyDisclosureRisk(disclosureDto);
    }

    @GetMapping("/risk")
    public ResponseEntity<Object> fetchAllDisclosureRisk() {
        return disclosureService.fetchAllDisclosureRisk();
    }

    @PostMapping("/history")
    public ResponseEntity<Object> fetchDisclosureHistory(@RequestBody DisclosureActionLogDto actionLogDto) {
        return disclosureService.fetchDisclosureHistory(actionLogDto);
    }

    @PostMapping("/riskStatus")
    public ResponseEntity<Object> checkRiskStatus(@RequestBody CoiDisclosureDto disclosureDto) {
        return disclosureService.checkDisclosureRiskStatus(disclosureDto);
    }

    @GetMapping("/projects/{disclosureId}")
    public ResponseEntity<Object>getDisclosureProjects(@PathVariable("disclosureId") Integer disclosureId) {
        return disclosureService.getDisclosureProjects(disclosureId);
    }

    @GetMapping("/lookups")
    public ResponseEntity<Object>getDisclosureLookups() {
        return disclosureService.getDisclosureLookups();
    }

    @PostMapping("/evaluateDisclosureQuestionnaire")
    public Boolean evaluateDisclosureQuestionnaire(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Request for evaluateDisclosureQuestionnaire");
        return disclosureService.evaluateDisclosureQuestionnaire(vo);
    }

    @PostMapping(value = "/updateProjectRelationship")
    public ResponseEntity<Object> updateProjectRelationship(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Request for updateProjectRelationship");
        return disclosureService.updateProjectRelationship(vo);
    }

    @GetMapping("/validateConflicts/{disclosureId}")
    public ResponseEntity<Object> validateConflicts(@PathVariable("disclosureId") Integer disclosureId) {
        logger.info("Requesting for validateConflicts");
        return disclosureService.validateConflicts(disclosureId);
    }

    @GetMapping("/validate/{moduleCode}/{moduleItemId}")
    public ResponseEntity<Object> validateDisclosure(@PathVariable("moduleCode") Integer moduleCode,
                                                     @PathVariable("moduleItemId") String moduleItemId) {
        return disclosureService.validateDisclosure(moduleCode, moduleItemId);
    }

    @PatchMapping("/assignAdmin")
    public ResponseEntity<Object> assignDisclosureAdmin(@RequestBody CoiDisclosureDto dto) {
        return disclosureService.assignDisclosureAdmin(dto);
    }

    @PostMapping("/sync")
    public void syncFCOIDisclosure(@RequestBody CoiDisclosureDto coiDisclosureDto) {
        disclosureService.syncFCOIDisclosure(coiDisclosureDto);
    }

    @GetMapping("/evaluateValidation/{disclosureId}/{disclosureNumber}")
    public ResponseEntity<Object> evaluateValidation(@PathVariable("disclosureId") Integer disclosureId,
                                                     @PathVariable("disclosureId") Integer disclosureNumber) {
        logger.info("Requesting for evaluateValidation");
        return disclosureService.evaluateValidation(disclosureId, disclosureNumber);
    }

    @PutMapping("/integration/syncNeeded")
    public void updateFcoiDisclSyncNeedStatus(@RequestBody DisclosureProjectDto projectDto) {
        disclosureService.updateFcoiDisclSyncNeedStatus(projectDto);
    }
}
