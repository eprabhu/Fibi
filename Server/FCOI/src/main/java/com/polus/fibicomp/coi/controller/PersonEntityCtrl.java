package com.polus.fibicomp.coi.controller;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.coi.dto.PersonEntityDto;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.coi.service.PersonEntityService;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/coi/personEntity")
public class PersonEntityCtrl {

    protected static Logger logger = LogManager.getLogger(PersonEntityCtrl.class.getName());

    @Autowired
    private ActionLogService actionLogService;

    @Autowired
    private PersonEntityService personEntityService;

    @Autowired
    private UserDocumentAuthorization documentAuthorization;

    @PostMapping
    public ResponseEntity<Object> createSFI(@RequestBody PersonEntity personEntity) {
        logger.info("Requesting for createSFI");
        return personEntityService.createPersonEntity(personEntity);
    }

    @PostMapping("/addRelationship")
    public ResponseEntity<Object> saveCoiFinancialEntityDetails(@RequestBody PersonEntityRelationship vo) {
        logger.info("Request for saveOrUpdateCoiFinancialEntityDetails");
        return personEntityService.saveOrUpdatePersonEntityRelationship(vo);
    }

    @GetMapping("/{personEntityId}")
    public ResponseEntity<Object> getPersonEntityDetails(@PathVariable("personEntityId") Integer personEntityId) {
        logger.info("Requesting for getPersonEntityDetails");
        if (!documentAuthorization.isAuthorized(Constants.COI_MODULE_CODE, String.valueOf(personEntityId), AuthenticatedUser.getLoginPersonId(),
                Constants.COI_SFI_SUBMODULE_CODE, null)) {
            return new ResponseEntity<>("Not Authorized to view this Disclosure", HttpStatus.FORBIDDEN);
        }
        return personEntityService.getPersonEntityDetails(personEntityId);
    }

    @PostMapping(value = "/dashboard")
    public ResponseEntity<Object> getPersonEntityDashboard(@RequestBody CoiDashboardVO vo) {
        logger.info("Requesting for getPersonEntityDetails");
        return personEntityService.getPersonEntityDashboard(vo);
    }

    @PutMapping
    public ResponseEntity<Object> updatePersonEntity(@RequestBody PersonEntityDto personEntityDto) {
        return personEntityService.updatePersonEntity(personEntityDto);
    }

    @DeleteMapping("/relationship/{personEntityRelId}/{personEntityId}")
    public ResponseEntity<Object> deletePersonEntityRelationship(@PathVariable(name = "personEntityRelId") Integer personEntityRelId,
                                                                 @PathVariable(name = "personEntityId") Integer personEntityId) {
        return personEntityService.deletePersonEntityRelationship(personEntityRelId, personEntityId);
    }

    @PutMapping("/activateInactivate")
    public ResponseEntity<Object> activateOrInactivatePersonEntity(@RequestBody PersonEntityDto personEntityDto) {
        return personEntityService.activateOrInactivatePersonEntity(personEntityDto);
    }

    @DeleteMapping("/{personEntityId}")
    public ResponseEntity<Object>deletePersonEntity(@PathVariable("personEntityId") Integer personEntityId) {
        return personEntityService.deletePersonEntity(personEntityId);
    }

    @PostMapping("/getRelationship")
    public ResponseEntity<Object> getPersonEntityRelationship(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Requesting for getPersonEntityRelationship");
        return personEntityService.getPersonEntityRelationship(vo);
    }

    @GetMapping("/{personEntityNumber}/latestVersion")
    public ResponseEntity<Object> getSFILatestVersion(@PathVariable("personEntityNumber") Integer personEntityNumber) {
        return personEntityService.getSFILatestVersion(personEntityNumber);
    }

    @GetMapping("/versions/{personEntityNumber}")
    public ResponseEntity<Object> getAllPersonEntityVersions(@PathVariable("personEntityNumber") Integer personEntityNumber) {
        logger.info("Requesting for getAllPersonEntityVersions");
        return personEntityService.getAllPersonEntityVersions(personEntityNumber);
    }

    @PostMapping("/history")
    public ResponseEntity<Object> fetAllPersonEntityActionLog(@RequestBody PersonEntityDto personEntityDto) {
        logger.info("Requesting for fetAllPersonEntityActionLog");
        return actionLogService.getAllPersonEntityActionLog(personEntityDto);
    }

    @PatchMapping("/checkFormCompleted/{personEntityId}")
    public ResponseEntity<Map<String, Object>> updatePersonEntityCompleteFlag(@PathVariable("personEntityId") Integer personEntityId) {
        logger.info("Requesting for updatePersonEntityCompleteFlag");
        return personEntityService.updatePersonEntityCompleteFlag(personEntityId);
    }

    @PostMapping("/fetch")
    public ResponseEntity<Object> getSFIOfDisclosure(@RequestBody ConflictOfInterestVO vo) {
        logger.info("Requesting for getSFIOfDisclosure");
        return personEntityService.getSFIOfDisclosure(vo);
    }

    @GetMapping("/details/{coiFinancialEntityId}")
    public ResponseEntity<Object> getSFIDetails(@PathVariable("coiFinancialEntityId") Integer coiFinancialEntityId) {
        logger.info("Requesting for getSFIDetails");
        return personEntityService.getSFIDetails(coiFinancialEntityId);
    }

    @PostMapping("/modify")
    public ResponseEntity<Object> modifyPersonEntity( @RequestBody PersonEntityDto personEntityDto) {
        logger.info("Requesting for modifyPersonEntity");
        return personEntityService.modifyPersonEntity(personEntityDto);
    }
}
