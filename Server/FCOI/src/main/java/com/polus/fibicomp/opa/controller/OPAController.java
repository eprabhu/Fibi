package com.polus.fibicomp.opa.controller;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.coi.service.ActionLogService;
import com.polus.fibicomp.opa.dto.CreateOpaDto;
import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.opa.service.OPAService;

@RestController
@RequestMapping("/opa")
public class OPAController {

	protected static Logger logger = LogManager.getLogger(OPAController.class.getName());

	@Autowired
	private OPAService opaService;

	@Autowired
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private ActionLogService actionLogService;

	@PostMapping("/createOPA")
	public ResponseEntity<Object> createOPADisclosure(@RequestBody CreateOpaDto dto) {
		logger.info("Request for createOPADisclosure");
		if(Boolean.TRUE.equals(opaService.canCreateOpaDisclosure(dto.getPersonId()))) {
			return opaService.createOpaDisclosure(dto.getPersonId(), dto.getHomeUnit());
		} else {
			return new ResponseEntity<>("Person has no right/entry to create OPA", HttpStatus.NO_CONTENT);
		}
	}

	@PatchMapping("/submit")
	public ResponseEntity<Object> submitOPADisclosure(@RequestBody OPASubmitDto opaSubmitDto) {
		logger.info("Request for submitOPADisclosure");
		return opaService.submitOPADisclosure(opaSubmitDto);
	}

	@PatchMapping("/withdraw")
	public ResponseEntity<Object> withdrawOPADisclosure(@RequestBody OPACommonDto opaCommonDto) {
		logger.info("Request for withdrawOPADisclosure");
		return opaService.withdrawOPADisclosure(opaCommonDto);
	}

	@PatchMapping("/return")
	public ResponseEntity<Object> returnOPADisclosure(@RequestBody OPACommonDto opaCommonDto) {
		logger.info("Request for returnOPADisclosure");
		return opaService.returnOPADisclosure(opaCommonDto);
	}

	@PatchMapping("/assignAdmin")
	public ResponseEntity<Object> assignAdminOPADisclosure(@RequestBody OPAAssignAdminDto assignAdminDto) {
		logger.info("Request for assignAdminOPADisclosure");
		return opaService.assignAdminOPADisclosure(assignAdminDto);
	}

	@PatchMapping("/complete/{opaDisclosureId}/{opaDisclosureNumber}")
	public ResponseEntity<Object> completeOPADisclosure(@PathVariable("opaDisclosureId") Integer opaDisclosureId,
														@PathVariable("opaDisclosureNumber") String opaDisclosureNumber) {
		logger.info("Request for completeOPADisclosure");
		return opaService.completeOPADisclosure(opaDisclosureId, opaDisclosureNumber);
	}

	@GetMapping("/getOPADisclosureHeader/{opaDisclosureId}")
	public ResponseEntity<Object> getOPADisclosure(@PathVariable("opaDisclosureId") Integer opaDisclosureId) {
		logger.info("Request for getOPADisclosure");
		if (!documentAuthorization.isAuthorized(Constants.OPA_MODULE_CODE, opaDisclosureId.toString(), AuthenticatedUser.getLoginPersonId())) {
			return new ResponseEntity<>("Not Authorized to view this Disclosure",HttpStatus.FORBIDDEN);
		}
		return opaService.getOPADisclosure(opaDisclosureId);
	}

	@PostMapping("/dashboard")
	public ResponseEntity<Object> dashboard(@RequestBody OPADashboardRequestDto requestDto) {
		logger.info("Request for opa dashboard");
		return opaService.getOPADashboard(requestDto);
	}

	@GetMapping("/opaDisclosureHistory/{opaDisclosureId}")
	public ResponseEntity<Object> geOpatDisclosureHistoryById(@PathVariable("opaDisclosureId") Integer opaDisclosureId) {
		return actionLogService.getOpaDisclosureHistoryById(opaDisclosureId);
	}

	@GetMapping("/getOpaPersonType")
	public ResponseEntity<Object> getOpaPersonType() {
		return opaService.getOpaPersonType();
	}

}
