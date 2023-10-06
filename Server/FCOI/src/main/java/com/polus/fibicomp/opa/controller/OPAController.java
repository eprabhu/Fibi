package com.polus.fibicomp.opa.controller;

import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PatchMapping;

import com.polus.fibicomp.opa.dto.CreateOpaDto;
import com.polus.fibicomp.opa.service.OPAService;

@RestController
@RequestMapping("/opa")
public class OPAController {

	protected static Logger logger = LogManager.getLogger(OPAController.class.getName());

	@Autowired
	private OPAService opaService;

	@PostMapping("/createOPA")
	public ResponseEntity<Object> createOPADisclosure(@RequestBody CreateOpaDto dto) {
		logger.info("Request for createOPADisclosure");
		if(Boolean.TRUE.equals(opaService.canCreateOpaDisclosure(dto.getPersonId()))) {
			return opaService.createOpaDisclosure(dto.getPersonId(), dto.getHomeUnit());
		}
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

	@PatchMapping("/submit")
	public ResponseEntity<Object> submitOPADisclosure(@RequestBody OPASubmitDto opaSubmitDto) {
		logger.info("Request for submitOPADisclosure");
		return opaService.submitOPADisclosure(opaSubmitDto);
	}

	@PatchMapping("/withdraw/{opaDisclosureId}/{opaDisclosureNumber}")
	public ResponseEntity<Object> withdrawOPADisclosure(@PathVariable("opaDisclosureId") Integer opaDisclosureId,
														@PathVariable("opaDisclosureNumber") String opaDisclosureNumber) {
		logger.info("Request for withdrawOPADisclosure");
		return opaService.withdrawOPADisclosure(opaDisclosureId, opaDisclosureNumber);
	}

	@PatchMapping("/return/{opaDisclosureId}/{opaDisclosureNumber}")
	public ResponseEntity<Object> returnOPADisclosure(@PathVariable("opaDisclosureId") Integer opaDisclosureId,
													  @PathVariable("opaDisclosureNumber") String opaDisclosureNumber) {
		logger.info("Request for returnOPADisclosure");
		return opaService.returnOPADisclosure(opaDisclosureId, opaDisclosureNumber);
	}

	@PostMapping("/assignAdmin")
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

	@PostMapping("/reassignAdmin")
	public ResponseEntity<Object> reassignAdminOPADisclosure(@RequestBody OPAAssignAdminDto assignAdminDto) {
		logger.info("Request for reassignAdminOPADisclosure");
		return opaService.reassignAdminOPADisclosure(assignAdminDto);
	}
}
