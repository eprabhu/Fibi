package com.polus.fibicomp.opa.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
		if(Boolean.TRUE.equals(opaService.isOpaDisclosureRequired(dto.getPersonId()))) {
			return opaService.createOpaDisclosure(dto.getPersonId(), dto.getHomeUnit());
		}
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

}
