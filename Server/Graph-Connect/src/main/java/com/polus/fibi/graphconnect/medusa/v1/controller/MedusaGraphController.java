package com.polus.fibi.graphconnect.medusa.v1.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibi.graphconnect.medusa.v1.service.MedusaGraphService;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;

@RestController
@RequestMapping("/medusa/v1")
public class MedusaGraphController {

	@Autowired
	MedusaGraphService service;

	private static final Logger logger = LoggerFactory.getLogger(MedusaGraphController.class);

	@GetMapping("/ping")
	public String greetings() {
		logger.debug("In the ping Call");
		return "Hello from Medusa Graph Connect " + Runtime.getRuntime().availableProcessors();
	}

	@PostMapping("import")
	String importFromRDBMS() {
		service.importDataFromRDBMS();
		return "Success";
	}

	@PostMapping("refresh")
	String refresh() {
		service.refreshDataFromRDBMS();
		return "Success";
	}

	@PostMapping("medusaGraph")
	ResponseEntity<ResponseDTO> medusaGraph(@RequestBody RequestDTO request) {
		ResponseDTO response = service.medusaGraph(request);
		return new ResponseEntity<>(response, HttpStatus.OK);
	}

}
