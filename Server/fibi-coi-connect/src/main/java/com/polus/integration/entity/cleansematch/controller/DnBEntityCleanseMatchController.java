package com.polus.integration.entity.cleansematch.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.entity.cleansematch.dto.DnBAPIResponse;
import com.polus.integration.entity.cleansematch.dto.EntityCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBEntityCleanseMatchRequestDTO;
import com.polus.integration.entity.cleansematch.service.DnBEntityCleanseMatchService;

@RestController
@RequestMapping("cleansematch/entity")
public class DnBEntityCleanseMatchController {

	@Autowired
	DnBEntityCleanseMatchService entityCleanseMatchService;

	@GetMapping("/ping")
	public ResponseEntity<String> ping() {
		return new ResponseEntity<String>("Hello from cleansematch/entity", HttpStatus.OK);
	}

	@GetMapping("runCleanseMatch")
	public ResponseEntity<EntityCleanseMatchAPIResponse> performCleanseMatch(
			@RequestBody DnBEntityCleanseMatchRequestDTO request) {
		EntityCleanseMatchAPIResponse response = entityCleanseMatchService.runCleanseMatch(request);
		return new ResponseEntity<EntityCleanseMatchAPIResponse>(response, HttpStatus.OK);

	}

	@GetMapping("runCleanseMatch1")
	public ResponseEntity<EntityCleanseMatchAPIResponse> performCleanseMatch() {
		DnBEntityCleanseMatchRequestDTO request = new DnBEntityCleanseMatchRequestDTO();
		request.setSourceDataName("Google");
		request.setCountryCode("US");
		EntityCleanseMatchAPIResponse response = entityCleanseMatchService.runCleanseMatch(request);
		return new ResponseEntity<EntityCleanseMatchAPIResponse>(response, HttpStatus.OK);

	}

}
