package com.polus.integration.entity.cleansematch.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse;
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
	public ResponseEntity<DnBCleanseMatchAPIResponse> performCleanseMatch(@RequestBody DnBEntityCleanseMatchRequestDTO request) {
		DnBCleanseMatchAPIResponse response = entityCleanseMatchService.runCleanseMatch(request);
		return new ResponseEntity<DnBCleanseMatchAPIResponse>(response, HttpStatus.OK);
		
	}
	
	@GetMapping("runCleanseMatchWithRawResponse")
	public ResponseEntity<String> performDirectCleanseMatch(@RequestBody DnBEntityCleanseMatchRequestDTO request) {
		String response = entityCleanseMatchService.runCleanseMatchWithRawResponse(request);
		return new ResponseEntity<String>(response, HttpStatus.OK);
		
	}
	
	@GetMapping("runCleanseMatch1")
	public ResponseEntity<DnBCleanseMatchAPIResponse> performCleanseMatch() {
		DnBEntityCleanseMatchRequestDTO request = new DnBEntityCleanseMatchRequestDTO();
		request.setSourceDataName("Google");
		request.setCountryCode("US");
		DnBCleanseMatchAPIResponse response = entityCleanseMatchService.runCleanseMatch(request);
		return new ResponseEntity<DnBCleanseMatchAPIResponse>(response, HttpStatus.OK);
		
	}
	
	@GetMapping("runCleanseMatch2")
	public ResponseEntity<String> performCleanseMatch2() {
		DnBEntityCleanseMatchRequestDTO request = new DnBEntityCleanseMatchRequestDTO();
		request.setSourceDataName("Google");
		request.setCountryCode("US");
		String response = entityCleanseMatchService.runCleanseMatchWithRawResponse(request);
		return new ResponseEntity<String>(response, HttpStatus.OK);
		
	}

}
