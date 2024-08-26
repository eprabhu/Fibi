package com.polus.integration.entity.cleansematch.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.entity.cleansematch.dto.DnBStageEntityMatchDTO;
import com.polus.integration.entity.cleansematch.service.BulkCleanseMatchService;

@RestController
@RequestMapping("cleansematch/bulk")
public class BulkCleanseMatchController {

	@Autowired
	BulkCleanseMatchService bulkCleanseMatchService;

	@GetMapping("/ping")
	public ResponseEntity<String> ping() {
		return new ResponseEntity<String>("Hello from cleansematch/bulk", HttpStatus.OK);
	}

	@GetMapping("/startBulkCleanseMatch")
	public ResponseEntity<String> startBulkCleanseMatch() {
		bulkCleanseMatchService.startBulkCleanseMatch();
		return new ResponseEntity<String>("Processing started", HttpStatus.OK);
	}

	@GetMapping("/getStatus")
	public ResponseEntity<List<DnBStageEntityMatchDTO>> getCompletedRecord() {
		List<DnBStageEntityMatchDTO> response = bulkCleanseMatchService.getCompletedRecord();
		return new ResponseEntity<List<DnBStageEntityMatchDTO>>(response, HttpStatus.OK);	
		
	}
   
	@GetMapping("/stopBulkCleanseMatch")
	public ResponseEntity<String> stopBulkCleanseMatch() {
		 bulkCleanseMatchService.stopBulkCleanseMatch();
		 return new ResponseEntity<String>("Processing stopped", HttpStatus.OK);		
	}
}
