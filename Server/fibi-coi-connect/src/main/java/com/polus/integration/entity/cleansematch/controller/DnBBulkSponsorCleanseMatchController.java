package com.polus.integration.entity.cleansematch.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.entity.cleansematch.dto.StageDnBEntityMatchDTO;
import com.polus.integration.entity.cleansematch.entity.StageDnBEntityMatch;
import com.polus.integration.entity.cleansematch.service.DnBBulkCleanseMatchService;

@RestController
@RequestMapping("cleansematch/bulk")
public class DnBBulkSponsorCleanseMatchController {

	@Autowired
	DnBBulkCleanseMatchService bulkCleanseMatchService;

	@GetMapping("/ping")
	public ResponseEntity<String> ping() {
		return ResponseEntity.accepted().body("Hello from cleansematch/bulk");
	}

	@GetMapping("/startBulkCleanseMatch")
	public ResponseEntity<String> startBulkCleanseMatch() {
		bulkCleanseMatchService.startBulkCleanseMatch();
		return ResponseEntity.accepted().body("Processing started");
	}

	@GetMapping("/getStatus")
	public List<StageDnBEntityMatchDTO> getCompletedRecord() {
		return bulkCleanseMatchService.getCompletedRecord();
	}
   
	@GetMapping("/stopBulkCleanseMatch")
	public ResponseEntity<String> stopBulkCleanseMatch() {
		 bulkCleanseMatchService.stopBulkCleanseMatch();
		 return ResponseEntity.accepted().body("Processing stopped");
	}
}
