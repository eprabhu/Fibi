package com.polus.fibi.graphconnect.coi.v1.controller;

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

import com.polus.fibi.graphconnect.coi.v1.service.COIGraphService;
import com.polus.fibi.graphconnect.model.RequestDTO;
import com.polus.fibi.graphconnect.model.ResponseDTO;

@RestController
@RequestMapping("/coi/v1")
public class COIGraphController {
	
	@Autowired
	COIGraphService service;
	
	private static final Logger logger = LoggerFactory.getLogger(COIGraphController.class);
	
	@GetMapping("/ping")
	public String greetings() {
		logger.debug("In the ping Call");
		return "Hello from Graph Connect "+Runtime.getRuntime().availableProcessors();
	}			

    @GetMapping("import")
    String importFromRDBMS() { 
    		service.importDataFromRDBMS();
    	  	return "Success";
    }    
    @GetMapping("refresh")
    String refresh() { 
    		service.refreshDataFromRDBMS();
    	  	return "Success";
    }   
    @PostMapping("entitygraph")
    ResponseEntity<ResponseDTO> entityGraph(@RequestBody RequestDTO request) { 
	 ResponseDTO response = service.entityGraph(request);
	 return new ResponseEntity<ResponseDTO>(response, HttpStatus.OK);
  }   
    
    
}

